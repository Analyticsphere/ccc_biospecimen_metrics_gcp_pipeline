# bsi_recurring_functions.R
# Contains reusable functions for BSI recurring metrics
# ------------------------------------------------------------

# ===============================
# DATA IMPORT
# ===============================

read_bsi_gcs <- function(download_date = Sys.Date(), bucket = NULL) {
  
  if (!is.null(bucket)) {
    gcs_global_bucket(bucket)
  }
  
  download_date <- as.Date(download_date)
  if (is.na(download_date)) {
    stop("`download_date` must be coercible to Date.")
  }
  
  date_folder <- format(download_date, "%Y-%m-%d")
  prefix <- glue::glue("{date_folder}/")
  
  objects <- gcs_list_objects(prefix = prefix)
  
  if (nrow(objects) == 0) {
    stop(glue::glue("No objects found for prefix: {prefix}"))
  }
  
  csv_objects <- objects %>%
    filter(
      str_detect(name, regex("\\.csv$", ignore_case = TRUE))
    )
  
  if (nrow(csv_objects) == 0) {
    stop(glue::glue("No CSV files found for prefix: {prefix}"))
  }
  
  data_raw <- csv_objects$name %>%
    purrr::map_dfr(function(obj) {
      
      tf <- tempfile(fileext = ".csv")
      
      gcs_get_object(
        object_name = obj,
        saveToDisk  = tf,
        overwrite   = TRUE
      )
      
      readr::read_csv(tf, col_types = readr::cols(.default = "c")) %>%
        janitor::clean_names() %>%
        mutate(source_file = basename(obj))
    }) %>%
    mutate(
      across(c(date_drawn, date_received, date_processed),
             ~ na_if(.x, "")),
      across(c(date_drawn, date_received, date_processed),
             ~ stringr::str_trim(.x)),
      date_drawn     = lubridate::mdy_hm(date_drawn),
      date_received  = lubridate::mdy_hm(date_received),
      date_processed = lubridate::mdy_hm(date_processed),
      data_delivery_date = download_date
    )
  
  return(data_raw)
}


# ===============================
# RM 1
# ===============================

rm_1 <- function(data, label = NULL) {
  
  by_setting <- data %>%
    group_by(collection_setting) %>%
    summarise(
      data_delivery_date = max(data_delivery_date, na.rm = TRUE),
      start_date = min(date_received, na.rm = TRUE),
      end_date   = max(date_received, na.rm = TRUE),
      n_tubes = n_distinct(bsi_id),
      n_participants = n_distinct(subject_id),
      .groups = "drop"
    )
  
  total_row <- data %>%
    summarise(
      data_delivery_date = max(data_delivery_date, na.rm = TRUE),
      collection_setting = "Total",
      start_date = min(date_received, na.rm = TRUE),
      end_date   = max(date_received, na.rm = TRUE),
      n_tubes = n_distinct(bsi_id),
      n_participants = n_distinct(subject_id)
    )
  
  bind_rows(by_setting, total_row) %>%
    select(data_delivery_date,
           collection_setting,
           start_date,
           end_date,
           n_tubes,
           n_participants) %>%
    rename(
      "BSI Download Date" = data_delivery_date,
      "Collection Setting" = collection_setting,
      "BPTL Receipt Date Range - Start" = start_date,
      "BPTL Receipt Date Range - End" = end_date,
      "Number of Tubes" = n_tubes,
      "Number of Participants" = n_participants
    )
}

# ===============================
# RM 2
# ===============================

rm_2 <- function(data, data_bio, download_date) {
  
  end_thursday <- floor_date(download_date, unit = "week", week_start = 5) - days(1)
  start_friday <- end_thursday - days(6)
  
  data_bio_week <- data_bio %>%
    filter(
      !is.na(tube_received_date_connect),
      as.Date(tube_received_date_connect) >= start_friday,
      as.Date(tube_received_date_connect) <= end_thursday
    )
  
  missing_BSI <- anti_join(
    data_bio_week %>% select(tube_id_connect),
    data %>% select(bsi_id),
    by = c("tube_id_connect" = "bsi_id")
  )
  
  if (nrow(missing_BSI) == 0) {
    return(
      data.frame(
        status = "All expected parent tubes received at BPTL are in BSI",
        stringsAsFactors = FALSE
      )
    )
  }
  
  rm2 <- data_bio %>%
    filter(tube_id_connect %in% missing_BSI$tube_id_connect) %>%
    select(Connect_ID, tube_id_connect, tube_received_date_connect) %>%
    filter(
      !is.na(tube_received_date_connect),
      as.Date(tube_received_date_connect) < download_date
    )
  
  return(rm2)
}


# ===============================
# RM 3
# ===============================


rm_3 <- function(df, output_dir = here::here("bsi_outputs", "rm3_outputs")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1. Categorize processing time
  df_cat <- df %>%
    mutate(
      processing_category = case_when(
        is.na(days_to_process) ~ "Not processed",
        days_to_process <= 1   ~ "<= 1 day",
        days_to_process == 2   ~ "2 days",
        days_to_process >= 3 & days_to_process <= 5 ~ "3–5 days",
        days_to_process > 5    ~ "> 5 days"
      ),
      processing_category = factor(
        processing_category,
        levels = c("<= 1 day", "2 days", "3–5 days", "> 5 days", "Not processed")
      )
    )
  
  # 2. Distribution table
  plot_data <- df_cat %>%
    count(tube_type, processing_category, name = "n") %>%
    complete(
      tube_type,
      processing_category,
      fill = list(n = 0)
    ) %>%
    group_by(tube_type) %>%
    mutate(
      total = sum(n),
      pct = if_else(total > 0, n / total, 0)
    ) %>%
    ungroup() %>%
    group_by(tube_type) %>%
    mutate(
      tube_label = paste0(tube_type, " (N = ", unique(total), ")")
    ) %>%
    ungroup()
  
  # 3. Plot
  plot <- ggplot(plot_data,
                 aes(x = processing_category, y = n)) +
    geom_col() +
    geom_text(
      aes(label = paste0(n, " (", scales::percent(pct, accuracy = 0.1), ")")),
      vjust = -0.3,
      size = 3
    ) +
    facet_wrap(~ tube_label, scales = "free_y") +
    labs(
      title = glue("Distribution of Time from Receipt at BPTL to Processing {start_friday} - {end_thursday}"),
      x = "Time to Processing",
      y = "Number of Samples",
      caption = "Blood and urine are expected to be processed within 48 hours and mouthwash within 10 days."
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4. Exceptions
  exceptions <- df_cat %>%
    filter(
      (tube_type == "Mouthwash" &
         (processing_category == "Not processed" | days_to_process > 5)) |
        (tube_type != "Mouthwash" &
           (processing_category == "Not processed" | days_to_process >= 3))
    ) %>%
    select(
      bsi_id,
      date_received,
      days_to_process,
      processing_category,
      saturday_receipt,
      tube_type
    ) %>%
    arrange(tube_type, date_received)
  
  # format outputs
  
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      gsub("[\u2010-\u2015\u2212]", "-", x, perl = TRUE)
    } else x
  })
  
  # ---- WRITE OUTPUTS ----
  plot_path <- file.path(
    output_dir,
    glue("RM3_processing_time_distribution_{download_date}.png")
  )
  
  table_path <- file.path(
    output_dir,
    glue("RM3_processing_time_table_{download_date}.csv")
  )
  
  exceptions_path <- file.path(
    output_dir,
    glue("RM3_exceptions_{download_date}.csv")
  )
  
  ggsave(plot_path, plot, width = 12, height = 8, dpi = 300)
  write.csv(plot_data, table_path, row.names = FALSE)
  write.csv(exceptions, exceptions_path, row.names = FALSE)
  
  # 5. Return structured output
  list(
    plot = plot,
    plot_data = plot_data,
    exceptions = exceptions,
    files = list(
      plot = plot_path,
      distribution_table = table_path,
      exceptions = exceptions_path
    )
  )
}


# ===============================
# RM 4
# ===============================

rm_4 <- function(df) {
  
  results <- df %>%
    filter(!is.na(vial_warnings), vial_warnings != "") %>%
    select(bsi_id, date_received, vial_warning = vial_warnings)
  
  if (nrow(results) == 0) {
    return(
      tibble::tibble(
        message = "No vial warnings found."
      )
    )
  }
  
  return(results)
}


# ===============================
# RM 5
# ===============================

rm_5 <- function(df, bsi_pull_date) {
  
  bsi_pull_date <- as.Date(bsi_pull_date)
  
  bad_dates <- df %>%
    filter(
      is.na(date_received) |
        is.na(date_processed) |
        date_processed < date_received |
        date_processed > bsi_pull_date
    ) %>%
    select(bsi_id, date_received, date_processed)
  
  if (nrow(bad_dates) == 0) {
    return(
      tibble::tibble(
        message = "No incorrect processing dates were found."
      )
    )
  }
  
  return(bad_dates)
}


# ===============================
# RM 6
# ===============================

rm_6 <- function(df) {
  
  valid_material_types <- c("WHOLE BL", "URINE", "SALIVA")
  
  results <- df %>%
    filter(is.na(material_type) |
             !material_type %in% valid_material_types) %>%
    select(bsi_id, material_type, additive_preservative)
  
  if (nrow(results) == 0) {
    return(
      tibble::tibble(
        message = "No unexpected material types were found."
      )
    )
  }
  
  return(results)
}


# ===============================
# RM 7
# ===============================
rm_7 <- function(df) {
  
  df <- df %>%
    mutate(date_received = as.Date(date_received))
  
  # Full sequence of dates in the data
  all_dates <- seq(
    min(df$date_received, na.rm = TRUE),
    max(df$date_received, na.rm = TRUE),
    by = "day"
  )
  
  result <- df %>%
    mutate(
      date_label = paste0(
        "Received on ",
        format(date_received, "%Y-%m-%d"),
        " (",
        wday(date_received, label = TRUE, abbr = TRUE),
        ")"
      )
    ) %>%
    count(
      tube_type,
      date_label,
      name = "n_tubes"
    ) %>%
    complete(
      tube_type,
      date_label = paste0(
        "Received on ",
        format(all_dates, "%Y-%m-%d"),
        " (",
        wday(all_dates, label = TRUE, abbr = TRUE),
        ")"
      ),
      fill = list(n_tubes = 0)
    ) %>%
    pivot_wider(
      names_from  = date_label,
      values_from = n_tubes,
      values_fill = 0
    ) %>%
    mutate(
      Total = rowSums(across(where(is.numeric)))
    ) %>%
    arrange(
      factor(
        tube_type,
        levels = c("SST", "EDTA", "Streck", "Urine", "Mouthwash")
      )
    )
  
  # ---- ADD TOTAL ROW ----
  
  numeric_cols <- result %>% select(where(is.numeric))
  
  total_row <-  result %>%
    summarise(
      across(where(is.numeric), \(x) sum(x, na.rm = TRUE))
    ) %>%
    mutate(tube_type = "Total") %>%
    select(tube_type, everything())
  
  bind_rows(result, total_row)
}



# ===============================
# RM 8
# ===============================


rm_8_1 <- function(df) {
  parent_child_map <- tibble::tribble(
    ~tube_type, ~expected_child_tube_type,
    "SST",      "Serum",
    "EDTA",     "EDTA plasma",
    "EDTA",     "EDTA buffy coat/RBC",
    "Streck",   "Streck plasma",
    "Streck",   "Streck buffy coat/RBC",
    "Mouthwash", "Mouthwash",
    "Urine",     "Urine"
  )
  
  parents <- df %>%
    filter(
      parent_tube == TRUE,
      unprocessed == FALSE,
      vial_status != "Discarded"
    ) %>%
    select(sample_id, bsi_id, tube_type) %>%
    left_join(parent_child_map, by = "tube_type")
  
  child_parent_links <- df %>%
    filter(
      parent_tube == FALSE,
      vial_status != "Discarded"
    ) %>%
    select(sample_id, tube_type) %>%
    distinct() %>%
    rename(child_tube_type = tube_type)
  
  parent_status <- parents %>%
    left_join(
      child_parent_links %>% mutate(has_child_vial = TRUE),
      by = c(
        "sample_id" = "sample_id",
        "expected_child_tube_type" = "child_tube_type"
      )
    ) %>%
    group_by(sample_id, bsi_id, tube_type) %>%
    summarise(
      has_child_vial = any(coalesce(has_child_vial, FALSE)),
      .groups = "drop"
    )
  
  summary_table <- parent_status %>%
    group_by(tube_type) %>%
    summarise(
      parent_tube_count = n(),
      parents_with_child_vial_count = sum(has_child_vial),
      pct_parents_with_child_vial = 100 * mean(has_child_vial),
      .groups = "drop"
    ) %>%
    arrange(desc(parent_tube_count))
  
  no_children_table <- parent_status %>%
    filter(!has_child_vial) %>%
    select(bsi_id, sample_id, tube_type) %>%
    arrange(tube_type, bsi_id)
  
  list(
    summary = summary_table,
    no_children = no_children_table
  )
}

  
  

rm_8_2 <- function(df) {
  
  category_levels <- c(
    "Fewer than expected",
    "Expected",
    "Greater than expected"
  )
  
  expected_counts <- bind_rows(
    tibble(
      collection_setting = NA_character_,
      tube_type = c(
        "Serum",
        "EDTA plasma",
        "EDTA buffy coat/RBC",
        "Streck plasma",
        "Streck buffy coat/RBC",
        "Urine",
        "Mouthwash"
      ),
      expected_count = c(10, 5, 3, 5, 3, 8, 2)
    ),
    tibble(
      collection_setting = "Clinical",
      tube_type = c(
        "Serum",
        "EDTA plasma",
        "EDTA buffy coat/RBC",
        "Streck plasma",
        "Streck buffy coat/RBC",
        "Urine",
        "Mouthwash"
      ),
      expected_count = c(10, 5, 3, 5, 3, 8, 2)
    )
  )
  
  individual_vial_counts <- df %>%
    filter(parent_tube == FALSE) %>%
    group_by(collection_setting, sample_id, tube_type) %>%
    summarise(
      actual_count = n(),
      site = first(site),
      vial_warnings = paste(unique(na.omit(vial_warnings)), collapse = "; "),
      .groups = "drop"
    )
  
  vial_comparison <- individual_vial_counts %>%
    left_join(
      expected_counts %>% filter(collection_setting == "Clinical"),
      by = c("collection_setting", "tube_type")
    ) %>%
    mutate(
      expected_count = ifelse(
        is.na(expected_count),
        ifelse(
          tube_type %in% c(
            "Serum",
            "EDTA plasma",
            "EDTA buffy coat/RBC",
            "Streck plasma",
            "Streck buffy coat/RBC",
            "Urine",
            "Mouthwash"
          ),
          c(
            "Serum" = 10,
            "EDTA plasma" = 5,
            "EDTA buffy coat/RBC" = 3,
            "Streck plasma" = 5,
            "Streck buffy coat/RBC" = 3,
            "Urine" = 8,
            "Mouthwash" = 2
          )[tube_type],
          NA_real_
        ),
        expected_count
      )
    ) %>%
    mutate(
      category = case_when(
        actual_count < expected_count ~ "Fewer than expected",
        actual_count == expected_count ~ "Expected",
        actual_count > expected_count ~ "Greater than expected"
      )
    )
  
  collection_settings <- unique(vial_comparison$collection_setting)
  
  # Map collection settings to table labels
  table_labels <- c(
    "Research" = "2a",
    "Clinical" = "2b",
    "Home" = "2c"
  )
  
  table_titles <- c(
    "Research" = "Table 2a. Number (and percent) of expected child vials created (Research)",
    "Clinical" = "Table 2b. Number (and percent) of expected child vials created (Clinical)",
    "Home" = "Table 2c. Number (and percent) of expected child vials created (Home)"
  )
  
  table_footnotes <- c(
    "Research" = "number of expected child vials are serum (10), EDTA plasma (5), EDTA buffy coat/RBC (3), Streck plasma (5), Streck buffy coat (3), Urine (8) and Mouthwash (2)",
    "Clinical" = "number of expected child vials are serum (10), EDTA plasma (5), EDTA buffy coat/RBC (3), Streck plasma (5), Streck buffy coat (3), Urine (8) and Mouthwash (2)",
    "Home" = "number of expected child vials are Mouthwash (2)"
  )
  
  results <- lapply(collection_settings, function(cs) {
    table_data <- vial_comparison %>%
      filter(collection_setting == cs) %>%
      mutate(
        category = factor(category, levels = category_levels)
      ) %>%
      group_by(tube_type, category) %>%
      summarise(
        n = n_distinct(sample_id),
        .groups = "drop"
      ) %>%
      group_by(tube_type) %>%
      mutate(
        total = sum(n),
        percent = round(100 * n / total, 1),
        display = paste0(n, " (", percent, "%)")
      ) %>%
      ungroup() %>%
      select(tube_type, category, display) %>%
      pivot_wider(
        names_from = category,
        values_from = display,
        names_expand = TRUE,
        values_fill = ""
      ) %>%
      select(tube_type, all_of(category_levels))
    
    unexpected_tubes <- vial_comparison %>%
      filter(category != "Expected") %>%
      select(sample_id, site, collection_setting, tube_type, actual_count, expected_count, vial_warnings, category) %>%
      arrange(sample_id, site, collection_setting, tube_type, actual_count, expected_count, vial_warnings, category)
    
    # Return list with table, title, and footnote
    list(
      table = table_data,
      unexpected_tubes = unexpected_tubes,
      title = table_titles[cs],
      footnote = table_footnotes[cs]
    )
  })
  
  names(results) <- paste("RM8_Table2", table_labels[collection_settings], sep = "_")
  
  return(results)
}


rm_8_3 <- function(df) {
  
  category_levels <- c(
    "Less than expected",
    "Expected",
    "More than expected"
  )
  
  expected_volumes <- bind_rows(
    tibble(
      collection_setting = "Research",
      tube_type = c(
        "Serum",
        "EDTA plasma",
        "EDTA buffy coat/RBC",
        "Streck plasma",
        "Streck buffy coat/RBC",
        "Urine",
        "Mouthwash"
      ),
      expected_volume = c(6.5, 2.8, 1.5, 4.0, 1.5, 8.0, 1.6)
    ),
    tibble(
      collection_setting = "Clinical",
      tube_type = c(
        "Serum",
        "EDTA plasma",
        "EDTA buffy coat/RBC",
        "Streck plasma",
        "Streck buffy coat/RBC",
        "Urine",
        "Mouthwash"
      ),
      expected_volume = c(6.5, 2.8, 1.5, 4.0, 1.5, 8.0, 1.6)
    ),
    tibble(
      collection_setting = "Home",
      tube_type = "Mouthwash",
      expected_volume = 1.6
    )
  )
  
  subject_level_volumes <- df %>%
    filter(
      parent_tube == FALSE,
      !is.na(volume),
      tube_type != "DNA"
    ) %>%
    group_by(collection_setting, site, sample_id, tube_type) %>%
    summarise(
      total_volume = round(sum(volume, na.rm = TRUE), 1),
      vial_warnings = paste(unique(na.omit(vial_warnings)), collapse = "; "),
      .groups = "drop"
    )
  
  volume_comparison <- subject_level_volumes %>%
    left_join(expected_volumes, by = c("collection_setting", "tube_type")) %>%
    mutate(
      category = case_when(
        total_volume < expected_volume - 0.01 ~ "Less than expected",
        abs(total_volume - expected_volume) <= 0.01 ~ "Expected",
        total_volume > expected_volume + 0.01 ~ "More than expected"
      )
    )
  
  volume_collection_settings <- unique(volume_comparison$collection_setting)
  
  table_labels <- c(
    "Research" = "3a",
    "Clinical" = "3b",
    "Home" = "3c"
  )
  
  table_titles <- c(
    "Research" = "Table 3a. Number (and percent) of child vial volumes less than expected, expected, and more than expected by material type (Research collections)",
    "Clinical" = "Table 3b. Number (and percent) of child vial volumes less than expected, expected, and more than expected by material type (Clinical collections)",
    "Home" = "Table 3c. Number (and percent) of child vial volumes less than expected, expected, and more than expected by material type (Home collections)"
  )
  
  table_footnotes <- c(
    "Research" = "Expected child vial volumes are serum (6.5 ml), EDTA plasma (2.8 ml), EDTA buffy coat/RBC (1.5 ml), Streck plasma (4.0 ml), Streck buffy coat/RBC (1.5 ml), Urine (8.0 ml), and Mouthwash (1.6 ml).",
    "Clinical" = "Expected child vial volumes are serum (6.5 ml), EDTA plasma (2.8 ml), EDTA buffy coat/RBC (1.5 ml), Streck plasma (4.0 ml), Streck buffy coat/RBC (1.5 ml), Urine (8.0 ml), and Mouthwash (1.6 ml).",
    "Home" = "Expected child vial volume is Mouthwash (1.6 ml)."
  )
  
  results <- lapply(volume_collection_settings, function(cs) {
    cs_data <- volume_comparison %>%
      filter(collection_setting == cs)
    
    table_data <- cs_data %>%
      mutate(category = factor(category, levels = category_levels)) %>%
      group_by(tube_type, category) %>%
      summarise(
        n = n_distinct(sample_id),
        .groups = "drop"
      ) %>%
      group_by(tube_type) %>%
      mutate(
        total = sum(n),
        percent = round(100 * n / total, 1),
        display = paste0(n, " (", percent, "%)")
      ) %>%
      ungroup() %>%
      select(tube_type, category, display) %>%
      pivot_wider(
        names_from = category,
        values_from = display,
        names_expand = TRUE,
        values_fill = ""
      ) %>%
      select(tube_type, all_of(category_levels))
    
    unexpected_volumes <- cs_data %>%
      filter(category != "Expected") %>%
      select(
        sample_id,
        site,
        collection_setting,
        tube_type,
        total_volume,
        expected_volume,
        vial_warnings, 
        category
      ) %>%
      arrange(
        sample_id,
        site,
        collection_setting,
        tube_type,
        total_volume,
        expected_volume,
        vial_warnings, 
        category
      )
    
    list(
      table = table_data,
      unexpected_volumes = unexpected_volumes,
      title = table_titles[cs],
      footnote = table_footnotes[cs]
    )
  })
  
  names(results) <- paste("RM8_Table3", table_labels[volume_collection_settings], sep = "_")
  
  return(results)
}



# ===============================
# RM 9
# ===============================

rm_9 <- function(df) {
  
  results <- df %>%
    filter(vial_description %in% c("Discarded", "Destroyed/Broken")) %>%
    select(bsi_id, material_type, date_received, site, vial_description)
  
  if (nrow(results) == 0) {
    return(
      tibble::tibble(
        message = "No discarded or broken vials were found."
      )
    )
  }
  
  return(results)
}

