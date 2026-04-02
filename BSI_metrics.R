# BSI_metrics.R ############################################################

# Script Name: BSI_metrics.R 
# Author: Michelle Hudson 
# Date: January 20 2026 
# Date updated: January 29 2026
# Description: This file cleans provided BSI data, performs analysis and formats
# multiple tables and figures from metrics requests identified by the
# biospecimen team related to parent tubes and individuals for the period 2022 -
# 2026.
# 
# Output:
#   - files
#   - codebook 
# ..........................................................................

## load tools --------------------------------------------------------------

# cleaning, analysis, and plotting tools
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(dplyr)
library(purrr)
# library(scales) - don't load this, just call it in ggplot where needed -- it
# masks a function and breaks the import code

# export tools 
library(writexl)
library(officer)
library(rvg)

# data exploration/documentation tools
library(psych)
library(skimr)
library(Hmisc)
library(summarytools)


## data loading, cleaning, and preparation -----------------------------------

# Read data into dataframe

data_path <- "bsi_data/20260120"

# multiple csv files need to be stitched together, nothing to join on, just rows. 
# this is excluding the file where the tubes contain no parent tubes. 

data_raw <- list.files(
  path = data_path,
  pattern = "\\.csv$",
  full.names = TRUE
) %>%
  discard(~ str_detect(.x, "CNX Not In Sequence Scheme\\.csv")) %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c")) %>%
            clean_names() %>%
            mutate(source_file = basename(.x)) # keep a record of the source file )
  ) %>%
  mutate(
    across(c(date_drawn, date_received, date_processed),
           ~ na_if(.x, "")),
    across(c(date_drawn, date_received, date_processed),
           ~ str_trim(.x))
  ) %>%
  mutate(
    date_drawn    = mdy_hm(date_drawn),
    date_received = mdy_hm(date_received),
    date_processed = mdy_hm(date_processed)
  )



# CONNECT ID = Subject ID, need to link up with BQ data possibly for participant
# tables later but I will leave it for now


# Add missing tubes from the 20260120 download 


not_processed_2025 <- read_csv(
  "bsi_data/373 not processed from 2025.csv",
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(source_file = "373 not processed from 2025.csv") %>%
  mutate(
    across(c(date_drawn, date_received, date_processed),
           ~ na_if(.x, "")),
    across(c(date_drawn, date_received, date_processed),
           ~ str_trim(.x))
  ) %>%
  mutate(
    date_drawn     = mdy_hm(date_drawn),
    date_received  = mdy_hm(date_received),
    date_processed = mdy_hm(date_processed)
  ) 


bsi_only_in_csv <- setdiff(
  unique(not_processed_2025$bsi_id),
  unique(data_raw$bsi_id)
)


not_processed_missing_from_data <- not_processed_2025 %>%
  filter(bsi_id %in% bsi_only_in_csv)


not_processed_missing_from_data <- not_processed_missing_from_data %>%
  mutate(
    sequence = str_pad(sequence, width = 4, side = "left", pad = "0")
  )

missing_cols <- setdiff(names(data_raw), names(not_processed_missing_from_data))

not_processed_missing_from_data <- not_processed_missing_from_data %>%
  mutate(across(all_of(missing_cols), ~ NA))

not_processed_missing_from_data <- not_processed_missing_from_data %>%
  select(all_of(names(data_raw)))

data_raw <- bind_rows(
  data_raw,
  not_processed_missing_from_data
)


# for this analysis, remove timestamps, as some dates do not have them 

data_raw <- data_raw %>%
  mutate(
    date_drawn     = as.Date(date_drawn),
    date_received  = as.Date(date_received),
    date_processed = as.Date(date_processed)
  )


data <- data_raw %>%
 # filter(subject_id!="QC Only") %>%  # leave this in the tube-level data, 
  #remove it from analysis for the individual - level data
  select(-obs) #dropping this variable as it is per-data-file and has no meaning for analysis 

# Create tube type categories based on definitions document (sequence number)
# 23 is in the documents but excluded because 1) there are none in the current data and 
# 2) it is listed as being assigned to both Heparin and SST but without any actual tubes, we can't tell 
# which is correct
# Stephanie confirmed that tube sequence 0061 is SUPPOSED to be Streck, but in the single instance in this data, it is SST



data <- data %>%
  mutate(
    tube_type = case_when(
      sequence %in% c("0001","0002","0011","0012","0021","0022","0031","0071","0081","0091") ~ "SST",
      sequence %in% c("0003","0013","0033","0063","0073","0083","0093") ~ "Heparin",
      sequence %in% c("0004","0014","0024","0034", "0064") ~ "EDTA",
      sequence == "0005" ~ "ACD",
      sequence %in% c("0006","0016","0026") ~ "Urine",
      sequence == "0007" ~ "Mouthwash",
      sequence %in% c("0060", "0061") ~ "Streck",
      TRUE ~ NA_character_
    )
  )


# tube checks
tube_61 <- data %>%
  filter(sequence == "0061") 

# recode valid tubes

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA027007 0061",
      "SST",
      tube_type
    )
  )


tube_23 <- data %>%
  filter(sequence == "0023")

# recode

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA054915 0023",
      "SST",
      tube_type
    )
  )



tube_44 <- data %>%
  filter(sequence == "0044")

# recode


data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA048752 0044",
      "EDTA",
      tube_type
    )
  )


tube_103_parent <- data %>% 
  filter(is.na(source_id), sequence == "0103")

# recode legitimate 103 parent tube to Heparin

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA027007 0103",
      "Heparin",
      tube_type
    )
  )


seq_subset_df <- data %>%
  filter(
    sequence %in% c("0050", "0051", "0052", "0053", "0054"),
  ) %>%
  select(
    bsi_id,
    sequence,
    material_type,
    additive_preservative
  )

# recode 50 - 54 to correct types 

data <- data %>%
  mutate(
    tube_type = case_when(
      bsi_id == "CXA048752 0054" ~ "EDTA",
      bsi_id == "CXA006066 0050" ~ "ACD",
      bsi_id == "CXA000846 0050" ~ "SST",
      bsi_id == "CXA000871 0050" ~ "Mouthwash",
      bsi_id == "CXA000898 0050" ~ "SST",
      bsi_id == "CXA027007 0051" ~ "SST",
      bsi_id == "CXA027007 0053" ~ "Heparin",
      bsi_id == "CXA068373 0054" ~ "EDTA",
      TRUE ~ tube_type
    )
  )


# it's parent tube if both the parent_id and source_id are blank

data <- data %>%
  mutate(
    parent_tube = is.na(parent_id) & is.na(source_id)
  )

# if date_processed is null, then the tube is unprocessed

data <- data %>%
  mutate(
    unprocessed = is.na(date_processed)
    )
  
# data_delivery_date = Jan 20 2026 - hard coded for now, if we receive more data
# deliveries, edit this to more flexibly reference the latest download

data <- data %>%
  mutate(
    data_delivery_date = as.Date("1/20/2026", format = "%m/%d/%Y")
  )

# make a container of tube types to loop over in metrics

tube_types <- c("SST", "EDTA", "Heparin", "ACD", "Streck", "Urine", "Mouthwash")


# Define tube groups for use in metrics 


blood_tubes     <- c("SST", "EDTA", "Heparin", "Streck", "ACD")
urine_tubes     <- "Urine"
mouthwash_tubes <- "Mouthwash"


# add a "days since receipt" variable

data <- data %>%
  mutate(
    days_since_receipt = as.numeric(difftime(data_delivery_date, date_received, units = "days"))
  )


# add variable "days_to_process" 

data <- data %>%
  mutate(days_to_process = as.numeric(difftime(date_processed, date_received, units = "days")))







## EDA & checks ------------------------------------------------------------

# ## tube types
# 
tube_type_counts <- data %>%
  filter(!is.na(tube_type)) %>%
  count(tube_type, name = "n_samples")

tube_type_counts_with_total <- tube_type_counts %>%
  bind_rows(
    tibble(
      tube_type = "Total",
      n_samples = sum(tube_type_counts$n_samples, na.rm = TRUE)
    )
  )

tube_type_counts <- data %>%
  # filter(!is.na(tube_type)) %>%
  group_by(tube_type) %>%
  summarise(
    n_samples = n(),
    n_parent_tubes = sum(parent_tube, na.rm = TRUE),
    .groups = "drop"
  )


writexl::write_xlsx(
  tube_type_counts,
  "bsi_exports/general/tube_type_counts_with_parent_tubes.xlsx"
)


## Year check in source files 

# year_drawn_by_source <- data %>%
#   mutate(year = year(date_drawn)) %>%
#   count(year, source_file, name = "n") %>%
#   arrange(year, source_file)
# 
# year_received_by_source <- data %>%
#   mutate(year = year(date_received)) %>%
#   count(year, source_file, name = "n") %>%
#   arrange(year, source_file)
# 
# year_processed_by_source <- data %>%
#   mutate(year = year(date_processed)) %>%
#   count(year, source_file, name = "n") %>%
#   arrange(year, source_file)



## Samples per subject check
# 
# subject_sample_counts <- data %>%
#   distinct(subject_id, sample_id) %>%
#   count(subject_id, name = "n_samples")
# 
# subjects_by_sample_count <- subject_sample_counts %>%
#   count(n_samples, name = "n_subjects") %>%
#   arrange(n_samples)
# 
# n_distinct(data$sample_id)
# 
# write_xlsx(subjects_by_sample_count, "bsi_exports/general/samples_by_subject.xlsx")

# samples per subject per tube type


subject_sample_counts_by_tube <- data %>%

  distinct(subject_id, sample_id, tube_type) %>%
  group_by(subject_id, tube_type) %>%
  summarise(
    n_samples = n(),
    .groups = "drop"
  )

subjects_by_sample_count_tube <- subject_sample_counts_by_tube %>%
  group_by(tube_type, n_samples) %>%
  summarise(
    n_subjects = n(),
    n_sample_ids = sum(n_samples),
    .groups = "drop"
  ) %>%
  arrange(tube_type, n_samples)

subjects_by_sample_count_tube %>%
  group_by(tube_type) %>%
  summarise(total_samples = sum(n_sample_ids))

data %>%
  distinct(sample_id, tube_type) %>%
  count(tube_type)

writexl::write_xlsx(
  subjects_by_sample_count_tube,
  "bsi_exports/metric5/samples_by_subject_and_tube.xlsx"
)



parent_type_missing <- data %>%
  filter(parent_tube == TRUE,
    is.na(tube_type)
  )






# METRIC #1 ---------------------------------------------------------------

# BSI Metric request #1: Weekly Received and Processed Specimens 
# A plot by week (weeks are considered Monday-Sunday) showing 
# 1. number of tubes received (based on “Date Received”) and 
# 2. number of tubes processed (based on “Date Processed”)
# Do this separately by year 2022, 2023, 2024, 2025 and by Tube Type 
# (SST, EDTA, heparin, ACD, Streck, urine, mouthwash) so there will be 28 separate figures


data_weekly <- data %>%
  mutate(
    week_received_start  = floor_date(date_received, unit = "week", week_start = 1),
    week_processed_start = floor_date(date_processed, unit = "week", week_start = 1),
    
    year_received  = year(date_received),
    year_processed = year(date_processed)
  )

years <- c(2022, 2023, 2024, 2025)

plots <- list()

for (yr in years) {
  for (tube in tube_types) {
    
    df_plot <- bind_rows(
      
      # Tubes received
      data_weekly %>%
        filter(
          tube_type == tube,
          year_received == yr
        ) %>%
        group_by(week = week_received_start) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(type = "Tubes received"),
      
      # Tubes processed
      data_weekly %>%
        filter(
          tube_type == tube,
          year_processed == yr
        ) %>%
        group_by(week = week_processed_start) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(type = "Tubes processed")
      
    ) %>%
      filter(!is.na(week)) %>%
      arrange(week)
    
    p <- ggplot(df_plot, aes(x = week, y = n, color = type)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      geom_text(
        aes(label = n),
        vjust = -0.7,
        size = 3,
        show.legend = FALSE
      ) +
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%Y-%m-%d",
        expand = c(0, 0)
      ) +
      labs(
        title = paste0(
          tube, " Tubes Received vs Processed (", yr, ")"
        ),
        x = "Week (Monday–Sunday)",
        y = "Number of tubes",
        color = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    plots[[paste(yr, tube, sep = "_")]] <- p
  }
}


walk2(
  plots,
  names(plots),
  ~ ggsave(
    filename = paste0("bsi_exports/metric1/", .y, "_weekly_received_processed.png"),
    plot = .x,
    width = 12,
    height = 8,
    dpi = 300
  )
)

plots_ordered <- plots[order(names(plots))]

# pdf 

pdf(
  "bsi_exports/metric1/metric1_weekly_received_processed_all_plots.pdf",
  width = 14,
  height = 8
)

for (p in plots_ordered) {
  print(p)
}

dev.off()



# power point 

ppt <- read_pptx()

for (p in plots) {
  
  ppt <- ppt %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with(
      value = dml(ggobj = p),
      location = ph_location(
        left   = 0.25,
        top    = 0.25,
        width  = 9.5,
        height = 6.8
      )
    )
}

print(
  ppt,
  target = "bsi_exports/metric1/metric1_weekly_received_processed_all_plots.pptx"
)



1# METRIC #2 ---------------------------------------------------------------

### BSI Metric Request #2 
### Number of tubes that have not been processed and 
### Number of participants with tubes that have not been processed
### Overall counts & percentages and stratified by year collected


## Table 2a. -------------------------------------------------------------------

# Parent tubes with no processing date (also the denominator) 

data %>%
  filter(
    parent_tube,
    unprocessed
  ) %>%
  nrow()

# unprocessed parent tubes stratified by year and type 


unprocessed_by_year <- data %>%
  filter(parent_tube == TRUE, unprocessed == TRUE) %>%
  mutate(year = year(date_received)) %>%
  count(year, tube_type, name = "unprocessed_count")

unprocessed_table <- data %>%
  filter(parent_tube == TRUE, unprocessed == TRUE) %>%
  mutate(
    year = year(date_received),
    tube_type = factor(
      tube_type,
      levels = c("SST", "EDTA", "Heparin", "Streck", "ACD", "Urine", "Mouthwash")
    )
  ) %>%
  count(year, tube_type, name = "unprocessed_count") %>%
  pivot_wider(
    names_from = year,
    values_from = unprocessed_count,
    values_fill = 0
  ) %>%
  arrange(tube_type)

unprocessed_table <- unprocessed_table %>%
  mutate(
    TOTAL = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  relocate(TOTAL, .before = 1)

unprocessed_table <- unprocessed_table %>%
  bind_rows(
    unprocessed_table %>%
      summarise(
        tube_type = "Total",
        across(where(is.numeric), sum)
      )
  )

write_xlsx(unprocessed_table, "bsi_exports/metric2/metric2_BSI_unprocessed_tubes_by_year_draft.xlsx")



## Table 2b. -------------------------------------------------------------


eligible_tubes  <- c(blood_tubes, urine_tubes, mouthwash_tubes)

# 1. Individual participants with at least one tube type 
# (blood, urine, or mouthwash) collected and with no processing date 
# for at least one of the tube types collected

participants_with_unprocessed <- data %>%
  filter(tube_type %in% eligible_tubes, subject_id!="QC Only") %>% # drop QC for participants counts 
  group_by(subject_id) %>%
  summarise(
    unprocessed_flag = any(unprocessed == TRUE),   # use your unprocessed variable
    .groups = "drop"
  ) %>%
  filter(unprocessed_flag == TRUE)



# 2. Individual participants with at least one tube type collected and with no
# processing date for all of the tube types collected


participants_all_unprocessed <- data %>%
  filter(tube_type %in% eligible_tubes, subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    has_any_tube   = n() > 0,
    all_unprocessed = all(unprocessed == TRUE),
    .groups = "drop"
  ) %>%
  filter(has_any_tube == TRUE, all_unprocessed == TRUE)



# 3. Individual participants with at least one blood tube collected but no blood
# samples (SST, EDTA, heparin, Streck, ACD) processed (could have urine and/or
# mouthwash processed)

participants_unprocessed_blood <- data %>%
  filter(tube_type %in% blood_tubes, subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    has_blood          = n() > 0,
    any_blood_processed = any(unprocessed == FALSE),  # processed = FALSE
    .groups = "drop"
  ) %>%
  filter(has_blood == TRUE, any_blood_processed == FALSE)


# 4. Individual participants with SST tube(s) collected but no SST tubes processed

participants_unprocessed_sst <- data %>%
  filter(tube_type == "SST", subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    has_sst          = n() > 0,
    any_sst_processed = any(unprocessed == FALSE),
    .groups = "drop"
  ) %>%
  filter(has_sst == TRUE, any_sst_processed == FALSE)


# 5. Individual participants with EDTA tube(s) collected but no EDTA tubes processed

participants_unprocessed_edta <- data %>%
  filter(tube_type == "EDTA", subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    has_edta          = n() > 0,
    any_edta_processed = any(unprocessed == FALSE),
    .groups = "drop"
  ) %>%
  filter(has_edta == TRUE, any_edta_processed == FALSE)



# 6. Individual participants with EDTA or SST tubes collected but no EDTA AND no
# SST tubes processed


participants_unprocessed_edta_sst <- data %>%
  filter(tube_type %in% c("EDTA", "SST"), subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    has_edta_or_sst      = n() > 0,
    any_edta_processed   = any(tube_type == "EDTA" & unprocessed == FALSE),
    any_sst_processed    = any(tube_type == "SST"  & unprocessed == FALSE),
    .groups = "drop"
  ) %>%
  filter(
    has_edta_or_sst == TRUE,
    any_edta_processed == FALSE,
    any_sst_processed == FALSE
  )


# Subject by year

subject_year <- data %>%
  filter(subject_id!="QC Only") %>% 
  group_by(subject_id) %>%
  summarise(year = year(min(date_received, na.rm = TRUE)), .groups = "drop")

# build dataframe

# Convert each results dataset into a labeled long form
df_any_unprocessed <- participants_with_unprocessed %>%
  mutate(metric = "any_unprocessed") %>%
  select(subject_id, metric)

df_all_unprocessed <- participants_all_unprocessed %>%
  mutate(metric = "all_unprocessed") %>%
  select(subject_id, metric)

df_unprocessed_blood <- participants_unprocessed_blood %>%
  mutate(metric = "unprocessed_blood") %>%
  select(subject_id, metric)

df_unprocessed_sst <- participants_unprocessed_sst %>%
  mutate(metric = "unprocessed_sst") %>%
  select(subject_id, metric)

df_unprocessed_edta <- participants_unprocessed_edta %>%
  mutate(metric = "unprocessed_edta") %>%
  select(subject_id, metric)

df_unprocessed_edta_sst <- participants_unprocessed_edta_sst %>%
  mutate(metric = "unprocessed_edta_sst") %>%
  select(subject_id, metric)


metric_order <- c(
  
  "any_unprocessed",
  "all_unprocessed",
  "unprocessed_blood",
  "unprocessed_sst",
  "unprocessed_edta",
  "unprocessed_edta_sst"
)

all_metrics <- bind_rows(
  df_any_unprocessed,
  df_all_unprocessed,
  df_unprocessed_blood,
  df_unprocessed_sst,
  df_unprocessed_edta,
  df_unprocessed_edta_sst
) %>%
  mutate(metric = factor(metric, levels = metric_order)) %>%
  left_join(subject_year %>% select(subject_id, year), by = "subject_id")

summary_by_year <- all_metrics %>%
  count(metric, year, name = "n") %>%
  pivot_wider(
    names_from = year,
    values_from = n,
    values_fill = 0
  ) %>%
  # Remove arrange(metric)
  relocate(
    all_of(
      as.character(
        sort(
          suppressWarnings(as.numeric(names(.)[names(.) != "metric"]))
        )
      )
    ),
    .after = metric
  )

write_xlsx(summary_by_year, "bsi_exports/metric2/metric2_BSI_unprocessed_participants_by_year_draft.xlsx")


# METRIC #3 ---------------------------------------------------------------

# BSI Priority 1 Metric #3
# Examination of the “time since received” for samples that have not been processed

# We want to decide what, if any, samples that have not yet been processed we should process.
# This will depend on the material type and how long it has been since the samples were
# received. We would like a bar chart, similar to the below. The time categories will be based on
# time between sample receipt and the date of the BSI download. We should not have any/many
# samples unprocessed in the past week, so we can collapse the lowest category to <= 7 days
# Do this separately for all Tube types (Serum, EDTA plasma, heparin plasma,
# ACD, Streck, urine, mouthwash) so there will be 7 plots. The counts should be
# number of parent tubes. 

# Here are the categories for the X axis:
#   1. <=7 days
# 2. > 7 days to <= 30 days
# 3. >30 days to <=90 days
# 4. >90 days to <=180 days
# 5. >180 days to <=365 days
# 6. >365 days to <=545 days (~18 months)
# 7. >545 days to <=730 days (2 years)
# 8. >730 days to <=1096 days (3 years)
# 9. >1096 days

# For calculating the percents, the denominator should be the number of unprocessed samples of
# each material type (should add up to ~85,000).

# Ideally, on the plot also include the number of samples that are represented in each bar.


data <- data %>%
  mutate(
    days_since_receipt = as.numeric(difftime(data_delivery_date, date_received, units = "days")),
    time_bin = case_when(
      days_since_receipt <= 7                     ~ "<= 7 days",
      days_since_receipt <= 30                    ~ "> 7 days to <= 30 days",
      days_since_receipt <= 90                    ~ ">30 days to <=90 days",
      days_since_receipt <= 180                   ~ ">90 days to <=180 days",
      days_since_receipt <= 365                   ~ ">180 days to <=365 days",
      days_since_receipt <= 545                   ~ ">365 days to <=545 days (~18 months)",
      days_since_receipt <= 730                   ~ ">545 days to <=730 days (2 years)",
      days_since_receipt <= 1096                  ~ ">730 days to <=1096 days (3 years)",
      TRUE                                        ~ ">1096 days"
    ),
    
    # ORDER THE FACTOR LEVELS EXACTLY IN THIS ORDER
    time_bin = factor(
      time_bin,
      levels = c(
        "<= 7 days",
        "> 7 days to <= 30 days",
        ">30 days to <=90 days",
        ">90 days to <=180 days",
        ">180 days to <=365 days",
        ">365 days to <=545 days (~18 months)",
        ">545 days to <=730 days (2 years)",
        ">730 days to <=1096 days (3 years)",
        ">1096 days"
      )
    )
  )

# make the denominator for percentage overall 

total_unprocessed_all <- data %>%
  filter(unprocessed == TRUE, parent_tube == TRUE) %>%
  nrow()


plots <- map(tube_types, function(tube) {
  
  df_plot <- data %>%
    filter(tube_type == tube, unprocessed == TRUE, parent_tube == TRUE) %>%
    group_by(time_bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      total = sum(n),
      pct = n / total_unprocessed_all,
      pct_label = scales::percent(pct, accuracy = 1),
      bar_label = paste0(n, " (", pct_label, " of total)")  
    )
  
  
  # Extract N for the title
  N_tube <- sum(df_plot$n)
  
  ggplot(df_plot, aes(x = time_bin, y = n)) +
    geom_col(fill = "steelblue") +
    geom_text(
      aes(label = bar_label, y = n + max(n) * 0.03),
      size = 4,
      vjust = 0
    ) +
    labs(
      title = paste0("Unprocessed Samples by Time Since Receipt: ", 
                     tube, " (N = ", N_tube, ")"),
      x = "Days Since Receipt (Categories)",
      y = "Count of Unprocessed Parent Tubes"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})



plots[[1]]   # SST
plots[[2]]   # EDTA
plots[[3]]   # Heparin
plots[[4]]   # ACD
plots[[5]]   # Streck
plots[[6]]   # Urine
plots[[7]]   # Mouthwash

 
walk2(plots, tube_types, ~ ggsave(
  filename = paste0("bsi_exports/metric3/metric3_", .y, ".png"),
  plot = .x,
  width = 12, height = 8
))

# pdf

pdf(
  file = "bsi_exports/metric3/metric3_unprocessed_parent_tubes_by_time.pdf",
  width = 12,
  height = 8
)

for (p in plots) {
  print(p)
}

dev.off()


#ppt

ppt <- read_pptx()

for (p in plots) {
  
  ppt <- ppt %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with(
      value = dml(ggobj = p),
      location = ph_location(
        left   = 0.25,
        top    = 0.25,
        width  = 9.5,
        height = 6.8
      )
    )
}

print(
  ppt,
  target = "bsi_exports/metric3/metric3_unprocessed_parent_tubes_by_time_bin.pptx"
)


# METRIC #4 ----------------------------------------------------------------

# BSI Priority 1 Metric #4 – Examination of the delayed processing time for
# samples that have been processed

# We want to understand the breadth of the issue of delayed processing and look
# at both samples that were processed with delays and samples that were not
# processed together on the same plot.


# 4a. Essentially, what is the distribution of processing time (date of receipt
# at the lab to date processed) for the specimens that were processed AND what
# is the distribution of time delay (from date received at the lab to date of
# the BSI download) for samples that have not been processed, overlaid on the
# same graph.  An example on how to portray this is below, but open to other
# suggestions.


# Do this separately for all Tube types (Serum, EDTA plasma, heparin plasma,
# ACD, Streck, urine, mouthwash) so there will be 7 plots.  The counts for 4a
# should be number of Parent Tubes


# Here are the categories for the X axis: 
#   <= 2 days  
# > 2 days to <=3 days  
# >3 days to <=7 days  
# > 7 days to <= 30 days  
# >30 days to <=90 days  
# >90 days to <=180 days  
# >180 days to <=365 days  
# >365 days to <=545 days (~18 months)  
# >545 days to <=730 days (2 years)  
# >730 days to <=1096 days (3 years) 
# >1096 days (3 years) 


## 4a. plots for tubes ----
## Time from sample receipt at BPTL until processed or time from sample receipt to 1/20/2026 

# Include the QC only samples that we originally had you exclude 

# For calculating the percents, the denominator should be the number of
# collected samples of each material type. Present
# separate numbers and percents for each colored part of each column, so that
# the total %s on each figure should be 100%

# Also include on the plot the number of samples that are represented in each bar. 




# add days since receipt time bins for metric 4

data <- data %>%
  mutate(
    days_since_receipt = as.numeric(
      difftime(data_delivery_date, date_received, units = "days")
    ),
    
    time_bin_4 = case_when(
      days_since_receipt <= 2    ~ "<= 2 days",
      days_since_receipt <= 3    ~ "> 2 days to <=3 days",
      days_since_receipt <= 7    ~ ">3 days to <=7 days",
      days_since_receipt <= 30   ~ "> 7 days to <= 30 days",
      days_since_receipt <= 90   ~ ">30 days to <=90 days",
      days_since_receipt <= 180  ~ ">90 days to <=180 days",
      days_since_receipt <= 365  ~ ">180 days to <=365 days",
      days_since_receipt <= 545  ~ ">365 days to <=545 days (~18 months)",
      days_since_receipt <= 730  ~ ">545 days to <=730 days (2 years)",
      days_since_receipt <= 1096 ~ ">730 days to <=1096 days (3 years)",
      TRUE                       ~ ">1096 days (3 years)"
    ),
    
    time_bin_4 = factor(
      time_bin_4,
      levels = c(
        "<= 2 days",
        "> 2 days to <=3 days",
        ">3 days to <=7 days",
        "> 7 days to <= 30 days",
        ">30 days to <=90 days",
        ">90 days to <=180 days",
        ">180 days to <=365 days",
        ">365 days to <=545 days (~18 months)",
        ">545 days to <=730 days (2 years)",
        ">730 days to <=1096 days (3 years)",
        ">1096 days (3 years)"
      )
    )
  )


    

# days to process time bin for metric 4

data <- data %>%
  mutate(
    days_to_process = as.numeric(
      difftime(date_processed, date_received, units = "days")
    ),
    
    time_bin_4_process = case_when(
      days_to_process <= 2    ~ "<= 2 days",
      days_to_process <= 3    ~ "> 2 days to <=3 days",
      days_to_process <= 7    ~ ">3 days to <=7 days",
      days_to_process <= 30   ~ "> 7 days to <= 30 days",
      days_to_process <= 90   ~ ">30 days to <=90 days",
      days_to_process <= 180  ~ ">90 days to <=180 days",
      days_to_process <= 365  ~ ">180 days to <=365 days",
      days_to_process <= 545  ~ ">365 days to <=545 days (~18 months)",
      days_to_process <= 730  ~ ">545 days to <=730 days (2 years)",
      days_to_process <= 1096 ~ ">730 days to <=1096 days (3 years)",
      TRUE                    ~ ">1096 days (3 years)"
    ),
    
    time_bin_4_process = factor(
      time_bin_4_process,
      levels = c(
        "<= 2 days",
        "> 2 days to <=3 days",
        ">3 days to <=7 days",
        "> 7 days to <= 30 days",
        ">30 days to <=90 days",
        ">90 days to <=180 days",
        ">180 days to <=365 days",
        ">365 days to <=545 days (~18 months)",
        ">545 days to <=730 days (2 years)",
        ">730 days to <=1096 days (3 years)",
        ">1096 days (3 years)"
      )
    )
  )




# total tubes 

total_parent <- data %>%
  filter(parent_tube == TRUE) %>%
  nrow()


tube_types_no_mw <- setdiff(tube_types, "Mouthwash")



# plots


# Define x-axis order once (critical)
time_bin_levels <- c(
  "<= 2 days",
  "> 2 days to <=3 days",
  ">3 days to <=7 days",
  "> 7 days to <= 30 days",
  ">30 days to <=90 days",
  ">90 days to <=180 days",
  ">180 days to <=365 days",
  ">365 days to <=545 days (~18 months)",
  ">545 days to <=730 days (2 years)",
  ">730 days to <=1096 days (3 years)",
  ">1096 days (3 years)"
)

plots <- map(tube_types_no_mw, function(tube) {
  
  df_plot <- data %>%
    filter(
      tube_type == tube,
      parent_tube == TRUE
    ) %>%
    mutate(
      status = if_else(unprocessed, "Unprocessed", "Processed"),
      status = factor(
        status,
        levels = c("Processed", "Unprocessed")
      ),
      time_bin_plot = if_else(
        unprocessed,
        as.character(time_bin_4),
        as.character(time_bin_4_process)
      ),
      time_bin_plot = factor(
        time_bin_plot,
        levels = time_bin_levels
      )
    ) %>%
    group_by(time_bin_plot, status) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      total = sum(n),
      pct = n / total,
      pct_label = scales::percent(pct, accuracy = 1),
      bar_label = paste0(n, " (", pct_label, ")")
    )
  
  N_tube <- sum(df_plot$n)
  
  ggplot(df_plot, aes(x = time_bin_plot, y = n, fill = status)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_text(
      aes(label = bar_label),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 3.8
    ) +
    labs(
      title = paste0(
        "Processing Status by Time Bin: ", tube,
        " (N = ", N_tube, ")"
      ),
      subtitle = "Processed vs unprocessed parent tubes",
      x = "Time Since Receipt at Laboratory (Unprocessed) or Time to Process (Processed)",
      y = "Count of Samples",
      fill = "Sample Status"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
})



plots[[1]]   # SST
plots[[2]]   # EDTA
plots[[3]]   # Heparin
plots[[4]]   # ACD
plots[[5]]   # Streck
plots[[6]]   # Urine

walk2(
  plots,
  tube_types_no_mw,
  ~ ggsave(
    filename = paste0("bsi_exports/metric4/metric4_tubes_", .y, ".png"),
    plot = .x,
    width = 12,
    height = 8
  )
)



### 4.: MOUTHWASH PLOTS WITH NEW BINS ----

# one change we would like on the mouthwash plot for metric 4.  Rather than
# cutting >5-7 days and >7-30 days, cut it >5-10 days and >10-30 days.




data <- data %>%
  mutate(
    time_bin_4_mw = case_when(
      days_since_receipt <= 2    ~ "<= 2 days",
      days_since_receipt <= 3    ~ "> 2 days to <=3 days",
      days_since_receipt <= 5    ~ ">3 days to <=5 days",
      days_since_receipt <= 10   ~ ">5 days to <=10 days",
      days_since_receipt <= 30   ~ ">10 days to <=30 days",
      days_since_receipt <= 90   ~ ">30 days to <=90 days",
      days_since_receipt <= 180  ~ ">90 days to <=180 days",
      days_since_receipt <= 365  ~ ">180 days to <=365 days",
      days_since_receipt <= 545  ~ ">365 days to <=545 days (~18 months)",
      days_since_receipt <= 730  ~ ">545 days to <=730 days (2 years)",
      days_since_receipt <= 1096 ~ ">730 days to <=1096 days (3 years)",
      TRUE                       ~ ">1096 days (3 years)"
    ),
    
    time_bin_4_mw = factor(
      time_bin_4_mw,
      levels = c(
        "<= 2 days",
        "> 2 days to <=3 days",
        ">3 days to <=5 days",
        ">5 days to <=10 days",
        ">10 days to <=30 days",
        ">30 days to <=90 days",
        ">90 days to <=180 days",
        ">180 days to <=365 days",
        ">365 days to <=545 days (~18 months)",
        ">545 days to <=730 days (2 years)",
        ">730 days to <=1096 days (3 years)",
        ">1096 days (3 years)"
      )
    )
  )

data <- data %>%
  mutate(
    time_bin_4_process_mw = case_when(
      days_to_process <= 2    ~ "<= 2 days",
      days_to_process <= 3    ~ "> 2 days to <=3 days",
      days_to_process <= 5    ~ ">3 days to <=5 days",
      days_to_process <= 10   ~ ">5 days to <=10 days",
      days_to_process <= 30   ~ ">10 days to <=30 days",
      days_to_process <= 90   ~ ">30 days to <=90 days",
      days_to_process <= 180  ~ ">90 days to <=180 days",
      days_to_process <= 365  ~ ">180 days to <=365 days",
      days_to_process <= 545  ~ ">365 days to <=545 days (~18 months)",
      days_to_process <= 730  ~ ">545 days to <=730 days (2 years)",
      days_to_process <= 1096 ~ ">730 days to <=1096 days (3 years)",
      TRUE                    ~ ">1096 days (3 years)"
    ),
    
    time_bin_4_process_mw = factor(
      time_bin_4_process_mw,
      levels = levels(time_bin_4_mw)
    )
  )

time_bin_levels_mw <- levels(data$time_bin_4_mw)


df_plot_mw <- data %>%
  filter(
    tube_type == "Mouthwash",
    parent_tube == TRUE
  ) %>%
  mutate(
    status = if_else(unprocessed, "Unprocessed", "Processed"),
    status = factor(
      status,
      levels = c("Processed", "Unprocessed")
    ),
    time_bin_plot = if_else(
      unprocessed,
      as.character(time_bin_4_mw),
      as.character(time_bin_4_process_mw)
    ),
    time_bin_plot = factor(
      time_bin_plot,
      levels = time_bin_levels_mw
    )
  ) %>%
  group_by(time_bin_plot, status) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    total = sum(n),
    pct = n / total,
    pct_label = scales::percent(pct, accuracy = 1),
    bar_label = paste0(n, " (", pct_label, ")")
  )

N_mw <- sum(df_plot_mw$n)


ggplot(df_plot_mw, aes(x = time_bin_plot, y = n, fill = status)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = bar_label),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.8
  ) +
  labs(
    title = paste0(
      "Processing Status by Time Bin: Mouthwash (N = ", N_mw, ")"
    ),
    subtitle = "Processed vs unprocessed parent tubes",
    x = "Time Since Receipt at the Laboratory (Unprocessed) or Time to Process (Processed)",
    y = "Count of Samples",
    fill = "Sample Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "bsi_exports/metric4/metric4_tubes_Mouthwash.png",
  width = 12,
  height = 8,
  dpi = 300
)


metric4_footnote <- 
  "Note: Blood and Urine charts use a different time categorization from Mouthwash chart"

plots <- lapply(
  plots,
  function(p) {
    p + labs(caption = metric4_footnote)
  }
)


p_mw <- ggplot(df_plot_mw, aes(x = time_bin_plot, y = n, fill = status)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = bar_label),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.8
  ) +
  labs(
    title = paste0(
      "Processing Status by Time Bin: Mouthwash (N = ", N_mw, ")"
    ),
    subtitle = "Processed vs unprocessed parent tubes",
    x = "Time Since Receipt at the Laboratory (Unprocessed) or Time to Process (Processed)",
    y = "Count of Samples",
    fill = "Sample Status",
    caption = metric4_footnote   # 👈 footnote
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


metric4_plots <- c(plots, list(Mouthwash = p_mw))


pdf(
  file = "bsi_exports/metric4/metric4_all_tubes.pdf",
  width = 12,
  height = 8
)

for (p in metric4_plots) {
  print(p)
}

dev.off()

ppt <- read_pptx()

for (p in metric4_plots) {
  
  ppt <- ppt %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with(
      value = dml(ggobj = p),
      location = ph_location(
        left   = 0.25,
        top    = 0.25,
        width  = 9.5,
        height = 6.8
      )
    )
}

print(
  ppt,
  target = "bsi_exports/metric4/metric4_all_tubes.pptx"
)



### 4a. as table ----------------------------------------------------------------

table_all <- map_dfr(tube_types, function(tube) {
  
  is_mw <- tube == "Mouthwash"
  
  data %>%
    filter(
      tube_type == tube,
      parent_tube == TRUE
    ) %>%
    mutate(
      tube_type = tube,
      status = if_else(unprocessed, "Unprocessed", "Processed"),
      status = factor(
        status,
        levels = c("Processed", "Unprocessed")
      ),
      time_bin = if_else(
        unprocessed,
        if (is_mw) as.character(time_bin_4_mw) else as.character(time_bin_4),
        if (is_mw) as.character(time_bin_4_process_mw) else as.character(time_bin_4_process)
      ),
      time_bin = factor(
        time_bin,
        levels = if (is_mw) time_bin_levels_mw else time_bin_levels
      )
    ) %>%
    group_by(tube_type, time_bin, status) %>%
    summarise(
      n = n(),
      .groups = "drop"
    )
}) %>%
  group_by(tube_type) %>%
  mutate(
    total_tube = sum(n),
    pct = n / total_tube,
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  ungroup() %>%
  arrange(tube_type, time_bin, status)


table_wide <- table_all %>%
  select(tube_type, time_bin, status, n, pct_label) %>%
  pivot_wider(
    names_from = status,
    values_from = c(n, pct_label)
  )

write_xlsx(
  table_wide,
  path = "bsi_exports/metric4/metric4_tubes_processing_status_by_tube_type.xlsx"
)


## 4b. plots for individuals ----

# 4b. We also want the same information for individual participants rather than
# for individual tubes. (Exclude QC only)

plots_individuals <- map(tube_types_no_mw, function(tube) {
  
  df_plot <- data %>%
    filter(
      subject_id != "QC Only",
      tube_type == tube,
      parent_tube == TRUE
    ) %>%
    mutate(
      status = if_else(unprocessed, "Unprocessed", "Processed"),
      status = factor(
        status,
        levels = c("Processed", "Unprocessed")
      ),
      time_bin_plot = if_else(
        unprocessed,
        as.character(time_bin_4),
        as.character(time_bin_4_process)
      ),
      time_bin_plot = factor(
        time_bin_plot,
        levels = time_bin_levels
      )
    ) %>%
    group_by(time_bin_plot, status) %>%
    summarise(
      n = n_distinct(subject_id),
      .groups = "drop"
    ) %>%
    mutate(
      total = sum(n),
      pct = n / total,
      pct_label = scales::percent(pct, accuracy = 1),
      bar_label = paste0(n, " (", pct_label, ")")
    )
  
  N_subjects <- sum(df_plot$n)
  
  ggplot(df_plot, aes(x = time_bin_plot, y = n, fill = status)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_text(
      aes(label = bar_label),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 3.8
    ) +
    labs(
      title = paste0(
        "Processing Status by Time Bin (Individuals): ", tube,
        " (N = ", N_subjects, ")"
      ),
      subtitle = "Processed vs unprocessed parent tubes (unique individuals)",
      x = "Time Since Receipt at Laboratory (Unprocessed) or Time to Process (Processed)",
      y = "Number of Individuals",
      fill = "Sample Status"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
})

walk2(
  plots_individuals,
  tube_types_no_mw,
  ~ ggsave(
    filename = paste0("bsi_exports/metric4/metric4_individuals_", .y, ".png"),
    plot = .x,
    width = 12,
    height = 8,
    dpi = 300
  )
)

## MOUTHWASH

df_plot_mw_individuals <- data %>%
  filter(
    tube_type == "Mouthwash",
    parent_tube == TRUE,
    subject_id != "QC Only"
  ) %>%
  mutate(
    status = if_else(unprocessed, "Unprocessed", "Processed"),
    status = factor(
      status,
      levels = c("Processed", "Unprocessed")
    ),
    time_bin_plot = if_else(
      unprocessed,
      as.character(time_bin_4_mw),
      as.character(time_bin_4_process_mw)
    ),
    time_bin_plot = factor(
      time_bin_plot,
      levels = time_bin_levels_mw
    )
  ) %>%
  group_by(time_bin_plot, status) %>%
  summarise(
    n = n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  mutate(
    total = sum(n),
    pct = n / total,
    pct_label = scales::percent(pct, accuracy = 1),
    bar_label = paste0(n, " (", pct_label, ")")
  )

N_mw_individuals <- sum(df_plot_mw_individuals$n)

p_mw_individuals <- ggplot(
  df_plot_mw_individuals,
  aes(x = time_bin_plot, y = n, fill = status)
) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = bar_label),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.8
  ) +
  labs(
    title = paste0(
      "Processing Status by Time Bin: Mouthwash (Individuals, N = ",
      N_mw_individuals, ")"
    ),
    subtitle = "Processed vs unprocessed parent tubes (unique individuals)",
    x = "Time Since Receipt at the Laboratory (Unprocessed) or Time to Process (Processed)",
    y = "Number of Individuals",
    fill = "Sample Status",
    caption = metric4_footnote
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave(
  filename = "bsi_exports/metric4/metric4_individuals_Mouthwash.png",
  plot = p_mw_individuals,
  width = 12,
  height = 8,
  dpi = 300
)

plots_individuals <- lapply(
  plots_individuals,
  function(p) p + labs(caption = metric4_footnote)
)

metric4_individual_plots <- c(
  plots_individuals,
  list(Mouthwash = p_mw_individuals)
)

# pdf

pdf(
  file = "bsi_exports/metric4/metric4_individuals_all_tubes.pdf",
  width = 12,
  height = 8
)

for (p in metric4_individual_plots) {
  print(p)
}

dev.off()

ppt <- read_pptx()

for (p in metric4_individual_plots) {
  ppt <- ppt %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with(
      value = dml(ggobj = p),
      location = ph_location(
        left   = 0.25,
        top    = 0.25,
        width  = 9.5,
        height = 6.8
      )
    )
}

print(
  ppt,
  target = "bsi_exports/metric4/metric4_individuals_all_tubes.pptx"
)






### 4b. as table ------------------------------------------------------------------

table_individuals_wide <- map_dfr(tube_types, function(tube) {
  
  is_mw <- tube == "Mouthwash"
  
  data %>%
    filter(
      subject_id != "QC Only",
      tube_type == tube,
      parent_tube == TRUE
    ) %>%
    mutate(
      tube_type = tube,
      status = if_else(unprocessed, "Unprocessed", "Processed"),
      status = factor(
        status,
        levels = c("Processed", "Unprocessed")
      ),
      time_bin = if_else(
        unprocessed,
        if (is_mw) as.character(time_bin_4_mw) else as.character(time_bin_4),
        if (is_mw) as.character(time_bin_4_process_mw) else as.character(time_bin_4_process)
      ),
      time_bin = factor(
        time_bin,
        levels = if (is_mw) time_bin_levels_mw else time_bin_levels
      )
    ) %>%
    group_by(tube_type, time_bin, status) %>%
    summarise(
      n = n_distinct(subject_id),
      .groups = "drop"
    )
}) %>%
  group_by(tube_type) %>%
  mutate(
    total_tube = sum(n),
    pct = n / total_tube,
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = status,
    values_from = c(n, pct_label),
    names_glue = "{status}_{.value}"
  ) %>%
  arrange(tube_type, time_bin)


write_xlsx(
  table_individuals_wide,
  path = "bsi_exports/metric4/metric4_individuals_processing_status_by_tube_type.xlsx"
)


# Metric 4.5 (teams message) ----------------------------------------------

# I need a number for this sentence:  In addition, over xx% of samples that were
# processed had been processed outside of the expected time frame.
#
# so it is basically metric #4 overall, not separated out by material type,
# except I'd need mouthwash cut at <=5 days and > 5 days.  I don't need a plot,
# just the total number and %
#
# [expected time frame is within two days of receipt for blood/urine and 5 days
# of receipt for mouthwash].


metric_4_1_by_tube <- data %>%
  filter(
    parent_tube == TRUE,
    !unprocessed
  ) %>%
  mutate(
    outside_expected = case_when(
      tube_type %in% c("SST", "EDTA", "Heparin", "Streck", "ACD", "Urine") &
        days_to_process > 2 ~ TRUE,
      
      tube_type == "Mouthwash" &
        days_to_process > 5 ~ TRUE,
      
      TRUE ~ FALSE
    )
  ) %>%
  group_by(tube_type) %>%
  summarise(
    total_processed = n(),
    n_outside = sum(outside_expected),
    pct_outside = n_outside / total_processed,
    .groups = "drop"
  ) %>%
  mutate(
    pct_outside_label = scales::percent(pct_outside, accuracy = 1)
  ) %>%
  arrange(desc(pct_outside))

write_xlsx(
  metric_4_1_by_tube,
  path = "bsi_exports/metric4/metric4_tubes_processed_outside_timeframe.xlsx"
)




# METRIC #5 ----------------------------------------------------------------

# BSI Priority 1, Metric #5 -- Processing of SST and EDTA tubes groups by individual
# Focusing more on SST and EDTA delayed processed tubes as these are the ones we
# are most concerned about On a per individual basis (by Subject ID), we want to
# understand the delayed processing status of SST and EDTA tube types. Use all
# the data on everyone with a blood collection
# (<878865966>BioFin_BaseBloodCol_v1r0= yes). If date processed is empty, then
# the sample was not processed.

# Question 1: are all SST tubes from an individual processed together? (all SST
# tubes from an individual have the same process date)

# Similarly, are all EDTA tubes from an individual processed together?

# To answer this, we would need a count of individuals where the process date of
# all parent tubes of the same type (SST & EDTA) match vs don’t match 
# Assuming the above indicates that tubes are kept together when processed, then we could
# move on to this table and can group all tubes of the same material type
# together. I think these should all be mutually exclusive and should cover
# everyone.

## Table 5a. ----

# Part 1a. Count of individuals where the process date of all SST tubes Match vs. Don't Match

# make a variable and stitch it into main data set - "yes" or "no" logical, then count them, 
# do the same for EDTA

sst_match <- data %>%
  filter(tube_type == "SST", subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    sst_dates_match = if_else(
      n() == 0, 
      NA, 
      length(unique(date_processed)) == 1
    ),
    .groups = "drop"
  )


# Part 1b.  Count of individuals and samples where the process date of all EDTA tubes Match vs. Don't Match



subject_flags <- data %>%
  filter(subject_id != "QC Only") %>%
  group_by(subject_id, tube_type) %>%
  summarise(
    dates_match = length(unique(date_processed)) == 1,
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = tube_type,
    values_from = dates_match,
    names_glue  = "{tolower(tube_type)}_dates_match"
  )

sample_flags <- data %>%
  filter(subject_id != "QC Only") %>%
  group_by(sample_id, tube_type) %>%
  summarise(
    dates_match = length(unique(date_processed)) == 1,
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = tube_type,
    values_from = dates_match,
    names_glue  = "{tolower(tube_type)}_dates_match"
  )

sst_compare <- subject_flags %>%
  count(sst_dates_match, name = "subject_id_n") %>%
  full_join(
    sample_flags %>%
      count(sst_dates_match, name = "sample_id_n"),
    by = "sst_dates_match"
  ) %>%
  arrange(sst_dates_match)

edta_compare <- subject_flags %>%
  count(edta_dates_match, name = "subject_id_n") %>%
  full_join(
    sample_flags %>%
      count(edta_dates_match, name = "sample_id_n"),
    by = "edta_dates_match"
  ) %>%
  arrange(edta_dates_match)

write_xlsx(
  list(
    sst_dates_match  = sst_compare,
    edta_dates_match = edta_compare
  ),
  "bsi_exports/metric5/subject_vs_sample_date_match.xlsx"
)



sample_flags <- data %>%
  filter(subject_id != "QC Only") %>%
  group_by(sample_id, tube_type) %>%
  summarise(
    dates_match = length(unique(date_processed)) == 1,
    .groups = "drop"
  ) %>%
  mutate(
    tube_type = tolower(tube_type),
    tube_type = paste0(tube_type, "_dates_match")
  ) %>%
  select(sample_id, tube_type, dates_match) %>%
  tidyr::pivot_wider(
    names_from  = tube_type,
    values_from = dates_match
  )


# Counts from the same table

sst_match_counts_sample <- sample_flags %>%
  count(sst_dates_match)

edta_match_counts_sample <- sample_flags %>%
  count(edta_dates_match)


# Export

write_xlsx(sample_flags, "bsi_exports/metric5/sample_date_match_flags.xlsx")
write_xlsx(sst_match_counts_sample, "bsi_exports/metric5/sst_test_sample.xlsx")
write_xlsx(edta_match_counts_sample, "bsi_exports/metric5/edta_test_sample.xlsx")



# full SST output

sst_date_mismatches <- data %>%
  filter(tube_type == "SST") %>%
  inner_join(
    sst_match_sample %>%
      filter(sst_sample_dates_match == FALSE),
    by = "sample_id"
  ) %>%
  select(
    sample_id,
    subject_id,
    bsi_id,
    tube_type,
    date_received,
    date_processed
  ) %>%
  arrange(sample_id, date_processed)


# investigating SST summary 

sst_sample_summary <- data %>%
  filter(tube_type == "SST", sample_id %in% sst_date_mismatches$sample_id) %>%
  group_by(sample_id) %>%
  summarise(
    n_tubes = n_distinct(bsi_id),
    n_processed = sum(!unprocessed, na.rm = TRUE),
    n_unprocessed = sum(unprocessed, na.rm = TRUE),
    
    processed_date_diff_days = if_else(
      sum(!is.na(as.Date(date_processed))) > 1,
      as.numeric(
        difftime(
          max(as.Date(date_processed), na.rm = TRUE),
          min(as.Date(date_processed), na.rm = TRUE),
          units = "days"
        )
      ),
      NA_real_
    ),
    
    .groups = "drop"
  )

## EDTA investigation

edta_match_sample <- data %>%
  filter(tube_type == "EDTA") %>%
  group_by(sample_id) %>%
  summarise(
    edta_sample_dates_match = n_distinct(as.Date(date_processed)) == 1,
    .groups = "drop"
  )

edta_date_mismatches <- data %>%
  filter(tube_type == "EDTA") %>%
  inner_join(
    edta_match_sample %>%
      filter(edta_sample_dates_match == FALSE),
    by = "sample_id"
  ) %>%
  select(
    sample_id,
    subject_id,
    bsi_id,
    tube_type,
    date_received,
    date_processed
  ) %>%
  arrange(sample_id, date_processed)




edta_sample_summary <- data %>%
  filter(
    tube_type == "EDTA",
    sample_id %in% edta_date_mismatches$sample_id
  ) %>%
  group_by(sample_id) %>%
  summarise(
    n_tubes = n_distinct(bsi_id),
    n_processed = sum(!unprocessed, na.rm = TRUE),
    n_unprocessed = sum(unprocessed, na.rm = TRUE),
    
    processed_date_diff_days = if_else(
      sum(!is.na(as.Date(date_processed))) > 1,
      as.numeric(
        difftime(
          max(as.Date(date_processed), na.rm = TRUE),
          min(as.Date(date_processed), na.rm = TRUE),
          units = "days"
        )
      ),
      NA_real_
    ),
    
    .groups = "drop"
  )

library(writexl)

write_xlsx(
  list(
    SST_sample_summary  = sst_sample_summary,
    EDTA_sample_summary = edta_sample_summary
  ),
  path = "bsi_exports/metric5/SST_EDTA_processing_date_mismatches_summary.xlsx"
)


write_xlsx(
  list(
    SST_sample_summary  = sst_date_mismatches,
    EDTA_sample_summary = edta_date_mismatches
  ),
  path = "bsi_exports/metric5/SST_EDTA_processing_date_mismatches_full_data.xlsx"
)




## Table 5b. ----


# Table title: Status of SST and EDTA tube processing for each Connect individual  


# Total Number of unique subject IDs with blood collected

total_subjects_with_blood <- data %>%
  filter(tube_type %in% blood_tubes, subject_id!="QC Only") %>%
  summarise(n_subjects = n_distinct(subject_id)) %>%
  pull(n_subjects)

total_subjects_with_blood

# Total Number of unique subject IDs with at least one EDTA and SST tubes collected

total_subjects_with_SST_and_EDTA <- data %>%
  filter(tube_type %in% c("SST", "EDTA"), subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    has_SST  = any(tube_type == "SST"),
    has_EDTA = any(tube_type == "EDTA"),
    .groups = "drop"
  ) %>%
  filter(has_SST & has_EDTA) %>%
  summarise(n_subjects = n()) %>%
  pull(n_subjects)

total_subjects_with_SST_and_EDTA


#### Categories NON EXCLUSIVE including MIXED ----
# much of  this is commented out because there are many issues with the mixed tube processing 
 
# # Both SST & EDTA processed <8 days
# 
# 
sst_edta_subjects <- data %>%
  filter(tube_type %in% c("SST", "EDTA"), subject_id!="QC Only") %>%
  group_by(subject_id) %>%
  summarise(
    has_sst  = any(tube_type == "SST"),
    has_edta = any(tube_type == "EDTA")
  ) %>%
  filter(has_sst & has_edta)
# 
# both_sst_edta_processed_lt8 <- data %>%
#   filter(subject_id %in% sst_edta_subjects$subject_id,
#          tube_type %in% c("SST", "EDTA")) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     all_lt8 = all(days_to_process < 8, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     both_processed_lt8 = all(all_lt8),   # SST + EDTA both satisfy condition
#     .groups = "drop"
#   ) %>%
#   filter(both_processed_lt8 == TRUE)
# 
# nrow(both_sst_edta_processed_lt8)
# 
# 
# # SST processed <8 days/EDTA processed >=8 days
# 
# sst_lt8_edta_ge8 <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = case_when(
#       tube_type == "SST"  ~ all(days_to_process < 8,  na.rm = TRUE),
#       tube_type == "EDTA" ~ all(days_to_process >= 8, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     sst_lt8_edta_ge8 = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(sst_lt8_edta_ge8)
# 
# nrow(sst_lt8_edta_ge8)
# 
# 
# 
# # SST processed <8 days/EDTA not processed
# 
# 
# sst_lt8_edta_not_processed <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = case_when(
#       tube_type == "SST"  ~ all(days_to_process < 8, na.rm = TRUE),
#       tube_type == "EDTA" ~ all(is.na(days_to_process))
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     sst_lt8_edta_not_processed = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(sst_lt8_edta_not_processed)
# 
# nrow(sst_lt8_edta_not_processed)
# 
# 
# # EDTA processed <8 days/SST processed >=8 days
# 
# edta_lt8_sst_ge8 <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = case_when(
#       tube_type == "EDTA" ~ all(days_to_process < 8,  na.rm = TRUE),
#       tube_type == "SST"  ~ all(days_to_process >= 8, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     edta_lt8_sst_ge8 = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(edta_lt8_sst_ge8)
# 
# nrow(edta_lt8_sst_ge8)
# 
# 
# 
# # EDTA processed <8 days/SST not processed
# 
# edta_lt8_sst_not_processed <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = case_when(
#       tube_type == "EDTA" ~ all(days_to_process < 8,  na.rm = TRUE),
#       tube_type == "SST"  ~ all(is.na(days_to_process))
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     edta_lt8_sst_not_processed = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(edta_lt8_sst_not_processed)
# 
# nrow(edta_lt8_sst_not_processed)
# 
# 
# 
# # Both SST & EDTA processed >=8 days
# 
# sst_edta_ge8 <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = case_when(
#       tube_type == "SST"  ~ all(days_to_process >= 8, na.rm = TRUE),
#       tube_type == "EDTA" ~ all(days_to_process >= 8, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     both_ge8 = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(both_ge8)
# 
# nrow(sst_edta_ge8)
# 
# 
# 
# # SST processed >= 8 days; EDTA not processed
# 
# sst_ge8_edta_not_processed <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = case_when(
#       tube_type == "SST"  ~ all(days_to_process >= 8, na.rm = TRUE),
#       tube_type == "EDTA" ~ all(is.na(days_to_process))
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     sst_ge8_edta_not_processed = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(sst_ge8_edta_not_processed)
# 
# nrow(sst_ge8_edta_not_processed)
# 
# 
# 
# # EDTA processed >= 8 days; SST not processed
# 
# edta_ge8_sst_not_processed <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = case_when(
#       tube_type == "EDTA" ~ all(days_to_process >= 8, na.rm = TRUE),
#       tube_type == "SST"  ~ all(is.na(days_to_process))
#     ),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     edta_ge8_sst_not_processed = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(edta_ge8_sst_not_processed)
# 
# nrow(edta_ge8_sst_not_processed)
# 
# 
# 
# 
# # Neither SST nor EDTA processed
# 
# neither_sst_nor_edta_processed <- data %>%
#   filter(
#     subject_id %in% sst_edta_subjects$subject_id,
#     tube_type %in% c("SST", "EDTA")
#   ) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     condition_met = all(is.na(days_to_process)),
#     .groups = "drop"
#   ) %>%
#   group_by(subject_id) %>%
#   summarise(
#     neither_processed = all(condition_met),
#     .groups = "drop"
#   ) %>%
#   filter(neither_processed)
# 
# nrow(neither_sst_nor_edta_processed)
# 
# 
# # final table 
# 
# processing_summary <- tibble::tibble(
#   Status = c(
#     "Both SST & EDTA processed <8 days",
#     "SST <8 days / EDTA ≥8 days",
#     "SST <8 days / EDTA not processed",
#     "EDTA <8 days / SST ≥8 days",
#     "EDTA <8 days / SST not processed",
#     "Both SST & EDTA processed ≥8 days",
#     "SST ≥8 days / EDTA not processed",
#     "EDTA ≥8 days / SST not processed",
#     "Neither SST nor EDTA processed"
#   ),
#   N = c(
#     nrow(both_sst_edta_processed_lt8),
#     nrow(sst_lt8_edta_ge8),
#     nrow(sst_lt8_edta_not_processed),
#     nrow(edta_lt8_sst_ge8),
#     nrow(edta_lt8_sst_not_processed),
#     nrow(sst_edta_ge8),
#     nrow(sst_ge8_edta_not_processed),
#     nrow(edta_ge8_sst_not_processed),
#     nrow(neither_sst_nor_edta_processed)
#   )
# )
# 
# processing_summary <- processing_summary %>%
#   mutate(
#     Percent = round(100 * N / total_subjects_with_SST_and_EDTA, 1)
#   )
# 
# processing_summary <- processing_summary %>%
#   mutate(
#     `N (%)` = paste0(N, " (", Percent, "%)")
#   )
# 
# sum(processing_summary$N) == total_subjects_with_SST_and_EDTA

##### Debugging mixed samples ----
# 
# subject_category_membership <- tibble::tibble(
#   subject_id = sst_edta_subjects$subject_id,
#   both_lt8                  = subject_id %in% both_sst_edta_processed_lt8$subject_id,
#   sst_lt8_edta_ge8           = subject_id %in% sst_lt8_edta_ge8$subject_id,
#   sst_lt8_edta_not_processed = subject_id %in% sst_lt8_edta_not_processed$subject_id,
#   edta_lt8_sst_ge8           = subject_id %in% edta_lt8_sst_ge8$subject_id,
#   edta_lt8_sst_not_processed = subject_id %in% edta_lt8_sst_not_processed$subject_id,
#   both_ge8                  = subject_id %in% sst_edta_ge8$subject_id,
#   sst_ge8_edta_not_processed = subject_id %in% sst_ge8_edta_not_processed$subject_id,
#   edta_ge8_sst_not_processed = subject_id %in% edta_ge8_sst_not_processed$subject_id,
#   neither_processed          = subject_id %in% neither_sst_nor_edta_processed$subject_id
# )
# 
# 
# subject_category_membership <- subject_category_membership %>%
#   mutate(n_categories = rowSums(across(-subject_id)))
# 
# table(subject_category_membership$n_categories)
# 
# subject_category_membership %>%
#   filter(n_categories > 1)
# 
# 
# subject_category_membership %>%
#   filter(n_categories == 0)
# 
# 
# data %>%
#   filter(subject_id %in% sst_edta_subjects$subject_id,
#          tube_type %in% c("SST", "EDTA")) %>%
#   group_by(subject_id, tube_type) %>%
#   summarise(
#     n_total = n(),
#     n_na = sum(is.na(days_to_process)),
#     n_non_na = sum(!is.na(days_to_process)),
#     .groups = "drop"
#   ) %>%
#   filter(n_na > 0 & n_non_na > 0)
#



#### Categories EXCLUSIVE ----
# drop mixed processing dates and ensure each subject is assigned to only one category

tube_status <- data %>%
  filter(
    subject_id %in% sst_edta_subjects$subject_id,
    tube_type %in% c("SST", "EDTA")
  ) %>%
  group_by(subject_id, tube_type) %>%
  summarise(
    status = case_when(
      all(is.na(days_to_process)) ~ "not_processed",
      all(days_to_process < 8,  na.rm = TRUE) &
        !any(is.na(days_to_process)) ~ "<8",
      all(days_to_process >= 8, na.rm = TRUE) &
        !any(is.na(days_to_process)) ~ ">=8",
      TRUE ~ "mixed"
    ),
    .groups = "drop"
  )


# # investigate the tube types of mixed
# 
mixed_subjects <- tube_status %>%
  group_by(subject_id) %>%
  summarise(
    has_mixed = any(status == "mixed"),
    .groups = "drop"
  ) %>%
  filter(has_mixed)
# 
# n_mixed_subjects <- nrow(mixed_subjects)
# 
# n_mixed_subjects
# 
# 
# tube_status %>%
#   filter(status == "mixed") %>%
#   count(tube_type)
# 
# mixed_subjects$subject_id
# 
# 
# tube_status %>%
#   filter(status == "mixed") %>%
#   arrange(subject_id, tube_type)

# mixed_subjects$subject_id
# 
# tube_status %>%
#   filter(status == "mixed") %>%
#   arrange(subject_id, tube_type)



clean_subjects <- tube_status %>%
  group_by(subject_id) %>%
  summarise(
    has_mixed = any(status == "mixed"),
    .groups = "drop"
  ) # %>%
  #filter(!has_mixed)

mixed_subjects$subject_id


clean_denominator <- nrow(clean_subjects)

subject_processing_status <- tube_status %>%
  filter(subject_id %in% clean_subjects$subject_id) %>%
  tidyr::pivot_wider(
    names_from = tube_type,
    values_from = status
  )

final_table <- subject_processing_status %>%
  count(SST, EDTA, name = "N") %>%
  mutate(
    Percent = round(100 * N / clean_denominator, 1)
  ) 

sum(final_table$N) == clean_denominator

##### export excel to /reports/ ----
write_xlsx(final_table, "bsi_exports/metric5/metric5_draft.xlsx")




# METRIC #6 ---------------------------------------------------------------

# EDTA Parent tubes

# Plots by year with # weeks to processing/receipt on the X axis and # of EDTA tubes on
# the y-axis, separately by year (2022, 2023, 2024, 2025) 

# Show three lines: 
# 1. EDTA tubes that were processed (based on time between receipt at BPTL and
# “Date Processed”) 
# 2. EDTA tubes that were not processed (use time from receipt at BPTL to date of BSI data receipt)
# 3. EDTA tubes that were the parent tubes to the buffy coat tubes shipped to CGR 
# for genotyping assays
# (these will be a subset of those in #1 and Kathleen will send a file with that
# information) 


data_edta <- data %>%
  filter(
    tube_type == "EDTA",
    parent_tube == TRUE
  ) %>%
  mutate(
    year_received = year(date_received),
    
    weeks_to_process = as.numeric(
      difftime(date_processed, date_received, units = "weeks")
    ),
    
    weeks_to_bsi = as.numeric(
      difftime(data_delivery_date, date_received, units = "weeks")
    )
  ) %>%
  filter(year_received %in% 2022:2025)

# read CGR file

library(readxl)

cgr_edta <- read_xlsx(
  path = "bsi_data/edta_buffycoat_cgr_2026-01-26.xlsx",
  sheet = 1  
) %>%
  clean_names() %>%
  mutate(cgr_parent = TRUE)

# normalize 

data_edta <- data_edta %>%
  mutate(bsi_id = as.character(bsi_id))

cgr_ids <- cgr_edta %>%
  mutate(source_id = as.character(source_id))


data_edta <- data_edta %>%
  mutate(
    cgr_parent = bsi_id %in% cgr_ids$source_id
  )

# Count CGR parents (rows)
sum(data_edta$cgr_parent)

# Count unique EDTA parent tubes sent to CGR
data_edta %>%
  filter(cgr_parent) %>%
  nrow()

cgr_edta %>%
  distinct(source_id) %>%
  nrow()

data_edta %>%
  distinct(bsi_id) %>%
  nrow()

length(intersect(
  unique(as.character(cgr_edta$source_id)),
  unique(as.character(data_edta$bsi_id))
))

length(setdiff(
  unique(as.character(cgr_edta$source_id)),
  unique(as.character(data_edta$bsi_id))
))


missing_source_ids <- setdiff(
  unique(as.character(cgr_edta$source_id)),
  unique(as.character(data_edta$bsi_id))
)

length(missing_source_ids)







# plot by years


years <- c(2022, 2023, 2024, 2025)

plots <- map(years, function(yr) {
  
  df_plot <- bind_rows(
    
    # Processed EDTA
    data_edta %>%
      filter(year_received == yr, unprocessed == FALSE) %>%
      mutate(
        weeks = floor(weeks_to_process),
        group = "Processed EDTA tubes"
      ),
    
    # Unprocessed EDTA
    data_edta %>%
      filter(year_received == yr, unprocessed == TRUE) %>%
      mutate(
        weeks = floor(weeks_to_bsi),
        group = "Unprocessed EDTA tubes"
      ),
    
    # CGR parents (subset of processed)
    data_edta %>%
      filter(
        year_received == yr,
        unprocessed == FALSE,
        cgr_parent == TRUE
      ) %>%
      mutate(
        weeks = floor(weeks_to_process),
        group = "EDTA parents to CGR buffy coats"
      )
    
  ) %>%
    filter(!is.na(weeks), weeks >= 0) %>%
    group_by(weeks, group) %>%
    summarise(n = n(), .groups = "drop")
  
  N_year <- sum(df_plot$n)
  
  ggplot(df_plot, aes(x = weeks, y = n, color = group)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1) +                     # 🔹 dots
    labs(
      title = paste0(
        "EDTA Parent Tubes by Time Since Receipt or Processing (", yr,
        ", N = ", N_year, ")"
      ),
      x = "Weeks since receipt or processing",
      y = "Count of EDTA parent tubes",
      color = "Tube category"
    ) +
    theme_minimal(base_size = 14)
})


plots[[1]]  # 2022
plots[[2]]  # 2023
plots[[3]]  # 2024
plots[[4]]  # 2025

walk2(
  plots,
  years,
  ~ ggsave(
    filename = paste0("bsi_exports/metric6/edta_parent_tubes_", .y, ".png"),
    plot = .x,
    width = 12,
    height = 8
  )
)

# pdf

metric6_plots <- setNames(plots, years)

pdf(
  file = "bsi_exports/metric6/edta_parent_tubes_by_year.pdf",
  width = 12,
  height = 8
)

for (p in metric6_plots) {
  print(p)
}

dev.off()


ppt <- read_pptx()

for (p in metric6_plots) {
  
  ppt <- ppt %>%
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with(
      value = dml(ggobj = p),
      location = ph_location(
        left   = 0.25,
        top    = 0.25,
        width  = 9.5,
        height = 6.8
      )
    )
}

print(
  ppt,
  target = "bsi_exports/metric6/edta_parent_tubes_by_year.pptx"
)


# tables


tables_by_year <- map(years, function(yr) {
  
  df_table <- bind_rows(
    
    # Processed EDTA
    data_edta %>%
      filter(year_received == yr, unprocessed == FALSE) %>%
      mutate(
        weeks = floor(weeks_to_process),
        group = "Processed EDTA tubes"
      ),
    
    # Unprocessed EDTA
    data_edta %>%
      filter(year_received == yr, unprocessed == TRUE) %>%
      mutate(
        weeks = floor(weeks_to_bsi),
        group = "Unprocessed EDTA tubes"
      ),
    
    # CGR parents
    data_edta %>%
      filter(
        year_received == yr,
        unprocessed == FALSE,
        cgr_parent == TRUE
      ) %>%
      mutate(
        weeks = floor(weeks_to_process),
        group = "EDTA parents to CGR buffy coats"
      )
    
  ) %>%
    filter(!is.na(weeks), weeks >= 0) %>%
    group_by(weeks, group) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = group,
      values_from = n,
      values_fill = 0
    ) %>%
    arrange(weeks)
  
  df_table
})

names(tables_by_year) <- years


tables_by_year[[1]]  # 2022
tables_by_year[[2]]  # 2023
tables_by_year[[3]]  # 2024
tables_by_year[[4]]  # 2025

table_all_years <- map_dfr(
  years,
  ~ tables_by_year[[as.character(.x)]] %>%
    mutate(year_received = .x),
  .id = NULL
) %>%
  relocate(year_received, .before = weeks)


library(writexl)

write_xlsx(
  tables_by_year,
  path = "bsi_exports/metric6/edta_parent_tubes_by_week_tables.xlsx"
)

write_xlsx(
  table_all_years,
  path = "bsi_exports/metric6/edta_parent_tubes_by_week_tables_all.xlsx"
)


# plot with all years

df_plot <- bind_rows(
  
  # Processed EDTA
  data_edta %>%
    filter(unprocessed == FALSE) %>%
    mutate(
      weeks = floor(weeks_to_process),
      group = "Processed EDTA tubes"
    ),
  
  # Unprocessed EDTA
  data_edta %>%
    filter(unprocessed == TRUE) %>%
    mutate(
      weeks = floor(weeks_to_bsi),
      group = "Unprocessed EDTA tubes"
    ),
  
  # CGR parents (subset of processed)
  data_edta %>%
    filter(
      unprocessed == FALSE,
      cgr_parent == TRUE
    ) %>%
    mutate(
      weeks = floor(weeks_to_process),
      group = "EDTA parents to CGR buffy coats"
    )
  
) %>%
  filter(!is.na(weeks), weeks >= 0) %>%
  group_by(weeks, group) %>%
  summarise(n = n(), .groups = "drop")

N_all <- sum(df_plot$n)

ggplot(df_plot, aes(x = weeks, y = n, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1) +
  labs(
    title = paste0(
      "EDTA Parent Tubes by Time Since Receipt or Processing (All Years, N = ",
      N_all, ")"
    ),
    x = "Weeks since receipt or processing",
    y = "Count of EDTA parent tubes",
    color = "Tube category"
  ) +
  theme_minimal(base_size = 14)


ggsave(
  filename = "bsi_exports/metric6/metric6_edta_parent_tubes_by_weeks_all_years.png",
  width = 12,
  height = 8,
  dpi = 300
)



outliers <- df_plot %>%
  filter(n > 5000) %>%
  arrange(desc(n))

outliers



ggplot(df_plot, aes(x = weeks, y = n, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1) +
  coord_cartesian(ylim = c(0, 5000)) +
  labs(
    title = paste0(
      "EDTA Parent Tubes by Time Since Receipt or Processing (All Years, N = ",
      N_all, ")"
    ),
    subtitle = "Y-axis capped at 5,000; weeks exceeding this threshold noted separately",
    x = "Weeks since receipt or processing",
    y = "Count of EDTA parent tubes",
    caption = "Y-axis truncated at 5,000. Outliers at week 0: Processed EDTA (n = 34,046); CGR parents (n = 20,087).",
    color = "Tube category"
  ) +
  theme_minimal(base_size = 14)


ggsave(
  filename = "bsi_exports/metric6/metric6_edta_parent_tubes_by_weeks_all_years_truncated.png",
  width = 12,
  height = 8,
  dpi = 300
)


if (nrow(outliers) > 0) {
  print(
    paste(
      "Outliers detected:",
      paste(
        paste0(
          "Week ", outliers$weeks,
          " (", outliers$group,
          ", n = ", outliers$n, ")"
        ),
        collapse = "; "
      )
    )
  )
}




# GROUPINGS / CONNECT ID IDENTIFICATION -----------------------------------
#
# Have you started to think about the groupings for the letters?  We had talked
# about:
#
# your samples were all processed within expected time frame most of your
# samples were processed within expected time frame but some were not (most
# likely scenario is urine & mouthwash are fine but we won't specify that
# because not everyone gave urine & mouthwash) most of your samples were
# processed outside of the expected time frame
#
# We have a meeting at 4pm today to discuss our definitions for “expected time
# frame”.  I was thinking it would be good to have some estimates as to how many
# would fall into the first category if we went with “expected” being <= 2 days
# for blood and urine and <=5 days for mouthwash.  (If a sample was not
# processed at all, it would fall into beyond the expected time frame). But I
# also don’t want to make extra work for you.
#
# What do you think about getting going now?  If we change the cutpoints, would
# it be relatively easy to update the code with the new cutpoints?  If it would
# not be too much extra work to get going, that would be great. Also, questions
# might come up while you are coding that we can try to address as soon as
# possible.
#
# I think group 1 is relatively straightforward, but we have not defined the
# nuanced difference between groups 2 and 3 so hold on that
# 


# Connect ID
# Blood tubes // blood tubes not processed (by type?) in time -- do by type and total 
# Urine tubes  // urine tubes not processed in time 
# Mouthwash tubes // mouthwash tubes not processed in time
# Group level for communication 
# perfect
# ok
# bad
# Just invent the categories for now.
# put this on BQ so they can pull it? 

# first cutoff, 2 days for blood/urine and 5 days for mouthwash

# relevant variables

#filter by parent_tube = TRUE 
#tube_type - all tube types 
#blood_tubes - blood specific tubes for a total blood count instead of by tube
#urine_tubes - urine tubes
#mouthwash_tubes - a separate mouthwash tube for saliva due to different delayed processing 

#subject_id (distinct)
#days_to_process 

# give a new dataframe that 
# collapses to one subject_id 
# and counts across that ID's blood_tubes, urine_tubes, and mouthwash_tubes
# the number of tubes given for each of blood_tubes, urine_tubes, and mouthwash_tubes
# the number of tubes NOT processed in an acceptable time frame:
#  <= 2 days for blood and urine
# <=5 days for mouthwash 
# format it so the resulting data is id | blood tubes given | blood tubes not processed etc


subject_tube_summary <- data %>%
  filter(parent_tube == TRUE) %>%
  mutate(
    tube_category = case_when(
      tube_type %in% c("ACD", "EDTA", "Heparin", "SST", "Streck") ~ "blood",
      tube_type %in% c("Urine")                                 ~ "urine",
      tube_type %in% c("Mouthwash")                             ~ "mouthwash",
      TRUE                                                      ~ NA_character_
    ),
    not_processed_ok = case_when(
      # NA days_to_process always counts as not OK
      is.na(days_to_process)                                   ~ TRUE,
      
      tube_category %in% c("blood", "urine") &
        days_to_process > 30                                    ~ TRUE,
      
      tube_category == "mouthwash" &
        days_to_process > 30                                    ~ TRUE,
      
      TRUE                                                     ~ FALSE
    )
  )

# group by subject ID 

subject_tube_summary <- subject_tube_summary %>%
  group_by(subject_id) %>%
  summarise(
    blood_tubes_given       = sum(tube_category == "blood", na.rm = TRUE),
    blood_tubes_not_ok      = sum(tube_category == "blood" & not_processed_ok),
    
    urine_tubes_given       = sum(tube_category == "urine", na.rm = TRUE),
    urine_tubes_not_ok      = sum(tube_category == "urine" & not_processed_ok),
    
    mouthwash_tubes_given   = sum(tube_category == "mouthwash", na.rm = TRUE),
    mouthwash_tubes_not_ok  = sum(tube_category == "mouthwash" & not_processed_ok),
    
    .groups = "drop"
  )


# all processed

subject_tube_summary <- subject_tube_summary %>%
  mutate(
    total_not_ok = blood_tubes_not_ok +
      urine_tubes_not_ok +
      mouthwash_tubes_not_ok,
    all_processed = total_not_ok == 0
  )


all_processed_counts <- subject_tube_summary %>%
  count(all_processed)


# none processed


subject_tube_summary <- subject_tube_summary %>%
  mutate(
    # category-level (NA if no tubes given)
    blood_none_processed = case_when(
      blood_tubes_given == 0                           ~ NA,
      blood_tubes_not_ok == blood_tubes_given          ~ TRUE,
      TRUE                                             ~ FALSE
    ),
    
    urine_none_processed = case_when(
      urine_tubes_given == 0                           ~ NA,
      urine_tubes_not_ok == urine_tubes_given          ~ TRUE,
      TRUE                                             ~ FALSE
    ),
    
    mouthwash_none_processed = case_when(
      mouthwash_tubes_given == 0                       ~ NA,
      mouthwash_tubes_not_ok == mouthwash_tubes_given  ~ TRUE,
      TRUE                                             ~ FALSE
    ),
    
    # subject-level AND across categories that exist
    none_processed =
      (is.na(blood_none_processed)     | blood_none_processed) &
      (is.na(urine_none_processed)     | urine_none_processed) &
      (is.na(mouthwash_none_processed) | mouthwash_none_processed) &
      # guardrail: must have at least one tube total
      (blood_tubes_given +
         urine_tubes_given +
         mouthwash_tubes_given > 0)
  )


# counts

processing_summary <- subject_tube_summary %>%
  mutate(
    processing_status = case_when(
      all_processed  ~ "all tubes processed within 30 (blood, urine) or 30 (mouthwash) days",
      none_processed ~ "no tubes processed within 30 (blood, urine) or 30 (mouthwash) days",
      TRUE           ~ "some tubes processed within 30 (blood, urine) or 30 (mouthwash) days"
    )
  ) %>%
  count(processing_status, name = "n")

processing_summary <- processing_summary %>%
  bind_rows(
    processing_summary %>%
      summarise(
        processing_status = "total",
        n = sum(n)
      )
  )


write_xlsx(
  list(
    processing_summary   = processing_summary,
    subject_tube_summary = subject_tube_summary
  ),
  "bsi_exports/categories/subject_processing_summary_30.xlsx"
)



# Codebooks ----------------------------------------------------

# codebooks 

print(dfSummary(data), method = "browser")
print(dfSummary(all_metrics), method = "browser")

