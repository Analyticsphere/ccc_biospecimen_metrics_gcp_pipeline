# BSI_recurring_metrics.R ############################################################

# Script Name: BSI_recurring_metrics.R 
# Author: Michelle Hudson 
# Date: February 2 2026
# Date updated: February 9 2026
# Description: 
# 
# Output:
#   - One combined excel file of tabular data
#   - One plot and two excel files for RM3
# ..........................................................................

## load tools --------------------------------------------------------------

# data download tools
library(plumber)
library(rmarkdown)
library(googleCloudStorageR)
library(gargle)
library(tools)
library(glue)
library(bigrquery)


# cleaning, analysis, and plotting tools
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(dplyr)
library(purrr)
library(readxl)
library(forcats)
# library(scales) - don't load this, just call it in ggplot where needed -- it
# masks a function and breaks the import code

# export tools 
library(writexl)
library(openxlsx)
library(officer)
library(rvg)

# data exploration/documentation tools
library(psych)
library(skimr)
library(Hmisc)
library(summarytools)


# BQ Pull -----------------------------------------------------------------

bq_auth()

project = "nih-nci-dceg-connect-prod-6d04"

# from participants

query_ppts <- "SELECT  Connect_ID, d_827220437 
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` where Connect_ID IS NOT NULL" 

# from cancer occurrence

query_cancer <- "SELECT Connect_ID, d_525972260 
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.cancerOccurrence` where Connect_ID IS NOT NULL"

# from biospec -- Connect ID, collection ID, tube types, tube received dates


# bio pull for tubes

query_bio <- "
SELECT
  Connect_ID,
  d_820476880 AS collection_id_connect,
  tube.tube_id_connect,
  tube.tube_received_date_connect,
  tube.tube_type_connect

FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen` b
CROSS JOIN UNNEST([
  STRUCT('SST1' AS tube_type_connect,
         d_299553921_d_825582494 AS tube_id_connect,
         d_299553921_d_926457119 AS tube_received_date_connect),

  STRUCT('SST2',
         d_703954371_d_825582494,
         d_703954371_d_926457119),

  STRUCT('SST3',
         d_376960806_d_825582494,
         d_376960806_d_926457119),

  STRUCT('SST4',
         d_232343615_d_825582494,
         d_232343615_d_926457119),

  STRUCT('SST5',
         d_589588440_d_825582494,
         d_589588440_d_926457119),

  STRUCT('EDTA1',
         d_454453939_d_825582494,
         d_454453939_d_926457119),

  STRUCT('EDTA2',
         d_677469051_d_825582494,
         d_677469051_d_926457119),

  STRUCT('EDTA3',
         d_683613884_d_825582494,
         d_683613884_d_926457119),

  STRUCT('ACD',
         d_652357376_d_825582494,
         d_652357376_d_926457119),

  STRUCT('Hep1',
         d_838567176_d_825582494,
         d_838567176_d_926457119),

  STRUCT('Hep2',
         d_958646668_d_825582494,
         d_958646668_d_926457119),

  STRUCT('STRECK',
         d_505347689_d_825582494,
         d_505347689_d_926457119),

  STRUCT('Urine',
         d_973670172_d_825582494,
         d_973670172_d_926457119),

  STRUCT('Mouthwash',
         d_143615646_d_825582494,
         COALESCE(d_143615646_d_926457119, d_143615646_d_826941471))
]) AS tube

WHERE
  d_820476880 IS NOT NULL
  AND (d_410912345 ='353358909' or SUBSTR(tube.tube_id_connect, 1, 3)='CHA')
  AND tube.tube_id_connect IS NOT NULL
"


# bio pull for collection setting

query_coll <- "
SELECT 
  d_820476880 AS collection_id_connect,
  CASE
    WHEN d_650516960 = '534621077' THEN 'Research'
    WHEN d_650516960 = '664882224' THEN 'Clinical'
    ELSE 'Home'
  END AS collection_setting
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`
WHERE
  d_820476880 IS NOT NULL
  AND (
    d_410912345 = '353358909'
    OR STARTS_WITH(d_820476880, 'CHA')
  )
"

  

# finish pull for each table, join in R for downstream join to BSI

# Participants
q_ppts <- bq_project_query(project, query_ppts)
data_ppts <- bq_table_download(q_ppts, bigint = "integer64")

# Cancer
q_cancer <- bq_project_query(project, query_cancer)
data_cancer <- bq_table_download(q_cancer, bigint = "integer64")

# Bio
q_bio <- bq_project_query(project, query_bio)
data_bio <- bq_table_download(q_bio, bigint = "integer64", page_size = NULL)

# Coll
q_coll <- bq_project_query(project, query_coll)
data_coll <- bq_table_download(q_coll, bigint = "integer64", page_size = NULL)


# dedupe ppts

data_ppts %>%
  count(Connect_ID) %>%
  filter(n > 1)


# dedupe cancer 

data_cancer_one <- data_cancer %>%
  group_by(Connect_ID) %>%
  summarise(
    cancer_diag = any(!is.na(d_525972260)),
    .groups = "drop"
  )

data_connect_persons <- data_ppts %>%
  left_join(data_cancer_one, by = "Connect_ID")


# bio

tube_dupes_full <- data_bio %>%
  group_by(tube_id_connect) %>%
  filter(n() > 1) %>%
  ungroup()


sum(is.na(data_bio$tube_received_date_connect))

data_bio %>%
  group_by(tube_type_connect) %>%
  summarise(
    total = n(),
    na_received_dates = sum(is.na(tube_received_date_connect)),
    pct_na = 100 * na_received_dates / total
  ) %>%
  arrange(desc(pct_na))


# fix dates in data_bio to remove timestamp

data_bio <- data_bio %>%
  mutate(
    tube_received_date_connect     = as.Date(tube_received_date_connect)
  )



# GCP Authentication ---------------------------------------------

# Authenticate with Google Storage, set up folders
scope <- c("https://www.googleapis.com/auth/cloud-platform")
token <- token_fetch(scopes = scope)
gcs_auth(token = token)


gcs_global_bucket("bsi_tables")

buckets <- gcs_list_buckets(project)
bucket <- "bsi_tables"



# Data import as a function -----------------------------------------------

read_bsi_gcs <- function(
    download_date = Sys.Date(),
    bucket = NULL
) {

  # set bucket
  if (!is.null(bucket)) {
    gcs_global_bucket(bucket)
  }
  
  # Coerce and validate date
  download_date <- as.Date(download_date)
  if (is.na(download_date)) {
    stop("`download_date` must be coercible to a Date (e.g., '2026-01-20')")
  }
  
  date_folder <- format(download_date, "%Y-%m-%d")
  
  # GCS "folder" prefix (IMPORTANT)
  prefix <- glue::glue("{date_folder}/")
  
  # List objects
  objects <- gcs_list_objects(prefix = prefix)
  
  if (nrow(objects) == 0) {
    stop(glue::glue("No objects found for prefix: {prefix}"))
  }
  
  # Filter CSVs
  csv_objects <- objects %>%
    filter(
      str_ends(name, "\\.csv"),
      !str_detect(name, "CNX Not In Sequence Scheme\\.csv"),
      !str_detect(name, "373 not processed from 2025\\.csv")
    )
  
  if (nrow(csv_objects) == 0) {
    stop(glue::glue("No CSV files found for prefix: {prefix}"))
  }
  
  # Read, stitch, clean
  data_raw <- csv_objects$name %>%
    map_dfr(function(obj) {
      
      tf <- tempfile(fileext = ".csv")
      
      gcs_get_object(
        object_name = obj,
        saveToDisk  = tf,
        overwrite  = TRUE
      )
      
      read_csv(tf, col_types = cols(.default = "c")) %>%
        clean_names() %>%
        mutate(source_file = basename(obj))
    }) %>%
    mutate(
      across(c(date_drawn, date_received, date_processed),
             ~ na_if(.x, "")),
      across(c(date_drawn, date_received, date_processed),
             ~ str_trim(.x))
    ) %>%
    mutate(
      date_drawn     = mdy_hm(date_drawn),
      date_received  = mdy_hm(date_received),
      date_processed = mdy_hm(date_processed), 
      data_delivery_date = download_date
    )
  
  data_raw
}


# Another function to get a single object (maybe unnecessary in weekly) --------

# additional_bsi_file <- function(
#     file_name,
#     download_date = Sys.Date()
# ) {
#   # Coerce and validate date
#   download_date <- as.Date(download_date)
#   if (is.na(download_date)) {
#     stop("`download_date` must be coercible to a Date (e.g., '2026-01-20')")
#   }
#   
#   date_folder <- format(download_date, "%Y-%m-%d")
#   
#   # Build full GCS object name
#   object_name <- glue("{date_folder}/{file_name}")
#   
#   # Determine file extension
#   ext <- tools::file_ext(file_name)
#   
#   if (!ext %in% c("csv", "xls", "xlsx")) {
#     stop("Unsupported file type: ", ext)
#   }
#   
#   # Temp file with matching extension
#   tf <- tempfile(fileext = paste0(".", ext))
#   
#   # Stream object to disk
#   gcs_get_object(
#     object_name = object_name,
#     saveToDisk  = tf,
#     overwrite  = TRUE
#   )
#   
#   # Read based on file type
#   data <- switch(
#     ext,
#     csv  = read_csv(tf, col_types = cols(.default = "c")),
#     xls  = read_excel(tf),
#     xlsx = read_excel(tf)
#   )
#   
#   # Clean and standardize
#   data %>%
#     clean_names() %>%
#     mutate(
#       source_file = file_name,
#       data_delivery_date = download_date
#     ) %>%
#     mutate(
#       across(c(date_drawn, date_received, date_processed),
#              ~ na_if(as.character(.x), "")),
#       across(c(date_drawn, date_received, date_processed),
#              ~ str_trim(.x))
#     ) %>%
#     mutate(
#       date_drawn     = mdy_hm(date_drawn),
#       date_received  = mdy_hm(date_received),
#       date_processed = mdy_hm(date_processed)
#     )
# }
#                               



# Call function methods ----------------------------------------------------

# sys date (today)
data_raw <- read_bsi_gcs()

# 
# # specific date (must be a bucket folder named this)
# data_raw <- read_bsi_gcs("2026-02-02")


# # explicit bucket
# data_raw <- read_bsi_gcs(
#   download_date = "2026-02-06",
#   bucket = "bsi_tables"
# )

# additional file

# not_processed_2025 <- additional_bsi_file(
#   "373 not processed from 2025.csv",
#   download_date = "2026-01-20" # if needed
# )


# Additional IDs were missing from January --------------------------------
# 
# bsi_only_in_csv <- setdiff(
#   unique(not_processed_2025$bsi_id),
#   unique(data_raw$bsi_id)
# )
# 
# 
# not_processed_missing_from_data <- not_processed_2025 %>%
#   filter(bsi_id %in% bsi_only_in_csv)
# 
# 
# not_processed_missing_from_data <- not_processed_missing_from_data %>%
#   mutate(
#     sequence = str_pad(sequence, width = 4, side = "left", pad = "0")
#   )
# 
# missing_cols <- setdiff(names(data_raw), names(not_processed_missing_from_data))
# 
# not_processed_missing_from_data <- not_processed_missing_from_data %>%
#   mutate(across(all_of(missing_cols), ~ NA))
# 
# not_processed_missing_from_data <- not_processed_missing_from_data %>%
#   select(all_of(names(data_raw)))
# 
# data_raw <- bind_rows(
#   data_raw,
#   not_processed_missing_from_data
# )

# Data cleaning -----------------------------------------------------------


# for this analysis, remove timestamps, as some dates do not have them 

data_raw <- data_raw %>%
  mutate(
    date_drawn     = as.Date(date_drawn),
    date_received  = as.Date(date_received),
    date_processed = as.Date(date_processed)
  )

# Select the week last Friday through Thursday if necessary

# Define the most recent completed Friday–Thursday window
# end_thursday <- floor_date(Sys.Date(), unit = "week", week_start = 5) - days(1)
# start_friday <- end_thursday - days(6)
# 
# data_raw <- data_raw %>%
#   filter(
#     date_received >= start_friday,
#     date_received <= end_thursday
#   ) 

data <- data_raw # %>%
  # filter(subject_id!="QC Only") %>%  # leave this in the tube-level data, 
  #remove it from analysis for the individual - level data
  # select(-obs) #dropping this variable as it is per-data-file and has no meaning for analysis 

# Create tube type categories based on definitions document (sequence number)
# 23 is in the documents but excluded because 1) there are none in the current data and 
# 2) it is listed as being assigned to both Heparin and SST but without any actual tubes, we can't tell 
# which is correct
# Stephanie confirmed that tube sequence 0061 is SUPPOSED to be Streck, but in the single instance in this data, it is SST

# new code does not have leading zeroes

# data <- data %>%
#   mutate(
#     tube_type = case_when(
#       sequence %in% c("0001","0002","0011","0012","0021","0022","0031","0071","0081","0091") ~ "SST",
#       sequence %in% c("0003","0013","0033","0063","0073","0083","0093") ~ "Heparin",
#       sequence %in% c("0004","0014","0024","0034", "0064") ~ "EDTA",
#       sequence == "0005" ~ "ACD",
#       sequence %in% c("0006","0016","0026") ~ "Urine",
#       sequence == "0007" ~ "Mouthwash",
#       sequence %in% c("0060", "0061") ~ "Streck",
#       TRUE ~ NA_character_
#     )
#   )


download_date <- data$data_delivery_date[1]

data <- data %>%
  mutate(
    tube_type = case_when(
      sequence %in% c("1","2","11","12","21","22","31","71","81","91") ~ "SST",
      sequence %in% c("3","13","33","63","73","83","93") ~ "Heparin",
      sequence %in% c("4","14","24","34","64") ~ "EDTA",
      sequence == "5" ~ "ACD",
      sequence %in% c("6","16","26") ~ "Urine",
      sequence == "7" ~ "Mouthwash",
      sequence %in% c("60", "61") ~ "Streck",
      TRUE ~ NA_character_
    )
  )

# it's parent tube if both the parent_id and source_id are blank

data <- data %>%
  mutate(
    parent_tube = is.na(parent_id) & is.na(source_id)
  )


# all parent tubes must have a tube type, if not, recode below

parent_type_missing <- data %>%
  filter(parent_tube == TRUE,
         is.na(tube_type)
  )

 
# tube checks
tube_61 <- data %>%
  filter(sequence == "61") 

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
  filter(sequence == "23")

# recode SST

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id %in% c("CXA054915 0023", "CXA068417 0023"),
      "SST",
      tube_type
    )
  )

# recode heparin 


data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id %in% c("CXA049823 0023", "CXA047055 0023", "CXA027007 0023"),
      "Heparin",
      tube_type
    )
  )

tube_43 <- data %>%
  filter(sequence == "43")

# recode

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA027007 0043",
      "Heparin",
      tube_type
    )
  )



tube_44 <- data %>%
  filter(sequence == "44")

# recode

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA027007 0044",
      "SST",
      tube_type
    )
  )


tube_103_parent <- data %>% 
  filter(is.na(source_id), sequence == "103")

# recode legitimate 103 parent tube to Heparin

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA027007 0103",
      "Heparin",
      tube_type
    )
  )


tube_203 <- data %>% 
  filter(is.na(source_id), sequence == "203")

# recode 

data <- data %>%
  mutate(
    tube_type = if_else(
      bsi_id == "CXA027007 0203",
      "Heparin",
      tube_type
    )
  )


seq_subset_df <- data %>%
  filter(
    sequence %in% c("50", "51", "52", "53", "54"),
  ) %>%
  select(
    bsi_id,
    sequence,
    material_type,
    additive_preservative, 
    tube_type
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

# check parent tubes again, should be zero missing tube type 

parent_type_missing <- data %>%
  filter(parent_tube == TRUE,
         is.na(tube_type)
  )



# check subject_ids

id_violations <- data %>%
  mutate(subject_id = as.character(subject_id)) %>%
  filter(nchar(subject_id) != 10)

# fix subject_ids

data <- data %>%
  mutate(
    subject_id = if_else(
      subject_id == "895668678",
      "8956686781",
      subject_id
    )
  )


# if date_processed is null, then the tube is unprocessed

data <- data %>%
  mutate(
    unprocessed = is.na(date_processed)
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

# add a variable "saturday receipt"

# if date_received is a Saturday, TRUE 

data <- data %>%
  mutate(saturday_receipt = wday(date_received) == 7)


# add PARTICIPANT data from connect pull

data <- data %>%
  left_join(data_connect_persons, by = c("subject_id" = "Connect_ID"))

# add COLLECTION data from connect pull

data <- data %>%
  left_join(data_coll, by = c("sample_id" = "collection_id_connect"))

# only do this for RM2 below. 
# # add TUBE level data from connect pull
# data <- data %>%
#   left_join(data_bio, by = c("bsi_id" = "tube_id_connect"))
# 


# mutate sites into their names

data <- data %>%
  mutate(
    site = case_when(
      d_827220437 == 472940358 ~ "Baylor Scott and White",
      d_827220437 == 125001209 ~ "KP Colorado",
      d_827220437 == 327912200 ~ "KP Georgia",
      d_827220437 == 300267574 ~ "KP Hawaii",
      d_827220437 == 452412599 ~ "KP Northwest",
      d_827220437 == 548392715 ~ "Henry Ford",
      d_827220437 == 531629870 ~ "HealthPartners",
      d_827220437 == 303349821 ~ "Marshfield",
      d_827220437 == 657167265 ~ "Sanford",
      d_827220437 == 809703864 ~ "UChicago"
    )
  )




# Categorization code -----------------------------------------------------


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
        days_to_process > 2                                    ~ TRUE,
      
      tube_category == "mouthwash" &
        days_to_process > 10                                    ~ TRUE,
      
      TRUE                                                     ~ FALSE
    )
  )

# group by subject ID 

subject_tube_summary <- subject_tube_summary %>%
  group_by(subject_id, site, cancer_diag) %>%
  summarise(
    blood_tubes_given       = sum(tube_category == "blood"),
    blood_tubes_not_ok      = sum(tube_category == "blood" & not_processed_ok),
    
    urine_tubes_given       = sum(tube_category == "urine"),
    urine_tubes_not_ok      = sum(tube_category == "urine" & not_processed_ok),
    
    mouthwash_tubes_given   = sum(tube_category == "mouthwash"),
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
      all_processed  ~ "all tubes processed within 2 (blood, urine) or 10 (mouthwash) days",
      none_processed ~ "no tubes processed within 2 (blood, urine) or 10 (mouthwash) days",
      TRUE           ~ "some tubes processed within 2 (blood, urine) or 10 (mouthwash) days"
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
  "bsi_exports/categories/subject_processing_summary_2-10.xlsx"
)


# sites with cancer diagnosis variable 

site_processing_summary <- subject_tube_summary %>%
  filter(!is.na(site), cancer_diag == "TRUE") %>%   
  mutate(
    some_processed = !all_processed & !none_processed
  ) %>%
  group_by(site, cancer_diag) %>%
  summarise(
    n_subjects        = n(),
    
    n_all_processed  = sum(all_processed,  na.rm = TRUE),
    pct_all_processed =
      n_all_processed / n_subjects,
    
    n_none_processed = sum(none_processed, na.rm = TRUE),
    pct_none_processed =
      n_none_processed / n_subjects,
    
    n_some_processed = sum(some_processed, na.rm = TRUE),
    pct_some_processed =
      n_some_processed / n_subjects,
    
    .groups = "drop"
  )


site_processing_summary <- site_processing_summary %>%
  bind_rows(
    site_processing_summary %>%
      summarise(
        site = "all_sites",
        
        n_subjects        = sum(n_subjects),
        
        n_all_processed  = sum(n_all_processed),
        pct_all_processed =
          n_all_processed / n_subjects,
        
        n_none_processed = sum(n_none_processed),
        pct_none_processed =
          n_none_processed / n_subjects,
        
        n_some_processed = sum(n_some_processed),
        pct_some_processed =
          n_some_processed / n_subjects
      )
  )

site_processing_summary %>%
  mutate(
    check = n_all_processed + n_none_processed + n_some_processed
  )



write_xlsx(
  list(
    site_processing_summary = site_processing_summary
  ),
  "bsi_exports/categories/site_processing_summary_cancer_2-10.xlsx"
)



# Export individual subject_ids renamed to Connect_ID based on the categorization above
# Three .csv files for import into BigQuery



# Weekly Metrics Code -----------------------------------------------------


### To do 

# For each set of dates, export an excel file
# Make code to write the tables to a list and get them all exported to an excel file
# Use RM numbering
# work on #2

# dates for first processing

data_jan_1_15 <- data %>%
  filter(date_received >= as.Date("2026-01-01"),
         date_received <= as.Date("2026-01-15"))

data_jan_16_22 <- data %>%
  filter(date_received >= as.Date("2026-01-16"),
         date_received <= as.Date("2026-01-22"))

data_jan_23_29 <- data %>%
  filter(date_received >= as.Date("2026-01-23"),
         date_received <= as.Date("2026-01-29"))

# RM #1 ------------------------------------------------------

# Date of BSI Download
# Date range of "Date Received at BPTL"
# Number of tubes
# Number of individual participants

rm_1 <- function(data, label = NULL) {
  
  by_setting <- data %>%
    group_by(collection_setting) %>%
    summarise(
      data_delivery_date = max(data_delivery_date, na.rm = TRUE),
      dates = label,
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
      dates = label,
      start_date = min(date_received, na.rm = TRUE),
      end_date   = max(date_received, na.rm = TRUE),
      n_tubes = n_distinct(bsi_id),
      n_participants = n_distinct(subject_id)
    )
  
  bind_rows(by_setting, total_row) %>%
    select(data_delivery_date, everything())
}



rm1_metric <- rm_1(data)


data_collection_setting_qc <- data%>% 
  filter(is.na(collection_setting))




# RM #2 -------------------------------------------------------------------

# Provide a list of parent tubes that were received at BPTL according to the
# Connect data System but are not in the BSI.  Ideally there are no samples on
# this list, in which case please indicate “All expected parent tubes received
# at BPTL are in BSI”


# Connect ID | Tube ID | Date Received at BPTL


# Define the most recent completed Friday–Thursday window for bio data based on 
# download date

end_thursday <- floor_date(download_date, unit = "week", week_start = 5) - days(1)
start_friday <- end_thursday - days(6)

data_bio_week <- data_bio %>%
  filter(
    !is.na(tube_received_date_connect),
    as.Date(tube_received_date_connect) >= start_friday,
    as.Date(tube_received_date_connect) <=  end_thursday
  )



#data$bsi_id that are not in data_bio$tube_id_connect-
missing_BPTL <- anti_join(data %>% select(bsi_id),
                          data_bio_week %>% select(tube_id_connect),
                          by = c("bsi_id" = "tube_id_connect"))


# look for this in larger data_bio set:

data_bio_missing <- data_bio %>%
  filter(tube_id_connect %in% missing_BPTL$bsi_id)


#data_bio$tube_id_connect that are not in data$bsi_id
missing_BSI <- anti_join(data_bio_week %>% select(tube_id_connect),
                        data %>% select(bsi_id),
                        by = c("tube_id_connect"= "bsi_id"))


rm2 <- data_bio %>% filter(tube_id_connect %in% missing_BSI$tube_id_connect) %>% 
  select(Connect_ID, tube_id_connect, tube_received_date_connect)

#remove shipments not yet receieved
rm2 <- rm2 %>% filter(!is.na(tube_received_date_connect))

#remove recent collections?
rm2 <- rm2 %>% filter(!(as.Date(tube_received_date_connect) >= download_date))

if (nrow(rm2) == 0) {
  rm2 <- data.frame(
    status = "All expected parent tubes received at BPTL are in BSI",
    stringsAsFactors = FALSE
  )
}

# currentDate <- Sys.Date()
# openxlsx::write.xlsx(rm2,glue("RecurringMetric2_{currentDate}_ALL_QC.xlsx"), rowNames = F,na="")
# 
# openxlsx::write.xlsx(missing_BPTL,glue("RecurringMetric2_{currentDate}_BPTL_missing_QC.xlsx"), rowNames = F,na="")

rm2_metric <-rm2


missing_tubes <- dplyr::bind_rows(
  missing_BPTL %>%
    dplyr::rename(tube_id = bsi_id) %>%
    dplyr::mutate(missing_from = "bio"),
  
  missingBSI %>%
    dplyr::rename(tube_id = tube_id_connect) %>%
    dplyr::mutate(missing_from = "BPTL")
)

normalize_id <- function(x) {
  toupper(gsub("[^A-Z0-9]", "", x))
}

missing_tubes_norm <- missing_tubes %>%
  mutate(
    id_norm = toupper(gsub("[^A-Z0-9]", "", tube_id))
  )

all_bio_ids <- data_bio %>%
  select(tube_id_connect) %>%
  distinct() %>%
  mutate(id_norm = toupper(gsub("[^A-Z0-9]", "", tube_id_connect)))

all_bptl_ids <- data %>%
  select(bsi_id) %>%
  distinct() %>%
  mutate(id_norm = toupper(gsub("[^A-Z0-9]", "", bsi_id)))

missing_tubes_norm %>%
  filter(missing_from == "bio") %>%
  left_join(all_bio_ids, by = "id_norm") %>%
  filter(!is.na(tube_id_connect))


missing_tubes_norm %>%
  filter(missing_from == "BPTL") %>%
  left_join(all_bptl_ids, by = "id_norm") %>%
  filter(!is.na(bsi_id))

data_bio %>%
  filter(tube_id_connect %in% missingBSI$tube_id_connect) %>%
  summarise(
    min_date = min(as.Date(tube_received_date_connect), na.rm = TRUE),
    max_date = max(as.Date(tube_received_date_connect), na.rm = TRUE),
    n_recent = sum(as.Date(tube_received_date_connect) >= bio_end_date)
  )

missing_tubes %>%
  mutate(last4 = sub(".*([0-9]{4}).*$", "\\1", tube_id)) %>%
  count(last4) %>%
  filter(n > 1) %>%
  arrange(desc(n))

missing_tube_summary <- missing_tubes %>%
  mutate(
    last4 = sub(".*([0-9]{4}).*$", "\\1", tube_id),
    prefix = substr(tube_id, 1, 3)
  ) %>%
  count(missing_from, prefix, last4) %>%
  arrange(desc(n))

dupes_data <- data %>%
  count(bsi_id, name = "n") %>%
  filter(n > 1)


dupes_data_full <- data %>%
  inner_join(dupes_data %>% select(bsi_id), by = "bsi_id")


dupes_bio <- data_bio %>%
  count(tube_id_connect, name = "n") %>%
  filter(n > 1)

dupes_bio_full <- data_bio %>%
  inner_join(dupes_bio %>% select(tube_id_connect),
             by = "tube_id_connect")



# RM #3 -------------------------------------------------------------------

# “Distribution of Date from Receipt at BPTL to Date of Processing”

# Categorize the number of dates from date received at BPTL to date processed into these groups:
# •	<=1 day
# •	2 days
# •	3-5 days
# •	>5 days
# •	Not processed 

# For each parent tube type (tube_type), show a figure with these time
# periods on the X axis and numbers on the y-axis. Display a bar chart including
# the number and % of samples in each column.  The denominator for the %
# calculation is the number of tubes of the parent type that were received at
# BPTL. 

# For mouthwash samples not processed or processed >5 days and for all other samples not
# processed or processed 3+ days, show a list with this information: 
# •	BSI ID (bsi_id)
# •	Date received (date_received)
# •	# days from Receipt at BPTL to day processed (days_to_process) 
# •	Saturday receipt (yes/no) (saturday_receipt)


rm_3 <- function(df, output_dir = "bsi_exports/recurring/rm3_outputs") {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
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
      y = "Number of Samples"
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

results <- rm_3(data)

rm3_metric <- data.frame(
  status = "RM3 is uploaded to /rm3_outputs",
  stringsAsFactors = FALSE
)

# RM #4 -------------------------------------------------------------------


# Print a list of all vial warnings including:
# •	BSI ID
# •	Date received at BPTL
# •	Vial Warning (vial_warnings)
# If no vial warnings were found, output “No vial warnings found.”

rm_4 <- function(df) {
  
  vial_warning_list <- df %>%
    filter(
      !is.na(vial_warnings),
      vial_warnings != ""
    ) %>%
    select(
      bsi_id,
      date_received,
      vial_warning = vial_warnings
    ) %>%
    arrange(date_received)
  
  print(vial_warning_list)
  
  invisible(vial_warning_list)
}


rm4_metric <- rm_4(data)



# RM #5 -------------------------------------------------------------------

# “Impossible or incorrect processing dates”

# Print out list of impossible or incorrect processing dates.  Base this on the following criteria:

# •	Processing date cannot be before the Date received at BPTL
# •	Processing date cannot be after the date the BSI data were pulled

# The list will include:

# •	BSI ID
# •	Date received at BPTL
# •	Date processed

# If no impossible or incorrect  processing dates were found, output 
# “No incorrect processing dates were found.”


rm_5 <- function(df, bsi_pull_date) {
  
  bsi_pull_date <- as.Date(bsi_pull_date)
  
  bad_dates <- df %>%
    mutate(
      date_received  = as.Date(date_received),
      date_processed = as.Date(date_processed)
    ) %>%
    filter(
      # Flag NA dates explicitly
      is.na(date_received) |
        is.na(date_processed) |
        # Flag invalid processed dates
        date_processed < date_received |
        date_processed > bsi_pull_date
    ) %>%
    select(
      bsi_id,
      date_received,
      date_processed
    ) %>%
    arrange(date_received)
  
  # If empty, return a one-row disclaimer WITH SAME COLUMNS
  if (nrow(bad_dates) == 0) {
    bad_dates <- tibble::tibble(
      bsi_id = NA_character_,
      date_received = as.Date(NA),
      date_processed = as.Date(NA),
      message = "No incorrect processing dates were found."
    )
  } else {
    bad_dates <- bad_dates %>%
      mutate(message = NA_character_)
  }
  
  return(bad_dates)
}


rm5_metric <- rm_5(data, bsi_pull_date = download_date)




# RM #6 -------------------------------------------------------------------

# “Unexpected Material Types”

# These are the only valid material types for Parent tubes:
# •	WHOLE BL
# •	URINE
# •	SALIVA

# Print out list of unexpected material types including:
# •	BSI ID (bsi_id)
# •	Material Type (material_type)
# •	Additive/Preservative (additive_preservative)

# If no unexpected material types were found, output “No unexpected material types were found.”

rm_6 <- function(df) {
  
  valid_material_types <- c("WHOLE BL", "URINE", "SALIVA")
  
  unexpected_materials <- df %>%
    filter(
      is.na(material_type) |
        !material_type %in% valid_material_types
    ) %>%
    select(
      bsi_id,
      material_type,
      additive_preservative
    ) %>%
    arrange(material_type, bsi_id)
  
  # If empty, return a one-row disclaimer WITH SAME COLUMNS
  if (nrow(unexpected_materials) == 0) {
    unexpected_materials <- tibble::tibble(
      bsi_id = NA_character_,
      material_type = NA_character_,
      additive_preservative = NA_character_,
      message = "No unexpected material types were found."
    )
  } else {
    unexpected_materials <- unexpected_materials %>%
      mutate(message = NA_character_)
  }
  
  return(unexpected_materials)
}


rm6_metric <- rm_6(data)



# RM #7 -------------------------------------------------------------------


# Number of tubes received per day, by tube type 
# Table of the number of # tubes received each day

# Rows: - SST, EDTA, Streck, Urine, Mouthwash
# Columns: Day of the week / full date - a week is Friday start - ends on Thursday


rm_7 <- function(df) {
  
  df <- df %>%
    mutate(date_received = as.Date(date_received))
  
  # Full sequence of dates in the data
  all_dates <- seq(
    min(df$date_received, na.rm = TRUE),
    max(df$date_received, na.rm = TRUE),
    by = "day"
  )
  
  df %>%
    mutate(
      date_label = paste0(
        date_received,
        "_",
        wday(date_received, label = TRUE, abbr = TRUE)
      )
    ) %>%
    count(
      tube_type,
      date_label,
      name = "n_tubes"
    ) %>%
  
      # Ensure every tube_type × every day exists
    complete(
      tube_type,
      date_label = paste0(
        all_dates,
        "_",
        wday(all_dates, label = TRUE, abbr = TRUE)
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
}


rm7_metric <- rm_7(data)





# Exports -----------------------------------------------------------------



bsi_metrics <- list(
  RM1_Data_Transfer_Summary = rm1_metric,
  RM2_Parent_Tubes_Not_in_BSI = rm2_metric,
  RM3_Receipt_to_Processing_Dates = rm3_metric,
  RM4_Vial_Warnings = rm4_metric,
  RM5_Invalid_Processing_Dates = rm5_metric,
  RM6_Unexpected_Material_Types = rm6_metric,
  RM7_Daily_Tube_Counts_by_Type = rm7_metric
)

write.xlsx(
  bsi_metrics,
  file = glue("bsi_exports/recurring/BSI_metrics_{download_date}.xlsx"),
  overwrite = TRUE
)


