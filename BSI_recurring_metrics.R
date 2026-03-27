# BSI_recurring_metrics.R ############################################################

# Script Name: BSI_recurring_metrics.R 
# Author: Michelle Hudson 
# Date: February 2 2026
# Date updated: March 27 2026
# Description: 
# 
# Output:
#   - One combined excel file of tabular data
#   - One plot for RM3
# ..........................................................................

## load tools --------------------------------------------------------------

# data download tools
library(googleCloudStorageR)
library(gargle)
library(bigrquery)


# cleaning, analysis, and plotting tools
library(tidyverse)
library(janitor)
library(lubridate)
library(tools)

# export tools 
library(openxlsx)
library(glue)
library(here)

# Source functions
source("bsi_functions.R")


# BQ Pull -----------------------------------------------------------------

bq_auth()
2

project = "nih-nci-dceg-connect-prod-6d04"

# from participants

query_ppts <- "
SELECT  
  Connect_ID, 
  d_827220437,
  CASE d_827220437
    WHEN '472940358' THEN 'Baylor Scott and White'
    WHEN '125001209' THEN 'KP Colorado'
    WHEN '327912200' THEN 'KP Georgia'
    WHEN '300267574' THEN 'KP Hawaii'
    WHEN '452412599' THEN 'KP Northwest'
    WHEN '548392715' THEN 'Henry Ford'
    WHEN '531629870' THEN 'HealthPartners'
    WHEN '303349821' THEN 'Marshfield'
    WHEN '657167265' THEN 'Sanford'
    WHEN '809703864' THEN 'UChicago'
    ELSE NULL
  END AS site
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants`
WHERE Connect_ID IS NOT NULL
"

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
2
gcs_auth(token = token)


gcs_global_bucket("bsi_tables")

# buckets <- gcs_list_buckets(project)
bucket <- "bsi_tables"


# Data download call function methods ----------------------------------------------------

# sys date (today)
data_raw <- read_bsi_gcs()


# # specific date (must be a bucket folder named this)
# data_raw <- read_bsi_gcs("2026-02-02") # the last full download before weekly

# 
data_raw <- read_bsi_gcs("2026-03-23")
# 

# # explicit bucket
# data_raw <- read_bsi_gcs(
#   download_date = "2026-02-06",
#   bucket = "bsi_tables"
# )

# Data cleaning -----------------------------------------------------------

# for this analysis, remove timestamps, as some dates do not have them 
data_raw <- data_raw %>%
  mutate(
    date_drawn     = as.Date(date_drawn),
    date_received  = as.Date(date_received),
    date_processed = as.Date(date_processed)
  )

 download_date <- as.Date(data_raw$data_delivery_date[1])

#Manual 
# download_date <- as.Date("01-26-2026", format = "%m-%d-%Y")


# Define the most recent completed Friday–Thursday window

end_thursday <- floor_date(download_date, unit = "week", week_start = 5) - days(1)
start_friday <- end_thursday - days(6)



# if needed
# 
# data_raw <- data_raw %>%
#   filter(
#     date_received >= start_friday,
#     date_received <= end_thursday
#   )

data <- data_raw


# it's parent tube if both the parent_id and source_id are blank

data <- data %>%
  mutate(
    parent_tube = is.na(parent_id) & is.na(source_id)
  )

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
      sequence %in% c("100","101","102","103","104","105","106","107","108","109") ~ "Serum",
      sequence %in% c("2001","2002", "2003") ~ "DNA",
      sequence %in% c("400","401","402","403","404") ~ "EDTA plasma",
      sequence %in% c("405","406","407") ~ "EDTA buffy coat/RBC",
      sequence %in% c("600","601","602","603","604","605","606","607") ~ "Urine",
      sequence %in% c("700","701") ~ "Mouthwash",
      sequence %in% c("800","801","802","803","804") ~ "Streck plasma",
      sequence %in% c("805","806","807") ~ "Streck buffy coat/RBC",
      TRUE ~ "QC check"
    )
  )


## Extract unexpected sequence values for QC
sequence_QC <- data %>%
  filter(
    tube_type == "QC check"
  ) %>%
  select(subject_id, sample_id, sequence)

## Remove only QC check rows where parent_tube == TRUE
data <- data %>% 
  filter(!(tube_type == "QC check"))

 # determine vial status and update description, removed discarded from analysis

data <- data %>% 
  mutate(
    vial_description = case_when(
      vial_status == "Empty" ~ "Aliquoted Parent",
      vial_status == "Reserved" ~ "Reserved Child",
      vial_status == "In" ~ "In Child",
      vial_status == "Discarded" ~ "Discarded",
      vial_status == "Destroyed/Broken" ~ "Destroyed/Broken",
      TRUE~ "QC check"
    )
  )
  
vial_status_QC <- data %>%
  filter(
    vial_description == "QC check") %>%
  select(subject_id, sample_id, sequence, vial_status)


# # check subject_ids

id_violations <- data %>%
  mutate(subject_id = as.character(subject_id)) %>%
  filter(nchar(subject_id) != 10)


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


# add a "days since receipt" variable and make volume a number

data <- data %>%
  mutate(
    volume = as.numeric(volume),
    days_since_receipt = as.numeric(difftime(data_delivery_date, date_received, units = "days"))
  )


# add variable "days_to_process" 

data <- data %>%
  mutate(days_to_process = as.numeric(difftime(date_processed, date_received, units = "days")))


# if date_received is a Saturday, TRUE 

data <- data %>%
  mutate(saturday_receipt = wday(date_received) == 7)


# add PARTICIPANT data from connect pull

data <- data %>%
  left_join(data_connect_persons, by = c("subject_id" = "Connect_ID"))

# add COLLECTION data from connect pull

data <- data %>%
  left_join(data_coll, by = c("sample_id" = "collection_id_connect"))


# Separate into Child and Parent code for RMs
# remove Discarded from this and then use "data" dataframe for RM9

data_no_discard <- data %>% 
  filter(!vial_description %in% c("Discarded", "Destroyed/Broken"))

data_parent <- data %>% 
  filter(parent_tube == TRUE, 
         !vial_description %in% c("Discarded", "Destroyed/Broken")
  )

data_child <- data %>% 
  filter(
    parent_tube == FALSE,
    !vial_description %in% c("Discarded", "Destroyed/Broken")
  )

# Weekly Metrics Code -----------------------------------------------------


# RM #1 ------------------------------------------------------

# Date of BSI Download
# Date range of "Date Received at BPTL"
# Number of tubes
# Number of individual participants

rm1_metric <- rm_1(data_parent)


# RM #2 -------------------------------------------------------------------

# Provide a list of parent tubes that were received at BPTL according to the
# Connect data System but are not in the BSI.  Ideally there are no samples on
# this list, in which case please indicate “All expected parent tubes received
# at BPTL are in BSI”

# Connect ID | Tube ID | Date Received at BPTL

# use data dataframe for this which includes discarded tubes, because they are discarded, not missing
# rm2 function will limit to parent tubes

rm2_metric <- rm_2(data, data_bio, download_date)







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

rm3_metric <- rm_3(data_parent)


# RM #4 -------------------------------------------------------------------


# Print a list of all vial warnings including:
# •	BSI ID
# •	Date received at BPTL
# •	Vial Warning (vial_warnings)
# If no vial warnings were found, output “No vial warnings found.”

rm4_metric <- rm_4(data_parent)



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


rm5_metric <- rm_5(data_parent, bsi_pull_date = download_date)



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


rm6_metric <- rm_6(data_parent)



# RM #7 -------------------------------------------------------------------


# Number of tubes received per day, by tube type 
# Table of the number of # tubes received each day

# Rows: - SST, EDTA, Streck, Urine, Mouthwash
# Columns: Day of the week / full date - a week is Friday start - ends on Thursday

rm7_metric <- rm_7(data_parent)




# RM #8 -------------------------------------------------------------------

# Evaluation of child vials // updated via email instruction 3/23/36

# Table 1: were any child vials created from parent
# tubes that were processed (by material type) 
# Number and percent of parent
# tubes with any child vials created 
# of parent tubes with a processed date, 
# of parent tubes with a processed date that had CV created 
# % calculation of the two above

rm8_metric_1 <- rm_8_1(data_no_discard)


# Table 2: How many child vials were created, by material type and collection setting
# < expected, expected, > expected


# Table 2a number and percent of child vials created, research 
# Table 2a
# footnote: number of expected child vials are serum (10), EDTA plasma (5), EDTA
# buffy coat/RBC (3), Streck plasma (5), Streck buffy coat/RBC (3), Urine (8)
# and Mouthwash (2)


# Serum
# EDTA-plasma
# EDTA-buffy coat
# Streck - plasma
# Streck - buffy coat
# Urine
# Mouthwash
# 



# Table 2b number and percent of child vials created, clinical
# Table 2b footnote: number of expected child vials are serum (10), EDTA plasma (5), EDTA
# buffy coat/RBC (3), Streck plasma (5), Streck buffy coat/RBC (3), Urine (8)
# and Mouthwash (2)

# Table 2c number and percent of child vials created, home
# Table 2c footnote: number of expected child vials are Mouthwash (2)


rm8_metric_2 <- rm_8_2(data_no_discard)


# For any tube that does not fall in the “Expected” column, we would like a print out of:
# Sample ID
# Site it was collected from
# Actual # of child vials created
# All vial warnings entered by the site


# Table 3: What is the total volume of material in the child vials that were created?
# by material type, collection setting, and site


# We have received updated volume expectations from the lab which are now the
# same for clinical and research: Serum 6.5ml EDTA plasma 2.8 ml EDTA buffy
# coat/RBC 1.5 ml Streck plasma 4 ml Streck buffy coat/RBC 1.5 ml Urine 8ml
# Mouthwash 1.6 ml

# Table 3a Research

# Table 3b Clinical

# Table 3c Home Table 3c footnote: Volume expected child vials are Mouthwash
# (1.6ml)

# Instead of showing the means & range, we think it would be more useful to make
# this similar to table 2, where you show the number of tubes that have fewer
# than expected, expected, and greater than expected aliquot sizes.  Then for
# any that do not fall in the Expected category, create a similar list as above
# of:
# 
# Sample ID
# Site it was collected from
# Actual volume of the child vial that was not expected
# All vial warnings entered by the site


rm8_metric_3 <- rm_8_3(data_no_discard)


# RM #9 -------------------------------------------------------------------

# For any tube marked as discarded, we would want a list including: Connect ID,
# Collection ID, material type, date received, and collection site (which of the
# 10 partner sites)


rm9_metric <- rm_9(data)



# Exports -----------------------------------------------------------------

dir.create(here::here("bsi_outputs"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = here::here(
    "bsi_outputs",
    paste0("RM3_processing_time_distribution_", download_date, ".png")
  ),
  plot = rm3_metric$plot,
  width = 12,
  height = 8,
  dpi = 300
)


# simple workbook export
# 
# bsi_metrics <- list(
#   RM1_Data_Transfer_Summary = rm1_metric,
#   RM2_Parent_Tubes_Not_in_BSI = rm2_metric,
#   RM3_Exceptions = rm3_metric$exceptions,
#   RM3_Receipt_to_Processing_Dates = rm3_metric$plot_data,
#   RM4_Vial_Warnings = rm4_metric,
#   RM5_Invalid_Processing_Dates = rm5_metric,
#   RM6_Unexpected_Material_Types = rm6_metric,
#   RM7_Daily_Tube_Counts_by_Type = rm7_metric,
#   RM8_Parents_With_Children = rm8_metric_1,
#   RM9_Discarded_Tubes = rm9_metric
# )
# 
# write.xlsx(
#   bsi_metrics,
#   file = glue("bsi_exports/recurring/BSI_metrics_test_{download_date}.xlsx"),
#   overwrite = TRUE
# )

# Create and export workbook

wb <- createWorkbook()

addWorksheet(wb, "RM1_Data_Transfer_Summary")
writeData(wb, "RM1_Data_Transfer_Summary", rm1_metric)

addWorksheet(wb, "RM2_Parent_Tubes_Not_in_BSI")
writeData(wb, "RM2_Parent_Tubes_Not_in_BSI", rm2_metric)

addWorksheet(wb, "RM3_Exceptions")
writeData(wb, "RM3_Exceptions", rm3_metric$exceptions)

addWorksheet(wb, "RM3_Receipt_to_Processing_Dates")
writeData(wb, "RM3_Receipt_to_Processing_Dates", rm3_metric$plot_data)

addWorksheet(wb, "RM4_Vial_Warnings")
writeData(wb, "RM4_Vial_Warnings", rm4_metric)

addWorksheet(wb, "RM5_Invalid_Processing_Dates")
writeData(wb, "RM5_Invalid_Processing_Dates", rm5_metric)

addWorksheet(wb, "RM6_Unexpected_Material_Types")
writeData(wb, "RM6_Unexpected_Material_Types", rm6_metric)

addWorksheet(wb, "RM7_Daily_Tube_Counts_by_Type")
writeData(wb, "RM7_Daily_Tube_Counts_by_Type", rm7_metric)

addWorksheet(wb, "RM8_1_Parents_With_Children")
start_row <- 1

rm8_1_header <- "Table 1. Parent tubes with child vials created"
rm8_1_footer <- "Percent is based on non-discarded, processed parent tubes."

writeData(
  wb,
  "RM8_1_Parents_With_Children",
  rm8_1_header,
  startRow = start_row,
  colNames = FALSE
)
writeData(
  wb,
  "RM8_1_Parents_With_Children",
  rm8_metric_1$summary,
  startRow = start_row + 1
)
writeData(
  wb,
  "RM8_1_Parents_With_Children",
  rm8_1_footer,
  startRow = start_row + nrow(rm8_metric_1$summary) + 3,
  colNames = FALSE
)


addWorksheet(wb, "RM8_1_Missing_Child_Vials")
writeData(wb, "RM8_1_Missing_Child_Vials", rm8_metric_1$no_children)


addWorksheet(wb, "RM8_2_Expected_Vials")
start_row <- 1

rm8_metric_2_order <- c("RM8_Table2_2a", "RM8_Table2_2b", "RM8_Table2_2c")
rm8_metric_2_order <- rm8_metric_2_order[rm8_metric_2_order %in% names(rm8_metric_2)]

for (nm in rm8_metric_2_order) {
  writeData(
    wb,
    "RM8_2_Expected_Vials",
    rm8_metric_2[[nm]]$title,
    startRow = start_row,
    colNames = FALSE
  )
  writeData(
    wb,
    "RM8_2_Expected_Vials",
    rm8_metric_2[[nm]]$table,
    startRow = start_row + 1
  )
  writeData(
    wb,
    "RM8_2_Expected_Vials",
    rm8_metric_2[[nm]]$footnote,
    startRow = start_row + nrow(rm8_metric_2[[nm]]$table) + 3,
    colNames = FALSE
  )
  start_row <- start_row + nrow(rm8_metric_2[[nm]]$table) + 6
}

addWorksheet(wb, "RM8_2_Unexpected_Vials")
writeData(wb, "RM8_2_Unexpected_Vials", rm8_metric_2$RM8_Table2_2a$unexpected_tubes)

addWorksheet(wb, "RM8_3_Expected_Volume")
start_row <- 1
rm8_metric_3_order <- c("RM8_Table3_3a", "RM8_Table3_3b", "RM8_Table3_3c")
rm8_metric_3_order <- rm8_metric_3_order[rm8_metric_3_order %in% names(rm8_metric_3)]

for (nm in rm8_metric_3_order) {
  writeData(
    wb,
    "RM8_3_Expected_Volume",
    rm8_metric_3[[nm]]$title,
    startRow = start_row,
    colNames = FALSE
  )
  writeData(
    wb,
    "RM8_3_Expected_Volume",
    rm8_metric_3[[nm]]$table,
    startRow = start_row + 1
  )
  writeData(
    wb,
    "RM8_3_Expected_Volume",
    rm8_metric_3[[nm]]$footnote,
    startRow = start_row + nrow(rm8_metric_3[[nm]]$table) + 3,
    colNames = FALSE
  )
  start_row <- start_row + nrow(rm8_metric_3[[nm]]$table) + 6
}

addWorksheet(wb, "RM8_3_Unexpected_Volume")
writeData(wb, "RM8_3_Unexpected_Volume", rm8_metric_3$RM8_Table3_3a$unexpected_volumes)

addWorksheet(wb, "RM9_Discarded_Tubes")
writeData(wb, "RM9_Discarded_Tubes", rm9_metric)

# Apply to every sheet
center_style <- createStyle(halign = "center", valign = "center")

for (sheet in names(wb)) {
  setColWidths(wb, sheet = sheet, cols = 1:50, widths = "auto")
  addStyle(
    wb,
    sheet = sheet,
    style = center_style,
    rows = 1:500,
    cols = 1:50,
    gridExpand = TRUE,
    stack = TRUE
  )
}

saveWorkbook(
  wb,
  file = here::here(
    "bsi_outputs",
    glue::glue("BSI_metrics_{download_date}.xlsx")
  ),
  overwrite = TRUE
)


