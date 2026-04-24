
# Upload all libraries ################################
library(DBI) 
library(bigrquery)
library(data.table) ###to write or read and data management 
library(tidyverse) ###for data management https://tidyselect.r-lib.org/reference/faq-external-vector.html
library(listr) ###to work on a list of vector, files or..
library(lubridate) ###date time
library(RColorBrewer) ###visions color http://www.sthda.com/english/wiki/colors-in-r
library(tinytex) #for pdf
library(rmarkdown) ###for the output tables into other files: pdf, rtf, etc.
library(janitor) #to get the summary sum
#library(epiDisplay) ##recommended applied here crosstable, tab1
library(gmodels) ##recommended but not applied in this R code
library(arsenal)
library(gtsummary)
library(kableExtra)
library(rio)
library(scales)
library(openxlsx)
library(glue)
library(logger) ## allows us to add messages in logs to find out when/where code breaks are happening
#install.packages("pak")
library(pak)
pak::pak("r-dbi/bigrquery")
pak::pak("tidyverse/dbplyr")

bq_auth()
2


#######################################################################

project <- "nih-nci-dceg-connect-prod-6d04"
billing <- "nih-nci-dceg-connect-prod-6d04" ##project and billing should be consistent

con <- dbConnect(
  bigrquery::bigquery(),
  project = project,
  dataset = "FlatConnect",
  billing = billing
)

## Will be adding log_info comments throughout report at timely or ticky spots to find code breaks in the logs
log_info("Finished setting up DBI connection")



## Needed for all csvs:
currentDate <- Sys.Date()
outputpath=''

##########################################################################


### BQ Pull
log_info("Starting DBI connection pull from the participants table for the logs")

logs_figs <- tbl(con,"participants") %>% 
  # Data destruction allowed (which would remove the active or passive flag)
  filter(d_831041022 == '353358909' | 
           # If they don't have data destroyed, they cannot be non-active
           (d_512820379 != '180583933' & 
              # Either passive OR active but not an active duplicate; passive duplicates are acceptable
              (d_512820379 != '486306141' | d_821247024 != '922622075'))) %>% 
  mutate(Site = case_when(d_827220437=='472940358' ~ "BSWH",
                          d_827220437=='125001209' ~ "KPCO",
                          d_827220437=='327912200' ~ "KPGA",
                          d_827220437=='300267574' ~ "KPHI",
                          d_827220437=='452412599' ~ "KPNW",
                          d_827220437=='548392715' ~ "HFH",
                          d_827220437=='531629870' ~ "HP",
                          d_827220437=='303349821' ~ "MF",
                          d_827220437=='657167265' ~ "SF",
                          d_827220437=='809703864' ~ "UC"),
         biospeDonation = ifelse((d_878865966=='104430631' | is.na(d_878865966)) & (d_167958071=='104430631' | is.na(d_167958071)) & 
                                   (d_684635302=='104430631' | is.na(d_684635302)),"No Sample Donations",
                                 ifelse(d_878865966 == '353358909' & d_167958071 == '353358909' & d_684635302 == '353358909',"Completed All 3 Sample Donations",
                                        "Completed Some but Not All 3 Sample Donations")),
         re_invited= case_when(is.na(d_439351436) ~ "No",
                               !is.na(d_439351436) ~ "Yes")) %>% 
  dplyr::select(Connect_ID, d_512820379, d_471593703, d_878865966, d_173836415_d_266600170_d_561681068,  
                d_914594314, d_827220437, d_439351436, d_821247024, d_173836415_d_266600170_d_915179629, 
                d_173836415_d_266600170_d_448660695, d_167958071, d_173836415_d_266600170_d_847159717, 
                Site, re_invited, biospeDonation, d_173836415_d_266600170_d_740582332, d_173836415_d_266600170_d_541483796_d_826941471,
                d_173836415_d_266600170_d_319972665_d_826941471, d_173836415_d_266600170_d_641006239_d_826941471,
                d_173836415_d_266600170_d_319972665_d_278539982, d_173836415_d_266600170_d_319972665_d_638781731) %>% 
  as_tibble()


log_info("Creating additional variables for the logs")

logs_figs <- logs_figs %>% 
  mutate(mw.time =case_when(d_173836415_d_266600170_d_915179629=="534621077" ~ ymd_hms(d_173836415_d_266600170_d_448660695)), #Research MW times only-- to many issues with manual entry time errors for Home Collections
  
         ## UC doesn't have biospecimen collections anymore so it's excluded 
         Site = factor(Site, levels = c("BSWH", "HFH","HP", "KPCO", "KPGA", "KPHI", "KPNW", "MF", "SF", "UC")),
  
  #date variables are all showing as character variables, but want to make sure the rounding of the original counts are preserved so keeping time 
  recruit_date = as.POSIXct(ymd_hms(d_471593703)),
  verification_date = as.POSIXct(ymd_hms(d_914594314)),
  reinvite_date = as.POSIXct(ymd_hms(d_439351436)),
  #ANY bio: research blood, urine, mw or clinical blood/urine
  bio_date = pmin(as.POSIXct(d_173836415_d_266600170_d_561681068),as.POSIXct(d_173836415_d_266600170_d_847159717), 
                  as.POSIXct(mw.time),as.POSIXct(d_173836415_d_266600170_d_740582332), na.rm = TRUE))



bio_figs_query <- "
SELECT CASE
    WHEN b.d_827220437 = '472940358' THEN 'BSWH'
    WHEN b.d_827220437 = '125001209' THEN 'KPCO'
    WHEN b.d_827220437 = '327912200' THEN 'KPGA'
    WHEN b.d_827220437 = '300267574' THEN 'KPHI'
    WHEN b.d_827220437 = '452412599' THEN 'KPNW'
    WHEN b.d_827220437 = '548392715' THEN 'HFH'
    WHEN b.d_827220437 = '531629870' THEN 'HP'
    WHEN b.d_827220437 = '303349821' THEN 'MF'
    WHEN b.d_827220437 = '657167265' THEN 'SF'
    WHEN b.d_827220437 = '809703864' THEN 'UC'
  END AS Site_acrnm,

  d_299553921_d_926457119, #SST1 
  d_703954371_d_926457119, #SST2 
  d_376960806_d_926457119, #SST3
  d_232343615_d_926457119, #SST4 
  d_589588440_d_926457119, #SST5 
  d_454453939_d_926457119, #EDTAT1 
  d_677469051_d_926457119, #EDTAT2 
  d_683613884_d_926457119, #EDTAT3 
  d_505347689_d_926457119, #STRT1
  d_973670172_d_926457119, #URN
  d_143615646_d_926457119  #MW
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` p
LEFT JOIN `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen` b
ON p.Connect_ID = b.Connect_ID
WHERE d_831041022 = '353358909' or 
# If they don't have data destroyed, they cannot be non-active
      (d_512820379 != '180583933' and 
# Either passive OR active but not an active duplicate; passive duplicates are acceptable
      (d_512820379 != '486306141' or d_821247024 != '922622075'))
"
bio_figs_table <- bq_project_query(project, bio_figs_query)
bio_figs <- bq_table_download(bio_figs_table, bigint="integer64",n_max = Inf, page_size = 10000)

bio_figs <- bio_figs %>% 
  mutate(Site_acrnm = factor(Site_acrnm, 
                             levels = c("BSWH", "HFH","HP", "KPCO", "KPGA", "KPHI", "KPNW", "MF", "SF", "UC")))



### Weeks in the logs are Monday-Sunday
#------------- Create weekly date sequence ---------------------------------------------- 
week_seq <- tibble(Week = seq(from = as.Date("2026-04-20"),
                              to = floor_date(Sys.Date(), unit = "week", week_start = 1),
                              by = "week"))


#--------------- Counts participants by week  --------------------------------------------

log_info("Generating weekly counts")

  
  verified_by_week <- week_seq %>%
    left_join(
      logs_figs %>%
        filter(d_821247024 == 197316935) %>% 
        mutate(verification_date = as.Date(verification_date),
               Week = floor_date(verification_date, unit = "week", week_start = 1)) %>%
        dplyr::count(Week, Site, name = "n") %>%
        group_by(Week) %>%
        mutate(`Total Verifications per week` = sum(n)) %>%
        ungroup() %>%
        arrange(Site) %>%
        pivot_wider(
          names_from  = Site,
          values_from = n,
          values_fill = 0
        ) %>%
        rename_with(
          ~ paste("Verified per week from", .),
          .cols = -c(Week, `Total Verifications per week`)
        ),
      by = "Week"
    ) %>%
    replace_na(list('Total Verifications per week' = 0)) %>%
    arrange(Week) %>% 
    select(-"Verified per week from UC") 

  

  
  all_invited_by_week <- week_seq %>%
    left_join(
      bind_rows(
        # Initial invitations
        logs_figs %>%
          filter(d_512820379 == '486306141') %>%
          mutate(invite_date = as.Date(recruit_date)),
        # Re-invitations
        logs_figs %>%
          filter(re_invited == "Yes" & d_512820379 == '486306141') %>%
          mutate(invite_date = as.Date(reinvite_date))
      ) %>%
        mutate(Week = floor_date(invite_date, unit = "week", week_start = 1)) %>%
        dplyr::count(Week, Site, name = "n") %>%
        group_by(Week) %>%
        mutate(`Total Invitations per week` = sum(n)) %>%
        ungroup() %>%
        arrange(Site) %>%
        pivot_wider(
          names_from  = Site,
          values_from = n,
          values_fill = 0
        ) %>%
        rename_with(
          ~ paste("Invitations per week", .),
          .cols = -c(Week, `Total Invitations per week`)
        ),
      by = "Week"
    ) %>%
    replace_na(list(`Total Invitations per week` = 0)) %>%
    arrange(Week) %>% 
    select(-"Invitations per week UC") 
  
  
  biospec_by_week <- week_seq %>%
    left_join(
      logs_figs %>%
        filter(d_821247024 == 197316935 & biospeDonation!="No Sample Donations") %>% 
        mutate(bio_date = as.Date(bio_date),
               Week = floor_date(bio_date, unit = "week", week_start = 1)) %>%
        dplyr::count(Week, Site, name = "n") %>%
        group_by(Week) %>%
        mutate(`Total Collections (Research and Clinical) per week` = sum(n)) %>%
        ungroup() %>%
        arrange(Site) %>%
        pivot_wider(
          names_from  = Site,
          values_from = n,
          values_fill = 0
        ) %>%
        rename_with(
          ~ paste("Total Collections per week from", .),
          .cols = -c(Week, `Total Collections (Research and Clinical) per week`)
        ),
      by = "Week"
    ) %>%
    replace_na(list('Total Collections (Research and Clinical) per week ' = 0)) %>%
    arrange(Week) %>% 
    select(-"Total Collections per week from UC") 
  
  
  blood_tubes_by_week <- week_seq %>%
    left_join(
      bio_figs %>%
        pivot_longer(
          cols      = c(d_299553921_d_926457119, #SST1 
                        d_703954371_d_926457119,  #SST2 
                        d_376960806_d_926457119,  #SST3
                        d_232343615_d_926457119,  #SST4 
                        d_589588440_d_926457119,  #SST5 
                        d_454453939_d_926457119,  #EDTAT1 
                        d_677469051_d_926457119,  #EDTAT2 
                        d_683613884_d_926457119,  #EDTAT3 
                        d_505347689_d_926457119), #STRT1 
          names_to  = "date_type",
          values_to = "blood_date"
        ) %>%
        filter(!is.na(blood_date)) %>%
        mutate(
          blood_date = as.Date(blood_date),
          Week = floor_date(blood_date, unit = "week", week_start = 1)
        ) %>%
        count(Week, Site_acrnm, name = "n") %>%
        group_by(Week) %>%
        mutate(`Total Blood Tubes Received at BPTL per week` = sum(n)) %>%
        ungroup() %>%
        arrange(Site_acrnm) %>%
        pivot_wider(
          names_from  = Site_acrnm,
          values_from = n,
          values_fill = 0
        ) %>%
        rename_with(
          ~ paste("Blood Tubes Received at BPTL per week from", .),
          .cols = -c(Week, `Total Blood Tubes Received at BPTL per week`)
        ),
      by = "Week"
    ) %>%
    replace_na(list("Total Blood Tubes Received at BPTL per week" = 0)) %>%
    arrange(Week)  %>% 
    select(-"Blood Tubes Received at BPTL per week from UC")
  
  
  urine_tubes_by_week <- week_seq %>%
    left_join(
      bio_figs %>%
        filter(!is.na(d_973670172_d_926457119)) %>%
        mutate(
          urine_date = as.Date(d_973670172_d_926457119),
          Week = floor_date(urine_date, unit = "week", week_start = 1)
        ) %>%
        count(Week, Site_acrnm, name = "n") %>%
        group_by(Week) %>%
        mutate(`Total Urine Tubes Received at BPTL per week` = sum(n)) %>%
        ungroup() %>%
        arrange(Site_acrnm) %>%
        pivot_wider(
          names_from  = Site_acrnm,
          values_from = n,
          values_fill = 0
        ) %>%
        rename_with(
          ~ paste("Urine Tubes Received at BPTL per week from", .),
          .cols = -c(Week, `Total Urine Tubes Received at BPTL per week`)
        ),
      by = "Week"
    ) %>%
    replace_na(list("Total Urine Tubes Received at BPTL per week" = 0)) %>%
    arrange(Week) %>% 
    select(-"Urine Tubes Received at BPTL per week from UC")
  
  
  rs_mw_tubes_by_week <- week_seq %>%
    left_join(
      bio_figs %>%
        filter(!is.na(d_143615646_d_926457119)) %>%
        mutate(
          urine_date = as.Date(d_143615646_d_926457119),
          Week = floor_date(urine_date, unit = "week", week_start = 1)
        ) %>%
        count(Week, Site_acrnm, name = "n") %>%
        group_by(Week) %>%
        mutate(`Total MW Tubes Received at BPTL per week (RESEARCH)` = sum(n)) %>%
        ungroup() %>%
        arrange(Site_acrnm) %>%
        pivot_wider(
          names_from  = Site_acrnm,
          values_from = n,
          values_fill = 0
        ) %>%
        rename_with(
          ~ paste("MW Tubes Received at BPTL per week from", .),
          .cols = -c(Week, `Total MW Tubes Received at BPTL per week (RESEARCH)`)
        ),
      by = "Week"
    ) %>%
    replace_na(list("Total MW Tubes Received at BPTL per week (RESEARCH)" = 0)) %>%
    arrange(Week) %>% 
    # Removing Sites that no longer collect MW
    select(-c("MW Tubes Received at BPTL per week from HFH",
              "MW Tubes Received at BPTL per week from UC"))
  
  
  HMW_tubes_by_week <- week_seq %>%
    left_join(
      logs_figs %>%
        pivot_longer(
          cols      = c(d_173836415_d_266600170_d_319972665_d_826941471, #Initial
                        d_173836415_d_266600170_d_541483796_d_826941471, #Replacement1
                        d_173836415_d_266600170_d_641006239_d_826941471), #Replacement2 
          names_to  = "date_type",
          values_to = "HMW_date"
        ) %>%
        filter(!is.na(HMW_date)) %>%
        mutate(
          HMW_date = as.Date(HMW_date),
          Week = floor_date(HMW_date, unit = "week", week_start = 1)
        ) %>%
        dplyr::count(Week, name = "Total MW Tubes Received at BPTL per week (HOME)"),
      by = "Week"
    ) %>%
    replace_na(list('Total MW Tubes Received at BPTL per week (HOME)' = 0)) %>%
    arrange(Week)
  
  
  
  req_kit_by_week <- week_seq %>%
    left_join(
      logs_figs %>%
        filter(d_173836415_d_266600170_d_319972665_d_638781731==353358909 & 
                 !is.na(d_173836415_d_266600170_d_319972665_d_278539982)) %>% 
        mutate(
          Req_Kit_date = as.Date(d_173836415_d_266600170_d_319972665_d_278539982),
          Week = floor_date(Req_Kit_date, unit = "week", week_start = 1)
        ) %>%
        dplyr::count(Week, name = "Total Home Mouthwash Request A Kit Invitations Sent per week"),
      by = "Week"
    ) %>%
    replace_na(list('Total Home Mouthwash Request A Kit Invitations Sent per week' = 0)) %>%
    arrange(Week)
  
  
  
  ########### Combine all into one table ########### 
  weekly_counts <- week_seq %>%
    left_join(all_invited_by_week, by = "Week") %>%
    left_join(verified_by_week, by = "Week") %>%
    left_join(biospec_by_week, by = "Week") %>%
    left_join(req_kit_by_week, by = "Week") %>%
    left_join(blood_tubes_by_week, by = "Week") %>%
    left_join(urine_tubes_by_week, by = "Week") %>%
    left_join(rs_mw_tubes_by_week, by = "Week") %>%
    left_join(HMW_tubes_by_week, by = "Week") %>%
    arrange(Week)
  
  
  weekly_counts <- weekly_counts %>% 
    dplyr::rename(Week_Starting_Monday = Week) %>% 
    mutate(recruit_week = row_number(),
           Week_Ending_Sunday = Week_Starting_Monday + days(6))
  
  

  
  weekly_counts <- weekly_counts %>%
    mutate(`Total Tubes Received at BPTL per week` = sum(`Total Blood Tubes Received at BPTL per week`, 
                                                         `Total Urine Tubes Received at BPTL per week`, 
                                                         `Total MW Tubes Received at BPTL per week (RESEARCH)`, 
                                                         `Total MW Tubes Received at BPTL per week (HOME)`)) 
  
  ########### Reorder the columns ########### 
  
# Need week starting, ending, invitations, verifs, R/C collections, Kit Req, Total Tubes, Blood, Urine, RMW, HMW
  weekly_counts <- weekly_counts[1, c(1, 60, 2:32, 61, 33:58)]
  


log_info("Finished the overall dataframe")



########### Export to Excel ########### 

log_info("Creating logs xlxs file")

### ALl Sites
box_dir_id = "378173022120"
write.csv(weekly_counts[which(weekly_counts$Week_Starting_Monday < as.Date(currentDate) & weekly_counts$Week_Ending_Sunday < as.Date(currentDate)),], 
          paste(outputpath,"CCC_biospecimens_weekly_log_", currentDate, "_boxfolder_",box_dir_id, ".csv",sep=""), na="", row.names=F)


log_info("Full code completed")
