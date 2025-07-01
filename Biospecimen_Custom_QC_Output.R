print("in Biospeciment_Custom_QC_Output.R!")
cat("in Biospeciment_Custom_QC_Output.R!")

library(bigrquery)
library(boxr)
library(lubridate)
library(dplyr)
library(stringr)
library(gt)
library(expss)
library(knitr)
library(kableExtra)
library(glue)
library(openxlsx)
bq_auth()
2


### Set Box folders for output CSV files 

boxfolder <- 221297686961



# # Retrieve credentials from environment variables and pass them to box_auth()-- will use later for pulling from box to add 'NewRules' column to QC report
# boxr::box_auth(
#   client_id = Sys.getenv("BOX_CLIENT_ID"),
#   client_secret = Sys.getenv("BOX_CLIENT_SECRET")
# )


############################################################








project <- "nih-nci-dceg-connect-prod-6d04"



bio <- "WITH T AS (
  SELECT Connect_ID
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`
  WHERE d_820476880 LIKE 'CXA%'
  GROUP BY Connect_ID
  HAVING COUNT(Connect_ID) = 1
)
SELECT * 
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`
WHERE (d_820476880 LIKE 'CHA%') 
   OR (d_820476880 LIKE 'CXA%' AND d_410912345='353358909' AND Connect_ID IN (SELECT Connect_ID FROM T));"

# parts <- "SELECT Connect_ID, d_173836415_d_266600170_d_139245758, d_173836415_d_266600170_d_156605577, d_173836415_d_266600170_d_184451682, d_173836415_d_266600170_d_185243482, d_173836415_d_266600170_d_198261154, d_173836415_d_266600170_d_210921343, 
# d_173836415_d_266600170_d_224596428, d_173836415_d_266600170_d_316824786, d_173836415_d_266600170_d_341570479, d_173836415_d_266600170_d_398645039, d_173836415_d_266600170_d_448660695, d_173836415_d_266600170_d_452847912, d_173836415_d_266600170_d_453452655, 
# d_173836415_d_266600170_d_530173840, d_173836415_d_266600170_d_534041351, d_173836415_d_266600170_d_541311218, d_173836415_d_266600170_d_561681068, d_173836415_d_266600170_d_592099155, d_173836415_d_266600170_d_693370086, d_173836415_d_266600170_d_718172863, 
# d_173836415_d_266600170_d_728696253, d_173836415_d_266600170_d_740582332, d_173836415_d_266600170_d_769615780, d_173836415_d_266600170_d_786930107, d_173836415_d_266600170_d_822274939, d_173836415_d_266600170_d_847159717, d_173836415_d_266600170_d_860477844, 
# d_173836415_d_266600170_d_880794013, d_173836415_d_266600170_d_915179629, d_173836415_d_266600170_d_939818935, d_173836415_d_266600170_d_982213346, d_684635302, d_254109640, d_878865966, d_167958071, d_827220437, 
# d_173836415_d_266600170_d_543608829, d_173836415_d_266600170_d_110349197, d_831041022, d_173836415_d_266600170_d_319972665_d_379252329, d_173836415_d_266600170_d_319972665_d_221592017, d_173836415_d_266600170_d_319972665_d_661940160, d_195145666
# FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` where Connect_ID IS NOT NULL and d_831041022='104430631'"

parts <- "SELECT Connect_ID, d_173836415_d_266600170_d_139245758, d_173836415_d_266600170_d_156605577, d_173836415_d_266600170_d_184451682, d_173836415_d_266600170_d_185243482, d_173836415_d_266600170_d_198261154, d_173836415_d_266600170_d_210921343, d_173836415_d_266600170_d_224596428, d_173836415_d_266600170_d_316824786, d_173836415_d_266600170_d_341570479, d_173836415_d_266600170_d_398645039, d_173836415_d_266600170_d_448660695, d_173836415_d_266600170_d_452847912, d_173836415_d_266600170_d_453452655, d_173836415_d_266600170_d_530173840, d_173836415_d_266600170_d_534041351, d_173836415_d_266600170_d_541311218, d_173836415_d_266600170_d_561681068, d_173836415_d_266600170_d_592099155, d_173836415_d_266600170_d_693370086, d_173836415_d_266600170_d_718172863, d_173836415_d_266600170_d_728696253, d_173836415_d_266600170_d_740582332, d_173836415_d_266600170_d_769615780, d_173836415_d_266600170_d_786930107, d_173836415_d_266600170_d_822274939, d_173836415_d_266600170_d_847159717, d_173836415_d_266600170_d_860477844, d_173836415_d_266600170_d_880794013, d_173836415_d_266600170_d_915179629, d_173836415_d_266600170_d_939818935, d_173836415_d_266600170_d_982213346, d_684635302, d_254109640, d_878865966, d_167958071, d_827220437, d_173836415_d_266600170_d_543608829, d_173836415_d_266600170_d_110349197, d_831041022, d_173836415_d_266600170_d_319972665_d_379252329, d_173836415_d_266600170_d_319972665_d_221592017, d_173836415_d_266600170_d_319972665_d_661940160, d_195145666, d_173836415_d_266600170_d_319972665_d_826941471, #d_173836415_d_266600170_d_319972665_d_759651991, 
d_173836415_d_266600170_d_319972665_d_687158491, d_173836415_d_266600170_d_541483796_d_379252329, 
d_173836415_d_266600170_d_541483796_d_221592017, d_173836415_d_266600170_d_541483796_d_826941471,
d_173836415_d_266600170_d_541483796_d_661940160, d_173836415_d_266600170_d_541483796_d_759651991, d_173836415_d_266600170_d_541483796_d_687158491, d_173836415_d_266600170_d_641006239_d_379252329, 
d_173836415_d_266600170_d_641006239_d_221592017, d_173836415_d_266600170_d_641006239_d_661940160, d_173836415_d_266600170_d_641006239_d_759651991, d_173836415_d_266600170_d_641006239_d_687158491, d_173836415_d_266600170_d_641006239_d_826941471

FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` where Connect_ID IS NOT NULL and d_831041022='104430631'"



parts_table <- bq_project_query(project, parts)

parts_data <- bq_table_download(parts_table, bigint = "integer64", n_max = Inf, page_size = 1000)

bio_table <- bq_project_query(project, bio)
bio_data <- bq_table_download(bio_table, bigint = "integer64", n_max = Inf, page_size = 1000)


bio_data$Connect_ID <- as.numeric(bio_data$Connect_ID)
parts_data$Connect_ID <- as.numeric(parts_data$Connect_ID)



#bioqc_join= left_join(parts_data, bio_data, by="Connect_ID") 

bioqc= inner_join(bio_data,parts_data,  by=c("Connect_ID", "d_827220437") )

knitr::opts_chunk$set(comment = NA)





bioqc <- bioqc %>%  mutate(Site=case_when(d_827220437==472940358 ~ "Baylor Scott and White Health",
                                          d_827220437==531629870 ~ "HealthPartners",
                                          d_827220437==548392715 ~ "Henry Ford Health System",
                                          d_827220437==303349821 ~ "Marshfield Clinical Health System",
                                          d_827220437==657167265 ~ "Sanford Health",
                                          d_827220437==809703864 ~ "University of Chicago Medicine",
                                          d_827220437==125001209 ~ "Kaiser Permanente Colorado",
                                          d_827220437==327912200 ~ "Kaiser Permanente Georgia",
                                          d_827220437==300267574 ~ "Kaiser Permanente Hawaii",
                                          d_827220437==452412599 ~ "Kaiser Permanente Northwest"))

bioqc$Collection_ID = bioqc$d_820476880



##### Making sure personal C drives aren't referenced if this code is being used by others


#Change to FALSE if referencing this code for Box outputs

write_to_local_drive = F #F



### This function below determines whether the file will be created locally or not.
local_drive= ifelse(write_to_local_drive, "~/Biospecimen/data/", "")


currentDate <- Sys.Date()


#################

## Sorting by Site causes an error message when there are no rows. This function eliminates that problem
safe_arrange <- function(df, ...) {
  if (nrow(df) > 0) {
    df %>% arrange(...)
  } else {
    df
  }
}




###########################################      RULES            #################################################################################################


## Note: Participants with errors whose data we will not correct will be excluded from the rule. Github contains records of these exclusions as well.



bioqc_csv <- bioqc %>%  
  mutate(# 1. (Derived) Clinical DB Blood RRL Received (BioClin_DBBloodRRL_v1r0): If all blood tubes are not collected, this should be no
         Rule1 = ifelse(d_232343615_d_593843561==104430631 & d_299553921_d_593843561==104430631 & d_376960806_d_593843561==104430631 & d_454453939_d_593843561==104430631 & 
                          d_589588440_d_593843561==104430631 & d_958646668_d_593843561==104430631 & d_677469051_d_593843561==104430631 & d_683613884_d_593843561==104430631 & 
                          d_703954371_d_593843561==104430631 & d_838567176_d_593843561==104430631 & 
                          d_173836415_d_266600170_d_534041351!=104430631, "Rule 1", " "),
         
         # 2.a. If all blood tubes are not collected, (Derived) Clinical Site Blood Collected (BioClin_SiteBloodColl_v1r0) should be no or NULL.
         Rule2a = ifelse(d_232343615_d_593843561==104430631 & d_299553921_d_593843561==104430631 & d_376960806_d_593843561==104430631 & 
                           d_454453939_d_593843561==104430631 & d_589588440_d_593843561==104430631 & d_958646668_d_593843561==104430631 & 
                           d_677469051_d_593843561==104430631 & d_683613884_d_593843561==104430631 & d_703954371_d_593843561==104430631 & 
                           d_838567176_d_593843561==104430631 &  d_173836415_d_266600170_d_693370086==353358909 & 
                           !(Connect_ID %in% c("2300063524","7848933050", "3467573584")), "Rule 2a", " "),
         
         # 2.b. If any urine tubes is not collected, (Derived) Clinical Site Urine Collected (BioClin_SiteUrineCollBL_v1r0) should be no or NULL.
         Rule2b = ifelse(d_973670172_d_593843561==104430631  & d_173836415_d_266600170_d_786930107==353358909 & 
                           !(Connect_ID %in% c(1140047316, 1215003341, 1250934825, 2300063524, 2039357566, 2755205973, 
                                               3287102562, 3862626013, 4479873637, 4980503471, 6321294709, 7352978604, 
                                               7882825421, 8184138489, 8633594373, 1102086935, 1375421153, 2871811858, 
                                               2887898061, 3456526723, 3992444928, 4207529981, 4838682809, 6467484794, 
                                               6523900663, 6538980272, 7106367407, 9366425281, 9652100378, 9672292896, 
                                               6422794867, 1349953410, 1731138933, 2769291903, 3589595480, 3722445358, 
                                               5972658064, 8553891957, 9121406600, 9575612025, 4057490101, 9167792140,
                                               8924667241, 9694753790, 5899565591, 6568449334, 1371560328, 8625295305,
                                               "6862754687", "9143436002", "3319331872", "7276829171", "9262617255",
                                               2431356225, 1703960468, '6437389686', '7338222763')), 
                         "Rule 2b", " "),
         
         # 3. (Derived) Baseline blood sample collected (BioFin_BaseBloodCol_v1r0): If all blood tubes are not collected, this should be no
         Rule3 = ifelse(d_232343615_d_593843561==104430631 & d_299553921_d_593843561==104430631 & d_376960806_d_593843561==104430631 & 
                          d_454453939_d_593843561==104430631 & d_589588440_d_593843561==104430631 & d_958646668_d_593843561==104430631 & 
                          d_677469051_d_593843561==104430631 & d_683613884_d_593843561==104430631 & d_703954371_d_593843561==104430631 & 
                          d_838567176_d_593843561==104430631 & d_878865966!=104430631 & 
                          Connect_ID!=2310991421, "Rule 3", " "),
         
         # 3b. (Derived) Baseline blood sample collected (BioFin_BaseBloodCol_v1r0): If any blood tubes are collected, this should be yes
         Rule3 = ifelse((d_232343615_d_593843561==353358909 | d_299553921_d_593843561==353358909 | d_376960806_d_593843561==353358909 | 
                           d_454453939_d_593843561==353358909 | d_589588440_d_593843561==353358909 | d_958646668_d_593843561==353358909 | 
                           d_677469051_d_593843561==353358909 | d_683613884_d_593843561==353358909 | d_703954371_d_593843561==353358909 | 
                           d_838567176_d_593843561==353358909) & d_878865966==104430631, "Rule 3b", " "),
         
         # 4. (Derived) Baseline urine sample collected (BioFin_BaseUrineCol_v1r0): If any urine tubes are not collected, this should be no
         Rule4 = ifelse(d_973670172_d_593843561==104430631  & d_678166505>=as.Date("2022-11-21") & d_167958071!=104430631, "Rule 4", " "),
         
         # 5. If BioClin_BldOrderPlacdDt_v1r0 occured before BioClin_UrnOrderPlacdDt_v1r0, then BioClin_BldUrnPlacedTm_v1r0 should = BioClin_BldOrderPlacdDt_v1r0
         Rule5 = ifelse(as.POSIXct(ymd_hms(d_173836415_d_266600170_d_769615780)) < as.POSIXct(ymd_hms(d_173836415_d_266600170_d_939818935)) & 
                          as.POSIXct(ymd_hms(d_173836415_d_266600170_d_184451682))!=as.POSIXct(ymd_hms(d_173836415_d_266600170_d_769615780)), "Rule 5", " "),
         
         # 6. If BioClin_BldOrderPlacdDt_v1r0 occured after BioClin_UrnOrderPlacdDt_v1r0, then BioClin_BldUrnPlacedTm_v1r0 should = BioClin_UrnOrderPlacdDt_v1r0
         Rule6 = ifelse(as.POSIXct(ymd_hms(d_173836415_d_266600170_d_769615780)) > as.POSIXct(ymd_hms(d_173836415_d_266600170_d_939818935)) & 
                          as.POSIXct(ymd_hms(d_173836415_d_266600170_d_184451682))!=as.POSIXct(ymd_hms(d_173836415_d_266600170_d_939818935)), "Rule 6", " "),
         
         #7. If BioFin_BaseUrineCol_v1r0 was collected, BioSpm_BloodSettingBL_v1r0 is clincial, and BioClin_DBUrineRRLDt_v1r0 occurred more than seven days ago, BioClin_SiteUrineColl_v1r0 must be yes
         Rule7 = ifelse(d_167958071==353358909 & d_650516960==664882224 & 
                          as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_541311218, units="days"), digits=0)) > 7  & 
                          is.na(d_173836415_d_266600170_d_786930107), "Rule 7", " "),
         
         #8. If BioFin_BaseBloodCol_v1r0 = yes, collection setting is Clincial and BioClin_DBBloodRRLDtBL_v1r0 occurred more than 7 days ago, BioClin_SiteBloodColl_v1r0 must be yes
         Rule8 = ifelse(d_878865966==353358909 & d_650516960==664882224 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0)) > 7  & 
                          is.na(d_173836415_d_266600170_d_693370086), "Rule 8", " "),
         
         #9. If all baseline samples are collected, and BioCol_ColTime_v1r0 occured on or after 11/21/2022 then SMMet_BLSamplesColl_v1r0 must be YES
         Rule9 = ifelse(d_878865966==353358909 & d_167958071==353358909 & d_684635302==353358909 & 
                          d_678166505>=as.Date("2022-11-21")  & d_254109640 !=353358909, "Rule 9", " "),
         
         #10. If BioFin_BaseBloodCol_v1r0 is yes and the collection setting is Research, and BioCol_ColTime_v1r0 occured on or after 11/21/2022 then (Autogenerated) Date/time Baseline Research Blood Collected must be populated  
         Rule10 = ifelse(d_878865966==353358909 & d_650516960==534621077 & 
                          d_678166505>=as.Date("2022-11-21") & is.na(d_173836415_d_266600170_d_561681068), "Rule 10", " "),
         
         #11. If BioFin_BaseUrineCol_v1r0 is yes and the collection setting is Research, and BioCol_ColTime_v1r0 occured on or after 11/21/2022 then (Autogenerated) Date/time Baseline Research Urine Collected must be populated
         Rule11 = ifelse(d_167958071==353358909 & d_650516960==534621077  & d_678166505>=as.Date("2022-11-21") & 
                           is.na(d_173836415_d_266600170_d_847159717), "Rule 11", " "),
         
         #12. If BioFin_BaseBloodCol_v1r0 is yes and the collection setting is Research, BioCol_ColTime_v1r0 must be populated
         Rule12 = ifelse(d_878865966==353358909 & d_650516960==534621077   & is.na(d_678166505), "Rule 12", " "),
         
         #13. If BioCol_ObjCollected_v1r0 (mouthwash), and BioCol_ColTime_v1r0 occured on or after 11/21/2022, then  BioFin_BaseMouthCol_v1r0 should be no
         Rule13 = ifelse(d_143615646_d_593843561==104430631  & d_678166505>=as.Date("2022-11-21") & d_684635302!=104430631, "Rule 13", " "),
         
         #14. If Urine tube was collected, site was Clinical, and BioReg_ArRegTime_v1r0 occurred more than 7 days ago, BioClin_SiteUrineColl_v1r0 must be yes
         Rule14 = ifelse(Connect_ID!=5667758818 & d_973670172_d_593843561==353358909  & d_650516960==664882224 & 
                           as.numeric(round(difftime(currentDate, d_915838974, units="days"), digits=0)) >7 & 
                           is.na(d_173836415_d_266600170_d_786930107), "Rule 14", " "),
         
         #15. IF BioClin_BldOrderPlaced_v1r0 = 1, THEN BioClin_BldOrderPlacdDt_v1r0 must be populated. HP, UC, and SF Excluded
         Rule15 = ifelse(is.na(d_173836415_d_266600170_d_769615780) & d_173836415_d_266600170_d_530173840==353358909	& 
                           d_827220437!=531629870 & d_827220437!=657167265 & d_827220437!=809703864, "Rule 15", " "),
         
         #16. IF BioClin_UrnOrdPlaced = 1, THEN BioClin_UrnOrderPlacdDt_v1r0 must be populated. HP, UC and SF Excluded
         Rule16 = ifelse(is.na(d_173836415_d_266600170_d_939818935) & d_173836415_d_266600170_d_860477844==353358909	& 
                           d_827220437!=531629870 & d_827220437!=657167265& d_827220437!=809703864, "Rule 16", " "),
         
         #17. If BioClin_DBBloodRRLDt_v1r0 occurred more than 4 days ago, (tube type) was collected and was not discarded, then BioBPTL_DateRec_v1r0 should be populated 
         Rule17_SST1 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                            d_299553921_d_593843561==353358909 & d_299553921_d_762124027==104430631 & is.na(d_299553921_d_926457119) &
                             Connect_ID!="7609852429", "Rule 17-SST1", " "),
         
         Rule17_SST2 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                d_703954371_d_593843561==353358909 & d_703954371_d_762124027==104430631 & is.na(d_703954371_d_926457119) & 
                                Connect_ID!="7609852429", "Rule 17-SST2", " "),
         
         Rule17_SST3 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                d_376960806_d_593843561==353358909 & d_376960806_d_762124027==104430631 & is.na(d_376960806_d_926457119) & 
                                !(Connect_ID %in% c("9729007313","7609852429", "6380609394")), "Rule 17-SST3", " "),
         
         Rule17_SST4 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                d_232343615_d_593843561==353358909 & d_232343615_d_762124027==104430631 & is.na(d_232343615_d_926457119) & 
                                !(Connect_ID %in% c("9729007313","7609852429", "6380609394")), "Rule 17-SST4", " "),
         
         Rule17_SST5 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                d_589588440_d_593843561==353358909 & d_589588440_d_762124027==104430631 & is.na(d_589588440_d_926457119) & 
                                !(Connect_ID %in% c("6877720616", "1158253659")), "Rule 17-SST5", " "),
         
         Rule17_EDTA1 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                 d_454453939_d_593843561==353358909 & d_454453939_d_762124027==104430631 & is.na(d_454453939_d_926457119) & 
                                 !(Connect_ID %in% c("9729007313","7609852429")), "Rule 17-EDTA1", " "),
         
         Rule17_EDTA2 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                 d_677469051_d_593843561==353358909 & d_677469051_d_762124027==104430631 & is.na(d_677469051_d_926457119) & 
                                 !(Connect_ID %in% c("9729007313","7609852429")), "Rule 17-EDTA2", " "),
         
         Rule17_EDTA3 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                 d_683613884_d_593843561==353358909 & d_683613884_d_762124027==104430631 & is.na(d_683613884_d_926457119) & 
                                 !(Connect_ID %in% c("9729007313","7609852429")), "Rule 17-EDTA3", " "),
         
         Rule17_ACD1 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                d_652357376_d_593843561==353358909 & d_652357376_d_762124027==104430631 & is.na(d_652357376_d_926457119) & 
                                d_827220437==548392715 & (d_650516960==664882224| d_827220437==657167265), "Rule 17-ACD1", " "),
         
         Rule17_HEP1 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                d_703954371_d_593843561==353358909 & d_703954371_d_762124027==104430631 & is.na(d_703954371_d_926457119) & 
                                Connect_ID!="7609852429", "Rule 17-HEP1", " "),
         
         Rule17_HEP2 = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                d_958646668_d_593843561==353358909 & d_958646668_d_762124027==104430631 &  is.na(d_958646668_d_926457119) & 
                                Connect_ID!="7609852429", "Rule 17-HEP2", " "),
         
         Rule17_URN = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_541311218, units="days"), digits=0))>4 & 
                               d_973670172_d_593843561==353358909 & d_973670172_d_762124027==104430631 & is.na(d_973670172_d_926457119) & 
                               !(Connect_ID %in% c("2215062576","4002548016","7609852429")), "Rule 17-URN", " "),
         
         Rule17_STRECK = ifelse(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                  d_505347689_d_593843561==353358909 & d_505347689_d_762124027==104430631 & is.na(d_505347689_d_926457119), "Rule 17-STRECK", " "),
         
         
         #18. Tube Date Received Research - If (tube type) was collected, and (tube type) was not discarded, then BioBPTL_DateRec_v1r0 for that tube type should be populated 
         Rule18_mw = ifelse(d_650516960==534621077 & !is.na(d_173836415_d_266600170_d_448660695) &
                              as.numeric(round(difftime(currentDate, as.Date(d_173836415_d_266600170_d_448660695), units="days"), digits=0))>4 & 
                                d_143615646_d_593843561==353358909 & d_143615646_d_762124027==104430631 & is.na(d_143615646_d_926457119) &
                              !(Connect_ID %in% c("2330722643", "3080768933", "9375710580", "3409243008")), "Rule 18-mw", " "),
         
         Rule18_SST1 = ifelse(d_650516960==534621077 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_561681068, units="days"), digits=0))>4 & 
                                d_299553921_d_593843561==353358909 & d_299553921_d_762124027==104430631 & is.na(d_299553921_d_926457119) & 
                                !(Connect_ID %in% c("3409243008","8598143342")), "Rule 18-SST1", " "),
         
         Rule18_EDTA1 = ifelse(d_650516960==534621077 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_561681068, units="days"), digits=0))>4 & 
                                 d_454453939_d_593843561==353358909 & d_454453939_d_762124027==104430631 & is.na(d_454453939_d_926457119) &
                                 !(Connect_ID %in% c("3409243008", "8193439990", "5743151720", "5861017989", "8598143342")), "Rule 18-EDTA1", " "),
         
         Rule18_ACD1 = ifelse((d_650516960==534621077 | d_827220437==657167265) & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_561681068, units="days"), digits=0))>4 & 
                                d_652357376_d_593843561==353358909 & d_652357376_d_762124027==104430631 & is.na(d_652357376_d_926457119) & 
                                !(Connect_ID %in% c("1735874266", "3409243008")), "Rule 18-ACD1", " "),
         
         Rule18_SST2 = ifelse(d_650516960==534621077 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_561681068, units="days"), digits=0))>4 & 
                                d_703954371_d_593843561==353358909 & d_703954371_d_762124027==104430631 & is.na(d_703954371_d_926457119) & 
                                Connect_ID!="3409243008", "Rule 18-SST2", " "),
         
         Rule18_HEP1 = ifelse(d_650516960==534621077 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_561681068, units="days"), digits=0))>4 & 
                                d_838567176_d_593843561==353358909 & d_838567176_d_762124027==104430631 &  is.na(d_838567176_d_926457119) & 
                                !(Connect_ID %in% c("3409243008", "8193439990", "5842989867")), "Rule 18-HEP1", " "),
         
         Rule18_URN = ifelse(d_650516960==534621077 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_561681068, units="days"), digits=0))>4 &  
                               d_973670172_d_593843561==353358909 & d_973670172_d_762124027==104430631 & is.na(d_973670172_d_926457119) & 
                               !(Connect_ID %in% c(9145733933,9618974099,8527264977,6473532641,4174960021, 4435276749, 1834747483)), 
                             "Rule 18-URN", " "),
         
         Rule18_STRECK = ifelse(d_650516960==534621077 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0))>4 & 
                                  d_505347689_d_593843561==353358909 & d_505347689_d_762124027==104430631 & is.na(d_505347689_d_926457119) & 
                                  d_827220437==548392715 & (d_650516960==664882224 | d_827220437==657167265), "Rule 18-STRECK", " "),
         
         
         #19. If BioClin_BldOrderPlcdBL_v1r0=yes or BioClin_UrnOrdPlacedBL_v1r0=yes, then BioClin_BldUrnPlcdTmBL_v1r0 is the first of BioClin_BldOrdPlacdDtBL_v1r0 and BioClin_UrnOrdPlcdDtBL_v1r0
         Rule19 = ifelse((d_173836415_d_266600170_d_530173840 == 353358909 | d_173836415_d_266600170_d_860477844 == 353358909) & 
                           d_173836415_d_266600170_d_184451682 != min(as.Date(d_173836415_d_266600170_d_769615780), as.Date(d_173836415_d_266600170_d_939818935)), "Rule 19", " "),
         
         
         #20. If any blood tube is collected and BioSpm_Setting_v1r0= Research then BioSpm_BloodSettingBL_v1r0 must be Research.
         Rule20 = ifelse((d_232343615_d_593843561==353358909 | d_299553921_d_593843561==353358909 | d_376960806_d_593843561==353358909 | d_454453939_d_593843561==353358909 | 
                            d_589588440_d_593843561==353358909 | d_958646668_d_593843561==353358909 | d_677469051_d_593843561==353358909 | d_683613884_d_593843561==353358909 | 
                            d_703954371_d_593843561==353358909 | d_838567176_d_593843561==353358909) & 
                           d_650516960==534621077 & d_173836415_d_266600170_d_592099155 !=534621077, "Rule 20", " "),
         
         
         #21. If any blood tube is collected and BioSpm_Setting_v1r0= Clinical then BioSpm_BloodSettingBL_v1r0 must be Clinical.
         Rule21 = ifelse((d_232343615_d_593843561==353358909 | d_299553921_d_593843561==353358909 | d_376960806_d_593843561==353358909 | 
                            d_454453939_d_593843561==353358909 | d_589588440_d_593843561==353358909 | d_958646668_d_593843561==353358909 | 
                            d_677469051_d_593843561==353358909 | d_683613884_d_593843561==353358909 | d_703954371_d_593843561==353358909 | 
                            d_838567176_d_593843561==353358909) & d_650516960==664882224 & d_173836415_d_266600170_d_592099155 !=664882224, "Rule 21", " "),
         
         
         #22. If the Urine tube is collected and BioSpm_Setting_v1r0= Clinical then BioSpm_UrineSettingBL_v1r0 must be Clinical.
         Rule22 = ifelse((d_973670172_d_593843561==353358909) & d_650516960==664882224 & d_173836415_d_266600170_d_718172863 !=664882224, "Rule 22", " "),
         
         
         #23. If the Urine tube is collected and BioSpm_Setting_v1r0= Research then BioSpm_UrineSettingBL_v1r0 must be Research.
         Rule23 = ifelse((d_973670172_d_593843561==353358909) & d_650516960==534621077 & d_173836415_d_266600170_d_718172863 !=534621077, "Rule 23", " "),
         
         
         #24. If the Mouthwash tube is collected and BioSpm_Setting_v1r0= Clinical then BioSpm_MWSettingBL_v1r0 must be Clinical.
         Rule24 = ifelse((d_143615646_d_593843561==353358909) & d_650516960==664882224 & d_173836415_d_266600170_d_915179629 !=664882224, "Rule 24", " "),
         
         
         #25. If the Mouthwash tube is collected and BioSpm_Setting_v1r0= Research then BioSpm_MWSettingBL_v1r0 must be Research.
         Rule25 = ifelse((d_143615646_d_593843561==353358909) & d_650516960==534621077 & d_173836415_d_266600170_d_915179629 !=534621077, "Rule 25", " "),
         
         
         #26.a. If BioFin_BaseBloodCol_v1r0= yes, and BioSpm_BloodSettingBL_v1r0= Research, then BioFin_ResearchBldTmBL_v1r0 must be populated.
         Rule26a = ifelse(d_878865966=="353358909" & d_173836415_d_266600170_d_592099155=="534621077" & is.na(d_173836415_d_266600170_d_561681068), "Rule 26a", " "),
         
         
         #26.b. If BioSpm_BloodSettingBL_v1r0 is populated, then BioFin_BaseBloodCol_v1r0= yes.
         Rule26b = ifelse(!is.na(d_173836415_d_266600170_d_592099155) & d_878865966=="104430631", "Rule 26b", " "),
         
         
         #26.c. If BioFin_BaseBloodCol_v1r0= yes, then BioSpm_BloodSettingBL_v1r0 must be populated.
         Rule26c = ifelse(d_878865966=="353358909" & is.na(d_173836415_d_266600170_d_592099155), "Rule 26c", " "),
         
         
         #26.d. If BioFin_ResearchBldTmBL_v1r0 is populated, then BioSpm_BloodSettingBL_v1r0 must be Research and BioFin_BaseBloodCol_v1r0 must be yes.
         Rule26d = ifelse(!is.na(d_173836415_d_266600170_d_561681068) & 
                            (d_173836415_d_266600170_d_592099155=="664882224" |is.na(d_173836415_d_266600170_d_592099155) | d_878865966=="104430631"), "Rule 26d", " "),
         
         
         #27.a. If BioFin_BaseBloodCol_v1r0= yes, and BioSpm_BloodSettingBL_v1r0= Research, then BioFin_ResearchBldTmBL_v1r0 must be populated.
         Rule27a = ifelse(d_167958071=="353358909" & d_173836415_d_266600170_d_718172863=="534621077" & is.na(d_173836415_d_266600170_d_847159717), "Rule 27a", " "),
         
         
         #27.b. If BioSpm_BloodSettingBL_v1r0 is populated, then BioFin_BaseBloodCol_v1r0= yes.
         Rule27b = ifelse(!is.na(d_173836415_d_266600170_d_718172863) & d_167958071=="104430631", "Rule 27b", " "),
         
         
         #27.c. If BioFin_BaseBloodCol_v1r0= yes, then BioSpm_BloodSettingBL_v1r0 must be populated.
         Rule27c = ifelse(d_167958071=="353358909" & is.na(d_173836415_d_266600170_d_718172863), "Rule 27c", " "),
         
         
         #27.d. If BioFin_ResearchBldTmBL_v1r0 is populated, then BioSpm_BloodSettingBL_v1r0 must be Research and BioFin_BaseBloodCol_v1r0 must be yes.
         Rule27d = ifelse(!is.na(d_173836415_d_266600170_d_847159717) & (d_173836415_d_266600170_d_718172863=="664882224" | 
                                                                           is.na(d_173836415_d_266600170_d_718172863) | d_167958071=="104430631"), "Rule 27d", " "),
         
         
         #28.a. If BioFin_BaseMouthCol_v1r0= yes, and BioSpm_MWSettingBL_v1r0 is populated, then BioFin_BMTimeBL_v1r0 must be populated.
         Rule28a = ifelse(d_684635302=="353358909" & !is.na(d_173836415_d_266600170_d_915179629) & is.na(d_173836415_d_266600170_d_448660695), "Rule 28a", " "),
         
         
         #28.b. If BioSpm_MWSettingBL_v1r0 is populated, then BioFin_BaseMouthCol_v1r0= yes.
         Rule28b = ifelse(!is.na(d_173836415_d_266600170_d_915179629) & d_684635302=="104430631", "Rule 28b", " "),
         
         
         #28.c. If BioFin_BaseMouthCol_v1r0= yes, then BioSpm_MWSettingBL_v1r0 must be populated.
         Rule28c = ifelse(d_684635302=="353358909" & is.na(d_173836415_d_266600170_d_915179629), "Rule 28c", " "),
         
         
         #28.d. If BioFin_BMTimeBL_v1r0 is populated, then BioSpm_MWSettingBL_v1r0 must be populated and BioFin_BaseMouthCol_v1r0 must be yes.
         Rule28d = ifelse(!is.na(d_173836415_d_266600170_d_448660695) & (is.na(d_173836415_d_266600170_d_915179629) | d_684635302=="104430631"), "Rule 28d", " "),
         
         
         #29.a. If BioFin_BaseBloodCol_v1r0= yes, BioSpm_BloodSettingBL_v1r0= Clinical, and BioClin_DBBloodRRLDt_v1r0 occurred more than seven days ago, then BioClin_ClinBloodTmBL_v1r0 must be populated.
         Rule29a = ifelse(d_878865966=="353358909" & d_173836415_d_266600170_d_592099155=="664882224" & 
                            as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0)) > 7 & 
                            is.na(d_173836415_d_266600170_d_982213346), "Rule 29a", " "),
         
         #30.a. 30.a. If BioFin_BaseUrineCol_v1r0= yes, and BioSpm_UrineSettingBL_v1r0= Clinical, and BioClin_DBUrineRRLDt_v1r0 occurred more than seven days ago, then BioClin_ClinicalUrnTmBL_v1r0 must be populated.
         Rule30a = ifelse(d_167958071=="353358909" & d_173836415_d_266600170_d_718172863=="664882224" & is.na(d_173836415_d_266600170_d_139245758) & 
                            as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_541311218, units="days"), digits=0)) > 7 & 
                            Connect_ID!="8047468301", "Rule 30a", " "),
         
         #49. If BioFin_BaseBloodCol_v1r0 was collected, BioSpm_BloodSettingBL_v1r0 is clinical, and BioClin_DBBloodRRLDtBL_v1r0 occurred more than seven days ago, BioClin_SiteBldLocBL_v1r0 must be populated.
         Rule49 = ifelse(d_878865966==353358909 & d_173836415_d_266600170_d_592099155==664882224 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0)) > 7  & 
                                   is.na(d_173836415_d_266600170_d_185243482), "Rule 49", " "),
         
         
         #50. If BioFin_BaseBloodCol_v1r0 was collected, BioSpm_BloodSettingBL_v1r0 is clinical, and BioClin_DBBloodRRLDtBL_v1r0 occurred more than seven days ago, BioClin_SntBloodAccIDBL_v1r0 or BioClin_PolyBloodIDBL_v1r0 must be populated.
         Rule50 = ifelse(d_878865966==353358909 & d_173836415_d_266600170_d_592099155==664882224 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0)) > 7  & 
                           (is.na(d_173836415_d_266600170_d_341570479) & is.na(d_173836415_d_266600170_d_543608829)), "Rule 50", " "),
         
         
         #51. If BioFin_BaseUrineCol_v1r0 was collected, BioSpm_UrineSettingBL_v1r0 is clinical, and BioClin_DBUrineRRLDtBL_v1r0 occurred more than seven days ago, BioClin_SiteUrLocatBL_v1r0 must be populated.
         Rule51 = ifelse(d_167958071==353358909 & d_173836415_d_266600170_d_718172863==664882224 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_541311218, units="days"), digits=0)) > 7  & 
                            is.na(d_173836415_d_266600170_d_452847912)  & Connect_ID!="8047468301", "Rule 51", " "), 
 
         #52. If BioFin_BaseUrineCol_v1r0 was collected, BioSpm_UrineSettingBL_v1r0 is clinical, and BioClin_DBUrineRRLDtBL_v1r0 occurred more than seven days ago, BioClin_SntUrineAccIDBL_v1r0 or BioClin_PolyUrineIDBL_v1r0 must be populated.
         Rule52 = ifelse(d_167958071==353358909 & d_173836415_d_266600170_d_718172863==664882224 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_541311218, units="days"), digits=0)) > 7  & 
                           (is.na(d_173836415_d_266600170_d_198261154) & is.na(d_173836415_d_266600170_d_110349197)), "Rule 52", " "),
        ##	54. If initial kit is shipped, BioKit_KitShipTm_v1r0 should be populated.
        Rule54 = ifelse(d_173836415_d_266600170_d_319972665_d_221592017=="277438316" & is.na(d_173836415_d_266600170_d_319972665_d_661940160), "Rule 54", " "),

        ###	55. If initial kit is received, BioKit_KitRecdTm_v1r0 should be populated.
        Rule55 = ifelse(d_173836415_d_266600170_d_319972665_d_221592017=="375535639" & is.na(d_173836415_d_266600170_d_319972665_d_826941471), "Rule 55", " "),

        ###	56. If initial kit BioKit_KitStatus_v1r0 is Assigned, Shipped or Received, then BioKit_KitAssembledID_v1r0 should be populated.
        Rule56 = ifelse((d_173836415_d_266600170_d_319972665_d_221592017=="241974920" | d_173836415_d_266600170_d_319972665_d_221592017=="277438316" |
                                  d_173836415_d_266600170_d_319972665_d_221592017=="375535639") & is.na(d_173836415_d_266600170_d_319972665_d_687158491), "Rule 56", " "),

        ###	57. If initial kit BioKit_KitStatus_v1r0 is Initialized, Address Printed or Undeliverable Address, then BioKit_KitAssembledID_v1r0 should be null.
        Rule57 = ifelse((d_173836415_d_266600170_d_319972665_d_221592017=="728267588" | d_173836415_d_266600170_d_319972665_d_221592017=="849527480" |
                                  d_173836415_d_266600170_d_319972665_d_221592017=="332067457") & !is.na(d_173836415_d_266600170_d_319972665_d_687158491), "Rule 57", " "),

        ###	58. If Replacement Kit 1 is shipped, BioKit_KitShipTm_v1r0 should be populated.
        Rule58 = ifelse(d_173836415_d_266600170_d_541483796_d_221592017=="277438316" & is.na(d_173836415_d_266600170_d_541483796_d_661940160), "Rule 58", " "), 

        ###	59. If Replacement Kit 1 is received, BioKit_KitRecdTm_v1r0 should be populated.
        Rule59 = ifelse(d_173836415_d_266600170_d_541483796_d_221592017=="375535639" & is.na(d_173836415_d_266600170_d_541483796_d_826941471), "Rule 59", " "), 

        ###	60. If Replacement Kit 1 BioKit_KitStatus_v1r0 is Assigned, Shipped or Received, then BioKit_KitAssembledID_v1r0 should be populated.
        Rule60 = ifelse((d_173836415_d_266600170_d_541483796_d_221592017=="241974920" | d_173836415_d_266600170_d_541483796_d_221592017=="277438316" |
                                     d_173836415_d_266600170_d_541483796_d_221592017=="375535639") & is.na(d_173836415_d_266600170_d_541483796_d_687158491), "Rule 60", " "),

        ###	61. If Replacement Kit 1 BioKit_KitStatus_v1r0 is Initialized, Address Printed or Undeliverable Address, then BioKit_KitAssembledID_v1r0 should be null.
        Rule61 = ifelse((d_173836415_d_266600170_d_541483796_d_221592017=="728267588" | d_173836415_d_266600170_d_541483796_d_221592017=="849527480" |
                                     d_173836415_d_266600170_d_541483796_d_221592017=="332067457") & !is.na(d_173836415_d_266600170_d_541483796_d_687158491), "Rule 61", " "),


        ###	62. If Replacement Kit 2 is shipped, BioKit_KitShipTm_v1r0 should be populated.
        Rule62 = ifelse(d_173836415_d_266600170_d_641006239_d_221592017=="277438316" & is.na(d_173836415_d_266600170_d_641006239_d_661940160), "Rule 62", " "),

        ###	63. If Replacement Kit 2 is received, BioKit_KitRecdTm_v1r0 should be populated.
        Rule63 = ifelse(d_173836415_d_266600170_d_641006239_d_221592017=="375535639" & is.na(d_173836415_d_266600170_d_641006239_d_826941471), "Rule 63", " "),


        ###	64. If Replacement Kit 2 BioKit_KitStatus_v1r0 is Assigned, Shipped or Received, then BioKit_KitAssembledID_v1r0 should be populated.
        Rule64 = ifelse((d_173836415_d_266600170_d_641006239_d_221592017=="241974920" | d_173836415_d_266600170_d_641006239_d_221592017=="277438316" |
                                     d_173836415_d_266600170_d_641006239_d_221592017=="375535639") & is.na(d_173836415_d_266600170_d_641006239_d_687158491), "Rule 64", " "),


        ###	65. If Replacement Kit 2 BioKit_KitStatus_v1r0 is Initialized, Address Printed or Undeliverable Address, then BioKit_KitAssembledID_v1r0 should be null.
        Rule65 = ifelse((d_173836415_d_266600170_d_641006239_d_221592017=="728267588" | d_173836415_d_266600170_d_641006239_d_221592017=="849527480" |
                                     d_173836415_d_266600170_d_641006239_d_221592017=="332067457") & !is.na(d_173836415_d_266600170_d_641006239_d_687158491), "Rule 65", " "),
        
        ###	70. If any blood tube collected is YES and not discarded, BLOOD BioClin_DBBloodRRLBL_v1r0 must be YES and BioClin_DBBloodRRLDtBL_v1r0 must be populated
        Rule70_SST1 = ifelse(d_299553921_d_593843561=="Yes" & d_299553921_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                           d_650516960=="Clinical", "Rule 70-SST1", " "),
        
        Rule70_SST2 = ifelse(d_703954371_d_593843561=="Yes" & d_703954371_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                           d_650516960=="Clinical", "Rule 70-SST2", " "),
        
        Rule70_SST3 = ifelse(d_376960806_d_593843561=="Yes" & d_376960806_d_762124027=="No" &
                                               (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                               d_650516960=="Clinical", "Rule 70-SST3", " "),
        
        Rule70_SST4 = ifelse(d_232343615_d_593843561=="Yes" & d_232343615_d_762124027=="No" &
                                               (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039))  &
                                               d_650516960=="Clinical", "Rule 70-SST4", " "),
        
        Rule70_SST5 = ifelse(d_589588440_d_593843561=="Yes" & d_589588440_d_762124027=="No" &
                                               (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                               d_650516960=="Clinical", "Rule 70-SST5", " "),
        
        Rule70_EDTA1 = ifelse(d_454453939_d_593843561=="Yes" & d_454453939_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                           d_650516960=="Clinical", "Rule 70-EDTA1", " "),
        
        Rule70_EDTA2 = ifelse(d_677469051_d_593843561=="Yes" & d_677469051_d_762124027=="No" &
                                                (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                                d_650516960=="Clinical", "Rule 70-EDTA2", " "),
        
        Rule70_EDTA3 = ifelse(d_683613884_d_593843561=="Yes" & d_683613884_d_762124027=="No"  &
                                                (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                                d_650516960=="Clinical", "Rule 70-EDTA3", " "),
        
        Rule70_ACD = ifelse(d_652357376_d_593843561=="Yes" & d_652357376_d_762124027=="No"  &
                                           (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                           d_650516960=="Clinical", "Rule 70-ACD", " "),
        
        Rule70_HEP1 = ifelse(d_838567176_d_593843561=="Yes" & d_838567176_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                           d_650516960=="Clinical", "Rule 70-HEP1", " "),
        
        Rule70_HEP2 = ifelse(d_958646668_d_593843561=="Yes" & d_958646668_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                           d_650516960=="Clinical", "Rule 70-HEP2", " "),
        
        Rule70_STRECK = ifelse(d_505347689_d_593843561=="Yes" & d_505347689_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_534041351!="Yes" | is.na(d_173836415_d_266600170_d_398645039)) &
                                           d_650516960=="Clinical", "Rule 70-STRECK", " "),

        ###	71. If BioCol_TubeColl_v1r0_UT1 is YES and BioCol_Discard_v1r0_UT1 is NO, then BioClin_DBUrineRRLBL_v1r0 must be YES and BioClin_DBUrineRRLDtBL_v1r0 must be populated
        Rule71 = ifelse(d_973670172_d_593843561=="Yes" & d_973670172_d_762124027=="No" & 
                                           (d_173836415_d_266600170_d_210921343!="Yes" | is.na(d_173836415_d_266600170_d_541311218)) &
                                           d_650516960=="Clinical", "Rule 71", " "),
        
        ###	72. If any blood tube or urine tube is YES and not discarded, BioClin_AnySpecRRLBL_v1r0 must be YES and BioClin_AnySpecRRLTmBL_v1r0 must be populated
        Rule72_SST1 = ifelse(d_299553921_d_593843561=="Yes" & d_299553921_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                           d_650516960=="Clinical", "Rule 72-SST1", " "),
        
        Rule72_SST2 = ifelse(d_703954371_d_593843561=="Yes" & d_703954371_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332))  &
                                           d_650516960=="Clinical", "Rule 72-SST2", " "),
        
        Rule72_SST3 = ifelse(d_376960806_d_593843561=="Yes" & d_376960806_d_762124027=="No" &
                                               (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332))  &
                                               d_650516960=="Clinical", "Rule 72-SST3", " "),
        
        Rule72_SST4 = ifelse(d_232343615_d_593843561=="Yes" & d_232343615_d_762124027=="No" &
                                               (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332))  &
                                               d_650516960=="Clinical", "Rule 72-SST4", " "),
        
        Rule72_SST5 = ifelse(d_589588440_d_593843561=="Yes" & d_589588440_d_762124027=="No" &
                                               (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                               d_650516960=="Clinical", "Rule 72-SST5", " "),
        
        Rule72_EDTA1 = ifelse(d_454453939_d_593843561=="Yes" & d_454453939_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                           d_650516960=="Clinical", "Rule 72-EDTA1", " "),
        
        Rule72_EDTA2 = ifelse(d_677469051_d_593843561=="Yes" & d_677469051_d_762124027=="No" &
                                                (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                                d_650516960=="Clinical", "Rule 72-EDTA2", " "),
        
        Rule72_EDTA3 = ifelse(d_683613884_d_593843561=="Yes" & d_683613884_d_762124027=="No"  &
                                                (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                                d_650516960=="Clinical", "Rule 72-EDTA3", " "),
        
        Rule72_ACD = ifelse(d_652357376_d_593843561=="Yes" & d_652357376_d_762124027=="No"  &
                                           (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                           d_650516960=="Clinical", "Rule 72-ACD", " "),
        
        Rule72_HEP1 = ifelse(d_838567176_d_593843561=="Yes" & d_838567176_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                           d_650516960=="Clinical", "Rule 72-HEP1", " "),
        
        Rule72_HEP2 = ifelse(d_958646668_d_593843561=="Yes" & d_958646668_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                           d_650516960=="Clinical", "Rule 72-HEP2", " "),
        
        Rule72_STRECK = ifelse(d_505347689_d_593843561=="Yes" & d_505347689_d_762124027=="No" &
                                           (d_173836415_d_266600170_d_316824786!="Yes" | is.na(d_173836415_d_266600170_d_740582332)) &
                                           d_650516960=="Clinical", "Rule 72-STRECK", " "),
        
        Rule72_URINE = ifelse(d_973670172_d_593843561=="Yes" & d_973670172_d_762124027=="No" & 
                                                    (d_173836415_d_266600170_d_210921343!="Yes" | is.na(d_173836415_d_266600170_d_541311218)) &
                                                    d_650516960=="Clinical", "Rule 72-URINE", " ")) 

 



####################### Poly Accession ID Rules


## 31. BioClin_DBBloodID_v1r0 must be in BioClin_PolyBloodID_v1r0 if and BioClin_DBBloodRRLDt_v1r0 occured more than 7 days ago


qc_7 <- bioqc %>%  filter(d_167958071==353358909 & d_650516960==664882224 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_541311218, units="days"), digits=0)) > 7  & 
                            is.na(d_173836415_d_266600170_d_786930107))

qc_8 <- bioqc %>%  filter(d_878865966==353358909 & d_650516960==664882224 & as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0)) > 7  & 
                            is.na(d_173836415_d_266600170_d_693370086)) %>%  select(Site, Connect_ID, d_173836415_d_266600170_d_693370086) %>%  safe_arrange(Site)
         
         
bioqc1 <- bioqc %>%  
  filter(as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_398645039, units="days"), digits=0)) > 7 &
                           !is.na(d_646899796_integer) & 
                           (Site=="Henry Ford Health System" | Site=="University of Chicago Medicine") & #only HP and UC sends this
                           !(Connect_ID %in% c( 
                             # HFH exclusions
                             2824989966,3759939890,4688823720,8589481620, 6336258662,5461650381,4900116130,4462616750, 9111871102,8476952861,2974054638,3544623873,
                           3293040395,7571439842,3165368475,6954174713, 6361337944,1629086681,3544623873,3293040395, 7571439842,3165368475,6954174713,6361337944,
                           1629086681,9768745029,3770024701,8836251245, 3312148528 ,
                           # UC exclusions
                           4782195164,6229346420,8336144924,7515171846 ,9963945282,9554775290,8570090186,4727512872, 5506703775,8123989364 ,1271036774,6288600449,
                           #SF exclusions ##this last one is somehow being shown as an error when it is not. Cannot find a workaround
                           9843228847, 7152112646)) &  
                           # Don't count the empty bracket people if they're in rule #8, because those would be expected
                           !(Connect_ID %in% qc_8$`Connect ID`))

#remove any brackets or blank spaces around polyID
str1 <- bioqc1$d_173836415_d_266600170_d_543608829
str1 <- str_trim(str1)
PolyBloodID <- unlist(strsplit(gsub("\\[|\\]", "", str1), ",")) 
bioqc1$d_646899796_integer <- str_trim(bioqc1$d_646899796_integer)

#Sometimes this poly accession comes in as an array, sometimes it doesn't, so just selecting the integer version of the variable
bioqc1 <- bioqc1 %>% mutate(BioClin_DBBloodID_v1r0=ifelse(Site=="Henry Ford Health System", paste0("L", d_646899796_integer), d_646899796_integer))

polyblood <- bioqc1 %>%  filter(!(bioqc1$BioClin_DBBloodID_v1r0 %in% PolyBloodID)) %>%  select(Site, Connect_ID, BioClin_DBBloodID_v1r0, d_173836415_d_266600170_d_543608829) %>% safe_arrange(Site)





## 32. BioClin_DBUrineID_v1r0 must be in BioClin_PolyUrineID_v1r0 if BioClin_DBUrineRRLDt_v1r0 occurred more than seven days ago.



bioqc2 <- bioqc %>%  filter( as.numeric(round(difftime(currentDate, d_173836415_d_266600170_d_541311218, units="days"), digits=0)) > 7 &
                               (Site=="Henry Ford Health System" | Site=="University of Chicago Medicine") & #only HP and UC sends this
                               !is.na(d_928693120_integer) & #d_173836415_d_266600170_d_110349197!="[]" & 
                               !(Connect_ID %in% c(1629086681, 3165368475, 3293040395, 3544623873, 6361337944, 6954174713, 7571439842,
                                                   3770024701, 8836251245, 9768745029, 5461650381, 4900116130, 4435276749, 6288600449,
                                                   6473532641, 6336258662, 8589481620, 4688823720, 3759939890, 2824989966)) &  #HFH exclusions
                               !(Connect_ID %in% qc_7$`Connect ID`)) # Don't count the empty bracket people if they're in rule #7, because those would be expected

#remove any brackets around polyID
str2 <- bioqc2$d_173836415_d_266600170_d_110349197
str2 <- str_trim(str2)
PolyUrineID <- unlist(strsplit(gsub("\\[|\\]", "", str2), ",")) 
bioqc2$d_928693120_integer <- str_trim(bioqc2$d_928693120_integer) 


#Sometimes this poly accession comes in as an array, sometimes it doesn't, so just selecting the integer version of the variable
bioqc2 <- bioqc2 %>%   mutate(BioClin_DBUrineID_v1r0=ifelse(Site=="Henry Ford Health System", paste0("L", d_928693120_integer),d_928693120_integer))

polyurine <- bioqc2 %>%  filter(!(bioqc2$BioClin_DBUrineID_v1r0 %in% PolyUrineID)) %>%  select(Site, Connect_ID, BioClin_DBUrineID_v1r0, d_173836415_d_266600170_d_110349197) %>% safe_arrange(Site)







  

bioqc_csv$Rule31 = ifelse(bioqc_csv$Connect_ID %in% polyblood$Connect_ID, "Rule 31", " ")
bioqc_csv$Rule32 = ifelse(bioqc_csv$Connect_ID %in% polyurine$Connect_ID, "Rule 32", " ")









########################## Home Collection Rules


MW <- "SELECT  Connect_ID, d_143615646_d_593843561, d_143615646_d_825582494, d_820476880, d_650516960, d_556788178
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`  where Connect_ID IS NOT NULL"
MW_table <- bq_project_query(project, MW)
MW_data <- bq_table_download(MW_table, bigint = "integer64", n_max = Inf, page_size = 1000)



## Need to remove blood/urine duplicates with the same logic as in the beginning of the code
## Keep the HMW collections and only the first blood/urine collection to properly remove duplicates
# 1. Keep all CHA rows (HMW)
cha_rows <- MW_data %>%
  filter(grepl("^CHA", d_820476880))

# 2. Among CXA rows, keep earliest finalized collection timestamp per Connect_ID-- this is what we did with the original biospecimen table duplicates
cxa_rows <- MW_data %>%
  filter(grepl("^CXA", d_820476880)) %>%
  group_by(Connect_ID) %>%
  slice_min(order_by = d_556788178, with_ties = FALSE) %>%
  ungroup()

# 3. Combine the two sets
MW_data_clean <- bind_rows(cha_rows, cxa_rows)





kit <- "SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.kitAssembly` where Connect_ID is not null"
kit_table <- bq_project_query(project, kit)
kit_data <- bq_table_download(kit_table, bigint = "integer64", n_max = Inf, page_size = 1000)

MW_data$Connect_ID <- as.numeric(MW_data$Connect_ID)
kit_data$Connect_ID <- as.numeric(kit_data$Connect_ID)




HMW= left_join(parts_data, kit_data, by="Connect_ID" ) 


HMW <- HMW %>%  mutate(Site=case_when(d_827220437==531629870 ~ "HealthPartners",
                                      d_827220437 == 548392715 ~ "Henry Ford Health System",
                                      d_827220437 == 303349821 ~ "Marshfield Clinical Health System",
                                      d_827220437 == 657167265 ~ "Sanford Health",
                                      d_827220437 == 809703864 ~ "University of Chicago Medicine",
                                      d_827220437 == 125001209 ~ "Kaiser Permanente Colorado",
                                      d_827220437 == 327912200 ~ "Kaiser Permanente Georgia",
                                      d_827220437 == 300267574 ~ "Kaiser Permanente Hawaii",
                                      d_827220437 == 452412599 ~ "Kaiser Permanente Northwest",
                                      d_827220437 == 472940358 ~ "Baylor Scott and White Health"))

Bio_HMW_merged = left_join(HMW, MW_data,  by="Connect_ID") 




# when participants get replacement kits, then get an additional row in the kit_assembly table
# those with initial kits will have a null d_426588510 value if kit sent before implementation, or d_426588510==663273321 if its more recent
# those with replacement kit 1 have one row with a null d_426588510 value, and one row with d_426588510==389478821
# those with replacement kit 2 have one row with a null d_426588510 value, and one row with d_426588510==389478821 AND one row with d_426588510 == 772116457
Bio_HMW_merged <- Bio_HMW_merged %>% filter(d_650516960=='664882224') %>%  #remove research collections 
  mutate(kit_level= case_when(d_426588510 == 772116457 ~ "Replacement Kit 2",
                              d_426588510 == 389478821 ~ "Replacement Kit 1",
                              TRUE ~ "Initial Kit"))

## Somehow the NAs are being separated
Bio_HMW_merged$kit_level[is.na(Bio_HMW_merged$kit_level)]= "Initial Kit"


#The merge isn't fully correct up until this point. 
#For participants with more then one home collection received, we need to keep the row where the first 9 characters of the Supply Kit ID = the Collection ID
## Supply Kit ID always has the Collection ID in it. Any other combos were incorrect and created falsely during the merge

# Step 1: Identify Connect_IDs with >1 'CHA'-starting rows
cha_ids <- Bio_HMW_merged %>%
  filter(grepl("^CHA", d_820476880)) %>%
  group_by(Connect_ID) %>%
  filter(n() > 1) %>%
  pull(Connect_ID) %>%
  unique()

# Step 2: Split data into two parts
processed <- Bio_HMW_merged %>%
  filter(Connect_ID %in% cha_ids) %>%
  mutate(cha_flag = grepl("^CHA", d_820476880)) %>%
  group_by(Connect_ID) %>%
  # Keep all non-CHA rows, and the matching CHA row if available
  filter(
    !cha_flag |
      d_820476880 == substr(d_194252513, 1, 9)
  ) %>%
  ungroup() %>%
  select(-cha_flag)

# Step 3: Add back all other data (those not in cha_ids)
Bio_HMW <- bind_rows(
  processed,
  Bio_HMW_merged %>% filter(!Connect_ID %in% cha_ids)
)

############################




# 34. If kit type is Mouthwash, then return kit tracking number, supply kit ID, return kit ID, collection cup ID, and collection card ID should populate.
all_populate <- HMW %>%  filter(((d_173836415_d_266600170_d_319972665_d_379252329==976461859 & 
                                    (d_173836415_d_266600170_d_319972665_d_221592017==241974920 | 
                                       d_173836415_d_266600170_d_319972665_d_221592017==277438316 |
                                       d_173836415_d_266600170_d_319972665_d_221592017==375535639)) | 
                                   (d_173836415_d_266600170_d_541483796_d_379252329==976461859 & 
                                      (d_173836415_d_266600170_d_541483796_d_221592017==241974920 | 
                                         d_173836415_d_266600170_d_541483796_d_221592017==277438316 |
                                         d_173836415_d_266600170_d_541483796_d_221592017==375535639)) | 
                                   (d_173836415_d_266600170_d_641006239_d_379252329==976461859 & 
                                      (d_173836415_d_266600170_d_641006239_d_221592017==241974920 | 
                                         d_173836415_d_266600170_d_641006239_d_221592017==277438316 |
                                         d_173836415_d_266600170_d_641006239_d_221592017==375535639))) &
                                  (is.na(d_972453354) | is.na(d_690210658) | is.na(d_194252513) | is.na(d_259846815) | is.na(d_786397882) ))  





# 35. If kit type is Mouthwash, then supply kit ID should be equal to return kit ID 
hmw1 <- HMW %>%  filter((d_173836415_d_266600170_d_319972665_d_379252329==976461859 | d_173836415_d_266600170_d_541483796_d_379252329==976461859 | 
                           d_173836415_d_266600170_d_641006239_d_379252329==976461859) & d_690210658!=d_194252513 & 
                          Connect_ID!=4765411890)



# 36.  If kit type is Mouthwash, then collection cup ID should be equal to collection card ID
hmw2 <- HMW %>%  filter((d_173836415_d_266600170_d_319972665_d_379252329==976461859 | d_173836415_d_266600170_d_541483796_d_379252329==976461859 | 
                           d_173836415_d_266600170_d_641006239_d_379252329==976461859) & d_259846815!=d_786397882) 




# 37. If kit status is Assigned, then supply kit tracking number should populate
asgn <- HMW %>%  filter((d_173836415_d_266600170_d_319972665_d_221592017==241974920 | d_173836415_d_266600170_d_541483796_d_221592017==241974920 | 
                           d_173836415_d_266600170_d_641006239_d_221592017==241974920) & 
                          (is.na(d_531858099) | is.na(d_426588510))) 




# 38. If kit status is Shipped, then supply kit tracking number, date/time kit shipped, and kit level should populate
# BioKit_SupplyKitTrack_v1r0  is all null here, but 39 rows in prod
shipt <- HMW %>%  filter((d_173836415_d_266600170_d_319972665_d_221592017==277438316 & (is.na(d_531858099) | is.na(d_173836415_d_266600170_d_319972665_d_661940160)  | is.na(d_426588510))) |
                           (d_173836415_d_266600170_d_541483796_d_221592017==277438316 & (is.na(d_531858099) | is.na(d_173836415_d_266600170_d_541483796_d_661940160)  | is.na(d_426588510))) |
                           (d_173836415_d_266600170_d_641006239_d_221592017==277438316 & (is.na(d_531858099) | is.na(d_173836415_d_266600170_d_641006239_d_661940160)  | is.na(d_426588510)))) 



# 39. If kit status is Received, then supply kit tracking number, date/time kit shipped, date/time kit received, and and kit level should populate
recvd <-Bio_HMW %>%  filter((d_173836415_d_266600170_d_319972665_d_221592017==375535639 & kit_level=="Initial Kit" & 
                               (is.na(d_531858099) | is.na(d_661940160) | is.na(d_826941471) |  is.na(d_137401245) | is.na(d_426588510))) |
                              (d_173836415_d_266600170_d_541483796_d_221592017==375535639 & kit_level=="Replacement Kit 1" & 
                                 (is.na(d_531858099) | is.na(d_661940160) | is.na(d_826941471) |  is.na(d_137401245) | is.na(d_426588510))) | 
                              (d_173836415_d_266600170_d_641006239_d_221592017==375535639 & kit_level=="Replacement Kit 2" & 
                                 (is.na(d_531858099) | is.na(d_661940160) | is.na(d_826941471) |  is.na(d_137401245) | is.na(d_426588510))))





# 40. If kit status is Received, then the mouthwash sample is collected.
collctd <- Bio_HMW %>%  filter(str_sub(d_820476880,1,3)=="CHA" & (d_173836415_d_266600170_d_319972665_d_221592017==375535639 | 
                                                                    d_173836415_d_266600170_d_541483796_d_221592017==375535639 |
                                                                    d_173836415_d_266600170_d_641006239_d_221592017==375535639) &
                                 (is.na(d_143615646_d_593843561) | d_143615646_d_593843561=="No"))




# 41. If kit status is Received, then the mouthwash sample tube ID should populate.
mwtubeid <- Bio_HMW %>%  filter(str_sub(d_820476880,1,3)=="CHA" & (d_173836415_d_266600170_d_319972665_d_221592017==375535639 | 
                                                                     d_173836415_d_266600170_d_541483796_d_221592017==375535639 |
                                                                     d_173836415_d_266600170_d_641006239_d_221592017==375535639) & is.na(d_143615646_d_825582494))




# 42. If kit status (from the participants table) is Received, and kit type is Mouthwash, then the populated mouthwash sample tube ID (full specimen ID) should equal the populated collection cup ID.
hmw3 <- Bio_HMW %>%  filter(str_sub(d_820476880,1,3)=="CHA" & 
                              ((d_173836415_d_266600170_d_319972665_d_221592017==375535639 & d_173836415_d_266600170_d_319972665_d_379252329==976461859) | 
                                 (d_173836415_d_266600170_d_541483796_d_221592017==375535639 & d_173836415_d_266600170_d_541483796_d_379252329==976461859) | 
                                 (d_173836415_d_266600170_d_641006239_d_221592017==375535639 & d_173836415_d_266600170_d_641006239_d_379252329==976461859)) & 
                              d_143615646_d_825582494!=d_259846815) 


# 43. If kit status is Received, and the kit type is Mouthwash, then the mouthwash collection setting is Home.
homsetting <- HMW %>%  filter(((d_173836415_d_266600170_d_319972665_d_221592017==375535639 & d_173836415_d_266600170_d_319972665_d_379252329==976461859) | 
                                 (d_173836415_d_266600170_d_541483796_d_221592017==375535639 & d_173836415_d_266600170_d_541483796_d_379252329==976461859) | 
                                 (d_173836415_d_266600170_d_641006239_d_221592017==375535639 & d_173836415_d_266600170_d_641006239_d_379252329==976461859)) & 
                                d_173836415_d_266600170_d_915179629!=103209024)





# 44. If the mouthwash collection setting is Home, and the mouthwash sample is collected, then baseline mouthwash is collected.
bl_mw <-  Bio_HMW %>%  filter(str_sub(d_820476880,1,3)=="CHA" & d_173836415_d_266600170_d_915179629==103209024 & d_143615646_d_593843561=="Yes" & 
                                (d_684635302=="No" | is.na(d_684635302)) )






# 45. If the mouthwash collection setting is Home, and the mouthwash sample is collected, then the mouthwash date/time collected should populate.
mw_dt <-  Bio_HMW %>%  filter(str_sub(d_820476880,1,3)=="CHA" & d_173836415_d_266600170_d_915179629==103209024 & d_143615646_d_593843561=="Yes" & 
                                is.na(d_173836415_d_266600170_d_448660695))





# 46. If BioSpm_MWSettingBL_v1r0 is Home and SrvMtW_TmComplete_v1r0 is more than 10 days ago, BioKit_KitStatusBL_v1r0 should be Received
kits_recvd <- Bio_HMW %>%
  filter(as.numeric(round(difftime(currentDate, d_195145666, units = "days"), digits = 0)) > 10 & 
         ((kit_level == "Replacement Kit 2" & d_173836415_d_266600170_d_641006239_d_221592017 != "375535639") |
            (kit_level == "Replacement Kit 1" & d_173836415_d_266600170_d_541483796_d_221592017 != "375535639") |
            (kit_level == "Initial Kit" & d_173836415_d_266600170_d_319972665_d_221592017 != "375535639")))


#47.BioFin_BMTimeBL_v1r0 must be in the structure of YYYY-MM-DDTHH:MM:SS or YYYY-MM-DDTHH:MM:SS.SSSZ
datetime_regex <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$|^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z$"

invalid_rows <- HMW %>% filter(!is.na(d_173836415_d_266600170_d_448660695) & !grepl(datetime_regex, d_173836415_d_266600170_d_448660695))


#66. If home mouthwash kit is Replacement 1 or Replacement 2, BioKit_KitStatus should not be Undeliverable Address.
undeliverables <- HMW %>%  filter(d_173836415_d_266600170_d_641006239_d_221592017=="332067457" & (d_426588510=="389478821" | d_426588510=="772116457"))

###	69. BioFin_BMTimeBL_v1r0 must occur after BioKit_KitShipTm_v1r0 or before BioKit_KitRecdTm_v1r0.
  ### Excludes those where BioFin_BMTimeBL_v1r0 is in the same month OR the month on either side of BioKit_KitShipTm_v1r0 or BioKit_KitRecdTm_v1r0
issue212 <- HMW %>%  filter( ((as.Date(d_173836415_d_266600170_d_448660695) <  d_173836415_d_266600170_d_319972665_d_661940160) |
                                (as.Date(d_173836415_d_266600170_d_448660695) > d_826941471))  & 
                               #If BioFin_BMTimeBL_v1r0 is in the same month OR the month on either side of BioKit_KitShipTm_v1r0 or BioKit_KitRecdTm_v1r0 then ignore the error.
                               (abs(as.numeric(str_sub(d_173836415_d_266600170_d_448660695, 6, 7)) - as.numeric(str_sub(d_173836415_d_266600170_d_319972665_d_661940160, 6, 7)))>1 |
                                  abs(as.numeric(str_sub(d_173836415_d_266600170_d_448660695, 6, 7)) - as.numeric(str_sub(d_826941471, 6, 7)))>1 ) &
                               #Exclude those with December(12)/January(01) dates, won't be caught 
                               !( (as.numeric(str_sub(d_173836415_d_266600170_d_448660695, 6, 7))==12 & as.numeric(str_sub(d_173836415_d_266600170_d_319972665_d_661940160, 6, 7))==1) | 
                                    (as.numeric(str_sub(d_173836415_d_266600170_d_448660695, 6, 7))==1 & as.numeric(str_sub(d_173836415_d_266600170_d_319972665_d_661940160, 6, 7))==12) | 
                                    (as.numeric(str_sub(d_173836415_d_266600170_d_448660695, 6, 7))==12 & as.numeric(str_sub(d_826941471, 6, 7))==1) | 
                                    (as.numeric(str_sub(d_173836415_d_266600170_d_448660695, 6, 7))==1 & as.numeric(str_sub(d_826941471, 6, 7))==12)) & 
                               Connect_ID!="2356168653")


#53. Connect ID and token in Participants Table must match Connect ID and token in Biospecimens table
tokens <- "SELECT 
CASE 
  WHEN b.d_827220437 = '472940358' THEN 'Baylor Scott and White Health'
  WHEN b.d_827220437 = '531629870' THEN 'HealthPartners'
  WHEN b.d_827220437 = '548392715' THEN 'Henry Ford Health System'
  WHEN b.d_827220437 = '303349821' THEN 'Marshfield Clinical Health System'
  WHEN b.d_827220437 = '657167265' THEN 'Sanford Health'
  WHEN b.d_827220437 = '809703864' THEN 'University of Chicago Medicine'
  WHEN b.d_827220437 = '125001209' THEN 'Kaiser Permanente Colorado'
  WHEN b.d_827220437 = '327912200' THEN 'Kaiser Permanente Georgia'
  WHEN b.d_827220437 = '300267574' THEN 'Kaiser Permanente Hawaii'
  WHEN b.d_827220437 = '452412599' THEN 'Kaiser Permanente Northwest'
END AS Site,
b.Connect_ID, d_820476880, 
b.token as bio_token, 
p.token as parts_token
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen` b
left join `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` p
on b.Connect_ID=p.Connect_ID
where b.token!=p.token
group by Site, b.Connect_ID, d_820476880, b.token, p.token
order by b.Connect_ID asc"

token_table <- bq_project_query(project, tokens)
token_match <- bq_table_download(token_table, bigint = "integer64")

colnames(token_match) <- c("Site", "Connect ID", "Collection ID", "Bio Table Token", "Parts. Table Token")




bioqc_csv$Rule34 = ifelse(bioqc_csv$Connect_ID %in% all_populate$Connect_ID, "Rule 34", " ")
bioqc_csv$Rule35 = ifelse(bioqc_csv$Connect_ID %in% hmw1$Connect_ID, "Rule 35", " ")
bioqc_csv$Rule36 = ifelse(bioqc_csv$Connect_ID %in% hmw2$Connect_ID, "Rule 36", " ")
bioqc_csv$Rule37 = ifelse(bioqc_csv$Connect_ID %in% asgn$Connect_ID, "Rule 37", " ")
bioqc_csv$Rule38 = ifelse(bioqc_csv$Connect_ID %in% shipt$Connect_ID, "Rule 38", " ")
bioqc_csv$Rule39 = ifelse(bioqc_csv$Connect_ID %in% recvd$Connect_ID, "Rule 39", " ")
bioqc_csv$Rule40 = ifelse(bioqc_csv$Connect_ID %in% collctd$Connect_ID, "Rule 40", " ")
bioqc_csv$Rule41 = ifelse(bioqc_csv$Connect_ID %in% mwtubeid$Connect_ID, "Rule 41", " ")
bioqc_csv$Rule42 = ifelse(bioqc_csv$Connect_ID %in% hmw3$Connect_ID, "Rule 42", " ")
bioqc_csv$Rule43 = ifelse(bioqc_csv$Connect_ID %in% homsetting$Connect_ID, "Rule 43", " ")
bioqc_csv$Rule44 = ifelse(bioqc_csv$Connect_ID %in% bl_mw$Connect_ID, "Rule 44", " ")
bioqc_csv$Rule45 = ifelse(bioqc_csv$Connect_ID %in% mw_dt$Connect_ID, "Rule 45", " ")
bioqc_csv$Rule46 = ifelse(bioqc_csv$Connect_ID %in% kits_recvd$Connect_ID, "Rule 46", " ")
bioqc_csv$Rule47 = ifelse(bioqc_csv$Connect_ID %in% invalid_rows$Connect_ID, "Rule 47", " ")
bioqc_csv$Rule53 = ifelse(bioqc_csv$Connect_ID %in% token_match$Connect_ID, "Rule 53", " ")
bioqc_csv$Rule66 = ifelse(bioqc_csv$Connect_ID %in% undeliverables$Connect_ID, "Rule 66", " ")
bioqc_csv$Rule69 = ifelse(bioqc_csv$Connect_ID %in% issue212$Connect_ID, "Rule 69", " ")




###################### Mismatched Blood/Urine Accession IDs and Duplicates XLXS--------------- separate from rules 




bioqc <- bioqc %>%  mutate(Site=case_when(d_827220437==531629870 ~ "HealthPartners",
                                          d_827220437==548392715 ~ "Henry Ford Health System",
                                          d_827220437==303349821 ~ "Marshfield Clinical Health System",
                                          d_827220437==657167265 ~ "Sanford Health",
                                          d_827220437==809703864 ~ "University of Chicago Medicine",
                                          d_827220437==125001209 ~ "Kaiser Permanente Colorado",
                                          d_827220437==327912200 ~ "Kaiser Permanente Georgia",
                                          d_827220437==300267574 ~ "Kaiser Permanente Hawaii",
                                          d_827220437==452412599 ~ "Kaiser Permanente Northwest"))









## Mismatched Blood CSV

qc1_blood <- bioqc %>%  
  filter(str_sub(d_173836415_d_266600170_d_341570479, 1, 11) != str_sub(d_646899796_integer, 1, 11)) %>%
  select(Site, Connect_ID, d_173836415_d_266600170_d_341570479, d_646899796_integer, d_556788178) %>%
  arrange(Site)

qc1_blood$d_556788178 <- as.POSIXct(ymd_hms(qc1_blood$d_556788178))

colnames(qc1_blood) <- c("Site", "Connect ID", "Accession ID sent from site", "Accession ID in Dashboard", "Collection Date/Time")

qc1_blood <- qc1_blood %>% arrange(Site, `Collection Date/Time`)

openxlsx::write.xlsx(qc1_blood,glue("{local_drive}Mismatched_Blood_Accession_IDs_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")




## Mismatches Urine CSV
qc1_urine <- bioqc %>%  
  filter(str_sub(d_173836415_d_266600170_d_198261154, 1, 11) != str_sub(d_928693120_integer, 1, 11)) %>%
  select(Site, Connect_ID, d_173836415_d_266600170_d_198261154, d_928693120_integer, d_556788178) %>%
  arrange(Site)

qc1_urine$d_556788178 <- as.POSIXct(ymd_hms(qc1_urine$d_556788178))

colnames(qc1_urine) <- c("Site", "Connect ID", "Accession ID sent from site", "Accession ID in Dashboard", "Collection Date/Time")

qc1_urine <- qc1_urine %>% arrange(Site, `Collection Date/Time`)

openxlsx::write.xlsx(qc1_urine,glue("{local_drive}Mismatched_Urine_Accession_IDs_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")









## Duplicates 

dup <- "WITH T AS (
  SELECT Connect_ID 
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`
  WHERE d_820476880 LIKE 'CXA%' 
  AND d_820476880 NOT LIKE 'CHA%'
  GROUP BY Connect_ID
  HAVING COUNT(Connect_ID) > 1
) 
SELECT * 
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`
WHERE Connect_ID IN (SELECT Connect_ID FROM T) and d_820476880 NOT LIKE 'CHA%'"


dup_table <- bq_project_query(project, dup)


dup_data <- bq_table_download(dup_table, bigint = "integer64", n_max = Inf, page_size = 1000)


dup_data$Connect_ID <- as.numeric(dup_data$Connect_ID)


bio_dup= left_join(dup_data, parts_data, by=c("Connect_ID", "d_827220437") )

bio_dup <- bio_dup %>%  mutate(Site=case_when(d_827220437==531629870 ~ "HP",
                                              d_827220437==548392715 ~ "HF",
                                              d_827220437==303349821 ~ "Marshfield",
                                              d_827220437==657167265 ~ "Sanford",
                                              d_827220437==809703864 ~ "UChicago",
                                              d_827220437==125001209 ~ "KPCO",
                                              d_827220437==327912200 ~ "KPGA",
                                              d_827220437==300267574 ~ "KPHI",
                                              d_827220437==452412599 ~ "KPNW"),
                               Finalized= case_when(d_410912345==104430631 ~ "No",
                                                    d_410912345==353358909 ~ "Yes",
                                                    TRUE ~ "Null"),
                               Setting = case_when(d_650516960== "664882224" ~ "Clinical",
                                                   d_650516960== "534621077" ~ "Research",
                                                   d_650516960== "103209024" ~ "Home",
                                                   is.na(d_650516960) ~ "Null"),
                               SST1_Tube_Collected= case_when(d_299553921_d_593843561==353358909 ~ "Yes",
                                                              TRUE ~ "No"),
                               SST2_Tube_Collected= case_when(d_703954371_d_593843561==353358909 ~ "Yes",
                                                              TRUE ~ "No"),
                               SST3_Tube_Collected= case_when(d_376960806_d_593843561==353358909 ~ "Yes",
                                                              TRUE ~ "No"),
                               SST4_Tube_Collected= case_when(d_232343615_d_593843561==353358909 ~ "Yes",
                                                              TRUE ~ "No"),
                               SST5_Tube_Collected= case_when(d_589588440_d_593843561==353358909 ~ "Yes",
                                                              TRUE ~ "No"),
                               HeparinT1_Tube_Collected= case_when(d_838567176_d_593843561==353358909 ~ "Yes",
                                                                   TRUE ~ "No"),
                               HeparinT2_Tube_Collected= case_when(d_958646668_d_593843561==353358909 ~ "Yes",
                                                                   TRUE ~ "No"),
                               EDTAT1_Tube_Collected= case_when(d_454453939_d_593843561==353358909 ~ "Yes",
                                                                TRUE ~ "No"),
                               EDTAT2_Tube_Collected= case_when(d_677469051_d_593843561==353358909 ~ "Yes",
                                                                TRUE ~ "No"),
                               EDTAT3_Tube_Collected= case_when(d_683613884_d_593843561==353358909 ~ "Yes",
                                                                TRUE ~ "No"),
                               ACD_Tube_Collected= case_when(d_652357376_d_593843561==353358909 ~ "Yes",
                                                             TRUE ~ "No"),
                               STRECK_Tube_Collected= case_when(d_505347689_d_593843561==353358909 ~ "Yes",
                                                                TRUE ~ "No"),
                               Urine_Tube_Collected= case_when(d_973670172_d_593843561==353358909 ~ "Yes",
                                                               TRUE ~ "No"),
                               MW_Tube_Collected= case_when(d_143615646_d_593843561==353358909 ~ "Yes",
                                                            TRUE ~ "No"))


finalized_counts <- bio_dup %>%
  group_by(Connect_ID) %>%
  summarize(Finalized_Count = sum(Finalized == "Yes"))

finalized_counts <- finalized_counts %>%
  mutate(Finalized_Status = ifelse(Finalized_Count > 1, "Yes", "No"))

bio_dup_with_status <- bio_dup %>%
  left_join(finalized_counts %>% select(Connect_ID, Finalized_Status), by = "Connect_ID")


dup_list <- bio_dup_with_status %>%
  select(Connect_ID, Site, Setting, d_820476880, d_556788178, Finalized, Finalized_Status,
         SST1_Tube_Collected, SST2_Tube_Collected, SST3_Tube_Collected, SST4_Tube_Collected,
         SST5_Tube_Collected, HeparinT1_Tube_Collected, HeparinT2_Tube_Collected, EDTAT1_Tube_Collected,
         EDTAT2_Tube_Collected, EDTAT3_Tube_Collected, ACD_Tube_Collected, STRECK_Tube_Collected,
         Urine_Tube_Collected, MW_Tube_Collected) %>%
  arrange(Connect_ID)

dup_list_by_tube <- dup_list %>%  
  group_by(Connect_ID) %>%
  filter(any(across(ends_with("_Tube_Collected"), ~ all(. == "Yes", na.rm = TRUE)))) %>%
  ungroup()

colnames(dup_list_by_tube) <- c("Connect ID", "Site", "Collection Setting", "Collection ID", "Collection Date/Time", "Collection Finalized", "True Duplicate",
                        "SST1_Tube_Collected", "SST2_Tube_Collected", "SST3_Tube_Collected", "SST4_Tube_Collected",
                        "SST5_Tube_Collected", "HeparinT1_Tube_Collected", "HeparinT2_Tube_Collected", "EDTAT1_Tube_Collected",
                        "EDTAT2_Tube_Collected", "EDTAT3_Tube_Collected", "ACD_Tube_Collected", "STRECK_Tube_Collected","Urine_Tube_Collected", "MW_Tube_Collected")




openxlsx::write.xlsx(dup_list_by_tube,glue("{local_drive}Duplicate_Baseline_Biospecimen_Collections_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")



###############################################################################################################################################################################





###############################     RULES - Duplicates 



# 48. If BioSpm_Visit_v1r0 is Baseline and the collection is finalized, then BioSpm_Setting_v1r0 for all collections for the participant should either all be Research or Clinical. This only applies to participants with duplicate baseline collections.

# Identify Connect_IDs with different d_650516960 values
different_values <- bio_dup_with_status %>%
  filter(d_410912345=="353358909" &
           !(Connect_ID %in% c(2589415525, 4545815202, 4545815202, 2589415525, 9055246833, 4987045154))) %>% 
  group_by(Connect_ID) %>% 
  summarize(unique_values = n_distinct(d_650516960), .groups = "drop") %>%
  filter(unique_values > 1) %>%
  pull(Connect_ID)


split_setting <- bio_dup_with_status %>% mutate(Site=case_when(d_827220437==472940358 ~ "Baylor Scott and White Health",
                                                               d_827220437==531629870 ~ "HealthPartners",
                                                               d_827220437==548392715 ~ "Henry Ford Health System",
                                                               d_827220437==303349821 ~ "Marshfield Clinical Health System",
                                                               d_827220437==657167265 ~ "Sanford Health",
                                                               d_827220437==809703864 ~ "University of Chicago Medicine",
                                                               d_827220437==125001209 ~ "Kaiser Permanente Colorado",
                                                               d_827220437==327912200 ~ "Kaiser Permanente Georgia",
                                                               d_827220437==300267574 ~ "Kaiser Permanente Hawaii",
                                                               d_827220437==452412599 ~ "Kaiser Permanente Northwest")) %>% 
  filter(Connect_ID %in% different_values)

split_setting$Collection_ID = split_setting$d_820476880

split_setting <- split_setting %>%  select(Connect_ID, Collection_ID)






###	67. If kit is an Initial kit, BioKit_DtKitReq_v1r0 should be null.
project_id <- "nih-nci-dceg-connect-prod-6d04"
dataset <- "FlatConnect"
table <- "participants"
column_to_check <- "d_173836415_d_266600170_d_319972665_d_759651991"

# Step 1: Check if the column exists
check_query <- glue::glue("
  SELECT column_name
  FROM `{project_id}.{dataset}.INFORMATION_SCHEMA.COLUMNS`
  WHERE table_name = '{table}'
    AND column_name = '{column_to_check}'
")

check_result <- bq_project_query(project_id, check_query)
column_check <- bq_table_download(check_result)

# Step 2: If it exists, run the query
if (nrow(column_check) > 0) {
  warning(glue::glue("Column {column_to_check} EXISTS in {table}! Pulling data..."))
  
  # Safe query to pull Connect_ID and the column
  data_query <- glue::glue("
    SELECT Connect_ID, `{column_to_check}`
    FROM `{project_id}.{dataset}.{table}`
    WHERE Connect_ID IS NOT NULL
  ")
  
  data_result <- bq_project_query(project_id, data_query)
  output_data <- bq_table_download(data_result)
  
} else {
  cat("No errors.")
  output_data <- tibble()
}

bioqc_csv$Rule67 = ifelse(bioqc_csv$Connect_ID %in% output_data$Connect_ID, "Rule 67", " ")




###	68. No participant should have more then one initial kit, replacement 1 kit, or replacement kit 2. 

Dup_kits <- "SELECT Connect_ID, 
case d_426588510
when '772116457' then 'Replacement Kit 2'
when'389478821' then 'Replacement Kit 1'
else 'Initial Kit' 
end as Kit_Level
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.kitAssembly` 
where Connect_ID!='7030029736'
group by Connect_ID, d_426588510
having count(*) >1"

Dup_kits_table <- bq_project_query(project, Dup_kits)
Dup_kits_rule <- bq_table_download(Dup_kits_table, bigint = "integer64")

bioqc_csv$Rule68 = ifelse(bioqc_csv$Connect_ID %in% Dup_kits_rule$Connect_ID, "Rule 68", " ")




######################. These rules need to include those with no biospecimen collections ###########################

parts_data <- parts_data %>%  mutate(Site=case_when(d_827220437==472940358 ~ "Baylor Scott and White Health",
                                                    d_827220437==531629870 ~ "HealthPartners",
                                                    d_827220437==548392715 ~ "Henry Ford Health System",
                                                    d_827220437==303349821 ~ "Marshfield Clinical Health System",
                                                    d_827220437==657167265 ~ "Sanford Health",
                                                    d_827220437==809703864 ~ "University of Chicago Medicine",
                                                    d_827220437==125001209 ~ "Kaiser Permanente Colorado",
                                                    d_827220437==327912200 ~ "Kaiser Permanente Georgia",
                                                    d_827220437==300267574 ~ "Kaiser Permanente Hawaii",
                                                    d_827220437==452412599 ~ "Kaiser Permanente Northwest"))

### 29.b. If BioClin_ClinBloodTmBL_v1r0 is populated, then BioSpm_BloodSettingBL_v1r0 must be Clinical and BioFin_BaseBloodCol_v1r0 must be yes.
c_blood3.4 <- parts_data %>%  filter(!is.na(d_173836415_d_266600170_d_982213346) & 
                                       ((d_173836415_d_266600170_d_592099155=="Research" | is.na(d_173836415_d_266600170_d_592099155)) | d_878865966=="No") &
                                       !(Connect_ID %in%  c("7848933050", "5885436394", "9258958214", "2300063524", "1176687465", "1850586900","6575901705", "3467573584", "1274744512",
                                                            '3362078899',  now)))


### 30.b. If BioClin_ClinicalUrnTmBL_v1r0 is populated, then BioSpm_UrineSettingBL_v1r0 must be Clinical and BioFin_BaseUrineCol_v1r0 must be yes.
c_urine3.4 <- parts_data %>%  filter(!is.na(d_173836415_d_266600170_d_139245758) & 
                                       ((d_173836415_d_266600170_d_718172863=="Research" | is.na(d_173836415_d_266600170_d_718172863)) | d_167958071=="No") &
                                       !(Connect_ID %in% c('6862754687', '1371560328', '1176687465', '5885436394', '1850586900', '3319331872', '6575901705', '6862754687',
                                                           '9258958214', '1250934825', '7882825421', '3287102562', '1215003341', '7352978604', '1140047316', '4479873637',
                                                           '8633594373', '2755205973', '3862626013', '8184138489', '4980503471', '2039357566', '6321294709', '2300063524',
                                                           '5899565591', '8625295305', '9167792140', '9143436002', '7276829171', '8625295305', '1274744512', '9262617255',
                                                           '4838682809', '2887898061', '2871811858', '4207529981', '3456526723', '9366425281', '7106367407', '6523900663',
                                                           '3992444928', '6538980272', '6467484794', '1375421153', '1102086935', '9672292896', '9652100378', '3362078899',
                                                           '6437389686', '5972658064', '8553891957', '9575612025', '2769291903', '1731138933', '3589595480','1349953410', 
                                                           '3722445358', '9694753790', '1703960468', '2512896461', '8924667241','2431356225', '4057490101')))



####################### Only want the output to contain those with errors
bioqc_csv2 <-  bioqc_csv %>% filter(!if_all(starts_with("Rule"), ~ . == " " | is.na(.)))


bioqc_csv_3 <- full_join(bioqc_csv2, split_setting, by=c("Connect_ID", "Collection_ID"))
bioqc_csv__3 <- full_join(bioqc_csv_3, c_blood3.4, by=c("Connect_ID", "Site"))
bioqc_csv3 <- full_join(bioqc_csv__3, c_urine3.4, by=c("Connect_ID", "Site"))
bioqc_csv3$Rule48 = ifelse(bioqc_csv3$Connect_ID %in% split_setting$Connect_ID, "Rule 48", " ")
bioqc_csv3$Rule29b = ifelse(bioqc_csv3$Connect_ID %in% c_blood3.4$Connect_ID, "Rule 29.b", " ")
bioqc_csv3$Rule30b = ifelse(bioqc_csv3$Connect_ID %in% c_urine3.4$Connect_ID, "Rule 30.b", " ")



#################################     Final Output of QC report #####################################################


## Make sure rules are in numerical order
#Grab all rule rules, account for weird rule structure like "Rule18_SST2" or "Rule29a"
rule_cols <- grep("^Rule\\d+", colnames(bioqc_csv3), value = TRUE)

rule_numbers <- as.numeric(gsub("^Rule(\\d+).*", "\\1", rule_cols))

sorted_rule_cols <- rule_cols[order(rule_numbers)]

bioqc_csv3 <- bioqc_csv3[, c("Connect_ID", "Collection_ID", "Site", sorted_rule_cols)]






bioqc_xlxs <- bioqc_csv3 %>%  select(Connect_ID, Collection_ID, Site, starts_with("Rule")) %>% arrange(Connect_ID)
openxlsx::write.xlsx(bioqc_xlxs,glue("Biospeimen_CustomQC_Rules_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")


