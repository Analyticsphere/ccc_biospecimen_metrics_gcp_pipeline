
library(bigrquery)
library(data.table)
library(dplyr)
library(rio)
library(glue)
#install.packages("openxlsx")
library(openxlsx)
library(logger) ## allows us to add messages in logs to find out when/where code breaks are happening
bq_auth()
2

## Will be adding log_info comments throughout report at timely or ticky spots to find code breaks in the logs
log_info("Libraries Loaded")



boxfolder <- 221280601453
currentDate <- Sys.Date()


project <- "nih-nci-dceg-connect-prod-6d04"

################################################################################################################




###########################################     CSV1:    Boxes     #############################################

log_info("Starting Box table SQL query")


boxes_bq_pull <- "SELECT 
bagID, 
bagType, 
tubeID, 
d_132929440 as BioPack_BoxID_v1r0, 
d_555611076 as BioPack_ModifiedTime_v1r0, 
d_656548982 as BioShip_ShipTime_v1r0, 
d_672863981 as BioPack_BoxStrtTime_v1r0, 
d_870456401 as BioBPTL_ShipComments_v1r0, 
d_926457119 as BioBPTL_DateRec_v1r0, 
d_948887825 as BioShip_SignEmail_v1r0, 
d_959708259 as BioPack_TrackScan1_v1r0,   

case when d_666553960='712278213' then 'FedEx'
when d_666553960='149772928' then  'World Courier' 
end as BioPack_Courier_v1r0,  

CASE d_560975149
  WHEN '777644826' THEN 'UC-DCAM'
  WHEN '692275326' THEN 'Marshfield'
  WHEN '813701399' THEN 'Weston'
  WHEN '698283667' THEN 'Lake Hallie'
  WHEN '834825425' THEN 'HP Research Clinic'
  WHEN '589224449' THEN 'Sioux Falls Imagenetics'
  WHEN '763273112' THEN 'KPCO RRL'
  WHEN '531313956' THEN 'KPHI RRL'
  WHEN '715632875' THEN 'KPNW RRL'
  WHEN '767775934' THEN 'KPGA RRL'
  WHEN '752948709' THEN 'Henry Ford Main Campus'
  WHEN '570271641' THEN 'Henry Ford West Bloomfield Hospital'
  WHEN '838480167' THEN 'Henry Ford Medical Center- Fairlane'
  WHEN '706927479' THEN 'HFH Livonia Research Clinic'
  WHEN '145191545' THEN 'Ingalls Harvey'
  WHEN '489380324' THEN 'River East'
  WHEN '120264574' THEN 'South Loop'
  WHEN '691714762' THEN 'Rice Lake'
  WHEN '487512085' THEN 'Wisconsin Rapids'
  WHEN '983848564' THEN 'Colby Abbotsford'
  WHEN '261931804' THEN 'Minocqua'
  WHEN '665277300' THEN 'Merrill'
  WHEN '467088902' THEN 'Fargo South University'
  WHEN '940329442' THEN 'Orland Park'
  WHEN '574368418' THEN 'HP Park Nicollet'
  WHEN '322059622' THEN 'HFH Pop-Up'
  WHEN '127626388' THEN 'Bismarck Medical Center'
  WHEN '246137578' THEN 'Sioux Falls Sanford Center'
  WHEN '723351427' THEN 'BCC- HWC, BC'
  WHEN '807443231' THEN 'BCC- All Saints (FW)'
  WHEN '475614532' THEN 'BCC- Plano'
  WHEN '809370237' THEN 'BCC- Worth St'
  WHEN '856158129' THEN 'BCC- Irving'
  WHEN '436956777' THEN 'NTX Biorepository'
  WHEN '288564244' THEN 'BCC- Fort Worth'
  WHEN '433070901' THEN 'Sioux Falls Edith Center'
  WHEN '769594361' THEN 'Fargo Amber Valley'
  WHEN '246153539' THEN 'Bemidji Clinic'
  WHEN '255636184' THEN 'Stevens Point'
  WHEN '813412950' THEN 'Neillsville'
  WHEN '755034888' THEN 'HFH Jackson'
  WHEN '483909879' THEN 'North Garland'
  WHEN '962830330' THEN 'Waco - MacArthur'
  WHEN '911683679' THEN 'HFH Detroit Northwest'
  WHEN '397883980' THEN 'Irving'
  WHEN '117840593' THEN 'Temple CDM'
  WHEN '574104518' THEN 'Temple Roney'
  else 'Missing'
END as BioShip_LocalID_v1r0, 
CASE d_789843387
  WHEN '531629870' THEN 'HealthPartners'
  WHEN '548392715' THEN 'Henry Ford Health System'
  WHEN '125001209' THEN 'Kaiser Permanente Colorado'
  WHEN '327912200' THEN 'Kaiser Permanente Georgia'
  WHEN '300267574' THEN 'Kaiser Permanente Hawaii'
  WHEN '452412599' THEN 'Kaiser Permanente Northwest'
  WHEN '303349821' THEN 'Marshfield Clinic Health System'
  WHEN '657167265' THEN 'Sanford Health'
  WHEN '809703864' THEN 'University of Chicago Medicine'
  WHEN '517700004' THEN 'National Cancer Institute'
  WHEN '181769837' THEN 'Other'
END as BioShip_LogSite_v1r0, 

case when d_105891443='353358909' then 'Yes'
else 'No' end as BioPack_TempProbe_v1r0, 
case when d_145971562='353358909' then 'Yes'
else 'No' end as BioShip_ShipSubmit_v1r0, 
case when d_333524031='353358909' then 'Yes'
else 'No' end as  BioBPTL_ShipRec_v1r0, 
case when d_842312685='353358909' then 'Yes'
else 'No' end as  BioPack_ContainsOrphan_v1r0, 

 FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes` 
WHERE DATE(d_656548982) >= DATE_SUB(CURRENT_DATE(), INTERVAL 31 DAY)
order by DATE(d_656548982) asc, bagID ASC"

boxes_bq <- bq_project_query(project, query=boxes_bq_pull)  
box1 <- bq_table_download(boxes_bq,bigint="integer64",n_max = Inf)

openxlsx::write.xlsx(box1,as.character(glue("Formatted_prod_flatBoxes_{currentDate}_boxfolder_{boxfolder}.xlsx")),row.names = F,na="")

log_info("Boxes table finished")


## Clearing up space in GCP memory 
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project')))
gc()









############################    CSV2:  KitAssembly_Table    ####################################################

log_info("Kit Assembly SQL query")

kitA <- "SELECT 
Connect_ID, 
d_194252513 as BioKit_ReturnKitID_v1r0,
d_259846815 as BioKit_MWCupID_v1r0,
d_690210658 as BioKit_SupplyKitID_v1r0,
d_786397882 as BioKit_MWCardID_v1r0,
d_341636034 as BioKit_KitDatePend_v1r0,
cast(d_531858099 as INT) as BioKit_SupplyKitTrack_v1r0,
d_661940160 as BioKit_KitShipTmBL_v1r0,
d_687158491 as BioKit_KitAssembledIDBL_v1r0,
d_755095663 as BioKit_MWKitComments_v1r0,
d_826941471 as BioKit_KitRecdTmBL_v1r0,
--CAST(d_972453354 AS NUMERIC) as BioKit_ReturnKitTrack_v1r0,
--CAST(REGEXP_REPLACE(d_972453354, r'[^0-9]', '') AS NUMERIC) AS BioKit_ReturnKitTrack_v1r0,
d_972453354 as BioKit_ReturnKitTrack_v1r0,



CASE
    WHEN d_633640710_d_100618603 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_100618603 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_OthPkgCond_v1r0,
  CASE
    WHEN d_137401245 = '353358909' THEN 'Yes'
    WHEN d_137401245 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_CollCardMissing_v1r0,
  CASE
    WHEN d_633640710_d_205954477 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_205954477 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_CollCupDamage_v1r0,
  CASE
    WHEN d_633640710_d_289239334 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_289239334 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_CollCupLeak_v1r0,
  CASE
    WHEN d_633640710_d_427719697 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_427719697 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_CollCupNotRet_v1r0,
  CASE
    WHEN d_633640710_d_541085383 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_541085383 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_IncMatRet_v1r0,
  CASE
    WHEN d_633640710_d_545319575 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_545319575 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_PkgCrushed_v1r0,
  CASE
    WHEN d_633640710_d_938338155 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_938338155 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_ImpPkging_v1r0,
  CASE
    WHEN d_633640710_d_950521660 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_950521660 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_PkgGoodCond_v1r0,
  CASE
    WHEN d_633640710_d_992420392 = '353358909' THEN 'Yes'
    WHEN d_633640710_d_992420392 = '104430631' THEN 'No'
    ELSE 'NA'
  END AS BioKit_EmptyCupRet_v1r0,


  CASE
    WHEN d_221592017 = '517216441' THEN 'Pending'
    WHEN d_221592017 = '849527480' THEN 'Address printed'
    WHEN d_221592017 = '241974920' THEN 'Assigned'
    WHEN d_221592017 = '277438316' THEN 'Shipped'
    WHEN d_221592017 = '375535639' THEN 'Received'
    ELSE 'NA'
  END AS BioKit_KitStatusBL_v1r0,

  CASE
    WHEN d_379252329 = '976461859' THEN 'Mouthwash'
    ELSE 'NA'
  END AS BioKit_KitTypeBL_v1r0,
    CASE
    WHEN d_418571751 = '266600170' THEN 'Baseline'
    ELSE 'NA'
  END AS BioKit_CollRound_v1r0,

  CASE
    WHEN d_426588510 = '663273321' THEN 'Initial Kit'
    WHEN d_426588510 = '389478821' THEN 'Replacement Kit 1'
    WHEN d_426588510 = '772116457' THEN 'Replacement Kit 2'
    ELSE 'NA'
  END AS BioKit_KitLevel_v1r0,

d_759651991 as BioKit_DtKitReq_v1r0 
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.kitAssembly` 
where Connect_ID is not null"


kit_A_table <- bq_project_query(project, kitA)
kitA_table <- bq_table_download(kit_A_table, bigint="integer64",n_max = Inf, page_size = 10000)


kitA_table$BioKit_ReturnKitTrack_v1r0 <- gsub("[()]", "", kitA_table$BioKit_ReturnKitTrack_v1r0)
#kitA_table$BioKit_SupplyKitTrack_v1r0 <- as.numeric(kitA_table$BioKit_SupplyKitTrack_v1r0)
kitA_table$BioKit_ReturnKitTrack_v1r0 <- as.numeric(kitA_table$BioKit_ReturnKitTrack_v1r0)


openxlsx::write.xlsx(kitA_table,as.character(glue("Connect_prod_KitAssembly_Table_{currentDate}_boxfolder_{boxfolder}.xlsx")),row.names = F,na="")

log_info("Finished Kit Assmebly Table")


## Clearing up space in GCP memory 
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project')))
gc()




################################################################################################################










#########################################           CSV4:  prod_recr_veriBiospe_Formats            ##########################################################

log_info("Pulling participants table variables for recr_veri_prod file")

prod_pts_biospec <- "SELECT  Connect_ID, 
d_827220437 as RcrtES_Site_v1r0, 
d_878865966 as BioFin_BaseBloodCol_v1r0, 
d_167958071 as BioFin_BaseUrineCol_v1r0, 
d_684635302 as BioFin_BaseMouthCol_v1r0, 
d_173836415_d_266600170_d_592099155 as BioSpm_BloodSettingBL_v1r0, 
d_173836415_d_266600170_d_718172863 as BioSpm_UrineSettingBL_v1r0, 
d_173836415_d_266600170_d_915179629 as BioSpm_MWSettingBL_v1r0, 
d_331584571_d_266600170_d_135591601 as BioChk_CompleteBL_v1r0, 
d_331584571_d_266600170_d_840048338 as BioChk_TimeBL_v1r0, 
d_331584571_d_266600170_d_343048998 as BioFin_CheckOutTmBL_v1r0, 
d_173836415_d_266600170_d_561681068 as BioFin_ResearchBldTmBL_v1r0, 
d_173836415_d_266600170_d_847159717 as BioFin_ResearchUrnTmBL_v1r0, 
d_173836415_d_266600170_d_448660695 as BioFin_BMTimeBL_v1r0, 
d_254109640 as  SMMet_BLSamplesColl_v1r0, 
d_173836415_d_266600170_d_185243482 as BioClin_SiteBldLocBL_v1r0, 
d_173836415_d_266600170_d_452847912 as BioClin_SiteUrLocatBL_v1r0, 
d_173836415_d_266600170_d_341570479 as BioClin_SntBloodAccIDBL_v1r0, 
d_173836415_d_266600170_d_198261154 as BioClin_SntUrineAccIDBL_v1r0, 
d_173836415_d_266600170_d_543608829 as  BioClin_PolyBloodIDBL_v1r0, 
d_173836415_d_266600170_d_110349197 as BioClin_PolyUrineIDBL_v1r0, 
d_173836415_d_266600170_d_693370086 as BioClin_SiteBloodCollBL_v1r0, 
d_173836415_d_266600170_d_982213346 as BioClin_ClinBloodTmBL_v1r0, 
d_173836415_d_266600170_d_786930107 as BioClin_SiteUrineCollBL_v1r0, 
d_173836415_d_266600170_d_139245758 as  BioClin_ClinicalUrnTmBL_v1r0, 
d_173836415_d_266600170_d_728696253 as BioClin_SiteBloodRRLBL_v1r0, 
d_173836415_d_266600170_d_822274939 as BioClin_SiteBldRRLDtBL_v1r0, 
d_173836415_d_266600170_d_453452655 as BioClin_SiteUrineRRLBL_v1r0, 
d_173836415_d_266600170_d_224596428 as BioClin_SiteUrnRRLDtBL_v1r0, 
d_173836415_d_266600170_d_880794013 as  BioClin_BldOrUrnPlcdBL_v1r0, 
d_173836415_d_266600170_d_184451682 as BioClin_BldUrnPlcdTmBL_v1r0, 
d_173836415_d_266600170_d_530173840 as BioClin_BldOrderPlcdBL_v1r0, 
d_173836415_d_266600170_d_769615780 as BioClin_BldOrdPlacdDtBL_v1r0, 
d_173836415_d_266600170_d_860477844 as BioClin_UrnOrdPlacedBL_v1r0, 
d_173836415_d_266600170_d_939818935 as  BioClin_UrnOrdPlcdDtBL_v1r0, 
d_173836415_d_266600170_d_534041351 as BioClin_DBBloodRRLBL_v1r0, 
d_173836415_d_266600170_d_398645039 as BioClin_DBBloodRRLDtBL_v1r0, 
d_173836415_d_266600170_d_210921343 as BioClin_DBUrineRRLBL_v1r0, 
d_173836415_d_266600170_d_541311218 as BioClin_DBUrineRRLDtBL_v1r0, 
d_173836415_d_266600170_d_316824786 as BioClin_AnySpecRRLBL_v1r0, 
d_173836415_d_266600170_d_740582332 as  BioClin_AnySpecRRLTmBL_v1r0, 
d_173836415_d_266600170_d_156605577 as BioClin_AnyBldUrnRecBL_v1r0, 
d_512820379 as RcrtSI_RecruitType_v1r0, 
d_699625233 as RcrtUP_Submitted_v1r0, 
d_821247024 as RcrtV_Verification_v1r0, 
d_914594314 as RcrtV_VerificationTm_V1R0, 
d_265193023 as  SrvBLM_ResSrvCompl_v1r0, 
d_222161762 as SrvBLM_TmComplete_v1r0, 
d_822499427 as SrvBLM_TmStart_v1r0, 
d_253883960 as SrvBlU_BaseComplete_v1r0, 
d_764863765 as SrvBlU_TmComplete_v1r0, 
d_534669573 as SrvBlU_TmStart_v1r0, 
d_526455436 as BioClin_BldAndUrnRef_v1r0, 
d_685002411_d_194410742 as  HdRef_Baseblood_v1r0, 
d_390198398 as HdRef_DateBaseblood_v1r0, 
d_547363263 as  SrvMtW_BaseComplete_v1r0, 
d_195145666 as SrvMtW_TmComplete_v1r0, 
d_286191859 as  SrvMtW_TmStart_v1r0, 
token
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` 
Where Connect_ID is not null and d_821247024 = '197316935'"
prod_pts_biospec_table <- bq_project_query(project, prod_pts_biospec)
bio_pts_data <- bq_table_download(prod_pts_biospec_table, bigint="integer64",n_max = Inf, page_size = 10000)



log_info("Pulling participants table HMW variables for recr_veri_prod file")


HMW_data <-"SELECT  Connect_ID, 
d_173836415_d_266600170_d_319972665_d_379252329 as BioKit_Mouthwash_Initial_BioKit_KitTypeBL_v1r0, 
d_173836415_d_266600170_d_319972665_d_221592017 as BioKit_Mouthwash_Initial_BioKit_KitStatusBL_v1r0,
d_173836415_d_266600170_d_319972665_d_661940160 as BioKit_Mouthwash_Initial_BioKit_KitShipTm_v1r0, 
d_173836415_d_266600170_d_319972665_d_687158491 as BioKit_Mouthwash_Initial_BioKit_KitAssembledlD_v1r0,
d_173836415_d_266600170_d_319972665_d_826941471 as BioKit_Mouthwash_Initial_BioKit_KitRecdTm_v1r0, 
--d_173836415_d_266600170_d_319972665_d_759651991 as BioKit_Mouthwash_Initial_BioKit_DtKitReq_v1r0, 

d_173836415_d_266600170_d_541483796_d_379252329 as BioKit_Mouthwash_R1_BioKit_KitTypeBL_v1r0, 
d_173836415_d_266600170_d_541483796_d_221592017 as BioKit_Mouthwash_R1_BioKit_KitStatusBL_v1r0,
d_173836415_d_266600170_d_541483796_d_661940160 as BioKit_Mouthwash_R1_BioKit_KitShipTm_v1r0,
d_173836415_d_266600170_d_541483796_d_687158491 as BioKit_Mouthwash_R1_BioKit_KitAssembledlD_v1r0,
d_173836415_d_266600170_d_541483796_d_826941471 as BioKit_Mouthwash_R1_BioKit_KitRecdTm_v1r0, 
d_173836415_d_266600170_d_541483796_d_759651991 as BioKit_Mouthwash_R1_BioKit_DtKitReq_v1r0, 

d_173836415_d_266600170_d_641006239_d_379252329 as BioKit_Mouthwash_R2_BioKit_KitTypeBL_v1r0, 
d_173836415_d_266600170_d_641006239_d_221592017 as BioKit_Mouthwash_R2_BioKit_KitStatusBL_v1r0, 
d_173836415_d_266600170_d_641006239_d_661940160 as BioKit_Mouthwash_R2_BioKit_KitShipTm_v1r0, 
d_173836415_d_266600170_d_641006239_d_687158491 as BioKit_Mouthwash_R2_BioKit_KitAssembledID_v1r0,
d_173836415_d_266600170_d_641006239_d_826941471 as BioKit_Mouthwash_R2_BioKit_KitRecdTm_v1r0, 
d_173836415_d_266600170_d_641006239_d_759651991 as BioKit_Mouthwash_R2_BioKit_DtKitReq_v1r0, 

FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` 
where Connect_ID IS NOT NULL and d_906417725='104430631'  and d_747006172='104430631' and d_987563196='104430631'"

##add status

HMW_data_table <- bq_project_query(project, HMW_data)
mw_addons <- bq_table_download(HMW_data_table, bigint="integer64",n_max = Inf, page_size = 10000)


recr_mw <- left_join(bio_pts_data, mw_addons, by="Connect_ID")

log_info("Mapping variables responses to English answer for recr_veri_prod file")

recr_mw <- recr_mw %>%
  mutate(across(everything(), ~ case_when(
    . == 353358909 ~ "Yes",
    . == 104430631 ~ "No",
    . == 534621077 ~ "Research",
    . == 664882224 ~ "Clinical",
    . == 103209024 ~ "Home",
    . == 972455046 ~ "Not Started",
    . == 615768760 ~ "Started",
    . == 231311385 ~ "Submitted",
    . == 197316935 ~ "Verified",
    . == 180583933 ~ "Not active",
    . == 486306141 ~ "Active",
    . == 854703046 ~ "Passive",
    . == 976461859 ~ "Mouthwash Kit",
    . == 517216441	~ "Pending",
    . == 728267588	~ "Initialized",
    . == 849527480	~ "Address printed",
    . == 241974920	~ "Assigned",
    . == 277438316	~ "Shipped",
    . == 375535639	~ "Received",
    . == 332067457	~ "Undeliverable address",
    . == 472940358 ~ "Baylor Scott and White Health",
    . == 531629870 ~ "HealthPartners",
    . == 548392715 ~ "Henry Ford Health",
    . == 303349821 ~ "Marshfield Clinic Health System",
    . == 657167265 ~ "Sanford Health",
    . == 809703864 ~ "University of Chicago Medicine",
    . == 125001209 ~ "Kaiser Permanente Colorado",
    . == 327912200 ~ "Kaiser Permanente Georgia",
    . == 300267574 ~ "Kaiser Permanente Hawaii",
    . == 452412599 ~ "Kaiser Permanente Northwest",
    TRUE ~ as.character(.)
  )))


openxlsx::write.xlsx(recr_mw,as.character(glue("Connect_prod_recr_veriBiospe_Formats_{currentDate}_boxfolder_{boxfolder}.xlsx")),row.names = F,na="")


log_info("Completed recr_veri_prod file")

## Clearing up space in GCP memory 
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project')))
gc()

#################################################################################################################




#############. Prepping data for the Biospecimen and Participant table merged outputs #################################


log_info("Pulling entire biospecimen table")

##Select the Biospecimen Data
bio_tb <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`") 
biospe <- bq_table_download(bio_tb,bigint="integer64",n_max = Inf, page_size = 1000)
cnames <- names(biospe)
###to check variables in recr_noinact_wl1

numbers_only <- function(x) !grepl("\\D", x)

for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(biospe,varname)
  biospe[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}

log_info("Pulling participant table schema")
##Select biospecimen data in recruitment 
recr_var <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect`.INFORMATION_SCHEMA.COLUMN_FIELD_PATHS WHERE table_name='participants'")
recrvar <- bigrquery::bq_table_download(recr_var, bigint="integer64",n_max = Inf, page_size = 10000)
urlfile<- "https://raw.githubusercontent.com/episphere/conceptGithubActions/master/csv/masterFile.csv" ###to grab the updated dd from github 
y <- read.csv(urlfile)
recrvar_nd <- recrvar[which(grepl("d_",recrvar$column_name)),c("column_name","field_path")]

recrvar_nd$last.CID <- ifelse(grepl("d_",recrvar_nd$column_name),substring(sapply(strsplit(recrvar_nd$column_name,"d_"),tail,1),1,9),NA)
recrvar_nd_label <- base::merge(recrvar_nd, y[,c("Primary.Source","conceptId.2","conceptId.3","Variable.Label","conceptId.4","Current.Format.Value")],by.x="last.CID",by.y="conceptId.3",all.x=TRUE)


log_info("BQ pull for participants table based on keywords")
recrvar.bio <- recrvar_nd_label[which(recrvar_nd_label$Primary.Source =="Biospecimen" | grepl("biospecimen|blood|urine|mouthwash|collection|Blood|Urine|Mouthwash|MW|Ur|Ur Surv|MW Surv",recrvar_nd_label$Variable.Label)),] 
select <- paste(recrvar.bio$column_name,collapse=",")

tb_bq <- eval(parse(text=paste("bq_project_query(project, query=\"SELECT token,Connect_ID,d_512820379,d_821247024,d_914594314,d_827220437,d_699625233, d_265193023, d_822499427, d_222161762, d_254109640,", select,
                               "FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` Where Connect_ID is not null and d_821247024 = '197316935' \")",sep=" "))) #removed "date", as it is no longer in the participants table


recr.bio <- bigrquery::bq_table_download(tb_bq,bigint="integer64",n_max = Inf, page_size = 1000)

log_info("Finished downloading the participants table data")

##to convert to numeric
cnames <- names(recr.bio)
recrver <- recr.bio
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(recrver,varname)
  recrver[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}

log_info("Labeling participants table data")
##to get the labels of each variable
recrver_CID <- as.data.frame(sapply((strsplit(colnames(recrver),"d_")),tail,1))
colnames(recrver_CID)[1] <- "CID"
recrver_CID$variable <- names(recr.bio)


## Clearing up space in GCP memory 
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project', 'biospe', 'recr.bio', 'numbers_only', 'y')))
gc()


#################################################################################################################















###########################################     CSV 3: Biospe_Formats                 ###############################################


log_info("Starting Biospe_Formats_csv")

###to get the formats and variable names from DD
factor_cid <-function(var,data){
  var <- as.factor(var)
  var_CIDs <- as.data.frame(cbind(unique(y$conceptId.4[grepl(paste(levels(var),collapse="|"),y$conceptId.4)]),unique(trimws(sapply(strsplit(y$Current.Format.Value[grepl(paste(levels(var),collapse="|"),y$conceptId.4)], "="),tail,1)))))
  
  var <- plyr::mapvalues(var,from=var_CIDs$V1,to=var_CIDs$V2)
}


## Need to update some of the variables based on the flattening

###BioClin_DBBloodID_v1r0
biospe$d_646899796 <- biospe$d_646899796_integer
biospe <- biospe %>%  select(-d_646899796_integer, -d_646899796_string)
###BioClin_DBUrineID_v1r0
biospe$d_928693120 <- biospe$d_928693120_integer
biospe <- biospe %>%  select(-d_928693120_integer, -d_928693120_string)

## Need to include some variables now in the participants table instead of the biospecimen table
moved <- recr.bio %>%  select(Connect_ID, d_173836415_d_266600170_d_534041351, d_173836415_d_266600170_d_398645039, 
                              d_173836415_d_266600170_d_210921343, d_173836415_d_266600170_d_541311218)

biospe$Connect_ID <- as.numeric(biospe$Connect_ID)
moved$Connect_ID <- as.numeric(moved$Connect_ID)
biospe_pts <- left_join(biospe, moved, by="Connect_ID")



log_info("Setting up variable names")

cnames <- names(biospe_pts)
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(biospe_pts,varname)
  biospe_pts[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}





biospe_CID <- as.data.frame(sapply((strsplit(colnames(biospe_pts),"d_")),tail,1))
colnames(biospe_CID)[1] <- "CID"

biospe_CID$variable <- names(biospe_pts)
y$conceptId.3 <- as.character(y$conceptId.3)
y$conceptId.4 <- as.numeric(y$conceptId.4)

log_info("Using DD to name nested variables")

biospe_CID_dd <- base::merge(biospe_CID, y[,c("Formula.for.Index","Primary.Source","conceptId.1","conceptId.2","conceptId.3","Variable.Name","Variable.Label","conceptId.4","Current.Format.Value")],by.x="CID",by.y="conceptId.3",all.x=TRUE)

biospe_CID_dd <- biospe_CID_dd %>% mutate(category = case_when(conceptId.4 == 104430631~1,
                                                               !is.na(conceptId.4) & conceptId.4 !='104430631' &  nchar(conceptId.4)>0 ~ 2,
                                                               is.na(conceptId.4) | nchar(conceptId.4) == 0 ~0),
                                          firstCID = sapply(strsplit(variable,"_"),"[",2),
                                          
                                          label.1st = case_when(sapply(strsplit(variable,"_"),"[",2) == "973670172"  ~ "UrineTube1",
                                                                sapply(strsplit(variable,"_"),"[",2) == "143615646"  ~ "MWTube1",
                                                                sapply(strsplit(variable,"_"),"[",2) == "299553921"  ~ "SSTube1",
                                                                sapply(strsplit(variable,"_"),"[",2) == "703954371"  ~ "SSTube2",
                                                                sapply(strsplit(variable,"_"),"[",2) == "454453939"  ~ "EDTATube1",
                                                                sapply(strsplit(variable,"_"),"[",2) == "838567176"  ~ "HeparinTube1",
                                                                sapply(strsplit(variable,"_"),"[",2) == "652357376"  ~ "ACDTube1",
                                                                sapply(strsplit(variable,"_"),"[",2) == "677469051"  ~ "EDTATube2",
                                                                sapply(strsplit(variable,"_"),"[",2) == "683613884"  ~ "EDTATube3",
                                                                sapply(strsplit(variable,"_"),"[",2) == "376960806"  ~ "SSTube3",
                                                                sapply(strsplit(variable,"_"),"[",2) == "232343615"  ~ "SSTube4",
                                                                sapply(strsplit(variable,"_"),"[",2) == "589588440"  ~ "SSTube5",
                                                                sapply(strsplit(variable,"_"),"[",2) == "958646668"  ~ "HeparinTube2",
                                                                sapply(strsplit(variable,"_"),"[",2) == "223999569"  ~ "BiohazardMW",
                                                                sapply(strsplit(variable,"_"),"[",2) == "787237543"  ~ "BiohazardBIU" ,
                                                                sapply(strsplit(variable,"_"),"[",2) == "505347689"  ~ "STRECKTube1" ))




biospe_CID_dd$matched <- ifelse(nchar(biospe_CID_dd$variable) <12 , 1, ifelse(nchar(biospe_CID_dd$variable) >12 & biospe_CID_dd$conceptId.1 == biospe_CID_dd$firstCID, 1, 0))


biospe_CID_dd <- biospe_CID_dd %>% arrange(CID, variable,desc(matched),Formula.for.Index)


log_info("Adding tube type to variable names")

biospe_CID_dd1 <- biospe_CID_dd[!duplicated(biospe_CID_dd[,c("CID","variable")]),]
biospe_CID_dd1$new.varname <- ifelse(biospe_CID_dd1$matched==1, biospe_CID_dd1$Variable.Name, paste(gsub("SST1","",biospe_CID_dd1$Variable.Name),biospe_CID_dd1$label.1st,sep="_"))
biospe_CID_dd1$new.varname[which(biospe_CID_dd1$variable=="d_926457119")] <- "BioBPTL_DateRec_v1r0"
biospe_CID_dd1$variable[is.na(biospe_CID_dd1$new.varname)]

biospe1 <- NULL
select0 <- biospe_CID_dd1$variable[which(biospe_CID_dd1$category==0 & !is.na(biospe_CID_dd1$Variable.Name))]
select2 <- biospe_CID_dd1$variable[which(biospe_CID_dd1$category==2)]
yes_no <- biospe_CID_dd1$variable[which(biospe_CID_dd1$category==1)]
biospe1 <- subset(biospe_pts,select =c("Connect_ID","token","siteAcronym",select0,select2)) #removed "date", as it is no longer in the participants table
n<- length(select0)
colnames(biospe1)[which(colnames(biospe1) %in% select0)] <-  biospe_CID_dd1$new.varname[which(biospe_CID_dd1$category==0 & !is.na(biospe_CID_dd1$Variable.Name))]


log_info("setting up variable type for mapping")

for (i in 1: length(select2)){
  eval(parse(text=paste("biospe1$",select2[i],"<-factor_cid(biospe1$",select2[i],")",sep="")))
}
colnames(biospe1)[which(colnames(biospe1) %in% select2)] <-  biospe_CID_dd1$new.varname[which(biospe_CID_dd1$category==2)]

for (i in 1:length(yes_no)){
  x <- yes_no[i]
  
  varname <- biospe_CID_dd1[grepl(yes_no[i],biospe_CID_dd1$variable) & biospe_CID_dd1$category==1,]$new.varname
  
  #type.labels <- dd$`Variable Label`[grepl(CID,dd$CID)]
  check <- as.data.frame(biospe_pts[,x])
  check$variable <- ifelse(check[,1]== 353358909, "Yes",ifelse(check[,1]==104430631,"No",NA))
  colnames(check)[2] <- varname
  biospe1 <-   cbind(biospe1,subset(check,select=varname))
}


colnames(biospe1) <- gsub("Object|Obj","Tube", colnames(biospe1))
colnames(biospe1) <- gsub("BioCol_Dev_v1r0","Deviation",colnames(biospe1))
biospe1 <- biospe1[,order(colnames(biospe1))]


log_info("Selecting final variables")

names_set <- c("Connect_ID","RcrtES_Site_v1r0","BioSpm_Visit_v1r0","BioSpm_Setting_v1r0","BioSpm_Location_v1r0","BioSpm_ColIDScan_v1r0",
               "BioReg_ArRegTime_v1r0", "BioCol_ColTime_v1r0", "BioRec_CollectFinal_v1r0","BioRec_CollectFinalTime_v1r0","BioClin_DBBloodID_v1r0",
               #"BioClin_DBBloodRRLBL_v1r0",
               #"BioClin_DBBloodRRLDtBL_v1r0",
               "BioClin_DBUrineID_v1r0",
               #"BioClin_DBUrineRRLBL_v1r0",
               #"BioClin_DBUrineRRLDtBL_v1r0",
               "BioCol_PhlebTMInitials_v1r0",
               "BioCol_ColNote1_v1r0",
               "BioCol_TubeColl_v1r0_Bio",
               "BioCol_TubeID_v1r0_Bio",
               "BioCol_TubeColl_v1r0_SST1",
               "BioCol_TubeID_v1r0_SST1",
               "BioCol_NotCol_v1r0_SST1",
               "BioCol_NotCol_v1r0_SST1",
               "BioCol_Discard_v1r0_SST1",
               "BioCol_TubeColl_v1r0_SST2",
               "BioCol_TubeID_v1r0_SST2",
               "BioCol_NotCol_v1r0_SST2",
               "BioCol_NotCol_v1r0_SST2",
               "BioCol_Discard_v1r0_SST2",
               "BioCol_TubeColl_v1r0_HT1",
               "BioCol_TubeID_v1r0_HT1",
               "BioCol_NotCol_v1r0_HT1",
               "BioCol_NotCol_v1r0_HT1",
               "BioCol_Discard_v1r0_HT1",
               "BioCol_TubeColl_v1r0_EDTAT1",
               "BioCol_TubeID_v1r0_EDTAT1",
               "BioCol_NotCol_v1r0_EDTAT1",
               "BioCol_NotCol_v1r0_EDTAT1",
               "BioCol_Discard_v1r0_EDTAT1",
               "BioCol_TubeColl_v1r0_ACDT1",
               "BioCol_TubeID_v1r0_ACDT1",
               "BioCol_NotCol_v1r0_ACDT1",
               "BioCol_NotCol_v1r0_ACDT1",
               "BioCol_Discard_v1r0_ACDT1",
               "BioCol_TubeColl_v1r0_STRT1",
               "BioCol_TubeID_v1r0_STRT1",
               "BioCol_NotCol_v1r0_STRT1",
               "BioCol_NotCol_v1r0_STRT1",
               "BioCol_Discard_v1r0_STRT1",
               "BioCol_TubeColl_v1r0_UT1",
               "BioCol_TubeID_v1r0_UT1",
               "BioCol_NotCol_v1r0_UT1",
               "BioCol_NotCol_v1r0_UT1",
               "BioCol_Discard_v1r0_UT1",
               "BioCol_TubeColl_v1r0_MWT1",
               "BioCol_TubeID_v1r0_MWT1",
               "BioCol_NotCol_v1r0_MWT1",
               "BioCol_NotCol_v1r0_MWT1",
               "BioCol_Discard_v1r0_MWT1",
               "BioCol_TubeColl_v1r0_BioMW",
               "BioCol_TubeID_v1r0_BioMW",
               "BioCol_TubeColl_v1r0_SST3",
               "BioCol_TubeID_v1r0_SST3",
               "BioCol_Discard_v1r0_SST3",
               "BioCol_TubeColl_v1r0_SST4",
               "BioCol_TubeID_v1r0_SST4",
               "BioCol_Discard_v1r0_SST4",
               "BioCol_TubeColl_v1r0_SST5",
               "BioCol_TubeID_v1r0_SST5",
               "BioCol_Discard_v1r0_SST5",
               "BioCol_TubeID_v1r0_HT2",
               "BioCol_TubeColl_v1r0_HT2",
               "BioCol_Discard_v1r0_HT2",
               "BioCol_TubeColl_v1r0_EDTAT2",
               "BioCol_TubeID_v1r0_EDTAT2",
               "BioCol_Discard_v1r0_EDTAT2",
               "BioCol_TubeColl_v1r0_EDTAT3",
               "BioCol_TubeID_v1r0_EDTAT3",
               "BioCol_Discard_v1r0_EDTAT3",
               "BioCol_CollBoxedStatus_v1r0",
               "BioBPTL_DateRec_v1r0_SST1",
               "BioBPTL_DateRec_v1r0_SST2",
               "BioBPTL_DateRec_v1r0_HT1",
               "BioBPTL_DateRec_v1r0_EDTAT1",
               "BioBPTL_DateRec_v1r0_ACDT1",
               "BioBPTL_DateRec_v1r0_STRT1",
               "BioBPTL_DateRec_v1r0_UT1",
               "BioBPTL_DateRec_v1r0_MWT1",
               "BioKit_KitRecdTmBL_v1r0_MWTube1",
               "BioBPTL_DateRec_v1r0_SST3",
               "BioBPTL_DateRec_v1r0_SST4",
               "BioBPTL_DateRec_v1r0_SST5",
               "BioBPTL_DateRec_v1r0_HT2",
               "BioBPTL_DateRec_v1r0_EDTAT2",
               "BioBPTL_DateRec_v1r0_EDTAT3",
               "BioRec_NoteOpt_v1r0",
               ##"BioCol_StrayTubesList_v1r0",
               "siteAcronym",
               "BioCol_Deviation_v1r0_SST1",
               "BioCol_DevNotes_v1r0_SST1",
               "Deviation_BrokenSST1",
               "Deviation_ClotLSST1",
               "Deviation_ClotSSST1",
               "Deviation_DiscardSST1",
               "Deviation_GelSST1",
               "Deviation_HemoSST1",
               "Deviation_LeakSST1",
               "Deviation_LowVolSST1",
               "Deviation_MislabDSST1",
               "Deviation_MislabRSST1",
               "Deviation_NotFoundSST1",
               "Deviation_OtherSST1",
               "Deviation_OutContSST1",
               "Deviation_SpeedHSST1",
               "Deviation_SpeedLSST1",
               "Deviation_TempHSST1",
               "Deviation_TempLSST1",
               "Deviation_TimeLSST1",
               "Deviation_TimeSSST1",
               "Deviation_TubeSzSST1",
               "BioCol_Deviation_v1r0_SST2",
               "BioCol_DevNotes_v1r0_SST2",
               "Deviation_BrokenSST2",
               "Deviation_ClotLSST2",
               "Deviation_ClotSSST2",
               "Deviation_DiscardSST2",
               "Deviation_GelSST2",
               "Deviation_HemoSST2",
               "Deviation_LeakSST2",
               "Deviation_LowVolSST2",
               "Deviation_MislabDSST2",
               "Deviation_MislabRSST2",
               "Deviation_NotFoundSST2",
               "Deviation_OtherSST2",
               "Deviation_OutContSST2",
               "Deviation_SpeedHSST2",
               "Deviation_SpeedLSST2",
               "Deviation_TempHSST2",
               "Deviation_TempLSST2",
               "Deviation_TimeLSST2",
               "Deviation_TimeSSST2",
               "Deviation_TubeSzSST2",
               "BioCol_Deviation_v1r0_SST3",
               "BioCol_DevNotes_v1r0_SST3",
               "Deviation_BrokenSST3",
               "Deviation_ClotLSST3",
               "Deviation_ClotS_SST3",
               "Deviation_DiscardSST3",
               "Deviation_GelSST3",
               "Deviation_HemoSST3",
               "Deviation_LeakSST3",
               "Deviation_LowVolSST3",
               "Deviation_MislabDSST3",
               "Deviation_MislabRSST3",
               "Deviation_NotFoundSST3",
               "Deviation_OtherSST3",
               "Deviation_OutContSST3",
               "Deviation_SpeedHSST3",
               "Deviation_SpeedLSST3",
               "Deviation_TempHSST3",
               "Deviation_TempLSST3",
               "Deviation_TimeLSST3",
               "Deviation_TimeSSST3",
               "Deviation_TubeSzSST3",
               "BioCol_Deviation_v1r0_SST4",
               "BioCol_DevNotes_v1r0_SST4",
               "Deviation_BrokenSST4",
               "Deviation_ClotLSST4",
               "Deviation_ClotSSST4",
               "Deviation_DiscardSST4",
               "Deviation_GelSST4",
               "Deviation_HemoSST4",
               "Deviation_LeakSST4",
               "Deviation_LowVolSST4",
               "Deviation_MislabDSST4",
               "Deviation_MislabRSST4",
               "Deviation_NotFoundSST4",
               "Deviation_OtherSST4",
               "Deviation_OutContSST4",
               "Deviation_SpeedHSST4",
               "Deviation_SpeedLSST4",
               "Deviation_TempHSST4",
               "Deviation_TempLSST4",
               "Deviation_TimeLSST4",
               "Deviation_TimeSSST4",
               "Deviation_TubeSzSST4",
               "BioCol_Deviation_v1r0_SST5",
               "BioCol_DevNotes_v1r0_SST5",
               "Deviation_BrokenSST5",
               "Deviation_ClotLSST5",
               "Deviation_ClotSSST5",
               "Deviation_DiscardSST5",
               "Deviation_GelSST5",
               "Deviation_HemoSST5",
               "Deviation_LeakSST5",
               "Deviation_LowVolSST5",
               "Deviation_MislabDSST5",
               "Deviation_MislabRSST5",
               "Deviation_NotFoundSST5",
               "Deviation_OtherSST5",
               "Deviation_OutContSST5",
               "Deviation_SpeedHSST5",
               "Deviation_SpeedLSST5",
               "Deviation_TempHSST5",
               "Deviation_TempLSST5",
               "Deviation_TimeLSST5",
               "Deviation_TimeSSST5",
               "Deviation_TubeSzSST5",
               "BioCol_Deviation_v1r0_HT1",
               "BioCol_DevNotes_v1r0_HT1",
               "Deviation_BrokenHT1",
               "Deviation_DiscardHT1",
               "Deviation_HemoHT1",
               "Deviation_LeakHT1",
               "Deviation_LowVolHT1",
               "Deviation_MislabelDHT1",
               "Deviation_MislabelRHT1",
               "Deviation_NotFoundHT1",
               "Deviation_OtherHT1",
               "Deviation_OutContamHT1",
               "Deviation_TempHHT1",
               "Deviation_TempLHT1",
               "Deviation_TubeSzHT1",
               "BioCol_Deviation_v1r0_HT2",
               "BioCol_DevNotes_v1r0_HT2",
               "Deviation_BrokenHT2",
               "Deviation_DiscardHT2",
               "Deviation_HemoHT2",
               "Deviation_LeakHT2",
               "Deviation_LowVolHT2",
               "Deviation_MislabelDHT2",
               "Deviation_MislabelRHT2",
               "Deviation_NotFoundHT2",
               "Deviation_OtherHT2",
               "Deviation_OutContamHT2",
               "Deviation_TempHHT2",
               "Deviation_TempLHT2",
               "Deviation_TubeSzHT2",
               "BioCol_Deviation_v1r0_EDTAT1",
               "BioCol_DevNotes_v1r0_EDTAT1",
               "Deviation_BrokenEDTAT1",
               "Deviation_DiscEDTAT1",
               "Deviation_HemoEDTAT1",
               "Deviation_LeakEDTAT1",
               "Deviation_LowVolEDTAT1",
               "Deviation_MislDEDTAT1",
               "Deviation_MislREDTAT1",
               "Deviation_NFEDTAT1",
               "Deviation_OtherEDTAT1",
               "Deviation_OutConEDTAT1",
               "Deviation_TempHEDTAT1",
               "Deviation_TempLEDTAT1",
               "Deviation_TubeSzEDTAT1",
               "BioCol_Deviation_v1r0_EDTAT2",
               "BioCol_DevNotes_v1r0_EDTAT2",
               "Deviation_BrokenEDTAT2",
               "Deviation_DiscEDTAT2",
               "Deviation_HemoEDTAT2",
               "Deviation_LeakEDTAT2",
               "Deviation_LowVolEDTAT2",
               "Deviation_MislDEDTAT2",
               "Deviation_MislREDTAT2",
               "Deviation_NFEDTAT2",
               "Deviation_OtherEDTAT2",
               "Deviation_OutConEDTAT2",
               "Deviation_TempHEDTAT2",
               "Deviation_TempLEDTAT2",
               "Deviation_TubeSzEDTAT2",
               "BioCol_Deviation_v1r0_EDTAT3",
               "BioCol_DevNotes_v1r0_EDTAT3",
               "Deviation_BrokenEDTAT3",
               "Deviation_DiscEDTAT3",
               "Deviation_HemoEDTAT3",
               "Deviation_LeakEDTAT3",
               "Deviation_LowVolEDTAT3",
               "Deviation_MislDEDTAT3",
               "Deviation_MislREDTAT3",
               "Deviation_NFEDTAT3",
               "Deviation_OtherEDTAT3",
               "Deviation_OutConEDTAT3",
               "Deviation_TempHEDTAT3",
               "Deviation_TempLEDTAT3",
               "Deviation_TubeSzEDTAT3",
               "BioCol_Deviation_v1r0_ACDT1",
               "BioCol_DevNotes_v1r0_ACDT1",
               "Deviation_BrokenACDT1",
               "Deviation_DiscardACDT1",
               "Deviation_HemoACDT1",
               "Deviation_LeakACDT1",
               "Deviation_LowVolACDT1",
               "Deviation_MislabDACDT1",
               "Deviation_MislabRACDT1",
               "Deviation_NotFndACDT1",
               "Deviation_OtherACDT1",
               "Deviation_OutContACDT1",
               "Deviation_TempHACDT1",
               "Deviation_TempLACDT1",
               "Deviation_TubeSzACDT1",
               "BioCol_Deviation_v1r0_STRT1",
               "BioCol_DevNotes_v1r0_STRT1",
               "Deviation_BrokenSTRT1",
               "Deviation_DiscardSTRT1",
               "Deviation_HemoSTRT1",
               "Deviation_LeakSTRT1",
               "Deviation_LowVolSTRT1",
               "Deviation_MislabDSTRT1",
               "Deviation_MislabRSTRT1",
               "Deviation_NotFndSTRT1",
               "Deviation_OtherSTRT1",
               "Deviation_OutContSTRT1",
               "Deviation_TempHSTRT1",
               "Deviation_TempLSTRT1",
               "Deviation_TubeSzSTRT1",
               "BioCol_Deviation_v1r0_UT1",
               "BioCol_DevNotes_v1r0_UT1",
               "Deviation_BrokenUT1",
               "Deviation_LeakUT1",
               "Deviation_LowVolUT1",
               "Deviation_MislabelDUT1",
               "Deviation_MislabelRUT1",
               "Deviation_NotFoundUT1",
               "Deviation_OtherUT1",
               "Deviation_OutContamUT1",
               "Deviation_TempHUT1",
               "Deviation_TempLUT1",
               "Deviation_UrVolUT1",
               "BioCol_Deviation_v1r0_MWT1",
               "BioCol_DevNotes_v1r0_MWT1",
               "Deviation_BrokenMWT1",
               "Deviation_LeakMWT1",
               "Deviation_LowVolMWT1",
               "Deviation_MislabDMWT1",
               "Deviation_MislabRMWT1",
               "Deviation_MWUnd30sMWT1",
               "Deviation_NotFoundMWT1",
               "Deviation_OtherMWT1",
               "Deviation_OutContMWT1",
               "BioKit_KitAssembledIDBL_v1r0", ## doesn't match JSON
               "token")


biospe1_final <- biospe1 %>% select(all_of(names_set))


log_info("Pulled all variables")


openxlsx::write.xlsx(biospe1_final,as.character(glue("Connect_prod_Biospe_Formats_{currentDate}_boxfolder_{boxfolder}.xlsx")),row.names = F,na="")

log_info("Finished Biospe_Formats csv")



## Clearing up space in GCP memory 
#rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project', 'y', 'recr.bio', 'recrver')))
#gc()



















log_info("FINISHED THE ENTIRE CODE")


