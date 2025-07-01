
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
#CAST(d_972453354 AS NUMERIC) as BioKit_ReturnKitTrack_v1r0,
#CAST(REGEXP_REPLACE(d_972453354, r'[^0-9]', '') AS NUMERIC) AS BioKit_ReturnKitTrack_v1r0,
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
d_173836415_d_266600170_d_316824786 as  BioClin_AnySpecRRLTmBL_v1r0, 
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
d_173836415_d_266600170_d_319972665_d_826941471 BioKit_Mouthwash_Initial_BioKit_KitRecdTm_v1r0, 
#d_173836415_d_266600170_d_319972665_d_759651991 as BioKit_Mouthwash_Initial_BioKit_DtKitReq_v1r0, 

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
















log_info("FINISHED THE ENTIRE CODE")


