
library(bigrquery)
library(data.table) ###to write or read and data management 
library(dplyr) ###data management
# library(boxr) ###read or write data from/to box
library(tidyverse) ###for data management
library(reshape)  ###to work on transition from long to wide or wide to long data
library(listr) ###to work on a list of vector, files or..
library(sqldf) ##sql
library(lubridate) ###date time
library(ggplot2) ###plots
#library(ggpubr) ###for the publications of plots
#library(RColorBrewer) ###visions color http://www.sthda.com/english/wiki/colors-in-r
library(gridExtra)
library(stringr) ###to work on patterns, charaters
#library(plyr)
library(tinytex) #for pdf
#library(rmarkdown) ###for the output tables into other files: pdf, rtf, etc.
library(janitor) #to get the summary sum
library(finalfit) #https://cran.r-project.org/web/packages/finalfit/vignettes/export.htmlt
library(expss) ###to add labels
library(epiDisplay) ##recommended applied here crosstable, tab1
library(summarytools) ##recommended
library(gmodels) ##recommended
library(magrittr)
library(arsenal)
library(gtsummary)
library(kableExtra)
library(gt)
library(crosstable)
#library(cowplot)
#library(webshot2)
library(glue) 
library(readr)
library(rio)
library(scales) #for commas in the thousands place to be applied automatically
#options(knitr.table.format = "latex")
currentDate <- Sys.Date()
currentDate <- as.Date(currentDate)

bq_auth()

### Set Box folders for output CSV files #######################################
use_test_box_folder <- TRUE # Local user sets this (Jake or Kelsey)

# Over-ride if using plumber api on GCP (USER DOESN'T TOUCH THIS!)
# Check if the environment variable exists and is not empty
if (!is.null(Sys.getenv("USE_TEST_BOX_FOLDER")) &&
    Sys.getenv("USE_TEST_BOX_FOLDER") != "") {
  use_test_box_folder <- as.logical(Sys.getenv("USE_TEST_BOX_FOLDER"))
}

if (use_test_box_folder) {
  boxfolder <- 222593912729 # test box folder
} else {
  boxfolder <- 221280601453 # destination of CSV files (not pdf)
}


##### Making sure personal C drives aren't referenced if this code is being used by others


#Change to FALSE if referencing this code
write_to_local_drive = F #F


### This function below is put before any write.csv functions, and "filename" is updated. It determines wheteher the file will be created locally or not.
#filename=
local_drive= ifelse(write_to_local_drive, "C:/Users/dowlingk2/Documents/Module-Missingness-and-Metrics/data/", "")


########################################################################################



##import data to R

project <- "nih-nci-dceg-connect-prod-6d04"

bio_tb <- bq_project_query(project, 
                           query='SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen_JP` where d_410912345 is not null')  
biospe <- bq_table_download(bio_tb,bigint="integer64",n_max = Inf) #, page_size = 1000)
cnames <- names(biospe)
###to check variables in recr_noinact_wl1

numbers_only <- function(x) !grepl("\\D", x)

for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(biospe,varname)
  biospe[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}


tb_box <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes_JP`")
box_wl_flat <- bigrquery::bq_table_download(tb_box,bigint="integer64",n_max = Inf, page_size = 1000)


urlfile<- "https://raw.githubusercontent.com/episphere/conceptGithubActions/master/csv/masterFile.csv" ###to grab the updated dd from github
y <- read.csv(urlfile) #the masterDD has just been updated which can't easily pulled from github directly as before. I need to download from the raw file in the github


dictionary <- rio::import("https://episphere.github.io/conceptGithubActions/aggregate.json",format = "json")
dd<-rbindlist(dictionary,fill=TRUE,use.names=TRUE,idcol="CID") #THIS TABLE HAS REPLICATED (CIDS+LABELS) WITH DIFFERENT VARIABLE NAMES,



#to convert the numeric variables
numbers_only <- function(x) !grepl("\\D", x)
cnames <- names(box_wl_flat)
###to check variables in recr_noinact_wl1
data2 <- box_wl_flat
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(data2,varname)
  data2[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}


##to select biospecimen data in recruitment 
recr_var <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect`.INFORMATION_SCHEMA.COLUMN_FIELD_PATHS WHERE table_name='participants_JP'")
recrvar <- bigrquery::bq_table_download(recr_var, bigint="integer64",n_max = Inf, page_size = 10000)
urlfile<- "https://raw.githubusercontent.com/episphere/conceptGithubActions/master/csv/masterFile.csv" ###to grab the updated dd from github 
y <- read.csv(urlfile)
recrvar_nd <- recrvar[which(grepl("d_",recrvar$column_name)),c("column_name","field_path")]

recrvar_nd$last.CID <- ifelse(grepl("d_",recrvar_nd$column_name),substring(sapply(strsplit(recrvar_nd$column_name,"d_"),tail,1),1,9),NA)
recrvar_nd_label <- base::merge(recrvar_nd, y[,c("Primary.Source","conceptId.2","conceptId.3","Variable.Label","conceptId.4","Current.Format.Value")],by.x="last.CID",by.y="conceptId.3",all.x=TRUE)


recrvar.bio <- recrvar_nd_label[which(recrvar_nd_label$Primary.Source =="Biospecimen" | grepl("biospecimen|blood|urine|mouthwash|collection|Blood|Urine|Mouthwash|MW|Ur|Ur Surv|MW Surv",recrvar_nd_label$Variable.Label)),] #49
query <- unique(recrvar.bio$column_name)
select <- paste(recrvar.bio$column_name,collapse=",")

tb_bq <- eval(parse(text=paste("bq_project_query(project, query=\"SELECT token,Connect_ID,d_512820379,d_821247024,d_914594314,d_827220437,d_699625233, d_265193023, d_822499427, d_222161762, state_d_667474224, ",
                               select,
                               "FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` Where d_821247024 = '197316935' and  (d_512820379='486306141' or d_512820379='854703046')  and d_831041022 = '104430631'\")",
                               sep=" ")))
#removed "date", as it is no longer in the participants table



recr.bio <- bigrquery::bq_table_download(tb_bq,bigint="integer64",n_max = Inf, page_size = 1000)




##to convert to numeric
cnames <- names(recr.bio)
recrver <- recr.bio
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(recrver,varname)
  recrver[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}
##to get the labels of each variable
recrver_CID <- as.data.frame(sapply((strsplit(colnames(recrver),"d_")),tail,1))
colnames(recrver_CID)[1] <- "CID"
recrver_CID$variable <- names(recr.bio)
recrver_ver1 <- base::merge(dd,recrver_CID,by="CID")
recrver1 <- expss::apply_labels(recrver,
                                d_512820379 = "Recruitment type",#RcrtSI_RecruitType_v1r0
                                d_512820379=c(	"Not active"=180583933,  
                                               "Active"= 486306141, 
                                               "Passive"=854703046),
                                d_821247024 = "Verification status", #RcrtV_Verification_v1r0
                                d_821247024 = c("Not yet verified" =875007964,
                                                "Verified"=197316935,  
                                                "Cannot be verified"=219863910, 
                                                "Duplicate"=922622075 , 
                                                "Outreach timed out"= 160161595),
                                d_827220437 = "Site",#RcrtES_Site_v1r0
                                d_827220437 = c("Baylor Scott and White Health"=472940358,"HealthPartners"= 531629870, 
                                                "Henry Ford Health"=548392715,"Marshfield Clinic Health System" = 303349821,
                                                "Sanford Health" = 657167265, "University of Chicago Medicine" = 809703864,
                                                "Kaiser Permanente Colorado" = 125001209,"Kaiser Permanente Georgia" = 327912200,
                                                "Kaiser Permanente Hawaii" = 300267574,"Kaiser Permanente Northwest" = 452412599),
                                d_914594314 = "Verification status time",   #RcrtV_VerificationTm_V1R0
                                d_878865966 = "Baseline blood sample collected", #BioFin_BaseBloodCol_v1r0
                                d_878865966 = c("Any blood collected"=353358909, "No blood collected"=104430631),
                                d_173836415_d_266600170_d_561681068 = "Date/time basline Research Blood Collected",#BL_BioFin_BBTime_v1r0
                                d_684635302 = "Baseline Mouthwash Collected",    #BioFin_BaseMWCol_v1r0
                                d_684635302 = c("Any mouthwash collected"=353358909,"No mouthwash collected"= 104430631  ),
                                d_173836415_d_266600170_d_448660695 = "Date/time Mouthwash Collected", #
                                d_167958071 = "Baseline Urine collected", 
                                d_167958071 = c( "Any urine collected"=353358909, "No urine collected"=104430631),
                                d_173836415_d_266600170_d_847159717 = "Baseline Date/time Urine collected",
                                d_331584571_d_266600170_d_135591601 = "Baseline Check In Complete",#BL_BioChk_Complete_v1r0
                                d_331584571_d_266600170_d_135591601 =  c( "No"=104430631, "Yes"=353358909 ),
                                d_331584571_d_266600170_d_840048338 = "Date/Time Baseline Check In Complete",
                                d_331584571_d_266600170_d_343048998 = "Time Baseline check out complete",#
                                
                                d_173836415_d_266600170_d_530173840 = "Blood Order Placed",#BioClin_BldOrderPlaced_v1r0  ## I ADDED
                                d_173836415_d_266600170_d_530173840 =c( "No"=104430631, "Yes"=353358909 ), ## I ADDED
                                
                                d_173836415_d_266600170_d_592099155 = "Blood Collection Setting",#BL_BioSpm_BloodSetting_v1r0  
                                d_173836415_d_266600170_d_592099155 =c("Research"=534621077, "Clinical"= 664882224,"Home" =103209024),
                                d_173836415_d_266600170_d_718172863 = "Urine Collection Setting",#BL_BioSpm_UrineSetting_v1r0
                                d_173836415_d_266600170_d_718172863 =c("Research"=534621077, "Clinical"= 664882224,"Home" =103209024),
                                d_173836415_d_266600170_d_915179629 = "Mouthwash Collection Setting",#BL_BioSpm_MWSetting_v1r0
                                d_173836415_d_266600170_d_915179629 =c("Research"=534621077, "Clinical"= 664882224,"Home" =103209024),
                                
                                d_265193023 = "Blood/urine/mouthwash combined research survey-Complete",   
                                d_265193023 = c(	"Not Started" =	972455046, "Started"= 615768760,"Submitted"= 231311385),#SrvBLM_ResSrvCompl_v1r0
                                d_822499427 = "Placeholder (Autogenerated) - Date/time Status of Start of blood/urine/mouthwash research survey",#SrvBLM_TmStart_v1r0
                                d_222161762  = "Placeholder (Autogenerated)- Date/Time blood/urine/mouthwash research survey completed" #   SrvBLM_TmComplete_v1r0
)
###to convert to categorical variables
#recrver1$RcrtSI_RecruitType_v1r0 <- factor(recrver1$d_512820379, exclude=NULL)
#recrver1$RcrtV_Verification_v1r0 <- factor(recrver1$d_821247024, exclude=NULL)
recrver1$BioFin_BaseMWCol_v1r0  <-factor(recrver1$d_684635302,exclude=NULL)
recrver1$BioFin_BaseBloodCol_v1r0 <- factor(recrver1$d_878865966,levels=c("Any blood collected","No blood collected"))
recrver1$BioFin_BaseUrineCol_v1r0 <- factor(recrver1$d_167958071,exclude=NULL)
recrver1$BioClin_BldOrderPlaced_v1r0 <- factor(recrver1$d_173836415_d_266600170_d_530173840,exclude=NULL) ## I ADDED
#recrver1$SrvBLM_ResSrvCompl_v1r0 <-factor(recrver1$d_265193023,exclude=NULL)



recrver1$BL_BioSpm_MWSetting_v1r0 <- factor(recrver1$d_173836415_d_266600170_d_915179629,levels=c("Clinical","Research","Home"))  
recrver1$BL_BioSpm_UrineSetting_v1r0 <- factor(recrver1$d_173836415_d_266600170_d_718172863, levels=c("Clinical","Research","Home"))  
recrver1$BL_BioSpm_BloodSetting_v1r0 <- factor(recrver1$d_173836415_d_266600170_d_592099155,levels=c("Clinical","Research","Home"))  

#recrver1$BL_BioChk_Complete_v1r0 <- factor(recrver1$d_331584571_d_266600170_d_135591601,exclude=NULL)
recrver1$Site <-factor(recrver1$d_827220437,levels=c("Baylor Scott and White Health","HealthPartners", "Henry Ford Health",
                                                     "Marshfield Clinic Health System",
                                                     "Sanford Health", "University of Chicago Medicine","Kaiser Permanente Colorado",
                                                     "Kaiser Permanente Georgia","Kaiser Permanente Hawaii","Kaiser Permanente Northwest"))








### Each week must start on Monday and end with the following Sunday to be consitent with all reports
veristart.date = as.Date(min(as.POSIXct(recr.bio$d_914594314),na.rm=TRUE))
#veristart.date + days(5)
#[1] "2021-08-01"

########################################################################################



######## Functions


n_pct_table <- function(x,y){
  #x<-clinicnotes.notcol$Site
  #y<-clinicnotes.notcol$notcol.notes
  ctable<- CrossTable(x,y)
  count <- as.data.frame.matrix(ctable$t)
  perc <- as.data.frame.matrix(ctable$prop.col)
  n<- as.numeric(length(levels(x)))
  m <- as.numeric(length(levels(y)))
  tb_combo <- array(pct_n(ctable$t,ctable$prop.col),dim=c(n,m)) 
  tb_combo <- as.data.frame(tb_combo)
  colnames(tb_combo) <- colnames(count)
  
  total <- sum(count)
  count$Total <- apply(count,1,sum)
  tb_combo$Total <- paste0(count$Total, "( ", round(100*(count$Total)/total,1), " %)")
  
  tb_combo$Site <- rownames(count)
  tb_combo[(n+1),c(1:(m+1))] <- as.character(apply(count,2,sum))
  tb_combo$Site[is.na(tb_combo$Site)] <- replace_na("Total")  
  return(tb_combo)
}



pct_n <- function(x,y){ paste0(x,"( ", round(100*y,2), " %)")}



create_tbl_cross <- function(data, row_var, colm_var, row_or_col_pct, row_name, column_name){
  cross__table <- data %>%  
    tbl_cross(
      row = !!sym(row_var),
      col = !!sym(colm_var),
      digits=c(0,1),
      percent = row_or_col_pct,
      label=list(!!sym(row_var) ~ row_name,
                 !!sym(colm_var) ~ column_name),
      missing="ifany",
      margin_text="Total") 
  
  #Ops doesn't like the grouped by variables on a different line the column labels
  cross_table_df <- as.data.frame(cross__table)[-1, ]
  
}




# Function to add total row, total column, and calculate row/column percentages
add_totals_and_percentages <- function(df, total_position = c("row", "col", "both"), 
                                       percentage_type = c("row", "col")) {
  
  # Ensure the 'total_position' and 'percentage_type' are valid options
  total_position <- match.arg(total_position)
  percentage_type <- match.arg(percentage_type)
  
  # Add totals (row, column, or both)
  if (total_position == "row" || total_position == "both") {
    df <- df %>%
      adorn_totals("row")  # Add total row
  }
  
  if (total_position == "col" || total_position == "both") {
    df <- df %>%
      adorn_totals("col")  # Add total column
  }
  
  # Save counts separately to combine with percentages
  counts <- df
  
  # Calculate percentages (row or column)
  df <- df %>%
    adorn_percentages(percentage_type) %>%
    adorn_pct_formatting(digits = 1)
  
  # Combine counts and percentages in the desired format: "count (percentage)"
  df <- counts %>%
    mutate(across(-1, ~ paste0(comma(.x), " (", df[[cur_column()]], ")")))
  
  return(df)
}

###########################################################################




################## Removing Duplicates

# Ops no longer wants duplicate collections in this report
## REMOVING DUPLICATES: Method- take the first collection and drop the second collection
##### NOTE: Home MW collections are separate collections BUT they are not finalized, so they will not be removed from the dataframe 


# Step 1: Find the duplicate rows
## 295- one person has three rows; 147 individuals 
actual_dups <- biospe %>%
  filter(!is.na(d_556788178)) %>%
  group_by(Connect_ID) %>%
  filter(n() > 1)

# Step2: Of the duplicate rows, keep the one with the first collection time stamp
#Keeping 147- each person keeps their lower collection time
rows_to_keep <- actual_dups %>%
  group_by(Connect_ID) %>%
  filter(d_556788178 == min(d_556788178, na.rm = TRUE)) %>%
  ungroup()


# Take all duplicate rows out of the original dataframe
#Take the 295 from the original 52650 rows, totaling ==52,355
non_duplicate_rows <- biospe %>%
  anti_join(actual_dups, by = c("Connect_ID", "d_556788178"))  # Ensure all columns are included

# Combine new overall dataframe with those duplicate rows we filtered with the earlier time stamp
#52,355+147=52502
no_dups <- bind_rows(rows_to_keep, non_duplicate_rows)



no_dups <- expss::apply_labels(no_dups,d_827220437 = "Site",#RcrtES_Site_v1r0
                               d_827220437 = c("Baylor Scott and White Health"=472940358, "HealthPartners"= 531629870, "Henry Ford Health"=548392715,
                                               "Marshfield Clinic Health System" = 303349821,"Sanford Health" = 657167265, 
                                               "University of Chicago Medicine" = 809703864, "Kaiser Permanente Colorado" = 125001209,
                                               "Kaiser Permanente Georgia" = 327912200,"Kaiser Permanente Hawaii" = 300267574,"Kaiser Permanente Northwest" = 452412599))

no_dups$Site <- factor(no_dups$d_827220437, levels= c("Baylor Scott and White Health", "HealthPartners", "Henry Ford Health","Marshfield Clinic Health System",
                                                      "Sanford Health", "University of Chicago Medicine","Kaiser Permanente Colorado","Kaiser Permanente Georgia",
                                                      "Kaiser Permanente Hawaii","Kaiser Permanente Northwest")) 



no_dups <- no_dups %>% 
  mutate(
    Site_location = case_when(
      d_951355211 == 777644826 ~ "UC-DCAM",
      d_951355211 == 145191545 ~ "Ingalls Harvey",
      d_951355211 == 489380324 ~ "River East",
      d_951355211 == 120264574 ~ "South Loop",
      d_951355211 == 834825425 ~ "HP Research Clinic",
      d_951355211 == 574368418 ~ "HP Park Nicollet",
      d_951355211 == 736183094 ~ "HFH K-13 Research Clinic",
      d_951355211 == 886364332 ~ "HFH Cancer Pavilion Research Clinic",
      d_951355211 == 706927479 ~ "HFH Livonia Research Clinic",
      d_951355211 == 322059622 ~ "HFH Pop-Up",
      d_951355211 == 692275326 ~ "Marshfield",
      d_951355211 == 813701399 ~ "Weston",
      d_951355211 == 698283667 ~ "Lake Hallie",
      d_951355211 == 691714762 ~ "Rice Lake",
      d_951355211 == 487512085 ~ "Wisconsin Rapids",
      d_951355211 == 983848564 ~ "Colby Abbotsford",
      d_951355211 == 261931804 ~ "Minocqua",
      d_951355211 == 665277300 ~ "Merrill",
      d_951355211 == 589224449 ~ "Sioux Falls Imagenetics",
      d_951355211 == 467088902 ~ "Fargo South University",
      d_951355211 == 567969985 ~ "MF Pop-Up",
      d_951355211 == 319518299 ~ "UCM Pop-Up",
      d_951355211 == 940329442 ~ "Orland Park",
      d_951355211 == 723351427 ~ "BCC- HWC",
      d_951355211 == 807443231 ~ "FW All Saints",
      d_951355211 == 475614532 ~ "BCC- Plano",
      d_951355211 == 809370237 ~ "BCC- Worth St",
      d_951355211 == 856158129 ~ "BCC- Irving",
      d_951355211 == 436956777 ~ "NTX Biorepository",
      d_951355211 == 288564244 ~ "BCC- Fort Worth",
      d_951355211 == 246137578 ~ "Edith Sanford Breast Center (coded as Sanford Center)",
      d_951355211 == 433070901 ~ "Edith Sanford Breast Center",
      d_951355211 == 769594361 ~ "Fargo Amber Valley",
      d_951355211 == 255636184 ~ "Stevens Point",
      is.na(d_951355211) & Site=='University of Chicago Medicine' ~ "Missing UC location",
      is.na(d_951355211) & Site=='HealthPartners' ~ "Missing HP location",
      is.na(d_951355211) & Site=='Marshfield Clinic Health System' ~ "Missing MF location",
      is.na(d_951355211) & Site=='Sanford Health' ~ "Missing SF location",
      is.na(d_951355211) & Site=='Henry Ford Health' ~ "Missing HFH location",
      is.na(d_951355211) & Site=='Baylor Scott and White' ~ "Missing BSWH location",
      TRUE ~ "Unidentified location"
    )
  )



recrver1 <- recrver1 %>%  mutate(Clin_Site_location=case_when(
  Site=="HealthPartners" & (d_173836415_d_266600170_d_185243482 == 2 | d_173836415_d_266600170_d_452847912 == 2) ~ "Elk River",
  Site=="HealthPartners" & (d_173836415_d_266600170_d_185243482 == 3 | d_173836415_d_266600170_d_452847912 == 3) ~ "Chanhassen",
  Site=="HealthPartners" & (d_173836415_d_266600170_d_185243482 == 4 | d_173836415_d_266600170_d_452847912 == 4) ~ "Park Nicollet Minneapolis Clinic",
  d_173836415_d_266600170_d_185243482 == 1050010095 | d_173836415_d_266600170_d_452847912 == 1050010095 |
    d_173836415_d_266600170_d_185243482 == 1050010034 | d_173836415_d_266600170_d_452847912 == 1050010034 ~ "Wyandotte",
  d_173836415_d_266600170_d_185243482 == 1060010063 |
    d_173836415_d_266600170_d_185243482 == 1060170018 |
    d_173836415_d_266600170_d_452847912 == 1060010063 |
    d_173836415_d_266600170_d_452847912 == 1060170018 ~ "Macomb",
  d_173836415_d_266600170_d_185243482 == 1040010047 |
    d_173836415_d_266600170_d_185243482 == 1040010046 |
    d_173836415_d_266600170_d_185243482 == 1011220006 |
    d_173836415_d_266600170_d_452847912 == 1040010047 |
    d_173836415_d_266600170_d_452847912 == 1040010046 |
    d_173836415_d_266600170_d_452847912 == 1011220006 ~ "West Bloomfield",
  d_173836415_d_266600170_d_185243482 == 1011410008 |
    d_173836415_d_266600170_d_452847912 == 1011410008 ~ "Royal Oak",
  d_173836415_d_266600170_d_185243482 == 1010180040 |
    d_173836415_d_266600170_d_452847912 == 1010180040 ~ "Fairlane (Dearborn)",
  d_173836415_d_266600170_d_185243482 == 1010120026 |
    d_173836415_d_266600170_d_452847912 == 1010120026 ~ "Columbus",
  d_173836415_d_266600170_d_185243482 == 1011660013  |
    d_173836415_d_266600170_d_452847912 == 1011660013 ~ "Plymouth",
  d_173836415_d_266600170_d_185243482 == 1010360011 |
    d_173836415_d_266600170_d_452847912 == 1010360011 ~ "Troy",
  d_173836415_d_266600170_d_185243482 == 1010350019 | 
    d_173836415_d_266600170_d_452847912 == 1010350019~ "Taylor",
  d_173836415_d_266600170_d_185243482 == 1010240013 |
    d_173836415_d_266600170_d_452847912 == 1010240013 ~ "Livonia",
  d_173836415_d_266600170_d_185243482 == 1010320019 |
    d_173836415_d_266600170_d_452847912 == 1010320019 ~ "Sterling Heights",
  d_173836415_d_266600170_d_185243482 == 1010010193 |
    d_173836415_d_266600170_d_185243482 == 1010010114 |
    d_173836415_d_266600170_d_452847912 == 1010010193 |
    d_173836415_d_266600170_d_452847912 == 1010010114 ~ "K1 HFH Main",
  d_173836415_d_266600170_d_185243482 == 1010200009 |
    d_173836415_d_266600170_d_452847912 == 1010200009 ~ "Ford Road",
  d_173836415_d_266600170_d_185243482 == 1010270015 |
    d_173836415_d_266600170_d_452847912 == 1010270015 ~ "NCO",
  d_173836415_d_266600170_d_185243482 == 1050020026 | 
    d_173836415_d_266600170_d_452847912 == 1050020026 ~ "Brownstown",
  d_173836415_d_266600170_d_185243482 == 1010170010 | 
    d_173836415_d_266600170_d_452847912 == 1010170010 ~ "E Jefferson Pathology",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==1 | d_173836415_d_266600170_d_452847912==1) ~ "South Loop",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==2 | d_173836415_d_266600170_d_452847912==2) ~ "Orland Park",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==3 | d_173836415_d_266600170_d_452847912==3) ~ "Kenwood",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==4 | d_173836415_d_266600170_d_452847912==4) ~ "River East",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==5 | d_173836415_d_266600170_d_452847912==5) ~ "Dearborn Station",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==6 | d_173836415_d_266600170_d_452847912==6) ~ "Ingalls",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==7 | d_173836415_d_266600170_d_452847912==7) ~ "DCAM (Hyde Park)",
  Site=="University of Chicago Medicine" & (d_173836415_d_266600170_d_185243482==25 | d_173836415_d_266600170_d_452847912==25) ~ "Crown Point",
  Site=="Henry Ford Health" & is.na(d_173836415_d_266600170_d_185243482) & is.na(d_173836415_d_266600170_d_452847912) ~ "Missing HFH location",
  Site=="HealthPartners" & is.na(d_173836415_d_266600170_d_185243482) & is.na(d_173836415_d_266600170_d_452847912) ~ "Missing HP location",
  Site=="University of Chicago Medicine" & is.na(d_173836415_d_266600170_d_185243482) & is.na(d_173836415_d_266600170_d_452847912) ~ "Missing UC location",
  d_173836415_d_266600170_d_185243482==22 | d_173836415_d_266600170_d_452847912==22 |
    d_173836415_d_266600170_d_185243482==24 | d_173836415_d_266600170_d_452847912==24 |
    d_173836415_d_266600170_d_185243482==28 | d_173836415_d_266600170_d_452847912==28 |
    d_173836415_d_266600170_d_185243482==33 | d_173836415_d_266600170_d_452847912==33 |
    d_173836415_d_266600170_d_185243482==34 | d_173836415_d_266600170_d_452847912==34 |
    d_173836415_d_266600170_d_185243482==35 | d_173836415_d_266600170_d_452847912==35 |
    d_173836415_d_266600170_d_185243482==38 | d_173836415_d_266600170_d_452847912==38 |
    d_173836415_d_266600170_d_185243482==40 | d_173836415_d_266600170_d_452847912==40 |
    d_173836415_d_266600170_d_185243482==55 | d_173836415_d_266600170_d_452847912==55 |
    d_173836415_d_266600170_d_185243482==71 | d_173836415_d_266600170_d_452847912==71 ~ "Oahu Clinics",
  d_173836415_d_266600170_d_185243482==23 | d_173836415_d_266600170_d_452847912==23 |
    d_173836415_d_266600170_d_185243482==26 | d_173836415_d_266600170_d_452847912==26 |
    d_173836415_d_266600170_d_185243482==27 | d_173836415_d_266600170_d_452847912==27 |
    d_173836415_d_266600170_d_185243482==32 | d_173836415_d_266600170_d_452847912==32 |
    d_173836415_d_266600170_d_185243482==37 | d_173836415_d_266600170_d_452847912==37 |
    d_173836415_d_266600170_d_185243482==49 | d_173836415_d_266600170_d_452847912==49 |
    d_173836415_d_266600170_d_185243482==56 | d_173836415_d_266600170_d_452847912==56 |
    d_173836415_d_266600170_d_185243482==69 | d_173836415_d_266600170_d_452847912==69 |
    d_173836415_d_266600170_d_185243482==74 | d_173836415_d_266600170_d_452847912==74 |
    d_173836415_d_266600170_d_185243482==76 | d_173836415_d_266600170_d_452847912==76 ~ "Non-Oahu Clinics",
  d_827220437==327912200 ~ "Kaiser Permanente Georgia",
  d_827220437==452412599 ~ "Kaiser Permanente Northwest",
  d_827220437==125001209 ~ "Kaiser Permanente Colorado",
  d_827220437==657167265 ~ "Sanford Health",
  TRUE ~ "Unidentified location"),
  BiospeCollection = ifelse((recrver1$d_878865966 == 353358909 | recrver1$d_167958071 == 353358909 | 
                               recrver1$d_684635302 == 353358909 ), "Any biospecimen Collections","No biospecimen collections"))

##############################################################################################################################




biocol.tb <- left_join(recrver1, no_dups, by=c("Connect_ID", "Site", "token"))


#################### Ad hoc Tables 1-4

## exclude hmw collections
table1 <- biocol.tb %>%
  mutate(BiospeCollection = ifelse((recrver1$d_878865966 == 353358909 | recrver1$d_167958071 == 353358909 |  
                                      (recrver1$d_684635302 == 353358909 & BL_BioSpm_MWSetting_v1r0=="Research") ), "Any biospecimen Collections","No biospecimen collections"), 
         biocol.appoint = case_when(BiospeCollection == "Any biospecimen Collections" & d_410912345== 353358909 ~ "Baseline Collection",
                                    TRUE ~ "No Baseline Collection"))

table1_totals <- create_tbl_cross(table1, "Site", "biocol.appoint", "row", "Site", " ")

biospe.t1 <- knitr::kable(table1_totals, col.names=c("Site", "Baseline Collection","No Baseline Collection", "Total Verified Participants"), caption='Table 1.1: Baseline Biospecimen Collection Status Among Verified Participants', row.names=FALSE,align=c("l","c","c","c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(table1_totals) - 1)) %>% 
  kable_styling(latex_options = "scale_down")



#adhoc1- hf before
HF1 <- table1 %>%  filter(Site=="Henry Ford Health" & as.Date(d_914594314)<="2023-07-03") %>% mutate(Site=droplevels(Site))
tablehf1_totals <- create_tbl_cross(HF1, "Site", "biocol.appoint", "row", "Site", " ")

biospe.t.1 <- knitr::kable(tablehf1_totals, col.names=c("Site", "Baseline Collection", "No Baseline Collection","Total Verified Participants"), 
                           caption='Ad Hoc 1- Baseline Biospecimen Collection Status Before HFH Clinical Launch', row.names=FALSE,align=c("l","c","c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(tablehf1_totals) - 1)) %>% 
  kable_styling(latex_options = "scale_down")




#adhoc2- hf after
HF2 <- table1 %>%  filter(Site=="Henry Ford Health" & as.Date(d_914594314)>"2023-07-03" & as.Date(d_914594314)<(currentDate-30)) %>% mutate(Site=droplevels(Site))
tablehf2_totals <- create_tbl_cross(HF2, "Site", "biocol.appoint", "row", "Site", " ")

biospe.t.2 <- knitr::kable(tablehf2_totals, col.names=c("Site", "Baseline Collection","No Baseline Collection", "Total Verified Participants"), 
                           caption='Ad Hoc 2- Baseline Biospecimen Collection Status After HFH Clinical Launch (7/3/2023)', row.names=FALSE,align=c("l","c","c","c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(tablehf2_totals) - 1)) %>% 
  kable_styling(latex_options = "scale_down")




#adho3- sf before
SF1 <- table1 %>%  filter(Site=="Sanford Health"  & as.Date(d_914594314)<="2024-08-25") %>% mutate(Site=droplevels(Site))
tablesf1_totals <- create_tbl_cross(SF1, "Site", "biocol.appoint", "row", "Site", " ")

biospe.t.3 <- knitr::kable(tablesf1_totals, col.names=c("Site", "Baseline Collection","No Baseline Collection", "Total Verified Participants"), 
                           caption='Ad Hoc 3- Baseline Biospecimen Collection Status Before SF Clinical Launch', row.names=FALSE,align=c("l","c","c","c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(tablesf1_totals) - 1)) %>% 
  kable_styling(latex_options = "scale_down")




#adhoc4- SF after
SF2 <- table1 %>%  filter(Site=="Sanford Health"  & as.Date(d_914594314)>"2024-08-25" & as.Date(d_914594314)<(currentDate-30)) %>% mutate(Site=droplevels(Site))
tablesf2_totals <- create_tbl_cross(SF2, "Site", "biocol.appoint", "row", "Site", " ")

biospe.t.4 <- knitr::kable(tablesf2_totals, col.names=c("Site", "Baseline Collection","No Baseline Collection", "Total Verified Participants"), 
                           caption='Ad Hoc 4- Baseline Biospecimen Collection Status After SF Clinical Launch (8/25/2024)', row.names=FALSE,align=c("l","c","c","c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(tablesf2_totals) - 1)) %>% 
  kable_styling(latex_options = "scale_down")





biospe.t.1  %>% kable_styling(latex_options = "scale_down") 
biospe.t.2  %>% kable_styling(latex_options = "scale_down") %>%  footnote(general = "Note: Limited to participants verified one more or more ago", general_title = "") 
biospe.t.3  %>% kable_styling(latex_options = "scale_down")
biospe.t.4  %>% kable_styling(latex_options = "scale_down") %>% footnote(general = "Note: Limited to participants verified one more or more ago", general_title = "")








