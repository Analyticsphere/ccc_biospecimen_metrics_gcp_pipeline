


library(bigrquery)
library(data.table)
library(dplyr)
library(rio)
library(glue)
#install.packages("openxlsx")
library(openxlsx)
bq_auth()
2



boxfolder <- 221280601453
currentDate <- Sys.Date()


project <- "nih-nci-dceg-connect-prod-6d04"


##Select the Biospecimen Data
bio_tb <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen_JP`") 
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




##Select biospecimen data in recruitment 
recr_var <- bq_project_query(project, query="SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect`.INFORMATION_SCHEMA.COLUMN_FIELD_PATHS WHERE table_name='participants_JP'")
recrvar <- bigrquery::bq_table_download(recr_var, bigint="integer64",n_max = Inf, page_size = 10000)
urlfile<- "https://raw.githubusercontent.com/episphere/conceptGithubActions/master/csv/masterFile.csv" ###to grab the updated dd from github 
y <- read.csv(urlfile)
recrvar_nd <- recrvar[which(grepl("d_",recrvar$column_name)),c("column_name","field_path")]

recrvar_nd$last.CID <- ifelse(grepl("d_",recrvar_nd$column_name),substring(sapply(strsplit(recrvar_nd$column_name,"d_"),tail,1),1,9),NA)
recrvar_nd_label <- base::merge(recrvar_nd, y[,c("Primary.Source","conceptId.2","conceptId.3","Variable.Label","conceptId.4","Current.Format.Value")],by.x="last.CID",by.y="conceptId.3",all.x=TRUE)


recrvar.bio <- recrvar_nd_label[which(recrvar_nd_label$Primary.Source =="Biospecimen" | grepl("biospecimen|blood|urine|mouthwash|collection|Blood|Urine|Mouthwash|MW|Ur|Ur Surv|MW Surv",recrvar_nd_label$Variable.Label)),] #49
#query <- unique(recrvar.bio$column_name)
select <- paste(recrvar.bio$column_name,collapse=",")


tb_bq <- eval(parse(text=paste("bq_project_query(project, query=\"SELECT token,Connect_ID,d_512820379,d_821247024,d_914594314,d_827220437,d_699625233, d_265193023, d_822499427, d_222161762, d_254109640,", select,
                               "FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` Where d_821247024 = '197316935' \")",sep=" "))) #removed "date", as it is no longer in the participants table


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


tubes <- unique(y[grepl(" Tube ",y$`Secondary.Source`),c("conceptId.1","Secondary.Source")])
urine <- dd[grepl("Urine",dd$`Variable Label`),]
mouthwash <- dd[grepl("Mouthwash",dd$`Variable Label`),]


#################################################################################################################












###########################################     CSV1:      flatBoxes     #############################################


###download the biospecimen data
box_CID <- as.data.frame(as.numeric(sapply((strsplit(colnames(box_wl_flat),"d_")),tail,1)))
box_CID$variable <- colnames(box_wl_flat)
colnames(box_CID)[1] <-"CID"
box_CID$CID <- ifelse(is.na(box_CID$CID), 0, box_CID$CID)
box_CID_dd <- base::merge(box_CID, y[,c("Primary.Source","conceptId.2","conceptId.3","Variable.Name","Variable.Label","conceptId.4","Current.Format.Value")],by.x="CID",by.y="conceptId.3",all.x=TRUE)
box_CID_dd$Variable.Name <- ifelse(is.na(box_CID_dd$Variable.Name), box_CID_dd$variable,box_CID_dd$Variable.Name)
box_CID_dd$category<- ifelse(is.na(box_CID_dd$conceptId.4), 0,ifelse(!is.na(box_CID_dd$conceptId.4) &  box_CID_dd$conceptId.4 !=104430631, 2, 1))
select1 <- box_CID_dd$variable[which(box_CID_dd$category==0)]
yes_no <- box_CID_dd$variable[which(box_CID_dd$category==1)]
box1 <- subset(box_wl_flat,select=select1)
colnames(box1) <- box_CID_dd$Variable.Name[which(box_CID_dd$category==0)]
##for the categorical variables
box1$BioPack_Courier_v1r0 <- ifelse(box_wl_flat$d_666553960==712278213, "FedEx",ifelse(box_wl_flat$d_666553960==149772928, "World Courier",NA))
box_wl_flat$d_560975149 <- as.factor(box_wl_flat$d_560975149)
d_560975149_CIDs <- as.data.frame(cbind(unique(y$conceptId.4[grepl(paste(levels(box_wl_flat$d_560975149),collapse="|"),y$conceptId.4)]),unique(trimws(sapply(strsplit(y$Current.Format.Value[grepl(paste(levels(box_wl_flat$d_560975149),collapse="|"),y$conceptId.4)], "="),tail,1)))))
box1$d_560975149 <- plyr::mapvalues(box_wl_flat$d_560975149,from=d_560975149_CIDs$V1,to=d_560975149_CIDs$V2)
colnames(box1)[which(colnames(box1)=="d_560975149") ]<- box_CID_dd$Variable.Name[which(box_CID_dd$variable=="d_560975149")]
box_wl_flat$d_789843387 <- as.factor(box_wl_flat$d_789843387)
d_789843387_CIDs <- as.data.frame(cbind(unique(y$conceptId.4[grepl(paste(levels(box_wl_flat$d_789843387),collapse="|"),y$conceptId.4)]),unique(trimws(sapply(strsplit(y$Current.Format.Value[grepl(paste(levels(box_wl_flat$d_789843387),collapse="|"),y$conceptId.4)], "="),tail,1)))))

box1$d_789843387 <- plyr::mapvalues(box_wl_flat$d_789843387,from=d_789843387_CIDs$V1,to=d_789843387_CIDs$V2)
colnames(box1)[which(colnames(box1)=="d_789843387") ]<- box_CID_dd$Variable.Name[which(box_CID_dd$variable=="d_789843387")]
##for binary variables
for (i in 1:length(yes_no)){
  x <- yes_no[i]
  
  varname <- box_CID_dd$Variable.Name[grepl(x,box_CID_dd$variable)]
  #type.labels <- dd$`Variable Label`[grepl(CID,dd$CID)]
  check <- as.data.frame(box_wl_flat[,grepl(x, colnames(box_wl_flat))])
  check$variable <- ifelse(check[,1]== 353358909, "Yes",ifelse(check[,1]==104430631,"No",NA))
  colnames(check)[2] <- varname
  box1 <-   cbind(box1,subset(check,select=varname))
}
#gsub("[^[:alnum:][:blank:]+?&/\\-]", "", c) #to remove any specific character in a variable in r
box1 <- box1  %>% mutate(d_238268405=gsub("^[:alnum:][:blank:]","",d_238268405),
                         d_238268405_1=case_when(substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"121149986"~"Crushed",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"200183516" ~"Vials - Incorrect Material Type",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"289322354" ~ "Material Thawed",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"387564837" ~ "Damaged Vials",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"399948893" ~ "Vials - Missing Labels",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"405513630" ~ "Cold Packs - none",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"442684673" ~ "Participant Refusal",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"595987358" ~ "Cold Packs - warm",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"613022284" ~ "No Refrigerant",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"631290535" ~ "Vials - Empty",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"678483571" ~ "Damaged Container (outer and/or inner)",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"679749262" ~ "Package in good condition",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"842171722" ~ "No Pre-notification",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"847410060" ~ "Improper Packaging",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"853876696" ~ "Manifest - not provided",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	 "909529446" ~ "Cold Packs - insufficient",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"922995819" ~ "Manifest/Paperwork/Vial information do not match",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"933646000" ~ "Other- Package Condition",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),2,10)==	"958000780" ~ "Shipment Delay") ,
                         d_238268405_2=case_when(substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"121149986"~"Crushed",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"200183516" ~"Vials - Incorrect Material Type",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"289322354" ~ "Material Thawed",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"387564837" ~ "Damaged Vials",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"399948893" ~ "Vials - Missing Labels",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"405513630" ~ "Cold Packs - none",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"442684673" ~ "Participant Refusal",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"595987358" ~ "Cold Packs - warm",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"613022284" ~ "No Refrigerant",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"631290535" ~ "Vials - Empty",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"678483571" ~ "Damaged Container (outer and/or inner)",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"679749262" ~ "Package in good condition",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"842171722" ~ "No Pre-notification",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"847410060" ~ "Improper Packaging",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"853876696" ~ "Manifest - not provided",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	 "909529446" ~ "Cold Packs - insufficient",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"922995819" ~ "Manifest/Paperwork/Vial information do not match",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"933646000" ~ "Other- Package Condition",
                                                 substring(gsub("^[:alnum:][:blank:]","",d_238268405),12,20)==	"958000780" ~ "Shipment Delay"))
box1<- box1 %>% mutate(BioPack_ShipCondtion_v1r0=ifelse(is.na(d_238268405_2), d_238268405_1, paste(d_238268405_1,d_238268405_2,sep=",")))
#box1 <- box1[, c(1:4, 6:22, 25)] --- why are some columns missing???
#missing: BioPack_BoxID_v1r0	BioPack_ModifiedTime_v1r0	BioShip_ShipTime_v1r0	BioPack_BoxStrtTime_v1r0	BioBPTL_ShipComments_v1r0	BioBPTL_DateRec_v1r0	BioShip_SignEmail_v1r0	BioPack_TrackScan1_v1r0		BioBPTL_ShipRec_v1r0
box1$BioPack_BoxID_v1r0 <- box_wl_flat$d_132929440
box1$BioPack_ModifiedTime_v1r0 <- box_wl_flat$d_555611076
box1$BioShip_ShipTime_v1r0 <- box_wl_flat$d_656548982
box1$BioPack_BoxStrtTime_v1r0 <- box_wl_flat$d_672863981
box1$BioBPTL_ShipComments_v1r0 <- box_wl_flat$d_870456401
box1$BioBPTL_DateRec_v1r0 <- box_wl_flat$d_926457119
box1$BioShip_SignEmail_v1r0 <- box_wl_flat$d_948887825
box1$BioPack_TrackScan1_v1r0 <- box_wl_flat$d_959708259
box1$BioBPTL_ShipRec_v1r0 <- box_wl_flat$d_333524031 #check for yes/no structuring
box1 <- box1 %>%  filter(as.Date(BioShip_ShipTime_v1r0) >= as.Date(currentDate - 31)) %>% #limiting to the last month 
  mutate(BioBPTL_ShipRec_v1r0= case_when(BioBPTL_ShipRec_v1r0==353358909 ~ "Yes",
                                                         BioBPTL_ShipRec_v1r0==104430631  ~ "No"))
box1 <- box1 %>%  select(bagID,	bagType,	tubeID,	BioPack_BoxID_v1r0,	BioPack_ModifiedTime_v1r0,	BioShip_ShipTime_v1r0,	BioPack_BoxStrtTime_v1r0,	BioBPTL_ShipComments_v1r0,	d_885486943,
                         BioBPTL_DateRec_v1r0,	BioShip_SignEmail_v1r0,	BioPack_TrackScan1_v1r0,	BioPack_Courier_v1r0,	BioShip_LocalID_v1r0,	BioShip_LogSite_v1r0,	BioPack_TempProbe_v1r0,
                         BioShip_ShipSubmit_v1r0,	BioPack_OrphanBag_v1r0,	BioBPTL_ShipRec_v1r0,	BioPack_ContainsOrphan_v1r0,	BioPack_ShipCondtion_v1r0)
box1 <- box1[, c(1:8, 10:21)]
openxlsx::write.xlsx(box1,glue("Formatted_prod_flatBoxes_JP_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")







###########################################     CSV 2: Biospe_Formats                 ###############################################
#11/20 updates
###to get the formats and variable names from DD
factor_cid <-function(var,data){
  var <- as.factor(var)
  var_CIDs <- as.data.frame(cbind(unique(y$conceptId.4[grepl(paste(levels(var),collapse="|"),y$conceptId.4)]),unique(trimws(sapply(strsplit(y$Current.Format.Value[grepl(paste(levels(var),collapse="|"),y$conceptId.4)], "="),tail,1)))))
  
  var <- plyr::mapvalues(var,from=var_CIDs$V1,to=var_CIDs$V2)
}





cnames <- names(biospe)
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(biospe,varname)
  biospe[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}






biospe_CID <- as.data.frame(sapply((strsplit(colnames(biospe),"d_")),tail,1))
colnames(biospe_CID)[1] <- "CID"

biospe_CID$variable <- names(biospe)
y$conceptId.3 <- as.character(y$conceptId.3)
y$conceptId.4 <- as.numeric(y$conceptId.4)

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



biospe_CID_dd1 <- biospe_CID_dd[!duplicated(biospe_CID_dd[,c("CID","variable")]),]
biospe_CID_dd1$new.varname <- ifelse(biospe_CID_dd1$matched==1, biospe_CID_dd1$Variable.Name, paste(gsub("SST1","",biospe_CID_dd1$Variable.Name),biospe_CID_dd1$label.1st,sep="_"))
biospe_CID_dd1$new.varname[which(biospe_CID_dd1$variable=="d_926457119")] <- "BioBPTL_DateRec_v1r0"
biospe_CID_dd1$variable[is.na(biospe_CID_dd1$new.varname)]

biospe1 <- NULL
select0 <- biospe_CID_dd1$variable[which(biospe_CID_dd1$category==0 & !is.na(biospe_CID_dd1$Variable.Name))]
select2 <- biospe_CID_dd1$variable[which(biospe_CID_dd1$category==2)]
yes_no <- biospe_CID_dd1$variable[which(biospe_CID_dd1$category==1)]
biospe1 <- subset(biospe,select =c("Connect_ID","token","siteAcronym",select0,select2)) #removed "date", as it is no longer in the participants table
n<- length(select0)
colnames(biospe1)[which(colnames(biospe1) %in% select0)] <-  biospe_CID_dd1$new.varname[which(biospe_CID_dd1$category==0 & !is.na(biospe_CID_dd1$Variable.Name))]



for (i in 1: length(select2)){
  eval(parse(text=paste("biospe1$",select2[i],"<-factor_cid(biospe1$",select2[i],")",sep="")))
}
colnames(biospe1)[which(colnames(biospe1) %in% select2)] <-  biospe_CID_dd1$new.varname[which(biospe_CID_dd1$category==2)]

for (i in 1:length(yes_no)){
  x <- yes_no[i]
  
  varname <- biospe_CID_dd1[grepl(yes_no[i],biospe_CID_dd1$variable) & biospe_CID_dd1$category==1,]$new.varname
  
  #type.labels <- dd$`Variable Label`[grepl(CID,dd$CID)]
  check <- as.data.frame(biospe[,x])
  check$variable <- ifelse(check[,1]== 353358909, "Yes",ifelse(check[,1]==104430631,"No",NA))
  colnames(check)[2] <- varname
  biospe1 <-   cbind(biospe1,subset(check,select=varname))
}


colnames(biospe1) <- gsub("Object|Obj","Tube", colnames(biospe1))
colnames(biospe1) <- gsub("BioCol_Dev_v1r0","Deviation",colnames(biospe1))
biospe1 <- biospe1[,order(colnames(biospe1))]



names_set <- c("Connect_ID","RcrtES_Site_v1r0","BioSpm_Visit_v1r0","BioSpm_Setting_v1r0","BioSpm_Location_v1r0","BioSpm_ColIDScan_v1r0",
               "BioReg_ArRegTime_v1r0", "BioCol_ColTime_v1r0", "BioRec_CollectFinal_v1r0","BioRec_CollectFinalTime_v1r0","BioClin_DBBloodID_v1r0",
               "BioClin_DBBloodRRLBL_v1r0",
               "BioClin_DBBloodRRLDtBL_v1r0",
               "BioClin_DBUrineID_v1r0",
               "BioClin_DBUrineRRLBL_v1r0",
               "BioClin_DBUrineRRLDtBL_v1r0",
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
               "BioCol_StrayTubesList_v1r0",
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
               "token")


biospe1_final <- biospe1 %>% select(names_set)

openxlsx::write.xlsx(biospe1_final,glue("Connect_prod_Biospe_Formats_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")















#########################################           CSV3:  veriBiospe_Formats            ##########################################################

kitstatus <- "SELECT Connect_ID, d_173836415_d_266600170_d_319972665_d_379252329, d_173836415_d_266600170_d_319972665_d_221592017, 
d_173836415_d_266600170_d_319972665_d_661940160, d_265193023, d_547363263, d_173836415_d_266600170_d_448660695, d_195145666, 
d_286191859, d_123868967, d_906417725, d_747006172, d_987563196, d_167958071, d_878865966, d_684635302, d_827220437, 
d_173836415_d_266600170_d_915179629, d_914594314, d_173836415_d_266600170_d_982213346,d_173836415_d_266600170_d_139245758, 
d_265193023, d_253883960, d_173836415_d_266600170_d_319972665_d_826941471 
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` 
where Connect_ID IS NOT NULL and d_906417725='104430631'  and d_747006172='104430631' and d_987563196='104430631'"
kitstatus_table <- bq_project_query(project, kitstatus)
kit_table <- bq_table_download(kitstatus_table, bigint="integer64",n_max = Inf, page_size = 10000)


#Adding mw variables to this csv
mw_addons <- kit_table %>% select(Connect_ID, d_173836415_d_266600170_d_319972665_d_221592017, d_173836415_d_266600170_d_319972665_d_379252329, d_173836415_d_266600170_d_319972665_d_661940160, d_173836415_d_266600170_d_319972665_d_826941471)

recr.bio$Connect_ID <- as.character(recr.bio$Connect_ID)
mw_addons$Connect_ID <- as.character(mw_addons$Connect_ID)
recrver$Connect_ID <- as.character(recrver$Connect_ID)


recr_mw <- left_join(recrver, mw_addons, by="Connect_ID")

recr.bio_mw <- left_join(recr.bio, mw_addons, by="Connect_ID")

y$conceptId.4 <- as.numeric(y$conceptId.4)

recrver_CID <- as.data.frame(substring(sapply((strsplit(colnames(recr_mw),"d_")),tail,1),1,9))

colnames(recrver_CID)[1] <- "CID"

recrver_CID$variable <- names(recr.bio_mw)



y$order <- rownames(y)



recrvar_CID_dd <- base::merge(recrver_CID, y[,c("Primary.Source","conceptId.2","conceptId.3","Variable.Name","Variable.Label","conceptId.4","Current.Format.Value","order")],by.x="CID",by.y="conceptId.3",all.x=TRUE)

recrvar_CID_dd <-recrvar_CID_dd %>% arrange(CID, variable,as.numeric(order),Variable.Name) #before remove the duplicates


recrvar_CID_dd <-recrvar_CID_dd[!duplicated(recrvar_CID_dd[,c("CID","variable")]),]






recrvar_CID_dd <- recrvar_CID_dd %>% mutate(category = case_when(conceptId.4 == 104430631~1,
                                                                 
                                                                 !is.na(conceptId.4) & conceptId.4 !=104430631 ~ 2,
                                                                 
                                                                 is.na(conceptId.4) ~0),
                                            
                                            Variable.Name=ifelse(is.na(Variable.Name), variable, Variable.Name))





recrbio1 <- NULL

select0 <- recrvar_CID_dd$variable[which(recrvar_CID_dd$category==0)]

select2 <- recrvar_CID_dd$variable[which(recrvar_CID_dd$category==2)]

yes_no <- recrvar_CID_dd$variable[which(recrvar_CID_dd$category==1)]

recrbio1 <- subset(recr.bio_mw,select =c(select0,select2))

colnames(recrbio1)[which(colnames(recrbio1) %in% select0)] <-  recrvar_CID_dd$Variable.Name[which(recrvar_CID_dd$category==0)]

n<- length(select0)

factor_cid <-function(var){
  
  var <- as.factor(var)
  
  var_CIDs <- as.data.frame(cbind(unique(y$conceptId.4[grepl(paste(levels(var),collapse="|"),y$conceptId.4)]),unique(trimws(sapply(strsplit(y$Current.Format.Value[grepl(paste(levels(var),collapse="|"),y$conceptId.4)], "="),tail,1)))))
  
  
  
  
  var <- plyr::mapvalues(var,from=var_CIDs$V1,to=var_CIDs$V2)
  
}




for (i in 1: length(select2)){
  
  eval(parse(text=paste("recrbio1$",select2[i],"<- factor_cid(recrbio1$", select2[i],")",sep="")))
  
  colnames(recrbio1)[n+i] <- recrvar_CID_dd$Variable.Name[grepl(select2[i],recrvar_CID_dd$variable)]
  
}




for (i in 1:length(yes_no)){
  
  x <- yes_no[i]
  
  varname <- recrvar_CID_dd$Variable.Name[grepl(x,recrvar_CID_dd$variable)]
  
  #type.labels <- dd$`Variable Label`[grepl(CID,dd$CID)]
  
  check <- as.data.frame(recr.bio_mw[,grepl(x, colnames(recr.bio_mw))])
  
  #check <-pull(recr.bio_mw,varname)
  
  check$variable <- ifelse(check[,1]== 353358909, "Yes",ifelse(check[,1]==104430631,"No",NA))
  
  colnames(check)[2] <- varname
  
  recrbio1 <-   cbind(recrbio1,subset(check,select=varname))
  
}


#need to make sure BioChk_CompleteBL_v1r0 is not in their twice--cols 39 & 40 right now
recrbio_1 <- recrbio1[!duplicated(names(recrbio1))]


#new column order requested 11/22/23
recrbio_1 <- recrbio_1[, c("Connect_ID","RcrtES_Site_v1r0","BioFin_BaseBloodCol_v1r0","BioFin_BaseUrineCol_v1r0","BioFin_BaseMouthCol_v1r0","BioSpm_BloodSettingBL_v1r0",
                           "BioSpm_UrineSettingBL_v1r0","BioSpm_MWSettingBL_v1r0","BioChk_CompleteBL_v1r0","BioChk_TimeBL_v1r0","BioFin_CheckOutTmBL_v1r0","BioFin_ResearchBldTmBL_v1r0",
                           "BioFin_ResearchUrnTmBL_v1r0","BioFin_BMTimeBL_v1r0", "SMMet_BLSamplesColl_v1r0","BioClin_SiteBldLocBL_v1r0","BioClin_SiteUrLocatBL_v1r0","BioClin_SntBloodAccIDBL_v1r0",
                           "BioClin_SntUrineAccIDBL_v1r0", "BioClin_PolyBloodIDBL_v1r0","BioClin_PolyUrineIDBL_v1r0","BioClin_SiteBloodCollBL_v1r0","BioClin_ClinBloodTmBL_v1r0",
                           "BioClin_SiteUrineCollBL_v1r0", "BioClin_ClinicalUrnTmBL_v1r0","BioClin_SiteBloodRRLBL_v1r0","BioClin_SiteBldRRLDtBL_v1r0","BioClin_SiteUrineRRLBL_v1r0",
                           "BioClin_SiteUrnRRLDtBL_v1r0", "BioClin_BldOrUrnPlcdBL_v1r0","BioClin_BldUrnPlcdTmBL_v1r0","BioClin_BldOrderPlcdBL_v1r0","BioClin_BldOrdPlacdDtBL_v1r0",
                           "BioClin_UrnOrdPlacedBL_v1r0", "BioClin_UrnOrdPlcdDtBL_v1r0","BioClin_DBBloodRRLBL_v1r0","BioClin_DBBloodRRLDtBL_v1r0","BioClin_DBUrineRRLBL_v1r0",
                           "BioClin_DBUrineRRLDtBL_v1r0","BioClin_AnySpecRRLBL_v1r0", "BioClin_AnySpecRRLTmBL_v1r0","BioClin_AnyBldUrnRecBL_v1r0","RcrtSI_RecruitType_v1r0",
                           "RcrtUP_Submitted_v1r0","RcrtV_Verification_v1r0","RcrtV_VerificationTm_V1R0", "SrvBLM_ResSrvCompl_v1r0","SrvBLM_TmComplete_v1r0","SrvBLM_TmStart_v1r0",
                           "SrvBlU_BaseComplete_v1r0","SrvBlU_TmComplete_v1r0","SrvBlU_TmStart_v1r0",
                           "BioClin_BldAndUrnRef_v1r0", "HdRef_Baseblood_v1r0","HdRef_DateBaseblood_v1r0",
                           "SrvMtW_TmComplete_v1r0", "SrvMtW_TmStart_v1r0", "SrvMtW_BaseComplete_v1r0", 
                           "BioKit_KitTypeBL_v1r0" , "BioKit_KitStatusBL_v1r0", "BioKit_KitShipTmBL_v1r0", "BioKit_KitRecdTmBL_v1r0", "token")]
#BioClin_DBUrineRRLBL_v1r0    showing as null but in the DD 
#"BioClin_PolyBloodIDBL_v1r0" "BioClin_PolyUrineIDBL_v1r0" showing as not in this df but variable name hasn't changed in DD


# #For some reason the yes/no of some columns are changing back to CIDs
recrbio_1[recrbio_1== as.character("353358909")] = "Yes"
recrbio_1[recrbio_1== as.character("104430631")] = "No"
# 
# recrbio1.replace({'353358909': 'Yes'}, regex=True, inplace=True)


# write.xlsx(recrbio1,paste(local_drive, "Connect_prod_recr_veriBiospe_Formats_",currentDate,".csv",sep=""),row.names = F,na="")
openxlsx::write.xlsx(recrbio_1,glue("Connect_prod_recr_veriBiospe_Formats_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")







############################    CSV4:  KitAssembly_Table    ####################################################

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



FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.kitAssembly_JP` 

where Connect_ID is not null"
kit_A_table <- bq_project_query(project, kitA)
kitA_table <- bq_table_download(kit_A_table, bigint="integer64",n_max = Inf, page_size = 10000)

#To remove any parenthases and avoid sceintific notation
#options(scipen = 999)
#library(bit64)
kitA_table$BioKit_ReturnKitTrack_v1r0 <- gsub("[()]", "", kitA_table$BioKit_ReturnKitTrack_v1r0)
#kitA_table$BioKit_SupplyKitTrack_v1r0 <- as.numeric(kitA_table$BioKit_SupplyKitTrack_v1r0)
kitA_table$BioKit_ReturnKitTrack_v1r0 <- as.numeric(kitA_table$BioKit_ReturnKitTrack_v1r0)


openxlsx::write.xlsx(kitA_table,glue("Connect_prod_KitAssembly_Table_{currentDate}_boxfolder_{boxfolder}.xlsx"),row.names = F,na="")


