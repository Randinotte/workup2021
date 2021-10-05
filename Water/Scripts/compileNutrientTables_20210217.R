#compileNutrientsTables used to make SAMPLE, SITE, and TABLE tables for Nutrients 
#Need to be combined for plottingNutrientsForDataQC.R
#RNN 2020-03-02 from KS's plotting nutrients for data QC.R

## load data and connect multiple files
rm(list=ls())  #clear variables
library(ggplot2)
library(lubridate)

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("C:/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
dir<-'Water/Output/compiledData'


#read in final data files for each analysis (make sure these are done, not missing any)
tp<-read.csv("Water/Output/compiledData/tp_20210216.csv",header=TRUE,stringsAsFactors = FALSE)
srp<-read.csv("Water/Output/compiledData/srp_20210217.csv",header=TRUE,stringsAsFactors = FALSE)
tn<-read.csv("Water/Output/compiledData/tn_20210216.csv",header=TRUE,stringsAsFactors = FALSE)
no3<-read.csv("Water/Output/compiledData/no3_20210217.csv",header=TRUE,stringsAsFactors = FALSE)
pop<-read.csv("Water/Output/compiledData/POP_merged_20210219.csv",header=TRUE,stringsAsFactors = F)
poc<-read.csv("Water/Output/compiledData/poc_merged_20210220.csv",header=TRUE,stringsAsFactors = F)

###tableNutrients_YYYYMMDD###
colnames(dbTable("WATER_CHEM"))
dbNut<-dbTable("WATER_CHEM")
checkCols(dbNut,tp)

#QC Group A: Nutrients
#organize columns
colnames(tp)
tp$parameter<-"TP"
tp$metadataID<-"Nutrients.20110601"
tp$updateID<-"tableNutrients.20210217"
tp$flag<-0
tp$QCcode<-NA
colnames(tp)[colnames(tp)=='TP']<-'parameterValue'
colnames(tp)[colnames(tp)=='site']<-'siteName'
tp$dateTimeSample<- parse_date_time(paste(tp$dateSample,tp$timeSample,sep=" "), orders = c("Y-m-d H:M", "m/d/Y H:M"), tz = "America/Chicago")
tp$dateSample<- as.Date(tp$dateTimeSample)
tp<-tp[colnames(dbNut)]

checkCols(dbNut,srp)
srp$parameter<-"SRP"
srp$metadataID<-"Nutrients.20110601"
srp$updateID<-"tableNutrients.20210217"
srp$flag<-0
srp$QCcode<-NA
colnames(srp)[colnames(srp)=='SRP']<-'parameterValue'
colnames(srp)[colnames(srp)=='site']<-'siteName'
srp$dateTimeSample<- parse_date_time(paste(srp$dateSample,srp$timeSample,sep=" "), orders = c("Y-m-d H:M", "m/d/Y H:M"), tz = "America/Chicago")
srp$dateSample<- as.Date(srp$dateTimeSample)
srp<-srp[colnames(dbNut)]

checkCols(dbNut,tn)
tn$parameter<-"TN"
tn$metadataID<-"Nutrients.20110601"
tn$updateID<-"tableNutrients.20210217"
tn$flag<-0
tn$QCcode<-NA
colnames(tn)[colnames(tn)=='TN']<-'parameterValue'
colnames(tn)[colnames(tn)=='site']<-'siteName'
tn$dateTimeSample<- parse_date_time(paste(tn$dateSample,tn$timeSample,sep=" "), orders = c("Y-m-d H:M", "m/d/Y H:M"), tz = "America/Chicago")
tn$dateSample<- as.Date(tn$dateTimeSample)
tn<-tn[colnames(dbNut)]

checkCols(dbNut,no3)
no3$parameter<-"nitrate"
no3$metadataID<-"Nutrients.20110601"
no3$updateID<-"tableNutrients.20210217"
no3$flag<-0
no3$QCcode<- NA
colnames(no3)[colnames(no3)=='NO3']<-'parameterValue'
colnames(no3)[colnames(no3)=='site']<-'siteName'
no3$dateTimeSample<-as.POSIXct(paste(no3$dateSample,no3$timeSample,sep=" "),format="%m/%d/%Y %H:%M")
no3$dateSample<- as.Date(no3$dateTimeSample)
no3<-no3[colnames(dbNut)]

#QC Group B: Particulates
checkCols(dbNut,pop)
pop$parameter<-"POP"
pop$metadataID<-"Nutrients.20110601"
pop$updateID<-"tableNutrients.20210217"
pop$flag<-0
pop$QCcode<-NA
colnames(pop)[colnames(pop)=='POP']<-'parameterValue'
colnames(pop)[colnames(pop)=='site']<-'siteName'
pop$dateTimeSample<- parse_date_time(paste(pop$dateSample,pop$timeSample,sep=" "), orders = c("Y-m-d H:M", "m/d/Y H:M"), tz = "America/Chicago")
pop$dateSample<- as.Date(pop$dateTimeSample)
pop<-pop[colnames(dbNut)]

checkCols(dbNut,poc)
poc<-poc[,colnames(dbNut)]
checkCols(pop,poc)

#bind groups for QC csv's (input for plottingNutrientsForDataQC.R)
newdataNut<-rbind(tp,srp,tn,no3)
newdataPart<-rbind(pop,poc)
newdata<-bind_rows(newdataNut,newdataPart) %>% arrange(dateSample,sampleID)


### !!! ###
write.csv(newdataNut,file.path(dir,"nutrients_20210217.csv"),row.names=FALSE)
write.csv(newdataPart,file.path(dir,"particulates_20210217.csv"),row.names=FALSE)
write.csv(newdata,file.path(dir,"tableNutrients_20210217.csv"),row.names=FALSE)



#making a temp POC/PON Long Lake file for Bella (2019-2020 in new WATER_CHEM format)
longPart<-newdataPart %>% 
  filter(lakeID %in% c("FE", "WL")) %>% 
  mutate(parameter = case_when(parameter == "POP" ~ "particulateP", TRUE~parameter))
myQC<-read.csv("Water/Output/checkCSVs/particulates_check_RandiPass.csv",header = T,stringsAsFactors = F)
myQC<- myQC %>% filter(updateID=="tableNutrients.20210217" 
                       & lakeID %in% c("FE","WL") 
                       & flag==1)

i=13
for (i in 1:nrow(myQC)){
  longPart$flag[longPart$sampleID==myQC$sampleID[i] & longPart$parameter==myQC$parameter[i]]<-myQC$flag[i]
  longPart$comments[longPart$sampleID==myQC$sampleID[i] & longPart$parameter==myQC$parameter[i]]<-"Will Rerun before database entry. Outside of normal range."
}

for(i in 1:nrow(longPart)){
  if(is.na(longPart$flag[i])){
    longPart$flag[i]<-"0"
  }
}

longPart$parameterValue<-round(longPart$parameterValue,digits = 3)
write.csv(longPart,"Water/Output/compiledData/particulate_forBella.20200220.csv",row.names = F)


#making a temp Nutrients Long Lake file for Presentation (2019-2020 in new WATER_CHEM format)
longNut<-newdataNut %>% 
  filter(lakeID %in% c("FE", "WL"))
myQC<-read.csv("Water/Output/checkCSVs/nutrients_check_RandiPass.csv",header = T,stringsAsFactors = F)
myQC<- myQC %>% filter(updateID=="tableNutrients.20210217" 
                       & lakeID %in% c("FE","WL") 
                       & flag %in% c(1,2))

i=13
for (i in 1:nrow(myQC)){
  longNut$flag[longNut$sampleID==myQC$sampleID[i] & longNut$parameter==myQC$parameter[i]]<-myQC$flag[i]
  longNut$comments[longNut$sampleID==myQC$sampleID[i] & longNut$parameter==myQC$parameter[i]]<-myQC$RandiComments[i]
}


#longNut$parameterValue<-round(longNut$parameterValue,digits = 3)
write.csv(longNut,"Water/Output/compiledData/nutrients_forPresentation.20210221.csv",row.names = F)




###---Can move over to plottingNutrientsForDataQC_20210216.R"
#Then change directory, db version, and include POC,PON,POP

###siteNutrients_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

siteNutrients <-newdata[order(newdata$lakeID),]
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteNutrients$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long","UTM")]
siteNutrients<-merge(siteNutrients,siteLakes,by="lakeID")
siteNutrients$siteID<-paste(siteNutrients$lakeID,"_",siteNutrients$siteName,sep="")
wantSite<-c("siteID","lakeID","siteName","lat","long","UTM","updateID")
siteNutrients<-siteNutrients[wantSite]
siteNutrients$updateID<-"siteNutrients.20200303"
#check that vector data formats are the same between siteChlorophyll and SITES table
str(dbTable("SITES"))
str(siteNutrients)
siteNutrients$lakeID<-as.character(siteNutrients$lakeID)
siteNutrients$siteName<-as.character(siteNutrients$siteID)

### !!! ###
#write.csv(siteNutrients,paste(dbdir,"siteNutrients_20200316.csv",sep=""),row.names=FALSE)


###sampleNutrients_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

sampleNutrients<-newdata[order(newdata$lakeID),]
sampleNutrients$updateID<-"sampleNutrients.20200303"
log<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv",stringsAsFactors = FALSE)
log<-log[c("sampleID","crew","weather","comments")]
sampleNutrients<-merge(sampleNutrients,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
sampleNutrients$comments<-paste(sampleNutrients$comments.x,sampleNutrients$comments.y,sep=' ')
fix<-sampleNutrients[is.na(sampleNutrients$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and LogFile.csv (and all derivitive data frames)
sampleNutrients$siteID<-paste(sampleNutrients$lakeID,"_",sampleNutrients$siteName,sep="")
wantSample<-c("siteID","sampleID","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","crew","weather","comments","metadataID","updateID")
sampleNutrients<-sampleNutrients[wantSample]
#check that vector data formats are the same between sampleNutrients and SAMPLES table
str(dbTable("SAMPLES"))
str(sampleNutrients)

### !!! ###
#write.csv(sampleNutrients,paste(dbdir,"sampleNutrients_20200316.csv",sep=""),row.names=FALSE)
