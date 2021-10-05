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
db="MFEdb_20210423.db" #Change name of db file with every update
dir<-'Water/Output/compiledData'


#read in final data files for each analysis (make sure these are done, not missing any)
tp<-read.csv("Water/Output/compiledData/tpQCd_20210429.csv",header=TRUE,stringsAsFactors = FALSE)
srp<-read.csv("Water/Output/compiledData/srpQCd_20210429.csv",header=TRUE,stringsAsFactors = FALSE)
tn<-read.csv("Water/Output/compiledData/tnQCd_20210429.csv",header=TRUE,stringsAsFactors = FALSE)
no3<-read.csv("Water/Output/compiledData/no3QCd_20210429.csv",header=TRUE,stringsAsFactors = FALSE)
pop<-read.csv("Water/Output/compiledData/popQCd_20210505.csv",header=TRUE,stringsAsFactors = F)
poc<-read.csv("Water/Output/compiledData/pocQCd_20210505.csv",header=TRUE,stringsAsFactors = F)

###tableNutrients_YYYYMMDD###
colnames(dbTable("WATER_CHEM"))
dbNut<-dbTable("WATER_CHEM")
checkCols(dbNut,tp)

#QC Group A: Nutrients
#organize columns
colnames(tp)
tp$parameter<-"TP"
tp$metadataID<-"Nutrients.20110601"
tp$updateID<-"tableNutrients.20210505"
tp$flag<- replace_na(tp$flag,"0")
tp$QCcode<-NA
colnames(tp)[colnames(tp)=='TP']<-'parameterValue'
colnames(tp)[colnames(tp)=='site']<-'siteName'
tp$dateTimeSample<- parse_date_time(paste(tp$dateSample,tp$timeSample,sep=" "), orders = c("Y-m-d H:M", "m/d/Y H:M"), tz = "America/Chicago")
tp$dateSample<- as.Date(tp$dateTimeSample)
tp<-tp[colnames(dbNut)]

checkCols(dbNut,srp)
srp$parameter<-"SRP"
srp$metadataID<-"Nutrients.20110601"
srp$updateID<-"tableNutrients.20210505"
srp$flag<-replace_na(srp$flag,"0")
srp$QCcode<-NA
colnames(srp)[colnames(srp)=='SRP']<-'parameterValue'
colnames(srp)[colnames(srp)=='site']<-'siteName'
srp$dateTimeSample<- parse_date_time(paste(srp$dateSample,srp$timeSample,sep=" "), orders = c("Y-m-d H:M", "m/d/Y H:M"), tz = "America/Chicago")
srp$dateSample<- as.Date(srp$dateTimeSample)
srp<-srp[colnames(dbNut)]

checkCols(dbNut,tn)
tn$parameter<-"TN"
tn$metadataID<-"Nutrients.20110601"
tn$updateID<-"tableNutrients.20210505"
tn$flag<-replace_na(tn$flag,"0")
tn$QCcode<-NA
colnames(tn)[colnames(tn)=='TN']<-'parameterValue'
colnames(tn)[colnames(tn)=='site']<-'siteName'
tn$dateTimeSample<- parse_date_time(paste(tn$dateSample,tn$timeSample,sep=" "), orders = c("Y-m-d H:M", "m/d/Y H:M"), tz = "America/Chicago")
tn$dateSample<- as.Date(tn$dateTimeSample)
tn<-tn[colnames(dbNut)]

checkCols(dbNut,no3)
no3$parameter<-"nitrate"
no3$metadataID<-"Nutrients.20110601"
no3$updateID<-"tableNutrients.20210505"
no3$flag<-replace_na(no3$flag,"0")
no3$QCcode<- NA
colnames(no3)[colnames(no3)=='NO3']<-'parameterValue'
colnames(no3)[colnames(no3)=='site']<-'siteName'
no3$dateTimeSample<-as.POSIXct(paste(no3$dateSample,no3$timeSample,sep=" "),format="%m/%d/%Y %H:%M")
no3$dateSample<- as.Date(no3$dateTimeSample)
no3<-no3[colnames(dbNut)]

#QC Group B: Particulates
checkCols(dbNut,pop)
pop$parameter<-"particulateP"
pop$metadataID<-"Nutrients.20110601"
pop$updateID<-"tableNutrients.20210505"
pop$flag<-replace_na(pop$flag,"0")
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
newdataNut<-rbind(tp,srp,pop,tn,no3,poc) %>% arrange(dateSample,sampleID)
newdataNut %>% group_by(parameter) %>% count() #dims match!


### !!! ###
# write.csv(newdataNut,file.path(dir,"nutrients_QCd_20210505.csv"),row.names=FALSE)
# write.csv(newdataPart,file.path(dir,"particulates_20210217.csv"),row.names=FALSE)
# write.csv(newdata,file.path(dir,"tableNutrients_20210217.csv"),row.names=FALSE)
write.csv(newdataNut, file.path(dir,"water_chem_QCd_20210505.csv"), row.names=FALSE)


###---Can move over to plottingNutrientsForDataQC_20210216.R"
#Then change directory, db version, and include POC,PON,POP

###siteNutrients_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

newdata<-newdataNut
siteNutrients <-newdata[order(newdata$lakeID),]
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteNutrients$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long")]
siteNutrients<-merge(siteNutrients,siteLakes,by="lakeID")
siteNutrients$siteName<- word(siteNutrients$sampleID,2,2,sep="_")
siteNutrients$siteID<-paste(siteNutrients$lakeID,"_",siteNutrients$siteName,sep="")

wantSite<-c("siteID","lakeID","siteName","lat","long","updateID")
siteNutrients<- siteNutrients[wantSite]
siteNutrients$updateID<-"siteNutrients.20210505"
#check that vector data formats are the same between siteChlorophyll and SITES table
str(dbTable("SITES"))
str(siteNutrients)
siteNutrients$lakeID<-as.character(siteNutrients$lakeID)
siteNutrients$siteName<-as.character(siteNutrients$siteName)

### !!! ###
#write.csv(siteNutrients,"Updates/update4/siteNutrients_20210505.csv",row.names=FALSE)


###sampleNutrients_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

sampleNutrients<-newdata[order(newdata$lakeID),]
for(i in 1:nrow(sampleNutrients)){
  if(nchar(word(sampleNutrients$sampleID[i],4,4,sep="_"))==3){
    sampleNutrients$sampleID[i]<- gsub(word(sampleNutrients$sampleID[i],4,4,sep="_"), paste0("0",word(sampleNutrients$sampleID[i],4,4,sep="_")),sampleNutrients$sampleID[i])
  }
}
sampleNutrients$updateID<-"sampleNutrients.20210505"
log2020<-read.csv("logFiles2020/correctedFiles/samplesIS.csv", header = T, stringsAsFactors = F)
log2019<- read.csv("C:/Users/notter/Box/MFE/Archives/OneDriveArchive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv", header = T, stringsAsFactors = F)
checkCols(log2019,log2020)
log<- bind_rows(log2019,log2020)
for(i in 1:nrow(log)){
  if(nchar(word(log$sampleID[i],4,4,sep="_"))==3){
    log$sampleID[i]<- gsub(word(log$sampleID[i],4,4,sep="_"), paste0("0",word(log$sampleID[i],4,4,sep="_")),log$sampleID[i])
  }
}
log<-log[c("sampleID","crew","weather","comments", "dateTimeSample","depthClass","depthTop","depthBottom")]
sampleNutrients<-merge(sampleNutrients,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
#sampleNutrients$comments<-paste(sampleNutrients$comments.x,sampleNutrients$comments.y,sep=' ')
sampleNutrients<-sampleNutrients %>% rename(comments = "comments.y")
fix<-sampleNutrients[is.na(sampleNutrients$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and LogFile.csv (and all derivitive data frames)
sampleNutrients$siteID<-paste(sampleNutrients$lakeID,"_",word(sampleNutrients$sampleID,2,2,sep="_"),sep="")
wantSample<-c("siteID","sampleID","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","crew","weather","comments","metadataID","updateID")
# sampleNutrients<-sampleNutrients[wantSample]
# samples<-dbTable("SAMPLES")
checkCols(dbTable("SAMPLES"),sampleNutrients)
sampleNutrients<-sampleNutrients[colnames(dbTable("SAMPLES"))]
sampleNutrients$dateTimeSample<- parse_date_time(sampleNutrients$dateTimeSample, orders = c("%m/%d/%Y HM", "%y-%m-%d HMS"), tz = "America/Chicago")
#check that vector data formats are the same between sampleNutrients and SAMPLES table
str(dbTable("SAMPLES"))
str(sampleNutrients)

### !!! ###
#write.csv(sampleNutrients,"Updates/update4/sampleNutrients_20210505.csv",row.names=FALSE)


###tableNutrients_YYYYMMDD###
tableNutrients<-read.csv("Water/Output/compiledData/water_chem_QCd_20210505.csv", header = T, stringsAsFactors = F)
dbWC<-dbTable("WATER_CHEM")
checkCols(dbWC, tableNutrients)

#fixes:
tableNutrients$updateID<- "tableNutrients.20210505"
tableNutrients$flag<- replace_na(tableNutrients$flag,"0")
for(i in 1:nrow(tableNutrients)){
  if(nchar(word(tableNutrients$sampleID[i],4,4,sep="_"))==3){
    tableNutrients$sampleID[i]<- gsub(word(tableNutrients$sampleID[i],4,4,sep="_"), paste0("0",word(tableNutrients$sampleID[i],4,4,sep="_")),tableNutrients$sampleID[i])
  }
}
#write.csv(tableNutrients,"Updates/update4/tableNutrients_20210505.csv",row.names=FALSE)

