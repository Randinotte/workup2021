#compilePOCTables used to make SAMPLE, SITE, and TABLE tables for POC 
#Need to be combined for plottingPOCForDataQC.R
#RNN 2020-03-02 from KS's plotting nutrients for data QC.R

## load data and connect multiple files
rm(list=ls())  #clear variables
library(ggplot2)

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20190612.db" #Change name of db file with every update
dir<-'Water/Output/compiledData'


#read in final data files for each analysis (make sure these are done, not missing any)
#poc<-newdata_wide #if still open from plottingPOCforDataQC.R
newdata<-read.csv("Water/Output/compiledData/poc_20210219.csv",header=TRUE,stringsAsFactors = FALSE)


###tableNutrients_YYYYMMDD###
colnames(dbTable("WATER_CHEM"))
dbPOC<-dbTable("WATER_CHEM")
str(dbPOC)
#organize columns
colnames(newdata)
str(newdata)
newdata$metadataID<-"Iso.POC.CEST.20121206"
newdata$updateID<-"tablePOC.20200312"
newdata$dateSample<-as.Date(newdata$dateSample,format="%m/%d/%Y")
newdata$dateTimeSample<-as.POSIXct(newdata$dateTimeSample,format="%m/%d/%Y %H:%M",tz="America/Chicago")
newdata$projectID<-as.character(newdata$projectID)
newdata$filterVol<-as.numeric(newdata$filterVol)

newtable<-rbind(dbPOC,newdata)
### !!! ###
#write.csv(newdata,paste(dbdir,"tablePOC_20200312.csv",sep=""),row.names=FALSE)


###siteNutrients_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

sitePOC <-newdata[order(newdata$lakeID),]
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(sitePOC$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long","UTM")]
sitePOC<-merge(sitePOC,siteLakes,by="lakeID")
sitePOC$siteID<-paste(sitePOC$lakeID,"_",sitePOC$siteName,sep="")
wantSite<-c("siteID","lakeID","siteName","lat","long","UTM","updateID")
sitePOC<-sitePOC[wantSite]
sitePOC$updateID<-"sitePOC.20200312"
#check that vector data formats are the same between siteChlorophyll and SITES table
str(dbTable("SITES"))
str(sitePOC)

### !!! ###
#write.csv(sitePOC,paste(dbdir,"sitePOC_20200312.csv",sep=""),row.names=FALSE)


###sampleNutrients_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

samplePOC<-newdata[order(newdata$lakeID),]
samplePOC$updateID<-"samplePOC.20200313"
log<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv")
log<-log[c("sampleID","crew","weather","comments")]
samplePOC<-merge(samplePOC,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
samplePOC$comments<-paste(samplePOC$comments.x,samplePOC$comments.y,sep=' ')
fix<-samplePOC[is.na(samplePOC$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and LogFile.csv (and all derivitive data frames)
samplePOC$siteID<-paste(samplePOC$lakeID,"_",samplePOC$siteName,sep="")
wantSample<-c("siteID","sampleID","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","crew","weather","comments","metadataID","updateID")
samplePOC<-samplePOC[wantSample]
#check that vector data formats are the same between samplePOC and SAMPLES table
str(dbTable("SAMPLES"))
str(samplePOC)
samplePOC$crew<-as.character(samplePOC$crew)
samplePOC$weather<-as.character(samplePOC$weather)

### !!! ###
#write.csv(samplePOC,paste(dbdir,"samplePOC_20200313.csv",sep=""),row.names=FALSE)
