#Compiles color data from the limno entry tool log 
#Generates a csv table for color that matches the dbTable("COLOR") for use in the script "plottingColorForQC.R"
rm(list=ls())
library(ggplot2)
## load data and connect multiple files
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20210423.db" 


data<-dbTable("COLOR")
length(colnames(data))

newdata<-read.csv("logFiles2020/correctedFiles/colorLogFile.csv")
newdata<-newdata[-1,] #removes dummy row from limno entry tool
length(colnames(newdata))

newdata$dateTimeSample<-as.POSIXct(paste(newdata$dateSample,newdata$timeSample,sep=" "), format= "%m/%d/%Y %H:%M", tz = "America/Chicago")
names(newdata)[4]<-"siteName"
newdata$g440<-round(newdata$abs440*23.03,digits=2)
newdata$metadataID<-"Color.20110601"
newdata$flag<-"0"
newdata$updateID<-"TBD"

want<-c(colnames(data))
newdata<-newdata[,want]

#check that vector data formats are the same between data and newdata table
str(data)
str(newdata)
df<-data.frame("vector"=(names(data)))
i=1

for(i in 1:ncol(newdata)){
  df$site[i]<-class(newdata[,i])
}
for(i in 1:ncol(data)){
  df$DB[i]<-class(data[,i])
}

i=2
df$DB<-as.character(df$DB)
for(i in 1:nrow(df)){
  if (df$DB[i]=="character"){
    newdata[,paste(df$vector[i])]<-as.character(newdata[,paste(df$vector[i])])
  }
  if (df$DB[i]=="numeric"){
    newdata[,paste(df$vector[i])]<-as.numeric(newdata[,paste(df$vector[i])])
  }
  if (df$DB[i]=="integer"){
    newdata[,paste(df$vector[i])]<-as.integer(newdata[,paste(df$vector[i])])
  }
  if (df$DB[i]=="factor"){
    newdata[,paste(df$vector[i])]<-as.factor(newdata[,paste(df$vector[i])])
  }
}
newdata$dateSample<-as.Date(newdata$dateSample,format="%m/%d/%Y")


#remove blank rows
#newdata<-newdata[!(is.na(newdata$projectID)),]


#write.csv(newdata,"Water/Output/compiledData/color_20210211.csv",row.names = F)

#SITE,SAMPLE,TABLE
###sitecolor_GAUGES_YYYYMMDD###
# source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
# dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
# db="MFEdb_20190612.db" 


library(lubridate)
newdata$dateSample<- as.Date(parse_date_time(newdata$dateSample, orders = c("%m/%d/%Y","%Y-%m-%d")))
newdata$dateTimeSample<- parse_date_time(newdata$dateTimeSample, orders = c("%m/%d/%Y HM","%Y-%m-%d HMS"), tz = "America/Chicago")

sitecolor <-newdata[order(newdata$lakeID),]
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(sitecolor$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long")]
sitecolor<-merge(sitecolor,siteLakes,by="lakeID")
sitecolor$siteID<-paste(sitecolor$lakeID,"_",sitecolor$siteName,sep="")
wantSite<-c("siteID","lakeID","siteName","lat","long","updateID")
sitecolor<-sitecolor[wantSite]
sitecolor$updateID<-"sitecolor.20210505"
#check that vector data formats are the same between sitecolor and SITES table
str(dbTable("SITES"))
str(sitecolor)
dbSites<-dbTable("SITES")
checkCols(dbSites,sitecolor)

write.csv(sitecolor,"Updates/update4/siteColor_20210505.csv",row.names=FALSE)


###samplecolor_YYYYMMDD###
# source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
# dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
# db="MFEdb_20190612.db" 

samplecolor<-newdata[order(newdata$lakeID),]
samplecolor$updateID<-"samplecolor.20210505"
#log<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv", stringsAsFactors = F)
log<- read.csv("logFiles2020/correctedFiles/samplesIS.csv",header = T,stringsAsFactors = F)
log<-log[c("sampleID","crew","weather","comments")]
samplecolor<-merge(samplecolor,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
fix<-samplecolor[is.na(samplecolor$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and colorLogFile.csv (and all derivitive data frames)
samplecolor$siteID<-paste(samplecolor$lakeID,"_",samplecolor$siteName,sep="")
#if two comments need to be merged: 
#samplecolor$comments<-paste(samplecolor$comments.y,". ",samplecolor$comments.x, sep="")
samplecolor<-samplecolor %>% rename(comments = "comments.y")
samplecolor<-samplecolor[colnames(dbTable("SAMPLES"))]
#names(samplecolor)[10]<-"comments"
#check that vector data formats are the same between samplecolor and SAMPLES table
str(dbTable("SAMPLES"))
str(samplecolor)


write.csv(samplecolor,"Updates/update4/sampleColor_20210505.csv",row.names=FALSE)


###tablecolor_YYYYMMDD###
# source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
# dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
# db="MFEdb_20190612.db" 

tablecolor<-newdata[order(newdata$lakeID),]
tablecolor$updateID<-"tablecolor.20210505"
wantTable<-colnames(dbTable("COLOR"))
tablecolor<-tablecolor[wantTable]
str(dbTable("COLOR"))
str(tablecolor)
checkCols(wantTable,tablecolor)

write.csv(tablecolor,"Updates/update4/tableColor_20210505.csv",row.names=FALSE)






