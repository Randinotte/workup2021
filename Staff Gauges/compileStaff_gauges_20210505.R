#RNN 2020-03-17
#pulls staff gauge height from limno entry csv's (data sheets)

rm(list=ls())

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("C:/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210423.db" #Change name of db file with every update


library(tidyverse)

dbStaff<-dbTable("Staff_gauges")


library(stringr)
#Read in log for list of staff measurements
log<-read.csv("logFiles2020/correctedFiles/samplesIS.csv", header = T, stringsAsFactors = F)
staff<-subset(log,log$depthClass=="Staff")
staff$projectID<-NA
staff$lakeID<-word(staff$sampleID,1,1,sep="_")
staff$siteName<-word(staff$siteID, 2,2, sep="_")
staff$metadataID<- dbStaff$metadataID[1]
staff$updateID<-"tableStaff.20210505"
want<-colnames(dbStaff)
checkCols(dbStaff, staff)
#staff<-staff[,c(want,"entryFile")]

#for loop that pulls water height and projectID's from entry csv files
dir<-"logFiles2020/sampleSheets2020"
files<-list.files(dir)
files<-files[grep('.csv',files)]
files<-files[files %in% staff$entryFile]

i=6
heights<-data.frame()
for (i in 1:length(files)){
  cur<-read.csv(file.path(dir,files[i]),header=F,stringsAsFactor=F)
  cur<-cur[cur[,1]%in%c('lakeID','date','time',cur[11:16,1]),1:3]
  cur<-cur[complete.cases(cur[,2]),]
  info<-data.frame(lakeID=cur[1,2],
                   siteName=as.character(word(cur$V1[5:nrow(cur)],1,1,sep="_")),
                   date=cur[2,2],
                   time=as.character(cur[3,2]))
  
  r=1
  for(r in 1:nrow(info)){
    #fix time formating
    if(grepl(":",info$time[r])){
      info$timeSample[r]<-as.character(info$time[r])
    }else{
      if(str_length(info$time[r])==3){
        info$timeSample[r]<-as.character(paste(substr(info$time[r],1,1),":",substr(info$time[r],2,3),sep=""))
      }else{
        if(str_length(info$time[r]==4)){
          info$timeSample[r]<-as.character(paste(substr(info$time[r],1,2),":",substr(info$time[r],3,4),sep=""))
        }
      }
    }
    
    #other formating
    info$time_flat<-gsub(":","",info$timeSample)
    info$dateSample<-as.Date(info$date,format="%Y%m%d")
    info$dateTimeSample<-paste(info$dateSample,info$timeSample,sep=" ")
    info$dateTimeSample<-as.POSIXct(info$dateTimeSample)
    
    #get water heights
    data<-data.frame(cur[5:nrow(cur),])
    names(data)[2:3]<-c("waterHeight","waterHeightUnits")
    info$waterHeight<-as.numeric(data$waterHeight)
    info$waterHeightUnits<-data$waterHeightUnits
  }#end of info(r)
  heights<-rbind(heights,info)
}#end of files(i)

#work with heights
heights$siteName<-as.character(heights$siteName)
heights$waterHeightUnits<-as.character(heights$waterHeightUnits)
for (h in 1:nrow(heights)){
  if(heights$siteName[h]=="lake"){
    heights$siteName[h]<-as.character("WholeLake")
  }
  
  if(heights$waterHeightUnits[h]=="ft"){
    heights$waterHeight_m[h]=heights$waterHeight[h]*0.3048
  }
  
  if(heights$waterHeightUnits[h]=="cm"){
    heights$waterHeight_m[h]=heights$waterHeight[h]/100
  }
  
  if(nchar(heights$time_flat[h])==3){
    heights$time_flat[h]<-paste0("0",heights$time_flat[h])
  }
}

#create columns for merge with df(staff)
heights$sampleID<-paste(heights$lakeID,"_",heights$siteName,"_",heights$date,"_",heights$time_flat,"_","Staff","_","0","_","Limno.Sample.20160505",sep="")
heights$time<-NULL
heights$time_flat<-NULL
heights$site<-NULL
heights$date<-NULL

length(staff$sampleID)
length(heights$sampleID)
weird<-heights[!(heights$sampleID %in% staff$sampleID),] #no staff height recorded
missing<-staff[!(staff$sampleID %in% heights$sampleID),] #staff height recorded as "NA"

staff<-merge(staff,heights[c("sampleID","waterHeight","waterHeightUnits","waterHeight_m")],by="sampleID")
staff<-staff[want]
checkCols(dbStaff,staff)

#get projectID
proj<- read.csv("logFiles2020/correctedFiles/unfilteredLogFile.csv", header = T, stringsAsFactors = F)
proj$shortID<-word(proj$sampleID,1,4,sep="_")
staff$shortID<-word(staff$sampleID,1,4,sep="_")
staff$shortID<-gsub("WholeLake","DeepHole",staff$shortID)

i=1
for(i in 1:nrow(staff)){
  if(staff$shortID[i] %in% proj$shortID){
    staff$projectID[i]<-proj$projectID[staff$shortID[i]==proj$shortID]
  }else{
    staff$projectID[i]<-3 
  }
}

staff$shortID<-NULL
#Way to remove non-staff related comments:
# #write.csv(staff$comments,"C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/staffGaugeComments.csv",row.names=F)
# new<-read.csv("C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/staffGaugeComments.csv",header=T,stringsAsFactors = F)
# 
# staff<-cbind(staff,new)
# staff$comments<-staff$commentsNew
# staff$comments.original<-NULL
# staff$commentsNew<-NULL

#remove non-staff comments:
staff$comments[!1:nrow(staff) %in% c(12,18,22)]<- NA
staff$comments[12]<- "inlet dry"
staff$comments[18]<- "staff underwater height estimated"
staff$comments[22]<- "staff underwater height estimated"

staff<-staff[want]

#Decided that the ME_WholeLake should be FE_WholeLake unless we decide otherwise later: 
for (i in 1:nrow(staff)){
  if(staff$lakeID[i]=="ME"){
    staff$lakeID[i]<-"FE"
    staff$sampleID[i]<-gsub("ME","FE",staff$sampleID[i])
  }
}
log<- read.csv("logFiles2020/correctedFiles/samplesIS.csv", header = T, stringsAsFactors = F)
for (i in 1:nrow(log)){
  if(log$siteID[i]=="ME_WholeLake"){
    log$siteID[i]<-"FE_WholeLake"
    log$sampleID[i]<-gsub("ME","FE",log$sampleID[i])
  }
}
#write.csv(log,"logFiles2020/correctedFiles/samplesIS.csv",row.names = F)

#Okie Dokie, now this needs to be made into table, sample, and site tables but I'm hungry and I don't want to do it right now

###siteSTAFF_GAUGES_YYYYMMDD###
# source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
# dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
# db="MFEdb_20190612.db" 

newdata<-staff
library(lubridate)
newdata$dateSample<- as.Date(parse_date_time(newdata$dateSample, orders = c("%m/%d/%Y","%Y-%m-%d")))
newdata$dateTimeSample<- parse_date_time(newdata$dateTimeSample, orders = c("%m/%d/%Y HM","%Y-%m-%d HMS"), tz = "America/Chicago")

siteStaff <-newdata[order(newdata$lakeID),]
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteStaff$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long")]
siteStaff<-merge(siteStaff,siteLakes,by="lakeID")
siteStaff$siteID<-paste(siteStaff$lakeID,"_",siteStaff$siteName,sep="")
wantSite<-c("siteID","lakeID","siteName","lat","long","updateID")
siteStaff<-siteStaff[wantSite]
siteStaff$updateID<-"siteStaff.20210505"
#check that vector data formats are the same between siteStaff and SITES table
str(dbTable("SITES"))
str(siteStaff)
dbSites<-dbTable("SITES")

write.csv(siteStaff,"Updates/update4/siteStaff_20210505.csv",row.names=FALSE)


###sampleStaff_YYYYMMDD###
# source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
# dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
# db="MFEdb_20190612.db" 

sampleStaff<-newdata[order(newdata$lakeID),]
sampleStaff$updateID<-"sampleStaff.20210505"
#log<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv", stringsAsFactors = F)
log<-log[c("sampleID","crew","weather","comments")]
sampleStaff<-merge(sampleStaff,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
fix<-sampleStaff[is.na(sampleStaff$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and chlLogFile.csv (and all derivitive data frames)
sampleStaff$siteID<-paste(sampleStaff$lakeID,"_",sampleStaff$siteName,sep="")
#if two comments need to be merged: 
  #sampleStaff$comments<-paste(sampleStaff$comments.y,". ",sampleStaff$comments.x, sep="")
sampleStaff<-sampleStaff %>% rename(comments = "comments.y")
sampleStaff<-sampleStaff[colnames(dbTable("SAMPLES"))]
#names(sampleStaff)[10]<-"comments"
#check that vector data formats are the same between sampleStaff and SAMPLES table
str(dbTable("SAMPLES"))
str(sampleStaff)

write.csv(sampleStaff,"Updates/update4/sampleStaff_20210505.csv",row.names=FALSE)


###tableStaff_YYYYMMDD###
# source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
# dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
# db="MFEdb_20190612.db" 

tableStaff<-newdata[order(newdata$lakeID),]
tableStaff$updateID<-"tableStaff.20210505"
wantTable<-colnames(dbTable("STAFF_GAUGES"))
tableStaff<-tableStaff[wantTable]
str(dbTable("Staff_Gauges"))
str(tableStaff)
checkCols(wantTable,tableStaff)

write.csv(tableStaff,"Updates/update4/tableStaff_20210505.csv",row.names=FALSE)
