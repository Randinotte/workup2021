#Intermediate fixes (Update 3)
#Used to make small fixes before larger 2020 data update

#List of fixes:
# 1) Adds 2019 and 2020 LIMNO_PROFILES




source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20210402.db" 
dbTableList() #checks db

library(tidyverse)
library(lubridate)

View(dbTable("VERSION_HISTORY"))
proj<-dbTable("PROJECTS")

#1) LIMNO_PROFILES
prof19<- read.csv("C:/Users/notter/Box/MFE/Archives/OneDriveArchive/Summer 2019/Limno/limnoEntryTool/logFiles2019/profilesIS.csv", header = T, stringsAsFactors = F)
prof20<- read.csv("logFiles2020/correctedFiles/profilesIS.csv", header = T, stringsAsFactors = F)
prof<- dbTable("LIMNO_PROFILES")

profNew<- bind_rows(prof19,prof20)  
profNew<- profNew %>% 
  mutate(dateSample = parse_date_time(profNew$dateSample, orders = c("%m/%d/%Y", "%Y-%m-%d"), tz = "America/Chicago")) %>% 
  mutate(dateSample = as.Date(profNew$dateSample, format = "%m/%d/%Y")) %>% 
  mutate(dateTimeSample = parse_date_time(profNew$dateTimeSample, orders = c("%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M:%S"), tz = "America/Chicago")) %>% 
  mutate(updateID = "tableLimno_Profiles.20210414") %>% 
  mutate(projectID = as.character(profNew$projectID))

profUpdate<- bind_rows(prof,profNew)
write.csv(profUpdate,file.path("Updates/update3","LIMNO_PROFILES_20210414.csv"), row.names = F)
write.table(profUpdate,file.path("Updates/update3",'LIMNO_PROFILES.txt'),quote = F,sep = '|',row.names=F,col.names=F)


#SITES#
###siteNutrients_YYYYMMDD###
newdata<-profNew
siteLimno <-newdata[order(newdata$lakeID),]
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteLimno$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long")]
siteLimno<-merge(siteLimno,siteLakes,by="lakeID")
siteLimno$siteName<- word(siteLimno$sampleID,2,2,sep="_")
siteLimno$siteID<-paste(siteLimno$lakeID,"_",siteLimno$siteName,sep="")

wantSite<-c("siteID","lakeID","siteName","lat","long","updateID")
siteLimno<- siteLimno[wantSite]
siteLimno$updateID<-"siteLimno.20210505"
#check that vector data formats are the same between siteChlorophyll and SITES table
str(dbTable("SITES"))
str(siteLimno)
siteLimno$lakeID<-as.character(siteLimno$lakeID)
siteLimno$siteName<-as.character(siteLimno$siteName)
#write.csv(siteLimno,"Updates/update4/siteLimnoProfiles_20210505.csv",row.names = F)


###sampleNutrients_YYYYMMDD###
sampleLimno<-newdata[order(newdata$lakeID),]
sampleLimno$updateID<-"sampleLimno.20210505"
log2020<-read.csv("logFiles2020/correctedFiles/samplesIS.csv", header = T, stringsAsFactors = F)
log2019<- read.csv("C:/Users/notter/Box/MFE/Archives/OneDriveArchive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv", header = T, stringsAsFactors = F)
checkCols(log2019,log2020)
log<- bind_rows(log2019,log2020)
i=90
for(i in 1:nrow(log)){
  if(nchar(word(log$sampleID[i],4,4,sep="_"))==3){
    log$sampleID[i]<- gsub(word(log$sampleID[i],4,4,sep="_"), paste0("0",word(log$sampleID[i],4,4,sep="_")),log$sampleID[i])
  }
}
# log<-log[c("sampleID","crew","weather","comments", "dateTimeSample","depthClass","depthTop","depthBottom")]
log<-log[c("sampleID","crew","weather","comments")]
sampleLimno<-merge(sampleLimno,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
#sampleLimno$comments<-paste(sampleLimno$comments.x,sampleLimno$comments.y,sep=' ')
sampleLimno<-sampleLimno %>% rename(comments = "comments.y")
fix<-sampleLimno[is.na(sampleLimno$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and LogFile.csv (and all derivitive data frames)
sampleLimno$siteID<-paste(sampleLimno$lakeID,"_",word(sampleLimno$sampleID,2,2,sep="_"),sep="")
wantSample<-c("siteID","sampleID","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","crew","weather","comments","metadataID","updateID")
# sampleLimno<-sampleLimno[wantSample]
# samples<-dbTable("SAMPLES")
checkCols(dbTable("SAMPLES"),sampleLimno)
sampleLimno<-sampleLimno[colnames(dbTable("SAMPLES"))]
sampleLimno$dateSample<-as.Date(sampleLimno$dateTimeSample)
#check that vector data formats are the same between sampleLimno and SAMPLES table
str(dbTable("SAMPLES"))
str(sampleLimno)



### !!! ###
#write.csv(sampleLimno,"Updates/update4/sampleLimno_20210505.csv",row.names=FALSE)

