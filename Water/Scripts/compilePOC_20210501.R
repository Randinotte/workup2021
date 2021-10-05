#Compiles chl data from .xlsx run files
rm(list=ls())  #clear variables

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210423.db" #Change name of db file with every update

dir<-'Water/Input/POC 2020'

files<-list.files(dir)
files<-files[grep('_run',files)]

library(readxl)
library(tidyverse)
library(lubridate)

#2020 POC----
year<-"2020"
#log<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/pocLogFile.csv",header=T, stringsAsFactors = F)
log<- read.csv("logFiles2020/correctedFiles/pocLogFile.csv",header = T, stringsAsFactors = F)
log<-log[2:nrow(log),]


poc<-data.frame() 
i=5
for(i in 1:length(files)){
  if(year == "2020"){ #saved different columns between years, must have been in Export settings on EA
    samples<-as.data.frame(read_excel(file.path(dir,files[i]),range="Summary Table!A16:AC100",col_names=F))
    samples<-na.omit(samples[,])
    colnames(samples)<-read_excel(file.path(dir,files[i]),range="Summary Table!A3:AC3",col_names=F)
    samples<-samples[c(1,3,4,13,25)]
    colnames(samples)[1:5]<-c("runID","trayLoc","pocID","PON.mg","POC.mg")
    samples$POC.mg<-as.numeric(samples$POC.mg)
    samples<- samples[!grepl("ontrol", samples$pocID),]
    if(any(!grepl("P",samples$pocID)==TRUE)){
      samples$pocID<-paste("P",samples$pocID,sep="")
    }
    samples<- left_join(samples, log, by="pocID")
    
    samples$POC<-round(samples$POC.mg/samples$volFiltered*1000,digits=3) #mg C/ volume in ml*(1000ml/1L) for mg/L
    samples$PON<-round(samples$PON.mg/samples$volFiltered*1000,digits=3)
    
    poc<-rbind(poc,samples)

  }
}

dupList<-poc[duplicated(poc$pocID),"pocID"] #lists duplicated samples
dup<-poc[poc$pocID%in%dupList,] %>% arrange(pocID) #lists duplicates and originals
notInLog<-poc[is.na(poc$lakeID),]
#write.csv(dup,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/POC dups.csv",row.names=F)

#making changes to poc after dups and errors identified:
poc_good<- poc %>% 
  mutate(pocID = case_when(trayLoc == "P1F3" ~ "P161", #fixes P112 dup
                           trayLoc == "P1F4" ~ "P38",
                           trayLoc == "P1F5" ~ "P37",
                           trayLoc == "P1F6" ~ "P36",
                           trayLoc == "P1F7" ~ "P35",
                           trayLoc == "P1F8" ~ "P34",
                           trayLoc == "P1F9" ~ "P30",
                           trayLoc == "P1F10" ~ "P29",
                           trayLoc == "P1F11" ~ "P28",
                           trayLoc == "P1F12" ~ "P113",
                           TRUE~pocID)) %>% 
  mutate(pocID = case_when(trayLoc == "P2A8" ~ "P43", #fixes P13 dup
                           trayLoc == "p1c3" ~ "P61", #fixes P31 dup 
                           trayLoc == "p1c2" ~ "P62", #fixes P32 dup
                           trayLoc == "p1b12" ~ "P64", #fixes P34 dup
                           trayLoc == "p1b11" ~ "P65", #fixes P35 dup
                           trayLoc == "P1D12" ~ "P33", #fixes P333 notInLog
                           TRUE~pocID)) %>% 
  select("pocID","runID","trayLoc","PON.mg","POC.mg") %>% 
  right_join(.,log,by = "pocID")  #makes list of all 194, with NA's for POC/PON where missing
poc_good<- poc_good %>%   
  add_column(POC = round(poc_good$POC.mg/poc_good$volFiltered*1000,digits=3)) %>% #mg C/ volume in ml*(1000ml/1L) for mg/L)
  add_column(PON = round(poc_good$PON.mg/poc_good$volFiltered*1000,digits=3))
               
missing<- poc_good[is.na(poc_good$PON.mg),]
dupList<-poc_good[duplicated(poc_good$poc_goodID),1] #lists duplicated samples
dup<-poc_good[poc_good$poc_goodID%in%dupList,] %>% arrange(poc_goodID) #lists duplicates and originals
poc2020<-poc_good
#write.csv(poc_good,"Water/Output/compiledData/particulates2020_20210501.csv",row.names=F)

#2019 POC----
year<-"2019"
log<-read.csv("C:/Users/notter/Box/MFE/Archives/OneDriveArchive/Summer 2019/Limno/limnoEntryTool/logFiles2019/pocLogFile.csv",header=T, stringsAsFactors = F)
log<-log[2:nrow(log),]

poc2019<- read.csv("C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/POC_merged6c.csv", header = T, stringsAsFactors = F)


missing<- poc2019[is.na(poc2019$PON.mg),]
dupList<-poc2019[duplicated(poc2019$poc2019ID),1] #lists duplicated samples
dup<-poc2019[poc2019$poc2019ID%in%dupList,] %>% arrange(poc2019ID) #lists duplicates and originals

#write.csv(poc2019,"Water/Output/compiledData/particulates2019_20210501.csv",row.names=F)

#Merge 2019 and 2020 data----
checkCols(poc2019, poc2020)
wc<-dbTable("WATER_CHEM")

pocM<- poc2019 %>% 
  bind_rows(poc2020)
checkCols(wc,pocM)

pocM<- pocM %>%
  add_column(parameter = NA) %>% 
  add_column(parameterValue = NA) %>% 
  add_column(QCcode = NA) %>% 
  select(colnames(wc), POC, PON) %>% 
  gather(parameter, parameterValue, c("POC","PON")) %>% 
  mutate(metadataID = "Iso.POC.CEST.20121206") %>% 
  mutate(updateID = "pocpon.20210501")
pocM$dateSample<- as.Date(parse_date_time(pocM$dateSample, orders = c("%Y-%m-%d", "%m/%d/%Y"))) 

#write.csv(pocM, "Water/Output/compiledData/poc_merged_20210501.csv", row.names = F)

#include reruns:
files<-list.files(dir)
files<-files[grep('rerun',files)]
i=1
samples<-as.data.frame(read_excel(file.path(dir,files[i]),range="Summary Table!A16:AC100",col_names=F))
samples<-na.omit(samples[,])
colnames(samples)<-read_excel(file.path(dir,files[i]),range="Summary Table!A3:AC3",col_names=F)
samples<-samples[c(1,3,4,13,25)]
colnames(samples)[1:5]<-c("runID","trayLoc","pocID","PON.mg","POC.mg")
samples$POC.mg<-as.numeric(samples$POC.mg)
#samples$pocID<-paste("P",samples$pocID,sep="")
samples$year<- substr(word(samples$trayLoc,4,4,sep=" "),1,4)
samples<-na.omit(samples)
s19<-samples %>% filter(year == "2019")
log19<-read.csv("C:/Users/notter/Box/MFE/Archives/OneDriveArchive/Summer 2019/Limno/limnoEntryTool/logFiles2019/pocLogFile.csv",header=T, stringsAsFactors = F)
log19<-log19[2:nrow(log19),]
s19<-left_join(s19,log19,by="pocID")

s20<-samples %>% filter(year == "2020")
log20<- read.csv("logFiles2020/correctedFiles/pocLogFile.csv",header = T, stringsAsFactors = F)
log20<-log20[2:nrow(log20),]
s20<-left_join(s20,log20,by="pocID")
checkCols(s19,s20)
samples<-rbind(s19,s20)

samples$POC<-round(samples$POC.mg/samples$volFiltered*1000,digits=3) #mg C/ volume in ml*(1000ml/1L) for mg/L
samples$PON<-round(samples$PON.mg/samples$volFiltered*1000,digits=3)

poc19<- read.csv("C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/POC_merged6c.csv", header = T, stringsAsFactors = F)
poc20<- read.csv("Water/Output/compiledData/particulates2020_20210501.csv", header = T, stringsAsFactors = F)
checkCols(poc19,poc20)
