#compileFishSamples.R
#grabbing data that goes into FISH_SAMPLES updates and making tables for the database update
#used 2020-11-17 to compile 2020 fish from:
  #Amaryllis Adey's 2020 Long Lake angling & minnow trapping, 
  #Camille Mosley's 2020 Jones Lake experiment (Jones Lake and CR),
  #Colin Dassow's 2019 Fishscapes data (*full)
  #Amaryllis Adey's 2019 Long Lake angling, minnow trapping, and electrofishing (*full)
    
#*The 2019 update (sampleFishInfo2019.20200529 and tableFishInfo2019.20200529) didn't go well.
    #For Colin's data, I used incomplete csv files because I didn't know that they weren't the most recent. 
      #Additionally, I created a time zone error issue for the dateTimeSet and dateTimeSample columns which caused 
      #the times to subtract an hour from their actual times. They then no longer matched the sampleID's time, which
      #alerted us to the problem. 
    #For Amaryllis' data, I learned that when I combined the AN, BE, and MT data, only MT was retained because  
      #AN and BE were in a different date format and they got booted out. 
    #Resolution: rather than piecemeal fixing and replacing, I'm pulling all the 2019 data in FISH_SAMPLES and FISH_INFO with
      #the updateID's given above and re-compiling both 2019 fish data streams in this script alongside the 2020 data.

rm(list=ls())  #clear variables
graphics.off()  #close figs

source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201020_3.5.3.db" 
dbTableList() #checks db

library(tidyverse)
library(lubridate)

#exploring FISH_SAMPLES
# fsamp<-dbTable("FISH_SAMPLES")
# unique(fsamp$gear)
# units<-dbTable("UNITS")
# units[units$tableName=="FISH_SAMPLES" & units$colName=="gear","description"]
# unique(fsamp$sampleGroup)

finfo<-dbTable("FISH_INFO")
fsamp<-dbTable("FISH_SAMPLES")

colFinfo<-colnames(finfo)
colFsamp<-colnames(fsamp)


# mortTypes<-finfo %>% 
#   group_by(updateID) %>% 
#   count(mortality) #learn that mortalities are very rarely flagged as "0", but left NULL. Will change AA's All$mortality 0's to Null's

#read in data sources for FISH_SAMPLES and FISH_INFO
  #Colin's "fishSamplesIS.csv" from the fish entry tool
  #Amaryllis' "fishSamplesIS.csv" generated from my script "IACUCfishCount.R"

# #Colin
#   #FISH_SAMPLES
#   #CD_samples<-read.csv("C:/Users/notter/OneDrive/Summer 2019/Fish/fishEntryTool/fishSamplesIS.csv", header = T, stringsAsFactors = F)
#   CD_samples<-read.csv("C:/Users/notter/Box/MFE/OneDriveArchive/Summer 2019/Fish/fishEntryTool/fishSamplesIS.csv", header = T, stringsAsFactors = F)
#   
#   
#     #check missing columns
#     colCD_samples<- colnames(CD_samples)
#     missingFromCD<- colFsamp[!colFsamp %in% colCD_samples]
#     extraInCD<- colCD_samples[!colCD_samples %in% colFsamp]
#     
#     #add/remove columns for final
#     CD_samples<- CD_samples %>% 
#       add_column(updateID = "sampleFishInfo2019.20200529") %>% 
#       select(colFsamp)
#   
#   #FISH_INFO
#   CD_info<-read.csv("C:/Users/notter/OneDrive/Summer 2019/Fish/fishEntryTool/fishInfoIS.csv", header = T, stringsAsFactors = F)
#   
#     #check missing columns
#     colCD_info<- colnames(CD_info)
#     missingFromCD<- colFinfo[!colFinfo %in% colCD_info]
#     extraInCD<- colCD_info[!colCD_info %in% colFinfo]
# 
#     
#     #add/remove columns for final
#     CD_info<- CD_info %>% 
#       add_column(updateID = "tableFishInfo2019.20200529") %>% 
#       rename(otolithSampled = "otolithSample") %>% 
#       rename(gonadSqueeze = "gonadSqueze") %>% 
#       select(colFinfo[1:38],"spineSample","scaleSample",colFinfo[39:40]) #spineSample and scaleSample are new for 2019
#   
  
  

#Amaryllis
  #FISH_SAMPLES
  AA_an_samples<- read.csv("Fish/input/Long Lake/Angling/fishSamplesIS.csv",header=T, stringsAsFactors = F)
    #File has two date formats, a blank row, and 20200703 is missing a lakeID and other sample info
  #Dead fish pickup: two dead fish were collected by hand from FE on a day the crew was out angling WL. 
      #They are included here to give them a sampleID. Amaryllis sent fishSamplesIS.csv with them sharing a sampleID 
      #but since they're from separate lakes on the same day they need separate sampleIDs and rows in this table. 
      #Leaving Fish A in row 13 (the existing row) and putting Fish B in row 14, which is randomly left blank and 
      #would have been deleted anyway.
    #Fish A: from FE   
    AA_an_samples$siteID[13]<-"FE_WholeShoreline"
    AA_an_samples$sampleID[13]<- paste("FE",AA_an_samples$sampleID[13],sep="_")
    AA_an_samples$effortUnits[13]<- NA
    AA_an_samples$useCPUE[13]<- "no"
    #Fish B: from WL
    AA_an_samples[14,]<- AA_an_samples[13,]
    AA_an_samples$siteID[14]<- gsub("FE","WL",AA_an_samples$siteID[14])
    AA_an_samples$sampleID[14]<- gsub("FE","WL",AA_an_samples$sampleID[14])
    
  #Fixing date formats: 
      str(AA_an_samples)
      AA_an_samples$dateSet<- as.Date(parse_date_time(AA_an_samples$dateSet,orders=c("m/d/y","Y-m-d")),format="%Y-%m-%d")
      AA_an_samples$dateSample<- as.Date(parse_date_time(AA_an_samples$dateSample,orders=c("m/d/y","Y-m-d")),format="%Y-%m-%d")
      AA_an_samples$dateTimeSet<- parse_date_time(AA_an_samples$dateTimeSet, orders=c("m/d/y HM", "m/d/y HMS"))
      AA_an_samples$dateTimeSample<- parse_date_time(AA_an_samples$dateTimeSample, orders=c("m/d/y HM", "m/d/y HMS"))

  # #!!! don't have AA_mt data anywhere    
  # #AA_mt_samples<-read.csv("C:/Users/notter/Box/MFE/OneDriveArchive/Summer 2019/Fish/Long Lake/fishSamplesIS_MT.csv", header = T, stringsAsFactors = F)
  # AA_mt_samples<- read.csv("Fish/input/Long Lake/minnowtraps/fishSamplesIS.csv") #this file doesn't exist yet because I only have MT fishInfoIS.csv
  # colnames(AA_an_samples)==colnames(AA_mt_samples)
  # AA_samples<-rbind(AA_an_samples, AA_mt_samples)
  # rm(AA_an_samples)
  # rm(AA_mt_samples)
  # 
  #   #check missing columns
  #   colAA_samples<- colnames(AA_samples)
  #   missingFromAA<- colFsamp[!colFsamp %in% colAA_samples]
  #   extraInAA<- colAA_samples[!colAA_samples %in% colFsamp]
  #   
  #   #add/remove columns for final
  #   AA_samples<- AA_samples %>% 
  #     add_column(updateID = "sampleFishInfo2019.20200529") %>% 
  #     select(colFsamp)
  
  #FISH_INFO
  #Checking to make sure that the "fishInfoIS_ALL.csv" from IACUC script matches the other tables
  # AA_anbe_info<-read.csv("C:/Users/notter/OneDrive/Summer 2019/Fish/Long Lake/fishInfoIS_AN-BE.csv", header = T, stringsAsFactors = F)
  # AA_mt_info<-read.csv("C:/Users/notter/OneDrive/Summer 2019/Fish/Long Lake/fishInfoIS_MT.csv", header = T, stringsAsFactors = F)
  # colnames(AA_anbe_info)==colnames(AA_mt_info)
  # nrow(AA_anbe_info)+nrow(AA_mt_info) #it does!
  AA_info<- read.csv("Fish/input/Long Lake/Angling/fishInfoIS.csv", header = T, stringsAsFactors = F)
    #Need to correct the two dead fish that were bagged
      which(word(AA_info$sampleID,5,5,sep="_")=="NA")
      #Fish A: FE?
      AA_info$sampleID[223]<- paste("FE",AA_info$sampleID[223],sep="_")
      AA_info$fishID[223]<- paste("FE",AA_info$fishID[223],sep="_")
      #Fish A: WL?
      AA_info$sampleID[224]<- paste("WL",AA_info$sampleID[223],sep="_")
      AA_info$fishID[224]<- paste("WL",word(AA_info$fishID[223],1,6,sep="_"),"1",sep="_") #changes fishID to _1 because it's now fishNum 1 of the new sampleID, not fishNum2 of the old one
      AA_info$fishNum[224]<- 1
      
    #check missing columns
    colAA_info<- colnames(AA_info)
    missingFromAA<- colFinfo[!colFinfo %in% colAA_info]
    extraInAA<- colAA_info[!colAA_info %in% colFinfo]
    
    #add/remove columns for final
    AA_info<- AA_info %>% 
      add_column(updateID = "tableFishInfo2019.20200529") %>% 
      select(colFinfo[1:38],"spineSample","scaleSample",colFinfo[39:40]) #spineSample and scaleSample are new for 2019
  
#Binding Colin and Amaryllis' tables
samples<- rbind(CD_samples, AA_samples)    
info<- rbind(CD_info, AA_info)

#checks
length(unique(samples$sampleID)) == nrow(samples)
length(unique(info$fishID)) == nrow(info) #info has a duplicated fishID

#identify duplicate
dup<-info$fishID[duplicated(info$fishID)]
dups<-subset(info,info$fishID %in% dup)
full_sample<-info$sampleID[info$fishID %in% dup]
full_samples<-subset(info, info$sampleID %in% full_sample) #makes sure there weren't other fish in that MT that need to be fixed too

#fix
info$fishNum[3327]<-"2"
info$fishID[3327]<- paste(word(info$fishID[3327],1,6, sep="_"),"_2",sep="")

#create SITES table
siteFish<-info[order(info$sampleID),]
siteFish$lakeID<- word(siteFish$sampleID,1,1,sep="_")
siteFish$siteName<-word(siteFish$sampleID,2,2,sep="_")
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteFish$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long","UTM")]
siteFish<-merge(siteFish, siteLakes, by="lakeID")
siteFish$siteID<-paste(siteFish$lakeID,siteFish$siteName, sep="_")
siteFish$updateID<-"siteFishInfo2019.20200529"
wantSite<-c("siteID","lakeID","siteName","lat","long","UTM","updateID")
siteFish<-siteFish[wantSite]
View(unique(siteFish$siteID)) #check for weird spellings, semi-duplicates

#write csv's for writingUpdatingDBtables.R
#(note because I got confused: fish samples don't go in the "SAMPLES" table. Their sites do go in "SITES")
outDir<- "C:/Users/notter/OneDrive/Randi/Database/currentDB"
write.csv(samples,file.path(outDir,"sampleFishInfo2019_20200529.csv"), row.names = F)
write.csv(info,file.path(outDir,"tableFishInfo2019_20200529.csv"), row.names = F)
write.csv(siteFish,file.path(outDir,"siteFishInfo2019_20200529.csv"), row.names = F)

