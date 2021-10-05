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

#Need to consider those with updateID "metadataFix.2020"

rm(list=ls())  #clear variables
graphics.off()  #close figs

source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201125.db" 
dbTableList() #checks db

library(tidyverse)
library(lubridate)
library(magrittr)

#View(dbTable("VERSION_HISTORY"))

fsamp<-dbTable("FISH_SAMPLES")
finfo<-dbTable("FISH_INFO")
errorSamp2019<-subset(fsamp, fsamp$dateSet>"2019-01-01")


#### 2019 Fishscapes: ####
###**FISH_SAMPLES####
fs19samp<- read.csv("Fish/input/2019 Fishscapes/fishSamplesIS.csv", header = T, stringsAsFactors = F)

#check missing/extra columns#
checkCols(fsamp, fs19samp)

#add/remove columns#
fs19samp<- fs19samp %>% 
  add_column(updateID = "sampleFishInfo.20201124") %>% 
  add_column(lakeID = word(fs19samp$sampleID,1,1,sep="_")) %>% 
  select(colnames(fsamp))
checkCols(fsamp, fs19samp)

#table fixes#
checkMet(fs19samp)
fs19samp<- fs19samp %>% 
  mutate(metadataID = case_when(grepl("hotBassLap",fs19samp$metadataID)~ "FishscapesSurvey.hotBassLap.20180607", 
                                grepl("1.5mile",fs19samp$metadataID)~ "FishscapesSurvey.1.5mile.20180607",
                                grepl("0.5mile",fs19samp$metadataID)~ "FishscapesSurvey.0.5mile.20180606", 
                                grepl("Angling",fs19samp$metadataID)~ "Fishscapes.Angling.20180625", 
                                TRUE ~ metadataID)) %>% 
  mutate(sampleID = case_when(grepl("hotBassLap",fs19samp$metadataID)~ paste(word(fs19samp$sampleID,1,5,sep="_"),"FishscapesSurvey.hotBassLap.20180607",sep="_"),
                              grepl("1.5mile",fs19samp$metadataID)~ paste(word(fs19samp$sampleID,1,5,sep="_"),"FishscapesSurvey.1.5mile.20180607",sep="_"), 
                              grepl("0.5mile",fs19samp$metadataID)~ paste(word(fs19samp$sampleID,1,5,sep="_"),"FishscapesSurvey.0.5mile.20180606",sep="_"),
                              grepl("Angling",fs19samp$metadataID)~ paste(word(fs19samp$sampleID,1,5,sep="_"),"Fishscapes.Angling.20180625",sep="_"), 
                              TRUE ~ sampleID)) %>% 
  mutate(dateSet = as.Date(fs19samp$dateSet)) %>%
  mutate(dateSample = as.Date(fs19samp$dateSample)) %>%
  mutate(dateTimeSet = parse_date_time(fs19samp$dateTimeSet, orders = "mdy HMS", tz = "America/New_York")) %>% 
  mutate(dateTimeSample = parse_date_time(fs19samp$dateTimeSample, orders = "mdy HMS", tz = "America/New_York"))
fs19samp<- fs19samp %>% 
  mutate(siteID = gsub("wholeShoreline","WholeShoreline",fs19samp$siteID)) %>% 
  mutate(sampleID = gsub("wholeShoreline","WholeShoreline",fs19samp$sampleID))
checkMet(fs19samp)
recreateSampleIDs(fs19samp, fish = T)

###**FISH_INFO####
fs19info<- read.csv("Fish/input/2019 Fishscapes/fishInfoIS.csv", header = T, stringsAsFactors = F)

#check missing/extra columns#
checkCols(finfo, fs19info)

#add/remove columns#
fs19info<- fs19info %>% 
  add_column(metadataID = word(fs19info$sampleID,6,6,sep="_")) %>% 
  add_column(updateID = "tableFishInfo.20201124") %>% #columns spineSample and scaleSample were new in 2019 but were already added because of the first update, don't need to add them again
  rename(otolithSampled = "otolithSample") %>% 
  rename(gonadSqueeze = "gonadSqueze") %>%
  select(colnames(finfo))
checkCols(finfo, fs19info)

#table fixes#
checkMet(fs19info)
fs19info<- fs19info %>% 
  mutate(metadataID = case_when(grepl("hotBassLap",fs19info$metadataID)~ "FishscapesSurvey.hotBassLap.20180607", 
                                grepl("1.5mile",fs19info$metadataID)~ "FishscapesSurvey.1.5mile.20180607", 
                                grepl("0.5mile",fs19info$metadataID)~ "FishscapesSurvey.0.5mile.20180606",
                                grepl("Angling",fs19info$metadataID)~ "Fishscapes.Angling.20180625", 
                                TRUE ~ metadataID)) %>% 
  mutate(sampleID = case_when(grepl("hotBassLap",fs19info$metadataID)~ paste(word(fs19info$sampleID,1,5,sep="_"),"FishscapesSurvey.hotBassLap.20180607",sep="_"), 
                              grepl("1.5mile",fs19info$metadataID)~ paste(word(fs19info$sampleID,1,5,sep="_"),"FishscapesSurvey.1.5mile.20180607",sep="_"), 
                              grepl("0.5mile",fs19info$metadataID)~ paste(word(fs19info$sampleID,1,5,sep="_"),"FishscapesSurvey.0.5mile.20180606",sep="_"), 
                              grepl("Angling",fs19info$metadataID)~ paste(word(fs19info$sampleID,1,5,sep="_"),"Fishscapes.Angling.20180625",sep="_"), 
                              TRUE ~ sampleID)) 
fs19info<- fs19info %>% 
  mutate(sampleID = gsub("wholeShoreline","WholeShoreline",fs19info$sampleID)) %>% 
  mutate(fishID = gsub("wholeShoreline","WholeShoreline",fs19info$fishID))
checkMet(fs19info)
recreateSampleIDs(fs19info, fish = T)


#### 2019 Long Lake: ####
###**FISH_SAMPLES####
ll19sampANBE<-read.csv("Fish/input/2019 Long Lake/fishSamplesIS_AN-BE.csv", header = T, stringsAsFactors = F)
ll19sampMT<-read.csv("Fish/input/2019 Long Lake/fishSamplesIS_MT.csv", header = T, stringsAsFactors = F)
checkCols(ll19sampANBE, ll19sampMT)
ll19samp<-rbind(ll19sampANBE,ll19sampMT)

#check missing/extra columns#
checkCols(fsamp, ll19samp)

#add/remove columns#
ll19samp<- ll19samp %>% 
  add_column(updateID = "sampleFishInfo.20201124") %>%
  add_column(lakeID = word(ll19samp$sampleID,1,1,sep="_")) %>% 
  select(colnames(fsamp))
checkCols(fsamp, ll19samp)

#table fixes#
checkMet(ll19samp)
ll19samp<- ll19samp %>% 
  mutate(dateSet = as.Date(parse_date_time(ll19samp$dateSet, orders = c("mdy", "ymd"), tz = "America/New_York"))) %>% 
  mutate(dateSample = as.Date(parse_date_time(ll19samp$dateSample, orders = c("mdy", "ymd"), tz = "America/New_York"))) %>%
  mutate(dateTimeSet = parse_date_time(ll19samp$dateTimeSet, orders = c("mdy HM","mdy HMS"), tz = "America/New_York")) %>% 
  mutate(dateTimeSample = parse_date_time(ll19samp$dateTimeSample, orders = c("mdy HM","mdy HMS"), tz = "America/New_York")) %>% 
  mutate(crew = gsub("\"", "", ll19samp$crew))
recreateSampleIDs(ll19samp, fish = T)

###**FISH_INFO####
ll19infoANBE<-read.csv("Fish/input/2019 Long Lake/fishInfoIS_AN-BE.csv", header = T, stringsAsFactors = F)
ll19infoMT<-read.csv("Fish/input/2019 Long Lake/fishInfoIS_MT.csv", header = T, stringsAsFactors = F)
checkCols(ll19infoANBE, ll19infoMT) #can't combine right away

#check missing/extra columns#
checkCols(finfo,ll19infoANBE)
checkCols(finfo, ll19infoMT)

#add/remove columns#
ll19infoANBE<- ll19infoANBE %>% 
  add_column(metadataID = word(ll19infoANBE$sampleID,6,6,sep="_")) %>% 
  add_column(updateID = "tableFishInfo.20201124") %>% 
  rename(otolithSampled = "otolithSample") %>% 
  rename(gonadSqueeze = "gonadSqueze") %>%
  select(colnames(finfo))

ll19infoMT<- ll19infoMT %>% 
  add_column(metadataID = word(ll19infoMT$sampleID,6,6,sep="_")) %>% 
  add_column(updateID = "tableFishInfo.20201124") %>% 
  rename(otolithSampled = "otolithSample") %>% 
  rename(gonadSqueeze = "gonadSqueze") %>%
  select(colnames(finfo))

#fixes duplicate MT fishID due to incorrect fishNum
ll19infoMT$fishNum[30]<-2
ll19infoMT$fishID[30]<- paste(ll19infoMT$sampleID[30],ll19infoMT$fishNum[30],sep="_")

#combine ANBE and MT
checkCols(ll19infoANBE, ll19infoMT)
ll19info<-rbind(ll19infoANBE, ll19infoMT)


#table fixes#
checkMet(ll19info)
all(unique(ll19info$species) %in% finfo$species == TRUE) #check species names/spellings
  ll19info<- ll19info %>% mutate(species = gsub("<ca>","",ll19info$species))
checkCols(finfo, ll19info)


### 2020 Long Lake: ####
###**FISH_SAMPLES####
ll20sampAN<-read.csv("Fish/input/2020 Long Lake/angling/fishSamplesIS.csv", header = T, stringsAsFactors = F)
  #--- finding fish---#
  #Jack and Amaryllis picked up two dead fish on 2020-07-03 and recorded their length, weight, and pit tag IDs. 
    #Basin and time weren't recorded, so I thought I could decide the basin based on where that fish had been caught previously.
    #Both fish were from EL/FE even though one was picked up in WL. Since I couldn't know which was which, I'm not including them in the database.
    #Later if they need sampleID's for otoliths, we should figure out another way to guess the fish. 
  deadFish<-finfo[finfo$tagRecapture %in% c("178752244","177417872"),] #both from EL/FE? I'm leaving them out.
  ll20sampAN<-ll20sampAN[c(1:12,15:nrow(ll20sampAN)),]
  
  #duplicate FE_WholeShoreline_20200701_1130_AN sample info
  # files<-list.files("Fish/input/2020 Long Lake/angling")
  # files<-files[grepl("angling",files)]
  # nrow(ll20sampAN)
  # ll20sampAN[!ll20sampAN$entryFile %in% files]
  # files[!files %in% ll20sampAN$entryFile] #two deleted dead fish
  ll20sampAN<-ll20sampAN[-11,]

ll20sampMT<-read.csv("Fish/input/2020 Long Lake/minnowTraps/fishSamplesIS.csv", header = T, stringsAsFactors = F)
checkCols(ll20sampAN, ll20sampMT)
ll20samp<-rbind(ll20sampAN,ll20sampMT)


#check missing/extra columns#
checkCols(fsamp, ll20samp)

#add/remove columns#
ll20samp<- ll20samp %>%
  add_column(updateID = "sampleFishInfo.20201124") %>%
  add_column(lakeID = word(ll20samp$sampleID,1,1,sep="_")) %>%
  select(colnames(fsamp))

#table fixes#
checkMet(ll20samp)
ll20samp<- ll20samp %>%
  mutate(dateSet = as.Date(parse_date_time(ll20samp$dateSet, orders = c("mdy", "ymd"), tz = "America/New_York"))) %>%
  mutate(dateSample = as.Date(parse_date_time(ll20samp$dateSample, orders = c("mdy", "ymd"), tz = "America/New_York"))) %>%
  mutate(dateTimeSet = parse_date_time(ll20samp$dateTimeSet, orders = c("mdy HM","mdy HMS","ymd HMS"), tz = "America/New_York")) %>%
  mutate(dateTimeSample = parse_date_time(ll20samp$dateTimeSample, orders = c("mdy HM","mdy HMS","ymd HMS"), tz = "America/New_York"))
recreateSampleIDs(ll20samp, fish = T)


###**FISH_INFO####
ll20infoAN<-read.csv("Fish/input/2020 Long Lake/angling/fishInfoIS.csv", header = T, stringsAsFactors = F)
  #--- remove 2 dead fish ---#
  ll20infoAN<- ll20infoAN[-c(223:224),]
ll20infoMT<-read.csv("Fish/input/2020 Long Lake/minnowTraps/fishInfoIS.csv", header = T, stringsAsFactors = F)
checkCols(ll20infoAN, ll20infoMT) #can't combine right away

#check missing/extra columns#
checkCols(finfo, ll20infoAN)
checkCols(finfo, ll20infoMT)

#add/remove columns#
ll20infoAN<- ll20infoAN %>%
  add_column(metadataID = word(ll20infoAN$sampleID,6,6,sep="_")) %>%
  add_column(updateID = "tableFishInfo.20201124") %>%
  rename(otolithSampled = "otolithSample") %>%
  rename(gonadSqueeze = "gonadSqueze") %>%
  select(colnames(finfo))
checkCols(finfo, ll20infoAN)

ll20infoMT<- ll20infoMT %>%
  add_column(metadataID = word(ll20infoMT$sampleID,6,6,sep="_")) %>%
  add_column(updateID = "tableFishInfo.20201124") %>%
  select(colnames(finfo))
checkCols(finfo, ll20infoMT)

#combine ANBE and MT
checkCols(ll20infoAN, ll20infoMT)
ll20info<-rbind(ll20infoAN, ll20infoMT)


#table fixes#
checkMet(ll20info)
unique(ll20info$species) %in% unique(finfo$species) #check species names/spellings
  ll20info<- ll20info %>% mutate(species = gsub("brook_stickelback","brook_stickleback",ll20info$species))
unique(ll20info$metadataID)
unique(word(ll20info$sampleID,6,6,sep="_"))
checkCols(finfo, ll20info) #can combine them right away, beware of multiple date formats


#### 2020 Jones Lake: ####
###**FISH_SAMPLES####
j20samp<-read.csv("Fish/input/2020 Jones Lake/fishSamplesIS.csv", header = T, stringsAsFactors = F)

#check missing/extra columns#
checkCols(fsamp, j20samp)

#add/remove columns#
j20samp<- j20samp %>%
  add_column(updateID = "sampleFishInfo.20201124") %>%
  add_column(lakeID = word(j20samp$sampleID,1,1,sep="_")) %>%
  select(colnames(fsamp))

#table fixes#
checkMet(j20samp) #multiple metadataID spellings
unique(word(j20samp$sampleID,6,6,sep="_")) #multiple metadataID's in sampleID
j20samp<- j20samp %>%
  mutate(dateSet = case_when(j20samp$dateSet == "0020-07-16" ~ "2020-07-16", TRUE ~ j20samp$dateSet)) %>% 
  mutate(dateSample = case_when(j20samp$dateSample == "0020-07-17" ~ "2020-07-17", TRUE ~ j20samp$dateSample)) %>%
  mutate(sampleID = gsub("00200717","20200717",j20samp$sampleID)) 
j20samp<- j20samp %>% 
  mutate(metadataID = "JonesLakeExperiment.20200607") %>%
  mutate(sampleID = paste(word(j20samp$sampleID,1,5,sep="_"),"JonesLakeExperiment.20200607",sep="_")) %>%
  mutate(dateSet = as.Date(j20samp$dateSet)) %>%
  mutate(dateSample = as.Date(j20samp$dateSample)) %>%
  mutate(dateTimeSet = parse_date_time(j20samp$dateTimeSet, orders = "mdy HMS", tz = "America/New_York")) %>%
  mutate(dateTimeSample = parse_date_time(j20samp$dateTimeSample, orders = "mdy HMS", tz = "America/New_York"))
j20samp<- j20samp %>% 
  mutate(siteID = gsub("wholeShoreline","WholeShoreline",j20samp$siteID)) %>%
  mutate(sampleID = gsub("wholeShoreline","WholeShoreline",j20samp$sampleID))
checkMet(j20samp)
recreateSampleIDs(j20samp, fish = T)

###**FISH_INFO####
j20info<- read.csv("Fish/input/2020 Jones Lake/fishInfoIS.csv", header = T, stringsAsFactors = F)

#check missing/extra columns#
checkCols(finfo, j20info)

#add/remove columns#
j20info<- j20info %>% 
  add_column(metadataID = word(j20info$sampleID,6,6,sep="_")) %>% 
  add_column(updateID = "tableFishInfo.20201124") %>% #columns spineSample and scaleSample were new in 2019 but were already added because of the first update, don't need to add them again
  rename(otolithSampled = "otolithSample") %>% 
  rename(gonadSqueeze = "gonadSqueze") %>%
  select(colnames(finfo))
checkCols(finfo, j20info)

#table fixes#
checkMet(j20info)
j20info<- j20info %>% 
  mutate(sampleID = gsub("00200717","20200717",j20info$sampleID)) %>% 
  mutate(fishID = gsub("00200717","20200717",j20info$fishID))
j20info<- j20info %>% 
  mutate(metadataID = "JonesLakeExperiment.20200607") %>% 
  mutate(sampleID = paste(word(j20info$sampleID,1,5,sep="_"),"JonesLakeExperiment.20200607",sep="_")) 
j20info<-j20info %>% 
  mutate(sampleID = gsub("wholeShoreline","WholeShoreline",j20info$sampleID)) %>% 
  mutate(fishID = gsub("wholeShoreline","WholeShoreline",j20info$fishID))

checkMet(j20info)
unique(word(j20info$sampleID,6,6,sep="_"))
checkCols(finfo,j20info)
all(unique(j20info$species) %in% unique(finfo$species) == TRUE)


#### Combining tables ####
fsampNEW<-rbind(fs19samp, ll19samp, ll20samp, j20samp)
finfoNEW<-rbind(fs19info, ll19info, ll20info, j20info)
  
#checks:
#samples
str(fsampNEW)  

nrow(fs19samp) + nrow(ll19samp) + nrow(ll20samp) + nrow(j20samp)
nrow(fsampNEW)
nrow(fs19info) + nrow(ll19info) + nrow(ll20info) + nrow(j20info)
nrow(finfoNEW)

#quick visual
unique(fsampNEW[,1])  
unique(fsampNEW[,2])
unique(fsampNEW[,3])
unique(fsampNEW[,4])
unique(fsampNEW[,5])
unique(fsampNEW[,6])
unique(fsampNEW[,7])
unique(fsampNEW[,8])
unique(fsampNEW[,9])
unique(fsampNEW[,10])
unique(fsampNEW[,11])
unique(fsampNEW[,12])
unique(fsampNEW[,13])
unique(fsampNEW[,14])
unique(fsampNEW[,15])
unique(fsampNEW[,16])
unique(fsampNEW[,17])
  fsampNEW<- fsampNEW %>% mutate(comments = gsub("\"","",fsampNEW$comments))
unique(fsampNEW[,18])
unique(fsampNEW[,19])

#metadataIDs
checkMet(fsampNEW) #need to add "JonesLakeExperiment.20200607" 
checkMet(finfoNEW)

#dateTimeSample == date and time in sampleID?
recreateSampleIDs(fsampNEW, fish = TRUE)
recreateSampleIDs(finfoNEW, fish = TRUE)
#---- could make a fishID checker, but if sampleIDs work and there are no duplicate fishIDs, then it should be good.

  
#are all FISH_INFO in FISH_SAMPLES?
checkINFO(finfoNEW, fsampNEW) 
#Corrected:
  #two dead fish from ll20info not removed, fixed in script

  
  
#duplicated sampleIDs or fishIDs?
checkDuplicates(fsampNEW, "sampleID")
checkDuplicates(finfoNEW, "fishID")
#Corrected:
  #"FE_WholeShoreline_20200701_1130_AN_MarkRecap.20120228" ll20samp
  #"FE_MT.014_20190817_1400_MT_MinnowTrapSurvey.20120228_1" ll19info fishLength==53 should be fishNum2 (and change fishID)

####Write Tables####
write.csv(fsampNEW, "Fish/output/fishSAMPLES.20201204.csv", row.names = F)
write.csv(finfoNEW, "Fish/output/fishINFO.20201204.csv", row.names = F)
  
#Notes:
#!before binding together the different tables, CHECK the TIMEZONE!
#!change "yellow_perch " to "yellow_perch" in finfo and all new tables, dace 
#!check timezones when binding new and old tables


  