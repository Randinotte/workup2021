#### Zooplankton Abundance/Biomass Script Update####
# AJR: 2018-10-18
# RNN: 2019-12-17
#Copied for use in workup2020 on 2020-11-17

# Purpose of script is to use count, subsample, and length data to calculate zooplankton abundance and biomass

rm(list=ls())  #clear variables
graphics.off()  #close figs

#Load libraries and dbFunctions and data template
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db= "MFEdb_20201120.db"
  #check db version
  dbTableList()
  View(dbTable("VERSION_HISTORY"))

library(tidyverse)
library(lattice)
library(stringr)
library(readxl)


#Load individual csvs for count, subsample, length data -- change files to appropriate title
zCount<- read_excel("Zoops/input/2020_zoopCounting.xlsx",2)
zCoef <- dbTable("Zoops_coefficients")
zLen <- read_excel("Zoops/input/2020_zoopCounting.xlsx",3)
zSub <- read_excel("Zoops/input/2020_zoopCounting.xlsx",1)

#fixes needed for datasheets
zCount<- zCount %>% 
  rename(dateSample = "dateTimeSample") %>% 
  mutate(dateSample = as.character(dateSample)) %>% 
  mutate(dateSample = case_when(dateSample == "2020-06-11" ~ as.Date("2020-06-09"), TRUE ~ as.Date(dateSample))) %>% #wrong sample date (switched for sample count date)
  mutate(lakeID = case_when(lakeID == "FE" & dateSample == "2020-07-21" ~ "WL", TRUE ~ lakeID)) %>% #incorrect lakeID
  mutate(taxa = case_when(taxa == "Cyclopoids" ~ "Cyclopoid", TRUE ~ taxa)) %>% #should be singular
  mutate(count = case_when(taxa == "Chaoborus" & count == "0" ~ 17, TRUE ~ count)) %>% #chaob count entered as 0 instead of 17 (from datasheet)
  mutate(count = case_when(taxa == "Holopedium" & lakeID == "WL" & dateSample == "2020-06-30" ~ 2, TRUE ~ count)) %>% #one daphnia was actually a holopedium
  mutate(count = case_when(taxa == "Daphnia" & lakeID == "WL" & dateSample == "2020-06-30" ~ 7, TRUE~ count)) #one daphnia was actually a holopedium

addLen<- read_excel("Zoops/input/zoopsAddLengths.xlsx") %>% #sheet of zoops RN measured because they were missing
  mutate(dateSample = as.Date(dateSample))
zLen<- zLen %>% 
  rename(dateSample = "dateTimeSample") %>% 
  mutate(dateSample = as.character(dateSample)) %>%
  mutate(dateSample = case_when(dateSample == "2020-06-11" ~ as.Date("2020-06-09"), TRUE ~ as.Date(dateSample))) %>% #wrong sample date (switched for sample count date)
  mutate(lakeID = case_when(dateSample == "2020-07-22" ~ "FE", TRUE ~ lakeID)) #wrong sample date
zLen<- rbind(zLen, addLen) %>% 
  arrange(dateSample, lakeID, taxa)

zSub<- zSub %>% 
  rename(dateSample = "dateTimeSample") %>% 
  add_row(projectID = 3, lakeID = "FE", siteName= "DeepHole", dateSample = as.Date("2020-08-04"), depthTop = 0, depthBottom = 8, 
          wtEmpty = 13.0, wtFull = 68.7, wtSubsample = 9.6, comment = "added to table by RNN in processing", SampleID = "Z849",
          .before = 19) %>% #subsample info missing from sheet, copied from paper datasheet
  add_row(projectID = 3, lakeID = "WL", siteName= "DeepHole", dateSample = as.Date("2020-08-04"), depthTop = 0, depthBottom = 8, 
          wtEmpty = 13.0, wtFull = 88.9, wtSubsample = 17.2, comment = "added to table by RNN in processing", SampleID = "Z850",
          .before = 20) #subsample info missing from sheet, copied from paper datasheet


#Load zooplankton log file to get sampleIDs and times for the zCount, zLen, and zSub tables (may not always be necessary)
log<- read.csv("logFiles2020/correctedFiles/zoopLogFile.csv",header = T, stringsAsFactors = F)
log<-log[-1,]
log$dateSample<- as.Date(log$dateSample, format="%m/%d/%Y")
log$shortID<-substr(log$sampleID,1,20)

#fixes for Z830-831, samples when Jack went out with Camille and taught her zoops
log<- log %>% 
  mutate(dateSample = as.character(dateSample)) %>%
  mutate(dateSample = case_when(zoopID %in% c("Z830","Z831") ~ as.Date("2020-06-29"), TRUE ~ as.Date(dateSample))) %>% 
  mutate(timeSample = case_when(zoopID == "Z830" ~ "12:00", TRUE ~ timeSample)) %>%
  mutate(sampleID = case_when(zoopID == "Z830" ~ "CR_DeepHole_20200629_1200_tow_8_Limno.Sample.20160505", TRUE ~ sampleID)) %>%
  mutate(shortID = case_when(zoopID == "Z830" ~ "CR_DeepHole_20200629", TRUE ~ shortID)) %>%
  mutate(timeSample = case_when(zoopID == "Z831" ~ "12:05", TRUE ~ timeSample)) %>%
  mutate(sampleID = case_when(zoopID == "Z831" ~ "CR_DeepHole_20200629_1205_tow_8_Limno.Sample.20160505", TRUE ~ sampleID)) %>%
  mutate(shortID = case_when(zoopID == "Z831" ~ "CR_DeepHole_20200629", TRUE ~ shortID))

#format zCount table
zCount$shortID<- paste(zCount$lakeID,"_",zCount$siteName,"_",gsub("-","",zCount$dateSample),sep="")
zCount <- log %>%
  select(shortID, timeSample, sampleID) %>% 
  right_join(zCount,log,by="shortID") %>% 
  mutate(dateTimeSample=paste(dateSample,timeSample)) %>% 
  select(-shortID,-timeSample) %>%
  select(projectID,sampleID,lakeID,siteName,dateSample,dateTimeSample,depthTop,depthBottom,taxa,count)

#format zSub
zSub <- zSub %>% rename(comments=comment) %>% rename(dateTimeSample = "dateSample")
zSub$shortID<- paste(zSub$lakeID,"_",zSub$siteName,"_",gsub("-","",zSub$dateTimeSample),sep="")
zSub <- log %>%
  select(shortID, timeSample, sampleID) %>% 
  right_join(zSub,log,by="shortID") %>% 
  mutate(dateTimeSample=paste(dateTimeSample,timeSample)) %>%  
  select(-shortID,-timeSample) %>%
  select(projectID,sampleID,lakeID,siteName,dateTimeSample,dateTimeSample,depthTop,depthBottom,wtEmpty,wtFull,wtSubsample,comments)

#format zLen
zLen$shortID<-paste(zLen$lakeID,"_",zLen$siteName,"_",gsub("-","",zLen$dateSample),sep="")
zLen <- log %>%
     select(shortID, timeSample, sampleID) %>% 
     right_join(zLen,log,by="shortID") %>% 
     mutate(dateTimeSample=paste(dateSample,timeSample)) %>%  
     select(-shortID,-timeSample) %>%
    select(projectID,sampleID,lakeID,siteName,dateSample,dateTimeSample,depthTop,depthBottom,taxa,length,width,fileName)

#If there's no updateID already in the file, put one in for each observation
zCount$updateID <- "longZoops_2020.20201123"
zLen$updateID <- "longZoops_2020.20201123"
zSub$updateID <- "longZoops_2020.20201123"

#If there's no depthClass already in the file, put one in for each observation
zCount$depthClass <- "tow"
zLen$depthClass <- "tow"
zSub$depthClass <- "tow"

#If there's no metadataID already in the file, put one in for each observation
zCount$metadataID <- "zoopCounts.20191212" #Stays the same unless the counting metadata file has changed
zLen$metadataID <- "zoopCounts.20191212"
zSub$metadataID <- "zoopCounts.20191212"

#Change metadataID portion of the sampleID
zCount$sampleID<- gsub("Limno.Sample.20160505","zoopCounts.20191212",zCount$sampleID)
zLen$sampleID<- gsub("Limno.Sample.20160505","zoopCounts.20191212",zLen$sampleID)
zSub$sampleID<- gsub("Limno.Sample.20160505","zoopCounts.20191212",zSub$sampleID)

#Fix zCoef,Count,Len taxa names to all lower case letters (if not, merge will goof)
zCount$taxa <- tolower(zCount$taxa)
zCoef$taxa <- tolower(zCoef$taxa)
zLen$taxa <- tolower(zLen$taxa)

#check that all the taxa names are correct before proceeding
countNames<-unique(zCount$taxa)
lenNames<-unique(zLen$taxa) 
dbCount<-dbTable("ZOOPS_ABUND_BIOMASS")
dbLen<-dbTable("ZOOPS_LENGTHS")
dbCountNames<-unique(dbCount$taxa) #singular, lowercase
dbLenNames<-unique(dbLen$taxa)
countNames %in% dbCountNames
lenNames %in% dbLenNames

#check dimensions 
nrow(zSub)
length(unique(zCount$sampleID))
nrow(zLen %>% group_by(lakeID, dateSample) %>% count())


#### Mass #### - need to do a seperate function for leptedora
#Calculate mass for each measured individual
zLen <- zLen %>%
  left_join(select(zCoef,taxa,intercept,slope), by="taxa") %>%
  group_by(taxa) %>%
  mutate(mass = ifelse(taxa == "leptodora", intercept*(length)^(slope),(exp(intercept+(slope*(log(length)))))))
zLen <- as.data.frame(zLen) %>%
  select(-intercept, -slope) #Removing extra columns


####Abundance ####
#Calculate subsample multiplier (for scaling up subsample to sample abundance)
zSub <- zSub %>%
  mutate(subMulti = wtSubsample/(wtFull-wtEmpty) )
  
#Calculate counts per sample, by scaling up with unique sampleID multiplier
netarea<-(0.3048/2)^2*pi #m2; 12 inch diameter net = 0.3048m diameter ***Can change radius if using dif sized net

zCount <- zCount %>%
  inner_join(select(zSub,sampleID,subMulti,comments), by='sampleID') %>%
  group_by(sampleID,taxa, depthBottom, depthTop) %>%
  mutate(sampleCount = (count/subMulti))

#Calculate abundance per m2
zCount <- zCount %>%
  mutate(abunM2 = (sampleCount/2)/netarea/0.25) #0.25 is a 'net efficiency' factor. Number was assumed by Patrick Kelly and Jim Coloso; should keep.

#Calculate abundance per m3
zCount <- zCount %>%
  mutate(abundance_num_m3 = abunM2/(depthBottom-depthTop)) %>%
  select(-subMulti, -sampleCount, -abunM2) %>%
  arrange(dateSample, lakeID, depthBottom) %>%
  distinct() #This just uses the first entry in zSub and zCount if there are replicate samples in the data

#### Biomass ####
# Add mean biomass to counts table (aka. abundance table)
zMassAvg <- zLen %>%
  group_by(sampleID, taxa) %>%
  summarise(meanMass_ug = mean(mass, na.rm = F))
zAbunNew <- zCount %>%
  left_join(zMassAvg, by=c("sampleID", "taxa")) #####?
# Biomass per m3
zAbunNew <- zAbunNew %>%
  mutate(biomass_gDryMass_m3 = abundance_num_m3*meanMass_ug/10^6) %>% 
  select(projectID, sampleID, lakeID, siteName, dateSample, dateTimeSample, depthClass, depthTop, depthBottom, taxa, count, meanMass_ug, abundance_num_m3, biomass_gDryMass_m3,
         metadataID, comments, updateID)

#check zAbunNew for irregularities
#meanMass_ug and biomass_gDryMass_m3 NA for some rows (usually missing for just Chaobs but nothing else).
  # missing<- zAbunNew[is.na(zAbunNew$meanMass_ug) & is.na(zAbunNew$biomass_gDryMass_m3),] 
  # missing<- missing[!missing$taxa=="chaoborus",]
  #Answer: these did not have lengths in zLen because they were never measured. I measured them and added them to zoopsAddLengths.xlsx


#### Organize columns and write CSV ####
str(zAbunNew)
zAbunNew$projectID<-as.character((zAbunNew$projectID))
zAbunNew$sampleID<-as.character(zAbunNew$sampleID)
zAbunNew$dateTimeSample<- as.POSIXct(zAbunNew$dateTimeSample, format="%Y-%m-%d %H:%M")
zAbunNew$count<-as.numeric(zAbunNew$count)
zAbunNew$depthTop<-as.numeric(zAbunNew$depthTop)
zAbunNew$depthBottom<-as.numeric(zAbunNew$depthBottom)
write.csv(zAbunNew, "Zoops/output/tableZOOPS_ABUND_BIOMASS.20201123.csv", row.names = F) #name file whatever makes sense



### Writing Zoops tables in db format ###
#dbTable("ZOOPS_ABUND_BIOMASS"): "tableZoop_AbundBiomass.20201123.csv" from above
#dbTable("ZOOPS_LENGTHS"): Based on zLen from above
names(zLen)
zLen$mass<-NA
zLen$eggs<-NA
zLen <- as.data.frame(zLen) %>%
  select(-fileName)
zLen<- zLen[names(dbTable("ZOOPS_LENGTHS"))]
write.csv(zLen,"Zoops/output/tableZOOPS_LENGTHS.20201123.csv",row.names = F)

#dbTable("ZOOPS_SUBSAMPLE"): Based on zSub from above
names(zSub)
names(dbTable("ZOOPS_SUBSAMPLE"))
zSub<-as.data.frame(zSub) %>%
  select(-subMulti)
zSub<-zSub[names(dbTable("ZOOPS_SUBSAMPLE"))]
write.csv(zSub,"Zoops/output/tableZOOPS_SUBSAMPLE.20201123.csv",row.names = F)

####Making a SITES table from ZOOPS_SUBSAMPLE####
#create SITES table
s<-zSub
siteZoop<- s[order(s$sampleID),]
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteZoop$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long")]
siteZoop<-merge(siteZoop, siteLakes, by="lakeID")
siteZoop$siteID<-paste(siteZoop$lakeID,siteZoop$siteName, sep="_")
siteZoop$updateID<-"longZoops_2020.20201123"
wantSite<-c("siteID","lakeID","siteName","lat","long","updateID")
siteZoop<-siteZoop[wantSite]
View(unique(siteZoop$siteID)) #check for weird spellings, semi-duplicates

write.csv(siteZoop,"Zoops/output/siteZoops_20201123.csv", row.names = F)


####Making a SAMPLES table from ZOOPS_SUBSAMPLE####
dbSamples<-dbTable("SAMPLES")
zSub<- zSub %>% 
  add_column(dateSample = as.Date(zSub$dateTimeSample))

#Get crew and weather from 2020 samplesIS.csv (needed for SAMPLES table)
log<-read.csv("logFiles2020/correctedFiles/samplesIS.csv", header = T, stringsAsFactors = F)
# log<-log[-1,]
log<-subset(log,log$depthClass=="tow")

samplesZoop<- log %>% 
  mutate(sampleID = gsub("Limno.Sample.20160505","zoopCounts.20191212", log$sampleID)) %>% 
  select(sampleID, crew, weather) %>% 
  right_join(zSub, log, by="sampleID") %>% 
  add_column(siteID = word(zSub$sampleID, 1,2,sep="_")) %>% 
  mutate(dateTimeSample = as.POSIXct(dateTimeSample)) %>% 
  mutate(crew = as.character(crew)) %>% 
  mutate(weather = as.character(weather)) %>% 
  select(colnames(dbSamples))

write.csv(samplesZoop, "Zoops/output/samplesZoops_20201123.csv", row.names = F)