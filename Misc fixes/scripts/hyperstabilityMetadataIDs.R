#sampleIDs in FISH_SAMPLES and FISH_INFO have "hyperstability.20180604" (not in METADATA) instead of "hyperstability.20180521" (Good)
#It seems like some of them were corrected but not all of them, trying to track it down
library(tidyverse)
library(lubridate)
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")


#most recent
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201125.db"
last(dbTable("VERSION_HISTORY")$dbTitle)
finfo<-dbTable("FISH_INFO")
fsamp<-dbTable("FISH_SAMPLES")

finfo %>% add_column(smeta = word(finfo$sampleID,6,6,sep="_")) %>% group_by(smeta) %>% count() #270 have "20180604" in sample/fishIDs
finfo %>% group_by(metadataID) %>% count() #270 have metadataID column "20180604"
View(fsamp %>% add_column(smeta = word(fsamp$sampleID,6,6,sep="_")) %>% group_by(smeta) %>% count()) #15 FISH_SAMPLES

#before updateID FISH_INFO_metadataID_add.20201120 that added a metadataID column to FISH_INFO
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201020_3.5.3.db"
last(dbTable("VERSION_HISTORY")$dbTitle)
finfo<-dbTable("FISH_INFO")
fsamp<-dbTable("FISH_SAMPLES")

View(finfo %>% add_column(smeta = word(finfo$sampleID,6,6,sep="_")) %>% group_by(smeta) %>% count()) #270 have "20180604" in sample/fishIDs
View(fsamp %>% add_column(smeta = word(fsamp$sampleID,6,6,sep="_")) %>% group_by(smeta) %>% count()) #17

#before updateID metadataFix.2020
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20200708.db"
last(dbTable("VERSION_HISTORY")$dbTitle)
finfo<-dbTable("FISH_INFO")
fsamp<-dbTable("FISH_SAMPLES")

View(finfo %>% add_column(smeta = word(finfo$sampleID,6,6,sep="_")) %>% group_by(smeta) %>% count()) #270 have "20180604" in sample/fishIDs
View(fsamp %>% add_column(smeta = word(fsamp$sampleID,6,6,sep="_")) %>% group_by(smeta) %>% count()) #17
View(dbTable("METADATA")) #"20180604" never existed and should just be replaced. Need to see where "20180604" exists in db

findID("hyperstability.20180604") #just in FISH_INFO (sampleID and metadataID) and FISH_SAMPLES (sampleID) in all versions

#Fix: changed "hyperstability.20180604" to 20180521 because Colin couldn't find anything for 0604.



