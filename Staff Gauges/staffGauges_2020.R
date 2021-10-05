# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("C:/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210423.db" #Change name of db file with every update

library(tidyverse)

dbS<-dbTable("STAFF_GAUGES")
newS<- read.csv("logFiles2020/correctedFiles/samplesIS.csv", header = T, stringsAsFactors = F)
checkCols(dbS, newS)
newS<- newS %>% 
  filter(depthClass == "Staff") %>% 
  mutate(depthClass = "staff") %>% 
  add_column(projectID = NA) %>% 
  add_column(lakeID = word(newS$sampleID,1,1,sep="_")) %>% 
  add_column(siteName = word(newS$sampleID,2,2,sep="_")) %>% 
  



