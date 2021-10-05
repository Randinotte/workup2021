rm(list=ls())
library(ggplot2)
## load data and connect multiple files
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20210423.db"

#seems like Kaija already fixed this with the updateID S_sampleIDReconstruct

dbChl<-dbTable("CHLOROPHYLL")
dbChl$time<- gsub(":","",substr(word(dbChl$dateTimeSample,2,2,sep=" "),1,5))
dbChl$IDtime<- word(dbChl$sampleID,4,4,sep="_")
for(i in 1:nrow(dbChl)){
  if(dbChl$time[i] == dbChl$IDtime[i]){
    dbChl$match[i]<- "match"
  }
}

#yep, she took care of it.