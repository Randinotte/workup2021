source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20210402.db" 
dbTableList() #checks db

library(tidyverse)
library(lubridate)

View(dbTable("VERSION_HISTORY"))

met<-dbTable("METADATA")
met<- read.csv("Updates/update1/metadata_fix.csv", header = T, stringsAsFactors = F) #has JonesLakeExperiment.20200607
dup<- met[duplicated(met$metadataID)==TRUE,]
full<-met[met$metadataID %in% dup$metadataID,]

metNew<- met[duplicated(met$metadataID)==FALSE,]
write.csv(metNew, "Misc fixes/output/METADATAfix.20201124.csv",row.names=F)
write.csv(metNew, "Updates/update3/METADATA.20210414.csv", row.names=F)
write.table(metNew,file.path("Updates/update3",'METADATA.txt'),quote = F,sep = '|',row.names=F,col.names=F) #has both JO and removed duplicates



