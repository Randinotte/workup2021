#setwd('C:/Users/notter/OneDrive/Randi/Database/currentDB') #set wd to where .db file is
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
dbdir="C:/Users/notter/Box/MFE/Database/Archived DB Files/MFEdb_20210217/"
db="MFEdb_20210217.db"
last(dbTable("VERSION_HISTORY")$dbTitle)

dbTableList()
View(dbTable("VERSION_HISTORY"))

dbSamp<-dbTable("SAMPLES")
