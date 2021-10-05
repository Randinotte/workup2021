#noticed by Kaija in Slack PMs on 12/18/20
#some fish in FISH_INFO have tagRecaptureType=="pit" & tagRecapture=="NA", which could be a problem


source('C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R') # Load 'dbTable' and 'dbTableList' functions
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201215.db" #make sure this script uses new database file
dbTableList() # Make sure that dbname is changed in this function (and in dbTable)
View(dbTable("VERSION_HISTORY"))


library(readxl)
prob<- read_excel("Misc fixes/input/pitTagMissing_sampleIDList.xlsx")
finfo<-dbTable("FISH_INFO")
probFull<- finfo %>% 
  filter(finfo$sampleID %in% prob$sampleID)

#Ok what's really weird is that all of the fish have the same tag number if they have one "900.2280006"
probFull$year<- substr(word(probFull$sampleID,3,3,sep="_"),1,4)
unique(probFull$year)
#they're all from 2018 and have 2020 metadataIDs because: 

probFull$tagRecapture<-as.character(probFull$tagRecapture)
hyper<-finfo %>% filter(finfo$projectID=="38")

#reached out to Colin on 12/18 in MFE/#database. Want to retrace from raw csv (either from indv. datasheet or entry tool output)
  #but can't find it in Box or Alex' archive

pit<- finfo %>% 
  filter(tagRecaptureType=="pit")
