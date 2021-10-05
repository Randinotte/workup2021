#Compiles chl data from .xlsx run files
rm(list=ls())  #clear variables

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("C:/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update


dir<-'C:/Users/notter/Google Drive/JonesLabData 2020/Chl 2020/'


files<-list.files(dir)
files<-files[grep('Chl_Run',files)]
 

##Settings######################################
savefile=1  #set to 1 to save data to Excel
################################################

#Create .csv files of only relevant data from chla .xlsx file
library(readxl)

all.chl=data.frame() #makes an all.chl df so that I can write to it later
i=3
for(i in 1:length(files)){
  data.all<-read_excel(file.path(dir,files[i]),range="B14:L200")
  data.all<-na.omit(data.all)
  data<-data.all[,c("SampleID","C#","sampleID","conc of chl in lake (ug/L)","Run #")]
  names(data) <- c("SampleID", "Cno","chlID","chl","runID")

  data$runID=i #fixes runID data entry misunderstanding. runID refers to run number, not sample # per run (as entered in 2019).
  
  savefilename<-paste(strsplit(files[i],'.',fixed=T)[[1]][1],'.csv',sep='')
  saveloc<-'C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/Chla/'
  write.csv(data,paste(saveloc,savefilename,sep=''),row.names=F)

  
  all.chl<-rbind(all.chl,data) #writes each .csv (a temp df called 'data' in the loop) to the all.chl df
  all.chl<-all.chl[order(all.chl$Cno),]
  }

#write.csv(all.chl,paste(saveloc,"mergeChlData.csv"),row.names=F)
dupList<-all.chl[duplicated(all.chl$Cno),1] #lists duplicated samples
dup<-all.chl[all.chl$Cno%in%dupList,] #lists duplicates and originals
#write.csv(dup,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/POPM dups.csv",row.names=F)
  #none! yay!

missing<- dataLog$chlID[!as.character(paste0("C",1:nrow(dataLog))) %in% all.chl$chlID]
  #none! yay!


#read in data log
dataLog<-read.csv("logFiles2020/correctedFiles/chlLogFile.csv", header = T, stringsAsFactors = F)
dataLog<-dataLog[-1,]
dataLog$Cno<-dataLog$chlID
dataLog$Cno<-gsub("C", "", as.character(dataLog$Cno))

#merge all.chl with the dataLog to add sample information
merge<-merge(all.chl,dataLog, by='Cno')
merge<-merge[,c("projectID","sampleID","lakeID","site","dateSample","timeSample","depthClass","depthTop","depthBottom","runID","chl","replicate","comments")]
colnames(merge)[colnames(merge)=="site"] <- "siteName"

#object merge is missing columns: dateTimeSample, flag, updateID, and metadataID
dbchl<-dbTable("CHLOROPHYLL")
checkCols(dbchl,merge)
merge$dateTimeSample<- as.POSIXct(paste(merge$dateSample,merge$timeSample,sep=" "), format = "%m/%d/%Y %H:%M", tz = "America/Chicago")
merge$dateSample<- as.Date(merge$dateSample, format = "%m/%d/%Y")
#merge$dateTimeSample=gsub(":00 EDT","",strptime(merge$dateTimeSample, format= "%m/%d/%Y %H:%M"))
#dateTimeSample needs to be in POSIXct format "%Y-%m-%d %H:%M"

merge$flag<-0
merge$updateID<-paste("TBD")
merge$metadataID<-paste("Chlorophyll.20110601")
#   reg<-c("FE","WL","MO","CR","HB","ME","PE","PA","BA","BO","BR","CB","NG","TU","WA","BY","JS","TF","EL")
#   full<-unique(merge$lakeID)
#   not<-full[!(full %in% reg)]
# for(row in 1:nrow(merge)){  
#   if(merge$lakeID[row] %in% not){
#     merge$metadataID[row]<-gsub("Limno.Sample.20160505","fishScapesLimno.20190319",merge$metadataID[row])
#   }
# }

merge<-merge[names(dbchl)] #make merge in order of the dbTable CHLOROPHYLL


#write a .csv file titled "chlorophyll_QC_20191112.csv" (columns match dbTable("CHLOROPHYLL"))
write.csv(merge,"Water/Output/compiledData/chlorophyll_20210218.csv", row.names = F)


