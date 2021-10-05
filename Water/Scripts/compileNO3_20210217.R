#Compiles NO3 data from .xlsx run files
rm(list=ls())  #clear variables

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
db="MFEdb_20210423.db"

dir<-'Water/Input/NO3 2020' #copy .xlsx NO3 files into a folder here so you don't mess up the original entry files in JonesLabData 2020/NO3 2020

###convert .xlsx files to .csv
library(rio)
files<-list.files(dir)
for(i in 1:length(files)){
  convert(file.path(dir,files[i]),file.path(dir,gsub(".xlsx",".csv",files[i])))
}
files<-list.files(dir)
files<-files[grep('.csv',files)]
###

i=1
no3<-data.frame() 
for(i in 1:length(files)){
  cur<-read.csv(file.path(dir,files[i]),stringsAsFactor=F)
  cur<-cur[cur[,1]%in%c('Wavelength','MAX','Dilution factor'),]
  cur<-cur[1:3,]
  cur<-data.frame(t(cur))
  cur$runID<-files[i]
  colnames(cur)<- c('sample','abs','dilution','runID')
  cur$abs<-as.numeric(as.character(cur$abs))
  cur<-na.omit(cur[,])
  if(!grepl("F", cur$sample[9])){
    cur$sample<- c(as.numeric(as.character(cur$sample[1:7])),paste0("F",as.numeric(as.character(cur$sample[8:nrow(cur)]))))
  }
  
  #cur$X21<-as.numeric(as.character(cur$X21))
  samples<-cur[grep('F.',cur[,1]),]
  stds<-cur[-grep('F.',cur[,1]),]
  colnames(stds)<-c('std','abs')
  colnames(samples)<-c('sample','abs','dilution','runID')
  stds$std<-as.numeric(as.character(stds$std))
  stds$abs<-as.numeric(as.character(stds$abs))
  samples$abs<-as.numeric(as.character(samples$abs))
  samples$sample<-as.character(samples$sample)
  samples$dilution<-as.numeric(as.character(samples$dilution))
  #samples<-na.omit(samples) # will get rid of dilution thing 
  
  plot(stds$std~stds$abs)
  abline(lm(stds$std~stds$abs))
  stdCurve<-lm(stds$std~stds$abs)$coefficients
  samples$NO3<-(stdCurve[1]+stdCurve[2]*samples$abs)*samples$dilution
  # check to see for dilutions 
  
  
  no3<-rbind(no3,samples)
}

# cracked<-c() #cracked or not enough to rerun
# colnames(no3)
# cracked<-data.frame(sample=cracked,abs=NA,dilution=NA,runID=NA,NO3=NA) #cracked or not enough to rerun
# no3<-rbind(no3,cracked)

log<- read.csv("logFiles2020/correctedFiles/filteredLogFile.csv",header=T, stringsAsFactors = F)
log<-log[2:nrow(log),]
no3M<-merge(no3,log,by.x='sample',by.y='filteredID')
#add comment to cracked samples 
for(i in 1:nrow(no3)){
  if(no3$sample[i]%in%cracked$sample){
  no3$comments[i]<-"cracked or not enough sample remaining"}
  }

dupList<-no3M[duplicated(no3M$sample),1] #lists duplicated samples
dup<-no3M[no3M$sample%in%dupList,] #lists duplicates and originals
#write.csv(dup,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/TN dups.csv",row.names=F)
  #OMG no duplicates AGAIN *triple air horns*

missing<- log$filteredID[!as.character(paste0("F",1:nrow(log))) %in% no3M$sample]
   
no3<-no3M
write.csv(no3, "Water/Output/compiledData/no3Full_20210217.csv", row.names = F) #contains dups after QC
write.csv(no3, "Water/Output/compiledData/no3_20210217.csv", row.names = F) #dups removed to not confuse further steps

# no3_good<-no3
# # no3_good<-no3[!(no3$sample%in%dup$sample &no3$abs%in%remABS),]
# # removed<-no3[(no3$sample%in%dup$sample &no3$abs%in%remABS),] #samples 
# 
# #Check how many remaining: 
# #remaining <- read_excel("C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/TN accounting.xlsx")
# #actually_done<-c("U70","U87","U120","U187") #typos in data sheets, fixed
# #remaining<-remaining[!(remaining$UID%in%actually_done),] #removes samples that actually had data and have been fixed
# #add1<-c("137","U137",NA,NA) #needs rerun because mixed up with U317 in same run, adding it to the remaining bucket and need to change location in tables
# #nrow(log)-(nrow(no3_good)+nrow(remaining)) #should equal zero. Has 7 that aren't run that aren't in the remaining frozen samples bucket.
# nrow(log)-nrow(no3_good)
# #rem<-merge(remaining,log,by.x="UID",by.y="filteredID") #makes table of remaining with sample info (helpful for run sheets)
# #rem<-rem[,c(1,4,5,6,8)]
# 
# find_missing<-merge(log,no3_good[,c("sample","abs","NO3")],by.x="filteredID",by.y="sample",all.x=TRUE)
# find_missing<-find_missing[is.na(find_missing$NO3),]
# #find_missing<-find_missing[!(find_missing$filteredID%in%remaining$FID),]
# find_missing<-find_missing[!(find_missing$filteredID%in%cracked$sample),] #still missing
# 
# #nrow(no3_good)+nrow(remaining)+nrow(find_missing)+length(cracked) 
# nrow(no3_good)+nrow(find_missing)
# #full<-c(no3_good$sample,remaining$UID,find_missing$filteredID,cracked) #mash up of all four lists
# full<-c(no3_good$sample,find_missing$filteredID)
# full[duplicated(full)] #list of samples in multiple tables. 
#   
# write.csv(no3_good,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/NO3_merged_20200313.csv",row.names=F) #Does not contain samples with no values (missing/cracked) 
# 

#quick check histograms
hist(no3$NO3)
dbNO3<-dbTable("WATER_CHEM") %>% filter(parameter == "nitrate" & !projectID == "30")
hist(dbNO3$parameterValue) 




















##Settings######################################
savefile=1  #set to 1 to save data to Excel
################################################

#Missing samples
have<-read.csv("C:/Users/notter/Google Drive/JonesLabData 2019/Data/NO3 2019/Missing samples.csv")
full<-1:357
miss<-full[!(full %in% have$fno)]
miss<-data.frame(fno=miss)

#read in data log
dataLog<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/filteredLogFile.csv")
dataLog$fno<-dataLog$filteredID
dataLog$fno<-gsub("F", "", as.character(dataLog$fno))

#merge all.chl with the dataLog to add sample information
merge<-merge(miss,dataLog, by='fno')
write.csv(merge,paste(dir,"Missing list.csv",sep=""))
write.csv(data.all,paste(saveloc,savefilename,sep=''),row.names=F)
merge<-merge[,c("projectID","sampleID","lakeID","site","dateSample","timeSample","depthClass","depthTop","depthBottom","runID","chl","replicate","comments")]
colnames(merge)[colnames(merge)=="site"] <- "siteName"























#Create .csv files of only relevant data from chla .xlsx file
library(readxl)

all.no3=data.frame() #makes an all.chl df so that I can write to it later
i=1
for(i in 1:length(files)){
  fID<-names(read_excel(file.path(dir,files[i]),range="I3:BN3"))
  fID<-fID[grep('F',fID)]
  data.all<-data.frame(fID=fID) #Do the same as above (with other variables) and then list them out in this dataframe 
  #data.all<-na.omit(data.all)
  #data<-data.all[,c("SampleID","C#","sampleID","conc of chl in lake (ug/L)","Run #")]
  #names(data) <- c("SampleID", "Cno","chlID","chl","runID")
  
  data.all$Fno<-sub('.', '', data.all$fID)
  data.all$runID=i #fixes runID data entry misunderstanding. runID refers to run number, not sample # per run (as entered in 2019).
  
  
  savefilename<-paste(strsplit(files[i],'.',fixed=T)[[1]][1],'.csv',sep='')
  saveloc<-'C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/NO3/'
  write.csv(data.all,paste(saveloc,savefilename,sep=''),row.names=F)
  
  
  all.no3<-rbind(all.no3,data.all) #writes each .csv (a temp df called 'data' in the loop) to the all.chl df
  all.no3$Fno<-as.numeric(all.no3$Fno)
  all.no3<-all.no3[order(all.no3$Fno),]
}

write.csv(all.chl,paste(saveloc,"mergeChlData.csv"),row.names=F)
