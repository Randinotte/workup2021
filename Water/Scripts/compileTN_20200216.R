## TN 2014; JAZ 2015-07-31; RNN 2020-02-04, RNN 2020-02-17

rm(list=ls())
# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
db="MFEdb_20210423.db"

dir<-'Water/Input/TN 2020'

###convert .xlsx files to .csv
files<-list.files(dir)
for(i in 1:length(files)){
  convert(file.path(dir,files[i]),file.path(dir,gsub(".xlsx",".csv",files[i])))
}
files<-list.files(dir)
files<-files[grep('.csv',files)]
#files<-files[-grep("rerun", files)]
###

library(readxl)
library(rio)
library(dplyr)

i=4
tn<-data.frame() 
for(i in 1:length(files)){
  cur<-read.csv(file.path(dir,files[i]),stringsAsFactor=F)
  cur<-cur[cur[,1]%in%c('Wavelength','MAX','Dilution factor'),]
  cur<-cur[1:3,]
  cur<-data.frame(t(cur))
  cur$runID<-files[i]
  colnames(cur)<- c('sample','abs','dilution','runID')
  cur$abs<-as.numeric(as.character(cur$abs))
  cur<-na.omit(cur[,])
  if(!grepl("U", cur$sample[9])){
    cur$sample<- c(as.numeric(as.character(cur$sample[1:7])),paste0("U",as.numeric(as.character(cur$sample[8:nrow(cur)]))))
  }

  samples<-cur[grep('U.',cur[,1]),]
  stds<-cur[-grep('U.',cur[,1]),]
  colnames(stds)<-c('std','abs')
  stds$std<-as.numeric(as.character(stds$std))
  stds$abs<-as.numeric(as.character(stds$abs))
  samples$abs<-as.numeric(as.character(samples$abs))
  samples$sample<-as.character(samples$sample)
  samples$dilution<-as.numeric(as.character(samples$dilution))
  #samples<-na.omit(samples) # will get rid of dilution thing 
  
  plot(stds$std~stds$abs)
  abline(lm(stds$std~stds$abs))
  stdCurve<-lm(stds$std~stds$abs)$coefficients
  samples$TN<-(stdCurve[1]+stdCurve[2]*samples$abs)*samples$dilution
  # check to see for dilutions 
  
  
  tn<-rbind(tn,samples)
}


#quick check histograms
hist(tn$TN)
dbTN<-dbTable("WATER_CHEM") %>% filter(parameter == "TN")
hist(dbTN$parameterValue)

# cracked<-c("U247","U212","U110","U344") #cracked or not enough to rerun
# colnames(tn)
# cracked<-data.frame(sample=cracked,abs=NA,dilution=NA,runID=NA,TN=NA) #cracked or not enough to rerun
# tn<-rbind(tn,cracked)


log<- read.csv("logFiles2020/correctedFiles/unfilteredLogFile.csv",header=T, stringsAsFactors = F)
log<-log[-1,]
log$unfilteredID<-gsub(" ","",log$unfilteredID)
tnM<-merge(tn,log,by.x='sample',by.y='unfilteredID')
#add comment to cracked samples 
for(i in 1:nrow(tnM)){
  if(tnM$sample[i]%in%cracked$sample){
  tnM$comments[i]<-"cracked or not enough sample remaining"}
  }

dupList<-tnM[duplicated(tnM$sample),1] #lists duplicated samples
dup<-tnM[tnM$sample%in%dupList,] #lists duplicates and originals
#write.csv(dup,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/TN dups.csv",row.names=F)

tnM$sample<-as.character(tnM$sample)
missing<- log$unfilteredID[!as.character(paste0("U",1:nrow(log))) %in% tnM$sample]
  #As in TP, U138 is duplicated and there's no way to know which is which. U138 and U148 need to be rerun. 

#Remove problems from output that gets used in further scripts (compileNutrientTables.R and plottingNutrientsforQC.R)
tn<-tnM %>% filter(!sample == "U138")
tnFull<-tnM

#no more dups or missing, yay!
write.csv(tnFull, "Water/Output/compiledData/tnFull_20210216.csv", row.names = F) #has duplicates after QC
write.csv(tn, "Water/Output/compiledData/tnCorrected_20210429.csv", row.names = F) #has duplicates removed

# remABS<-c(0.177,0.072,0.761,0.062,1.03,0.254,0.243,0.406,0.286) #abs values of samples that you want to remove (looking through the TPdups.csv)
# 
# tn_good<-tn[!(tn$sample%in%dup$sample &tn$abs%in%remABS),]
# removed<-tn[(tn$sample%in%dup$sample &tn$abs%in%remABS),] #samples 

#Check how many remaining: 
#remaining <- read_excel("C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/TN accounting.xlsx")
#actually_done<-c("U70","U87","U120","U187") #typos in data sheets, fixed
#remaining<-remaining[!(remaining$UID%in%actually_done),] #removes samples that actually had data and have been fixed
# add1<-c("137","U137",NA,NA) #needs rerun because mixed up with U317 in same run, adding it to the remaining bucket and need to change location in tables
# add2<-c("317","U317",NA,NA)   
# add3<-c("12","U12",NA,NA)   #notes indicate need for rerun, very high
# add4<-c("187","U187",NA,NA) #found in freezer, added to remaining
# add5<-c("296","U296",NA,NA) #found in freezer, added to remaining
# add6<-c("62","U62",NA,NA)   #found in freezer, added to remaining
# remaining<-rbind(remaining,add1, add2, add3, add4,add5,add6)
# nrow(log)-(nrow(tn_good)+nrow(remaining)) #should equal zero. Has 7 that aren't run that aren't in the remaining frozen samples bucket.
# rem<-merge(remaining,log,by.x="UID",by.y="unfilteredID")
# rem<-rem[,c(1,4,5,6,8)]

# find_missing<-merge(log,tn_good[,c("sample","abs","TN")],by.x="unfilteredID",by.y="sample",all.x=TRUE)
# find_missing<-find_missing[is.na(find_missing$TN),]
# #find_missing<-find_missing[!(find_missing$unfilteredID%in%remaining$UID),]
# find_missing<-find_missing[!(find_missing$unfilteredID%in%cracked$sample),] #still missing
# 
# nrow(tn_good)+nrow(remaining)+nrow(find_missing)+length(cracked) 
# #full<-c(tn_good$sample,remaining$UID,find_missing$unfilteredID,cracked) #mash up of all four lists
# full<-c(tn_good$sample,find_missing$unfilteredID,cracked$sample)
# full[duplicated(full)] #list of samples in multiple tables. 
#   #U95 is in tn_good and remaining. tn_good sample info matches handwritten Run9 log. Need to check U95 in remaining.Is in remaining. WHY?
#   #U295 is in tn_good and remaining. tn_good sample info matches handwritten Run12 log. Need to check U295 in remaining.Is not in remaining. WHY?

#write.csv(tn_good,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/TN_merged_20200313.csv",row.names=F) #Does not contain samples with no values (missing/cracked) 


 
# ########
# tn<-tn[!duplicated(tn$sample),]
# 
# tnLog2019<-read.csv('C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/unfilteredLogFile.csv',stringsAsFactor=F)
# tn2<-tn
# colnames(tn2)<-c('unfilteredID','abs','dilution','TN')
# tn2<-merge(tn2,tnLog2019,by='unfilteredID',all.x=T)
# tn2<-tn2[,-3]
# 
# #write.csv(tn2,'C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/TN/.csv',row.names=F)
# saveloc<-'C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/TN'
# write.csv(tn2,file.path(saveloc,files[i]),row.names=FALSE)
# 
# 
# 
# # samples$TN<-ifelse(samples$sample%in%c('U391','U255'),samples$TN*2,samples$TN)


