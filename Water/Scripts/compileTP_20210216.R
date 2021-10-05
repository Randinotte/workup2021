#Compiles chl data from .xlsx run files
rm(list=ls())  #clear variables

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
db="MFEdb_20210423.db"

dir<-'C:/Users/notter/Google Drive/JonesLabData 2020/TP 2020'



files<-list.files(dir)
files<-files[grep('run',files)]


library(readxl)
library(tidyverse)
tp<-data.frame() 
i=3
for(i in 1:length(files)){
  stds<-as.data.frame(read_xlsx(file.path(dir,files[i]),range="A3:B9",col_names=F))
  colnames(stds)<-c('std','abs')
  
  samples<-as.data.frame(read_xlsx(file.path(dir,files[i]),range="A16:D200",col_names=F))
  samples<-na.omit(samples[,])
  colnames(samples)<-c('sample','abs','conc','df')
  if(!grepl("U", samples$sample[1])){
    samples$sample<- paste0("U",samples$sample)
  }
  samples$conc<-NULL
  samples$runID<-files[i]
  
  
  stds$std<-as.numeric(as.character(stds$std))
  stds$abs<-as.numeric(as.character(stds$abs))
  samples$abs<-as.numeric(as.character(samples$abs))
  samples$sample<-as.character(samples$sample)
  samples<-na.omit(samples) # will get rid of dilution thing 
  
  plot(stds$std~stds$abs)
  abline(lm(stds$std~stds$abs))
  stdCurve<-lm(stds$std~stds$abs)$coefficients
  samples$TP<-(stdCurve[1]+stdCurve[2]*samples$abs)*samples$df
 
  
  tp<-rbind(tp,samples)
}
#add in list of cracked samples so that they can be added to the database too
#cracked<-c("U254","U253","U234","U247","U264","U110","U212")
#cracked<-data.frame(sample=cracked,abs=NA,df=NA,runID=NA,TP=NA) #cracked or not enough to rerun
#tp<-rbind(tp,cracked)

log<- read.csv("logFiles2020/correctedFiles/unfilteredLogFile.csv",header=T, stringsAsFactors = F)
log<-log[-1,]
log$unfilteredID<-gsub(" ","",log$unfilteredID)
tpM<-merge(tp,log,by.x='sample',by.y='unfilteredID')
#add comment to cracked samples 
for(i in 1:nrow(tpM)){
  if(tpM$sample[i]%in%cracked$sample){
  tpM$comments[i]<-"cracked or not enough sample remaining"}
  }


dupList<-tpM[duplicated(tpM$sample),1] #lists duplicated samples
dup<-tpM[tpM$sample%in%dupList,] #lists duplicates and originals
#write.csv(dup,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/tpM dups.csv",row.names=F)

tpM$sample<-as.character(tpM$sample)
missing<- log$unfilteredID[!as.character(paste0("U",1:nrow(log))) %in% tpM$sample]
# U138 is also duplicated in Run1 of TN (ran same day on same pee cups), so maybe there's something wrong with the U138 labels? 
    #Or more likely, I wrote "U138" on both 60ml polypropylene bottles at the time of dispensing.
    #Because I can't know which is the correct U138, both U138 and U148 will need to be rerun for TP and TN. 

#Fix dups/missing where possible: 
#Can't fix any this run, but here's how to do it: 
  # tp[(tp$sample == "U138" & tp$abs == 0.361), "sample"] <- "U148"
  # tpM<-merge(tp,log,by.x='sample',by.y='unfilteredID') #fixed version
  # tp<-tpM

#Remove problems from output that gets used in further scripts (compileNutrientTables.R and plottingNutrientsforQC.R)
tp<-tpM %>% filter(!sample == "U138")

#no more dups or missing, yay!
write.csv(tpM, "Water/Output/compiledData/tpFull_20210216.csv", row.names = F) #contains all data, even duplicates that confuse further steps
write.csv(tp, "Water/Output/compiledData/tp_20210216.csv", row.names = F) #doesn't contain duplicates that confuse further steps


# #################################################################
# ###--- Repeat for other nutrients (in their compile scripts, then move over to "plottingNutrientsforQC.R" ---###
# 
# #bring in QC list
# QC<-read.csv('C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/nutrients_check.csv',header=TRUE)
# QC<-merge(QC,log[,c("sampleID","unfilteredID")],all.x=TRUE)
# 
# 
# remABS<-c(0.177,0.072,0.761,0.062,1.03,0.254,0.243,0.406,0.286) #abs values of samples that you want to remove (looking through the TPdups.csv)
# 
# tp_good<-tp[!(tp$sample%in%dup$sample &tp$abs%in%remABS),]
# removed<-tp[(tp$sample%in%dup$sample &tp$abs%in%remABS),] #samples (mostly from Run8 that had abs values and nothing else. Can't attribute them to anything and their duplicated UID's had run sheet info that matched the log info for the sample.) )
# 
# #Check how many remaining: (skip to find_missing if there are no runs left) 
# # remaining <- read_excel("C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/TP accounting.xlsx")
# # add1<-c("213","U213",NA,NA) #found U213 in freezer, adding it to the remaining bucket and need to change location in tables
# # add2<-c("75","U75",NA,NA)   #found U75 in freezer, adding it to the remaining bucket
# # add3<-c("235","U235",NA,NA) #found U235 in freezer, was in Run3 twice and needs to be rerun, added to remaining bucket
# # add4<-c("232","U232",NA,NA) #found U232 in freezer, never run, added to remaining bucket
# # remaining<-rbind(remaining,add1, add2, add3, add4)
# # actually_done<-c("U70","U87","U120","U187",samples$sample) #typos in data sheets, fixed
# # remaining<-remaining[!(remaining$UID%in%actually_done),]
# #nrow(log)-(nrow(tp_good)+nrow(remaining)) #should equal zero. Has 7 that aren't run that aren't in the remaining frozen samples bucket.
# 
# find_missing<-merge(log,tp_good[,c("sample","abs","TP")],by.x="unfilteredID",by.y="sample",all.x=TRUE)
# find_missing<-find_missing[is.na(find_missing$TP),]
# #find_missing<-find_missing[!(find_missing$unfilteredID%in%remaining$UID),]
# find_missing<-find_missing[!(find_missing$unfilteredID%in%cracked$sample),] #still missing
# 
# #nrow(tp_good)+nrow(remaining)+nrow(find_missing)+length(cracked)
# nrow(tp_good)+nrow(find_missing)
# #full<-c(tp_good$sample,remaining$UID,find_missing$unfilteredID,cracked) #mash up of all four lists
# full<-c(tp_good$sample,find_missing$unfilteredID,cracked$sample)
# full[duplicated(full)] #list of samples in multiple tables. 
#   #need to find the find_missing samples and add them to the rerun list. 
#     #U75  (found, rerun) 
#     #U235 (found, rerun) was run twice in Run3 for an unknown reason/typo, needs to be rerun for a conclusive [TP]
#     #U232 (found, rerun) 
#     #U264 (found, empty), not enough for rerun, added to "cracked"
#     #U110 (still missing) was run, no abs recorded, can't find
#     #U212 (still missing)
# 
# #What else needs to be rerun? I'll decide that after doing QC I think. Includes negative values, but what else?
# 
# write.csv(tp_good,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/TP_merged_20200313.csv",row.names=F) #Does not contain samples with no values (missing/cracked) 
# 
# hist(tp_good$TP,ylim=c(0,50),xlim=c(0,2500))
# nu<-dbTable("NUTRIENTS")
# nu<-subset(nu,nu$parameter=="TP")
# hist(nu$parameterValue,ylim=c(0,50))




