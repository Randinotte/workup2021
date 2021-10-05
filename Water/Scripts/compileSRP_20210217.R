#Compiles chl data from .xlsx run files
rm(list=ls())  #clear variables

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
db="MFEdb_20210423.db"

dir<-'C:/Users/notter/Google Drive/JonesLabData 2020/SRP 2020'
#setwd(dir)


files<-list.files(dir)
files<-files[grep('un',files)]


library(readxl)
library(tidyverse)
srp<-data.frame() 
i=1
for(i in 1:length(files)){
  stds<-as.data.frame(read_xlsx(file.path(dir,files[i]),range="A3:B9",col_names=F))
  colnames(stds)<-c('std','abs')
  
  samples<-as.data.frame(read_xlsx(file.path(dir,files[i]),range="A16:D98",col_names=F))
  colnames(samples)<-c('sample','abs','conc','df')
  samples<-na.omit(samples[,])
  if(grepl("F",samples[1])=="FALSE"){
    samples$sample<-paste("F",samples$sample,sep="")
  }
  samples$conc<-NA #can compare R-calculated value to excel calculated by turning on/off
  samples$runID<-files[i]
  
  
  stds$std<-as.numeric(as.character(stds$std))
  stds$abs<-as.numeric(as.character(stds$abs))
  samples$abs<-as.numeric(as.character(samples$abs))
  samples$sample<-as.character(samples$sample)

  
  plot(stds$std~stds$abs)
  abline(lm(stds$std~stds$abs))
  stdCurve<-lm(stds$std~stds$abs)$coefficients
  samples$SRP<-(stdCurve[1]+stdCurve[2]*samples$abs)*samples$df
   
  options(scipen=500) #was forcing scientific notation before adding this
  srp<-rbind(srp,samples)
}

# cracked<-c("F128")
# colnames(srp)
# cracked<-data.frame(sample=cracked,abs=NA,df=NA,runID=NA,SRP=NA) #cracked or not enough to rerun
# srp<-rbind(srp,cracked)


log<- read.csv("logFiles2020/correctedFiles/filteredLogFile.csv",header=T, stringsAsFactors = F)
log<-log[2:nrow(log),]
srpM<-merge(srp,log,by.x='sample',by.y='filteredID')
srpM$ID<-as.numeric(gsub("F","",srpM$sample))
#add comment to cracked samples 
for(i in 1:nrow(srp)){
  if(srp$sample[i]%in%cracked$sample){
  srp$comments[i]<-"cracked or not enough sample remaining"}
  }

dupList<-srpM[duplicated(srpM$sample),1] #lists duplicated samples
dup<-srpM[srpM$sample%in%dupList,] #lists duplicates and originals
#write.csv(dup,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/srpM dups.csv",row.names=F)
  #NO DUPLICATES!! *air horns*

srpM$sample<-as.character(srpM$sample)
missing<- log$filteredID[!as.character(paste0("F",1:nrow(log))) %in% srpM$sample]
  #What?! no missing samples?! *double air horns*
srp<-srpM

write.csv(srp, "Water/Output/compiledData/srpFull_20210217.csv", row.names = F)
write.csv(srp, "Water/Output/compiledData/srp_20210217.csv", row.names = F)

#remABS<-c(0.177,0.072,0.761,0.062,1.03,0.254,0.243,0.406,0.286) #abs values of samples that you want to remove (looking through the TPdups.csv)

#srp_good<-srp[!(srp$sample%in%dup$sample &srp$abs%in%remABS),]
# srp_good<-srp
# removed<-srp[(srp$sample%in%dup$sample &srp$abs%in%remABS),] #samples (mostly from Run8 that had abs values and nothing else. Can't attribute them to anything and their duplicated UID's had run sheet info that matched the log info for the sample.) )

#Check how many remaining: 
#remaining <- read_excel("C:/Users/notter/Google Drive/Randi/Admin/TP accounting.xlsx") #list of samples that haven't been run yet
#actually_done<-c("U70","U87","U120","U187")
#remaining<-remaining[!(remaining$UID%in%actually_done),]
#add<-c("213","U213",NA,NA) #found U213 in freezer, adding it to the remaining bucket and need to change location in tables
#remaining<-rbind(remaining,add)
#nrow(log)-(nrow(tp_good)+nrow(remaining)) #should equal zero. Has 7 that aren't run that aren't in the remaining frozen samples bucket.

# find_missing<-merge(log,srp_good[,c("sample","abs","SRP")],by.x="filteredID",by.y="sample",all.x=TRUE)
# find_missing<-find_missing[is.na(find_missing$SRP),]
# #find_missing<-find_missing[!(find_missing$filteredID%in%remaining$UID),] #lists samples that don't have SRP and aren't still waiting to be run in "remaining"
# find_missing<-find_missing[!(find_missing$filteredID%in%cracked$sample),]

#nrow(sp_good)+nrow(remaining)+nrow(find_missing)+length(cracked) #Still 358? Which one is duplicated somewhere?
#nrow(srp_good)+nrow(find_missing) #should equal nrow(log)

#dims still wrong? Try seeing if a sample is in more than one table
#full<-c(srp_good$sample,remaining$UID,find_missing$unfilteredID,cracked) #mash up of all four lists
#full[duplicated(full)] #list of samples in multiple tables. 
  #U175 has a value in tp_good but was in the remaining bucket. It didn't have "TP" written on top so I'll have it rerun to make sure that the value is correct. 
  #need to find the find_missing samples and add them to the rerun list. 

#What else needs to be rerun? I'll decide that after doing QC I think. Includes negative values, but what else?

#write.csv(srp_good,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/SRP_merged_20200313.csv",row.names=F) #Does not contain samples with no values (missing/cracked)

#quick check histograms
hist(srp$SRP)
dbSRP<-dbTable("WATER_CHEM") %>% filter(parameter == "SRP")
hist(dbSRP$parameterValue) 




