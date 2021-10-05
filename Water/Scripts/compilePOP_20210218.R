#Compiles chl data from .xlsx run files
rm(list=ls())  #clear variables

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("C:/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210423.db" #Change name of db file with every update

dir<-"C:/Users/notter/Google Drive/JonesLabData 2020/POP 2020/"
dir<-'C:/Users/notter/Google Drive/JonesLabData 2020/POP 2019/Data'


files<-list.files(dir)
files<-files[grep('un',files)]


library(readxl)
library(tidyverse)
POP<-data.frame() 
i=2
for(i in 1:length(files)){
  stds<-as.data.frame(read_xlsx(file.path(dir,files[i]),range="A3:B9",col_names=F))
  colnames(stds)<-c('std','abs')
  
  samples<-as.data.frame(read_xlsx(file.path(dir,files[i]),range="A16:D200",col_names=F))
  samples<-na.omit(samples[,])
  colnames(samples)<-c('sample','abs','conc','df')
  if(!grepl("P", samples$sample[1])){
    samples$sample<- paste0("P",samples$sample)
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
  samples$POPinBottle<-(stdCurve[1]+stdCurve[2]*samples$abs)*samples$df
  
  POP<-rbind(POP,samples)
}

#2019 fixes:
pop<-POP %>%
  mutate(sample = case_when(sample=="P294" & grepl("Run2",runID) ~ "P249", #typo
                            sample=="P299" & grepl("Run2",runID) ~ "P229", #typo
                            sample=="P325" & grepl("Run4",runID) ~ "P225", #typo
                            TRUE ~ sample))%>%
  add_column(removeID = paste(POP$sample,substr(POP$runID,1,4),sep="_")) %>%
  filter(!removeID %in% c("P168_Run4","P168_Run6","P181_Run6","P34_Run5", "P64_Run6")) %>%
  select(colnames(POP))

POP<-pop

#2020 fixes:
pop<-POP %>% 
  filter(!sample %in% c("P6")) %>% 
  select(colnames(POP))

POP<-pop

#add in list of cracked samples so that they can be added to the database too
#cracked<-c("U254","U253","U234","U247","U264","U110","U212")
#cracked<-data.frame(sample=cracked,abs=NA,df=NA,runID=NA,POP=NA) #cracked or not enough to rerun
#POP<-rbind(POP,cracked)

log<- read.csv("logFiles2020/correctedFiles/pocLogFile.csv",header=T, stringsAsFactors = F)
log<- read.csv("C:/Users/notter/Box/MFE/Archives/OneDriveArchive/Summer 2019/Limno/limnoEntryTool/logFiles2019/pocLogFile.csv", header = T, stringsAsFactors = F)
log<-log[-1,]
log$pocID<-gsub(" ","",log$pocID)
checkCols(log,POP)
POPM<-merge(POP,log,by.x='sample',by.y='pocID')
#add comment to cracked samples 
for(i in 1:nrow(POPM)){
  if(POPM$sample[i]%in%cracked$sample){
  POPM$comments[i]<-"cracked or not enough sample remaining"}
  }

#calculate POP in lake water using filter volume from log:
POPM$POP<- (POPM$POPinBottle *.03)/(POPM$volFiltered/1000) #Formula: (Concentration in bottle/ 30 ml vol water added to filter in L)/(filter volume (mL)/1000) = (mass POP on filter (ug))/(volume filtered (L)) = POP ug/L


dupList<-POPM[duplicated(POPM$sample),1] #lists duplicated samples
dup<-POPM[POPM$sample%in%dupList,] #lists duplicates and originals
#write.csv(dup,"Water/Output/checkCSVs/pop2019_dups.csv",row.names=F)
  #2019
    #3 dups resolved as typos
  #2020
    #6 and 9 were both labeled "P6" on the bottles, can't tell which is which so they need to be rerun

POPM$sample<-as.character(POPM$sample)
missing<- log$pocID[!as.character(paste0("P",1:nrow(log))) %in% POPM$sample]
#2019
  #too many options to narrow all down (many duplicates in run6 somehow). Reruns needed: P15,38,81,88,168,252,311,332

#2020  
  #P9 - duplicated, see above
  #P42, P46, P73, P96, P116, P128, P185, P192- I accidentally added two filters each to four bottles, couldn't tell filters apart so all 8 need to be rerun.

# pacct<-read_excel("Water/Output/checkCSVs/2019POPaccounting.xlsx")
# furn<-pacct[,1:3]
# runs<-pacct[,4:5]
# dups<-na.omit(pacct[,7:8])
# 
# 
# f6<- furn %>% filter(FrunID==6)
# p181<- f6$`Furnace log`[!f6$`Furnace log` %in% r6$pID] 
# 
# r6<- runs %>% filter(runID==6)
# r6$pID[!r6$pID %in% f6$`Furnace log`]


reruns<- c(dupList,missing)
reruns<- log[log$pocID %in% reruns,]
#write.csv(reruns, "Water/Output/checkCSVs/pop_rerunsList_2020.csv", row.names = F)

QCreruns<- read.csv("Water/Output/checkCSVs/particulates_check_StuartComments.csv", header = T, stringsAsFactors = F)
log2020<- read.csv("logFiles2020/correctedFiles/pocLogFile.csv",header=T, stringsAsFactors = F)
log2019<- read.csv("C:/Users/notter/Box/MFE/Archives/OneDriveArchive/Summer 2019/Limno/limnoEntryTool/logFiles2019/pocLogFile.csv", header = T, stringsAsFactors = F)
log2020<-log2020[-1,]
log2019<-log2019[-1,]
checkCols(log2020,log2019)
logM<- rbind(log2019,log2020)

QCreruns<- QCreruns[,-13] %>% 
 # filter(parameter == "particulateP") %>% 
  filter(grepl("Will rerun",Randi.s.Comments) | grepl("good to check",Stuart.s.thoughts)) %>% 
  select(sampleID, parameter, parameterValue) %>% 
  left_join(logM, "sampleID")
QCreruns<- QCreruns %>% 
  mutate(dateSample = as.Date(QCreruns$dateSample, format = "%m/%d/%Y")) %>% 
  arrange(parameter, dateSample)
#write.csv(QCreruns, "Water/Output/checkCSVs/pop_QCreruns.20210501.csv", row.names = F)
  


#Fix dups/missing where possible: 
#Can't fix any this run, but here's how to do it:
# POP[(POP$sample == "U138" & POP$abs == 0.361), "sample"] <- "U148"
# POPM<-merge(POP,log,by.x='sample',by.y='unfilteredID') #fixed version
# POP<-POPM

#Remove problems from output that gets used in further scripts (compileNutrientTables.R and plottingNutrientsforQC.R)
#POP<-POPM %>% filter(!sample == "U138")
#POPM2<-merge(POPM,log,by.x='sample',by.y='pocID') #fixed version
POP2019<-POPM
POP2020<-POPM

#write
write.csv(POP2019, "Water/Output/compiledData/POP2019_20210219.csv", row.names = F)
write.csv(POP2020, "Water/Output/compiledData/POP2020_20210219.csv", row.names = F)

write.csv(POP2019, "Water/Output/compiledData/POP2019full_20210505.csv", row.names = F) #does not remove duplicates
write.csv(POP2020, "Water/Output/compiledData/POP2020full_20210505.csv", row.names = F) #does not remove duplicates


#bind and correct colnames
POP2020<- read.csv("Water/Output/compiledData/POP2020_20210219.csv", header = T, stringsAsFactors = F)
POP2019<- read.csv("Water/Output/compiledData/POP2019_20210219.csv", header = T, stringsAsFactors = F)
checkCols(POP2019,POP2020)
POP<-rbind(POP2019,POP2020)
write.csv(POP,"Water/Output/compiledData/POP_merged_20210219.csv",row.names = F)
write.csv(POP,"Water/Output/compiledData/POPfull_merged_20210505.csv",row.names = F)


#################################################################
###--- Repeat for other nutrients (in their compile scripts, then move over to "plottingNutrientsforQC.R" ---###

#bring in QC list
QC<-read.csv('C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/nutrients_check.csv',header=TRUE)
QC<-merge(QC,log[,c("sampleID","unfilteredID")],all.x=TRUE)


remABS<-c(0.177,0.072,0.761,0.062,1.03,0.254,0.243,0.406,0.286) #abs values of samples that you want to remove (looking through the POPdups.csv)

POP_good<-POP[!(POP$sample%in%dup$sample &POP$abs%in%remABS),]
removed<-POP[(POP$sample%in%dup$sample &POP$abs%in%remABS),] #samples (mostly from Run8 that had abs values and nothing else. Can't attribute them to anything and their duplicated UID's had run sheet info that matched the log info for the sample.) )

#Check how many remaining: (skip to find_missing if there are no runs left) 
# remaining <- read_excel("C:/Users/notter/Google Drive/Randi/Sample Analysis/compiling data/POP accounting.xlsx")
# add1<-c("213","U213",NA,NA) #found U213 in freezer, adding it to the remaining bucket and need to change location in tables
# add2<-c("75","U75",NA,NA)   #found U75 in freezer, adding it to the remaining bucket
# add3<-c("235","U235",NA,NA) #found U235 in freezer, was in Run3 twice and needs to be rerun, added to remaining bucket
# add4<-c("232","U232",NA,NA) #found U232 in freezer, never run, added to remaining bucket
# remaining<-rbind(remaining,add1, add2, add3, add4)
# actually_done<-c("U70","U87","U120","U187",samples$sample) #typos in data sheets, fixed
# remaining<-remaining[!(remaining$UID%in%actually_done),]
#nrow(log)-(nrow(POP_good)+nrow(remaining)) #should equal zero. Has 7 that aren't run that aren't in the remaining frozen samples bucket.

find_missing<-merge(log,POP_good[,c("sample","abs","POP")],by.x="unfilteredID",by.y="sample",all.x=TRUE)
find_missing<-find_missing[is.na(find_missing$POP),]
#find_missing<-find_missing[!(find_missing$unfilteredID%in%remaining$UID),]
find_missing<-find_missing[!(find_missing$unfilteredID%in%cracked$sample),] #still missing

#nrow(POP_good)+nrow(remaining)+nrow(find_missing)+length(cracked)
nrow(POP_good)+nrow(find_missing)
#full<-c(POP_good$sample,remaining$UID,find_missing$unfilteredID,cracked) #mash up of all four lists
full<-c(POP_good$sample,find_missing$unfilteredID,cracked$sample)
full[duplicated(full)] #list of samples in multiple tables. 
  #need to find the find_missing samples and add them to the rerun list. 
    #U75  (found, rerun) 
    #U235 (found, rerun) was run twice in Run3 for an unknown reason/typo, needs to be rerun for a conclusive [POP]
    #U232 (found, rerun) 
    #U264 (found, empty), not enough for rerun, added to "cracked"
    #U110 (still missing) was run, no abs recorded, can't find
    #U212 (still missing)

#What else needs to be rerun? I'll decide that after doing QC I think. Includes negative values, but what else?

write.csv(POP_good,"C:/Users/notter/Google Drive/Randi/Sample analysis/compiling data/POP_merged_20200313.csv",row.names=F) #Does not contain samples with no values (missing/cracked) 

hist(POP_good$POP,ylim=c(0,50),xlim=c(0,2500))
nu<-dbTable("NUTRIENTS")
nu<-subset(nu,nu$parameter=="POP")
hist(nu$parameterValue,ylim=c(0,50))




