#DOC.r
#JJColoso 12/4/13
###  Modified by jzwart 2013-12-04; 2016-10-20 JAZ; 2019-11-04 RNN updates for minor naming convention changes in 2019 limnoEntryTool log files
#Script to read in raw data from Shimadzu TOC-V analyzer at CEST, calculate DOC and TN from standards, create output and summary file.  This works with or without TN data
#new option to remove bad standards from analysis.  Simply list the index of each standard to remove, where stds are listed in ascending order starting with C standards, then N standards.
#Note: this doesn't work if the data has quotes around text.  This is an option in the Export Options on the TOC-V software.  Make sure this is set to off before exporting data.  

rm(list=ls())  #clear variables
graphics.off()  #close figs

source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201020_3.5.3.db" 

library(tidyverse)
library(lubridate)

dir<- file.path(getwd(),'DOC/input')
files<-list.files(dir)
files<-files[grep('.txt',files)]
q= 6 #run number (watch file order when file # >10. Eg. Run 10 becomes q=2 because they aren't in numeric order anymore.)
files[q]
##Settings######################################
savefile=1  #set to 1 to save data to Excel
################################################
#
#Load Data
data.all<- read.table(file.path(dir,files[q]),sep="\t",header=T, skip=1) #only reads in the Run #q

#pull out columns of interest
data<-data.all[,c("Type","Anal.","Sample.ID","Spl..No.","Inj..No.","Analysis.Inj..",
                  "Area","Conc.","Excluded")] #RNN changed "Sample.Name" to "Sample.ID" to match run 1-2 text files. May need to correct in future. 

# data<-data.all[,c("Sample.Name","Spl..No.","Inj..No.","Analysis.Inj..",
#                   "Area","Conc.","Excluded")]

# renaming duplicates that happened within a run #
#this sample has a duplicate in this run, so it's name needs to change before the average is calculated because it's done by Sample.ID
# data$Sample.ID<-as.character(data$Sample.ID)
#Run4:
# data[177:179, "Sample.ID"]<- "81b"
#Run5:
# data[179:181,"Sample.ID"]<- "74b"
# data[135:137,"Sample.ID"]<- "80b"
# data[144:147,"Sample.ID"]<- "82b"

#Remove all excluded peaks
data<-data[data$Excluded==0,]
data$Area<-as.numeric(data$Area)

#pull out standards and find mean area for each standard, plot std curve
stds<-data[data$Type=="Standard",c("Anal.","Spl..No.","Area","Conc.")]
std.mean<-aggregate(Area~Spl..No.+Conc.+Anal.,data=stds,mean)

#windows()
std.npoc<-std.mean[std.mean$Anal.=="NPOC",]
fit.npoc<-lm(std.npoc$Conc.~std.npoc$Area)
plot(std.npoc$Area,std.npoc$Conc.,main=paste("NPOC   R2=",round(summary(fit.npoc)$r.squared,5)))
abline(fit.npoc)

################# Check for bad standards ####################
#std.mean
#badstd=c(6)  #list index of standards to throw out - Std order is C standards first then N stds.  i.e. if there are 7 C standards, the first N std is #8.  
# notes: run 10 & 11 for 2016 had bad high standards 5-7 (way too high and causing EL and WL to be ~20 mg L-1); run 9 standards are shit (took out stds 2,5,6,7) -
#  using run 8 standards for run 9 
#graphics.off()
#throw out bad stds
#if (!is.null(badstd)) {
  #std.mean.bad<-std.mean[badstd,]
  #std.mean<-std.mean[-badstd,]
  #} else {
    #std.mean.bad<-c()
    #std.mean.bad$Anal.<-'X'
  #}

#windows()
#std.npoc<-std.mean[std.mean$Anal.=="NPOC",]
#fit.npoc<-lm(std.npoc$Conc.~std.npoc$Area)
#plot(std.npoc$Area,std.npoc$Conc.,main=paste("NPOC   R2=",round(summary(fit.npoc)$r.squared,5)))
#abline(fit.npoc)
#if (sum(std.mean.bad$Anal.=='NPOC')>0){
  #bad<-which(std.mean.bad$Anal.=='NPOC')
  #points(std.mean.bad$Area[bad],std.mean.bad$Conc[bad],pch=16,col='red')
#}

##############################################################

#calculate mean peak area for each sample
if('dummy' %in% tolower(data$Sample.ID)){
  data<-data[-grep('dummy',tolower(data$Sample.ID),ignore.case=T),] #removes 'dummy' rows from data
}


samples<-data[data$Type=='Unknown',]
data.mean<-aggregate(Area~Sample.ID+Analysis.Inj..,data=samples,mean)

#calculate DOC mg/L and TN mg/L for each sample
data.mean.doc<-data.mean[data.mean$Analysis.Inj..=="NPOC",]
data.mean.doc$DOC_mgL<-data.mean.doc$Area*coefficients(fit.npoc)[2]+coefficients(fit.npoc)[1]

if ("TN" %in% std.mean$Anal.){
  data.mean.tn<-data.mean[data.mean$Analysis.Inj..=="TN",] 
  data.mean.tn$TN_mgL<-data.mean.tn$Area*coefficients(fit.tn)[2]+coefficients(fit.tn)[1]
}  else { #if not TN data, call TN 0 for now
    data.mean.tn<-data.mean.doc
    data.mean.tn$DOC_mgL<-data.mean.tn$DOC*0
    names(data.mean.tn)[4]<-"TN_mgL"
  }

#Create combined DOC and TN dataframe
output<-merge(data.mean.doc,data.mean.tn,by.x="Sample.ID",by.y="Sample.ID")
output<-output[c('Sample.ID','DOC_mgL','TN_mgL')]

#pull out any control standards
ctrls<-c(grep('c0',output$Sample.ID,ignore.case=T),grep('c10',output$Sample.ID,ignore.case=T))
controls<-output[ctrls,]
print(controls)

#remove any non-samples (controls, blanks)
output<-output[-ctrls,]
#output<-output[-grep('blank',tolower(output$Sample.Name)),]

#plot data
#windows()
par(mfcol=c(1,2))
plot(output$DOC_mgL,main="DOC")

if ("TN" %in% std.mean$Anal.){
  plot(output$TN_mgL,main="TN")
} else output<-output[,1:2] #cut out TN

# un-renaming duplicates that happened within a run #
#changing it's ID back to what it was truly labeled so it can get that log info
# data$Sample.ID<-as.character(data$Sample.ID)
#Run4:
# output[74, "Sample.ID"]<- "81"
#Run5:
# output[49,"Sample.ID"]<- "74"
# output[56,"Sample.ID"]<- "80"
# output[58,"Sample.ID"]<- "82"

#read in data log
dataLog<- read.csv("logFiles2020/correctedFiles/docLogFile.csv",stringsAsFactors = F)
if ("TN" %in% std.mean$Anal.){
  colnames(output)<-c('docID','DOC_mgL','TN_mgL') #TN included
} else colnames(output)<-c('docID','DOC_mgL') #no TN

output$docID= paste("D",output$docID,sep="")#adds a "D" in front of all docID values because the D is in the log file starting 2019
output<- merge(dataLog,output,by='docID',all.y=T) #now able to merge them because dataLog file has "D#" 

#save
savefilename<-paste(strsplit(files[q],'.',fixed=T)[[1]][1],'.csv',sep='')
saveloc<- "DOC/output/processedOutput/"
write.csv(output,paste(saveloc,savefilename,sep=''),row.names=F)

#-#-# Repeat all of the above for each DOC run, changing "q" to reflect the file number in the list of .txt files #-#-#

#compile run files 
out<- "DOC/output/processedOutput"
files<-list.files(out)
files<-files[grep('.csv',files)]
all.doc=data.frame() #makes an all.doc df so that I can write to it later
i=2
for(i in 1:length(files)){
  data.all<-read.csv(file.path(out,files[i]),stringsAsFactors = F,header = T)
  #data.all<-na.omit(data.all)
  #data<-data.all[,c("sampleID","docID","DOC_mgL")]
  #names(data) <- c("SampleID","docID","DOC")
  
  data.all$runID=gsub("_","",substr(files[i],12,12))

  
  # savefilename<-paste(strsplit(files[i],'.',fixed=T)[[1]][1],'.csv',sep='')
  # saveloc<-'DOC/output/'
  # write.csv(data,paste(saveloc,savefilename,sep=''),row.names=F)
  
  
  all.doc<-rbind(all.doc,data.all) #writes each .csv (a temp df called 'data' in the loop) to the all.chl df
  all.doc<-all.doc[order(all.doc$docID),]
}

#write.csv(all.doc,"DOC/output/mergeDOCData.csv",row.names=F)
all.doc<-read.csv("DOC/output/mergeDOCData.csv", header = T, stringsAsFactors = F)

#put all.doc into DOC db format: 
doc<-dbTable("DOC")
all.doc$dateSample<- as.Date(parse_date_time(all.doc$dateSample, "mdy"))

#commenting
all.doc<- all.doc %>% 
  mutate(comments = ifelse(dateSample == "2020-06-16", "Sample labels duplicated for FE and WL on 6/16 and 6/23.No way to tell them apart, so duplicated samples (eg. 4 from FE PML, 4 from FE Hypo, etc.) were averaged and the average value was used for both 6/16 and 6/23.",
                           comments)) %>% 
  mutate(comments = ifelse(runID %in% c(5,6), "Samples sat thawed and acidified for 30 days before running due to TOC machine unavailability. Samples stored in refrigerator and vials were parafilmed.",
                           comments)) %>% 
  mutate(comments = ifelse(runID == 6, paste(comments, "First run failed on TOC machine due to mechanical error with no recoverable data. Approx. 15ml of remaining sample was preserved for use in second sampling attempt (this one). Number of injections reduced from 3-5 to 3-4 to reduce sample volume needed. May slightly increase variability between replicates and reduce presicion of DOC_mgL average. Likely not a problem except at very low levels (<5mgL)",sep=" "),
                           comments))

all.doc<- all.doc %>%
  rename(siteName = "site") %>%
  add_column(dateTimeSample = as.POSIXct(paste(all.doc$dateSample, all.doc$timeSample, sep=" "))) %>%
  rename(DOC = "DOC_mgL") %>%
  add_column(TN_DOC = NA) %>%
  add_column(metadataID = "DOC.20110601") %>%
  add_column(flag = NA) %>%
  add_column(updateID = "tableDOC_2020.20201118") %>%
  select(colnames(doc)) %>% 
  arrange(dateTimeSample)

#write output for new DOC QC tool to send to Kaija
#write.csv(all.doc,"DOC/output/allDOC_forQC.csv", row.names=F)

#-#-# Below: scratch to figure out the 6/16 6/23 mixup of labels #-#-#
#Resolved in the above script by changing duplicate docID's temporarily within runs that had duplicates within a run. 
#They needed to be separated so the injections could be averaged between individual samples. Then the duplicate ID that 
#matched the bottle and run sheet was added back on so that it could be given the docLogFile information that matched its
#label. The resulting outcome produces duplicate samples for 6/16 and no data for 6/23. Going to see how to run it through 
#Kaija's new DOC QC tool.

# #try to figure out which duplicates are which
# #subset all.doc to just Long Lake for the whole summer
# longDOC<-subset(all.doc,all.doc$lakeID %in% c("FE","WL"))
# longDOC$dateSample<-as.Date(longDOC$dateSample,format="%m/%d/%Y")
# #plot DOC over time per lake
# library(ggplot2)
# ggplot(longDOC, aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=lakeID))
# DH<-ggplot(longDOC[longDOC$depthClass %in% c("PML","Hypo"),],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass)) +scale_x_date(date_breaks = "1 week",date_labels=("%m-%d"))
# feDH<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$depthClass %in% c("PML","Hypo"),],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass)) +scale_x_date(date_breaks = "1 week",date_labels=("%m-%d"))
# fePML<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$depthClass =="PML",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color= runID))
# feHypo<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$depthClass =="Hypo",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color= runID))
# feI<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$site =="Inlet1",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass))
# wlDH<-ggplot(longDOC[longDOC$lakeID=="WL"& longDOC$depthClass %in% c("PML","Hypo"),],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass)) +scale_x_date(date_breaks = "1 week",date_labels=("%m-%d"))
# wlPML<-ggplot(longDOC[longDOC$lakeID=="WL"& longDOC$depthClass =="PML",],aes(x=dateSample, y=DOC_mgL)) +geom_point()


#weird dimensions in run4
# run5full<-as.character(unique(data.all$Sample.ID))
# run5full<- subset(run5full,!run5full %in% c("Dummy","Untitled","C0","C10"))
# missing<-run5full[!run5full %in% run5incl]
# runsheet<-c(134,142,124,143,113,119,118,122,80,77,82,112,116,135,74,24,76,10,39,97,56,48,38,50,93,166,52,89,12,67,57,33,31,
#             78,80,72,14,82,8,18,15,2,22,100,29,4,74,35,87,20,63,84,66,75,27,6,95,46,59,86,26,70,61,54,92,43,41)
# missing<-runsheet[!runsheet %in% run5full]
# duplicated<-runsheet[duplicated(runsheet)]

#ID's 73-82 (Long on 6/16/20) duplicated sample stickers and ID's 101-110 (Long on 6/23/20) are missing. 
  #for now, I want to remove all of the ID's 73-82 and send off to Stuart

# all.doc$id<-as.numeric(gsub("D","",all.doc$docID))
# all.docStu<-subset(all.doc,!all.doc$id %in% c(73:82))
# stuLong<-subset(all.docStu,all.docStu$lakeID %in% c("FE","ME","WL"))
# all.docStu$dateSample<-parse_date_time(all.docStu$dateSample, "mdy")
# #rename columns
# all.docStu2<-all.docStu %>% 
#   rename(siteName = "site") %>% 
#   add_column(dateTimeSample = as.POSIXct(paste(all.docStu$dateSample, all.docStu$timeSample, sep=" "))) %>% 
#   rename(DOC = "DOC_mgL") %>% 
#   add_column(TN_DOC = "NA") %>% 
#   add_column(metadataID = "NA") %>% 
#   add_column(flag = "NA") %>% 
#   add_column(updateID = "NA") %>% 
#   select(colnames(doc))
# 
# write.csv(all.docStu2,"DOC/output/earlyDOCruns_20201028.csv",row.names = F)
