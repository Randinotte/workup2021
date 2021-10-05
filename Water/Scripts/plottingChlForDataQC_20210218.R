#Original script by KMS (C:\Old Data\KSaunders files\Documents\R\R files\plotting DOCchl for data QC.R), updates by RNN in 2019. 
#Contents: Script for QCing yearly data, plus writing .csv output for the SITES, SAMPLES, and TABLES tables that are needed in writingUpdatingDBtables.R


rm(list=ls())
library(ggplot2)
library(stringr)
## load data and connect multiple files
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20210112.db" 
#setwd('C:/Users/notter/Google Drive/Randi/Database/plots/Data QC plots/Chl QC 2019 plots')
QClist<-(c('BA','BO','BR','CB','CR','EL','FE','HB','MO','NG','PA','PE','TU','WA','WL','ME')) #pulls only data from QC lakes
data<-dbTable('CHLOROPHYLL',lakeID=QClist)

#changes depth class "MidEpi" to "PML" and "EL" to "FE" because they get QC'd the same way.
#don't need to change back because the SITES, SAMPLES, and TABLE tables are just newdata, not all data
for(i in 1:nrow(data)){
  if(data$depthClass[i]=='MidEpi'){
    data$depthClass[i]<-("PML")
  }
  if(data$lakeID[i]=='EL'){
    data$lakeID[i]<-("FE")
  }  
} 

#fixing times and timezones in data
data<-data
data<- data %>% 
  mutate(dateTimeSample = case_when(dateTimeSample > "2019-04-02 00:00" ~ (data$dateTimeSample + hours(1)), TRUE ~ dateTimeSample))

#brings in new year's data and appends it onto the all previous data
newdata<-read.csv("Water/Output/compiledData/chlorophyll_20210218.csv",header = TRUE, stringsAsFactors = F) #comes from the CompileChla.R script
data<-subset(data, data$flag == 0)
#newdata$dateTimeSample= strptime(x = as.character( newdata$dateTimeSample ), format = "%m/%d/%Y %H:%M")
newdata$dateTimeSample= as.POSIXct(newdata$dateTimeSample,tz="America/Chicago")
#newdata$dateSample= strptime(x = as.character( newdata$dateSample ), format = "%m/%d/%Y")
#newdata$dateSample= as.POSIXct(newdata$dateSample,tz="America/Chicago")
#newdata$dateSample<- as.Date(newdata$dateSample, format = "%m/%d/%Y")
data<-rbind(data,newdata)
newdata$siteName<-as.character(newdata$siteName)
data<-subset(data, lakeID %in% QClist) #pulls non-QC data out of 'data' that was added in 2019
data<-subset(data, depthClass %in% c("PML","Hypo")) #include "surface" here for DOC and color samples (inlets/outlets)
        ##Could choose not to subset into PML and Hypo here and then I can make both figures in the for loop

#for loop that  #1)subsets all data into unique lake/depthClass (lakeDepth) combinations
                #2)calculates the mean and sd for each lakeDepth
                #3)flags any new data points outside +/- 2*s and adds them to a df called "check"
                #4)creates a plot of all data for that lake/depth over time, saves it to a list of plots for every unique lake/depth combo
                #5)prints and saves pairs of figures for each lake (PML and Hypo)
i=2
j=2
check<-data.frame()
unique_lakeDepths<-(unique(data[c("lakeID","depthClass")]))
unique_lakeDepths <- unique_lakeDepths[order(unique_lakeDepths$lakeID),]
unique_lakeDepths$m<-"NA"
unique_lakeDepths$s<-"NA"
plot_list = list()

j=27  
for(j in 1:nrow(unique_lakeDepths)){
  #pulls out the unique lake/depthClass data for all years
  lakedepth<-subset(data,data$lakeID==unique_lakeDepths$lakeID[j] & data$depthClass==unique_lakeDepths$depthClass[j])
  
  #calculates the all years mean and sd for just one lake/depth pair at a time
  m<-mean(lakedepth$chl)
  s<-sd(lakedepth$chl)
  
  #adds the lake/depth's mean and sd to the lakeDepths df for record keeping
  unique_lakeDepths$m[j]<-m
  unique_lakeDepths$s[j]<-s

  
  #flags any new data points that have a value greater than 2*s in a column called lakedepth$check
  for(r in 1:nrow(lakedepth)){
    if(lakedepth$chl[r]>(m+s*2) | lakedepth$chl[r]<(m-s*2)){
      lakedepth$check[r]=1
    }else{
      lakedepth$check[r]=0
      }
    }

  #adds all the sample information from any flagged rows into the df "check"
  #colnames(check1)<-colnames(lakedepth) #gives column names equal to data
  check1<-lakedepth[lakedepth$check==1,]
  check<-rbind(check, check1)
  check<-subset(check,dateTimeSample>="2020-01-01 00:00:00")
  

  #creates a plot of all data at that lake/depthClass combo
  testplot1 <- ggplot(data = lakedepth, aes(x = dateTimeSample, y = chl)) + geom_point() + geom_line() +
   geom_hline(yintercept = m) + geom_hline(yintercept = m + 2*s, linetype= 'dashed') + geom_hline(yintercept = m - 2*s, linetype= 'dashed') + 
    ggtitle(paste(unique_lakeDepths$lakeID[j],unique_lakeDepths$depthClass[j],"Chl")) + scale_y_continuous(paste("Chl","ug/L")) + theme(legend.position = "none") + theme_bw()
  
  plot_list[[j]] = testplot1
}

#write.csv(check,"Water/Output/checkCSVs/chl_checkNEW.csv")

###### End of big for loop
###### Don't need to continue if you're not re-printing the plots to PDF
plot(plot_list[[12]]) #how to plot just one plot from the list
lakepairs<-data.frame(PML=seq(1,30,2),HYPO=seq(2,30,2),lakeID=unique(unique_lakeDepths$lakeID))
lakepairs[15,1:2]<- c(30,29)
lakepairs[6,1:2]<-c(12,11)

p=2
for (p in 1:nrow(lakepairs)){
  plots<-cowplot::plot_grid(plot_list[[lakepairs$PML[p]]],plot_list[[lakepairs$HYPO[p]]],nrow=2,ncol=1)
  cowplot::save_plot(file.path("Water/QC plots/Chlorophyll",paste(lakepairs$lakeID[p],"Chl_plots.pdf")), plots, base_height = 10, base_width = 15)
}

# #plot check plots (larger)
# checkSites<- check %>% 
#   distinct(lakeID,depthClass)
# 
# for(i in 1:nrow(checkSites)){
#   checkSites$depthClass[i]<-gsub("Hypo","HYPO",checkSites$depthClass[i])
#   checkSites$plotNo[i]<- lakepairs[lakepairs$lakeID==checkSites$lakeID[i],colnames(lakepairs)==checkSites$depthClass[i]]
#   checkSites$depthClass[i]<-gsub("HYPO","Hypo",checkSites$depthClass[i])
# }
# 
# check_plot_list = list()
# 
# for(j in 1:nrow(checkSites)){
#   #creates a plot of all data at that lake/depthClass combo
#   lakedepth<-subset(data, data$lakeID==checkSites$lakeID[j] & data$depthClass==checkSites$depthClass[j])
#   testplot1 <- ggplot(data = lakedepth, aes(x = dateSample, y = chl)) + geom_point() + geom_line() +
#     geom_hline(yintercept = m) + geom_hline(yintercept = m + 2*s, linetype= 'dashed') + geom_hline(yintercept = m - 2*s, linetype= 'dashed') + 
#     ggtitle(paste(checkSites$lakeID[j],checkSites$depthClass[j],"Chl")) + scale_y_continuous(paste("Chl","ug/L")) + theme(legend.position = "none") + theme_bw()+
#     scale_x_date(limits = as.Date(c("2019-04-01","2020-11-01")))
#   
#   check_plot_list[[j]] = testplot1
# }
# check_plot_list[[2]]
# i=1
# for(p in 1:nrow(checkSites)){
#   plot<- check_plot_list[[checkSites$plotNo[i]]]
#   cowplot::save_plot(file.path("Water/QC plots/Chlorophyll/checkPlots",paste(checkSites$lakeID[p],checkSites$depthClass[p],"Chl_CHECK.pdf",sep="_")), plot, base_height = 5, base_width = 10)
# }

#find run info for checks
check<-read.csv("Water/Output/checkCSVs/chl_check.csv",header = T, stringsAsFactors = F)
check$repID<-paste(check$sampleID,check$replicate,sep = "_")
newdata$repID<-paste(newdata$sampleID,newdata$replicate,sep="_")
checkM<-merge(check, newdata, by = "repID")
checkM<-subset(checkM,checkM$check=="1")



############ Flag newdata
#After evaluating the "check" output csv and flagging samples in the csv, continue below: 

#check<-read.csv("C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/Chla/chl_check.csv",stringsAsFactors = F)
check<- read.csv("Water/Output/checkCSVs/chl_check_StuartComments.csv",header = T, stringsAsFactors = F)
check$dateSample<-as.Date(check$dateSample,format="%m/%d/%Y")
  #check$dateTimeSample somehow subtracted one hour from the real sample time, correct time still listed in the sampleID so I'm leaving it. Still good in newdata.
check$comments<- check$QC..goes.in.db.table.

i=13
for (i in 1:nrow(check)){
  newdata$flag[newdata$sampleID==check$sampleID[i] & newdata$replicate==check$replicate[i]]<-check$flag[i]
  newdata$comments[newdata$sampleID==check$sampleID[i] & newdata$replicate==check$replicate[i]]<-check$comments[i]
}
nrow(subset(newdata,newdata$flag==1)) #Check that this matches the number in df "check"




################################
#Making SITE, SAMPLE, and TABLE tables for export to "Current Database" folder
#Make these !!!!AFTER!!!! QC has been done and flags, comments, and updateIDs have been added to the newdata df

###siteChlorophyll_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20210112.db" 

siteChlorophyll <-newdata[order(newdata$lakeID),]
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteChlorophyll$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long","UTM")]
siteChlorophyll<-merge(siteChlorophyll,siteLakes,by="lakeID")
siteChlorophyll$siteID<-paste(siteChlorophyll$lakeID,"_",siteChlorophyll$siteName,sep="")
wantSite<-c("siteID","lakeID","siteName","lat","long","UTM","updateID")
siteChlorophyll<-siteChlorophyll[wantSite]
siteChlorophyll$updateID<-"siteChlorophyll.20200203"
#check that vector data formats are the same between siteChlorophyll and SITES table
str(dbTable("SITES"))
str(siteChlorophyll)
db<-dbTable("SITES")
df<-data.frame("vector"=(names(db)))
i=1
for(i in 1:ncol(siteChlorophyll)){
  df$site[i]<-class(siteChlorophyll[,i])
}
for(i in 1:ncol(db)){
  df$DB[i]<-class(db[,i])
}

i=2
df$DB<-as.character(df$DB)
for(i in 1:nrow(df)){
  if (df$DB[i]=="character"){
    siteChlorophyll[,paste(df$vector[i])]<-as.character(siteChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="numeric"){
    siteChlorophyll[,paste(df$vector[i])]<-as.numeric(siteChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="integer"){
    siteChlorophyll[,paste(df$vector[i])]<-as.integer(siteChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="factor"){
    siteChlorophyll[,paste(df$vector[i])]<-as.factor(siteChlorophyll[,paste(df$vector[i])])
  }
}
write.csv(siteChlorophyll,paste(dbdir,"siteChlorophyll_20200203.csv",sep=""),row.names=FALSE)

###sampleChlorophyll_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

sampleChlorophyll<-newdata[order(newdata$lakeID),]
sampleChlorophyll$updateID<-"sampleChlorophyll.20200203"
log<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv")
log<-log[c("sampleID","crew","weather","comments")]
sampleChlorophyll<-merge(sampleChlorophyll,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
fix<-sampleChlorophyll[is.na(sampleChlorophyll$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and chlLogFile.csv (and all derivitive data frames)
#colin<-subset(fix,fix$lakeID!="HB"&colin$lakeID!="PE"&colin$lakeID!="PA"&colin$replicate=="1")
#colin<-colin[c("sampleID","dateSample","dateTimeSample","depthClass","depthTop","depthBottom")]
#fix<-subset(colin, colin$lakeID=="HB"|colin$lakeID=="PE"|colin$lakeID=="PA")
sampleChlorophyll$siteID<-paste(sampleChlorophyll$lakeID,"_",sampleChlorophyll$siteName,sep="")
wantSample<-c("siteID","sampleID","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","crew","weather","comments.y","metadataID","updateID")
sampleChlorophyll<-sampleChlorophyll[wantSample]
names(sampleChlorophyll)[10]<-"comments"
#check that vector data formats are the same between sampleChlorophyll and SAMPLES table
str(dbTable("SAMPLES"))
str(sampleChlorophyll)
db<-dbTable("SAMPLES")
df<-data.frame("vector"=(names(db)))
i=1
for(i in 1:ncol(sampleChlorophyll)){
  df$sample[i]<-class(sampleChlorophyll[,i])
}
for(i in 1:ncol(db)){
  df$DB[i]<-class(db[,i])
}

i=2
df$DB<-as.character(df$DB)
for(i in 1:nrow(df)){
  if (df$DB[i]=="character"){
    sampleChlorophyll[,paste(df$vector[i])]<-as.character(sampleChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="numeric"){
    sampleChlorophyll[,paste(df$vector[i])]<-as.numeric(sampleChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="integer"){
    sampleChlorophyll[,paste(df$vector[i])]<-as.integer(sampleChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="factor"){
    sampleChlorophyll[,paste(df$vector[i])]<-as.factor(sampleChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="Date"){
    sampleChlorophyll[,paste(df$vector[i])]<-as.Date(sampleChlorophyll[,paste(df$vector[i])])
  }
}
#sampleChlorophyll$dateSample<-as.Date(sampleChlorophyll$dateSample,format="%m/%d/%Y")
write.csv(sampleChlorophyll,paste(dbdir,"sampleChlorophyll_20200203.csv",sep=""),row.names=FALSE)

###tableChlorophyll_YYYYMMDD###
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20190612.db" 

tableChlorophyll<-newdata[order(newdata$lakeID),]
tableChlorophyll$updateID<-"tableChlorophyll.20200203"
wantTable<-c("projectID","sampleID","lakeID","siteName","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","runID","chl","replicate","metadataID","comments","flag","updateID")
tableChlorophyll<-tableChlorophyll[wantTable]
str(dbTable("CHLOROPHYLL"))
str(tableChlorophyll)
db<-dbTable("CHLOROPHYLL")
df<-data.frame("vector"=(names(db)))
i=1
for(i in 1:ncol(tableChlorophyll)){
  df$table[i]<-class(tableChlorophyll[,i])
}
for(i in 1:ncol(db)){
  df$DB[i]<-class(db[,i])
}

i=2
df$DB<-as.character(df$DB)
for(i in 1:nrow(df)){
  if (df$DB[i]=="character"){
    tableChlorophyll[,paste(df$vector[i])]<-as.character(tableChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="numeric"){
    tableChlorophyll[,paste(df$vector[i])]<-as.numeric(tableChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="integer"){
    tableChlorophyll[,paste(df$vector[i])]<-as.integer(tableChlorophyll[,paste(df$vector[i])])
  }
  if (df$DB[i]=="factor"){
    tableChlorophyll[,paste(df$vector[i])]<-as.factor(tableChlorophyll[,paste(df$vector[i])])
  }
}
tableChlorophyll$dateSample<-as.Date(tableChlorophyll$dateSample,format="%m/%d/%Y")
write.csv(tableChlorophyll,paste(dbdir,"tableChlorophyll_20200203.csv",sep=""),row.names=FALSE)










################################
#Extra code left over from Katie's first version
#plot by depth
lakedepth = subset(data, data$depthClass == depth)#for plotting all other depths
lakedepth = subset(data, data$depthClass == depth | data$depthClass == 'MidEpi')#for plotting PML
#lakedepth$parameterValue <- as.numeric(lakedepth$parameterValue)

#plot by site
depth<- 'Outlet'
lakedepth<-subset(data, data$siteName == 'WC1' | data$siteName == 'WC2')#for subsetting multiple sites
lakedepth = subset(data,data$siteName == depth)
lakedepth = subset(lakedepth,lakedepth$depthClass == 'Surface')

#calculate mean and std dev of response variable
m<-mean(lakedepth$chl)
s<-sd(lakedepth$chl)
m1<-mean(lakedepth$DOC)
s1<-sd(lakedepth$DOC)

#plot by depth for Chl
testplot1 <- ggplot(data = lakedepth, aes(x = dateTimeSample, y = chl)) + geom_point() + geom_line() + 
  geom_hline(yintercept = m) + geom_hline(yintercept = m + 2*s, linetype= 'dashed') + geom_hline(yintercept = m - 2*s, linetype= 'dashed') + 
  ggtitle(paste(lake,depth,"Chl")) + scale_y_continuous(paste("Chl","ug/L")) + theme(legend.position = "none") + theme_bw()
windows()
plot(testplot1)
#plot by depth for DOC
testplot8 <- ggplot(data = lakedepth, aes(x = dateTimeSample, y = DOC)) + geom_point() + geom_line() + geom_hline(yintercept = m1) + 
  geom_hline(yintercept = m1 + 2*s1, linetype= 'dashed') + geom_hline(yintercept = m1 - 2*s1, linetype= 'dashed') +
  ggtitle(paste(lake,depth,"DOC")) + scale_y_continuous(paste("DOC","mg/L")) + theme(legend.position = "none")+ theme_bw()

plots<-cowplot::plot_grid(testplot1, testplot2,testplot3,testplot4,testplot5,testplot6,testplot7,testplot8, nrow = 4, ncol = 2)
cowplot::save_plot(paste(lake,"Chl_DOC_plots.pdf"), plots, base_height = 40, base_width = 30)

#Key to testplot names
  ######1: Hypo Chl
  #####2: PML Chl
  #3: Hypo DOC
  #4: PML DOC
  #5: Inlet1 DOC
  #6: Inlet2 DOC
  #7: Inlet3 DOC
  #8: Outlet DOC

#plot only 2019 data
newlakedepth = subset(newdata, newdata$depthClass == "PML" & newdata$lakeID== "TU")
testplot2019p <- ggplot(data = newlakedepth, aes(x = dateTimeSample, y = chl)) + geom_point() + geom_line() + 
  geom_hline(yintercept = m) + geom_hline(yintercept = m + 2*s, linetype= 'dashed') + geom_hline(yintercept = m - 2*s, linetype= 'dashed') + 
  scale_y_continuous(paste("Chl","ug/L")) + theme(legend.position = "none") + theme_bw()
#windows()
plot(testplot2019p) 
#won't plot data from newdata because it's in POSIXlt, not POSIXct. When rbind(data,newdata) it seems to make eveything
  #stay in POSIXct. Not sure how to plot just the 2019 data if the POSIXlt format stays.

#reviewing flag comments from past years
flags<-dbTable("CHLOROPHYLL")
flags<-subset(flags,flags$flag==1)
com<-data.frame(unique(flags$comments))
