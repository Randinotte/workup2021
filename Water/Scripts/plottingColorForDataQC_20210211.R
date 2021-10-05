#Original script by KMS (C:\Old Data\KSaunders files\Documents\R\R files\plotting DOCchl for data QC.R), updates by RNN in 2019. 
#Contents: Script for QCing yearly data, plus writing .csv output for the SITES, SAMPLES, and TABLES tables that are needed in writingUpdatingDBtables.R


rm(list=ls())
library(ggplot2)
## load data and connect multiple files
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")

dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20210112.db" 
#setwd('C:/Users/notter/Google Drive/Randi/Database/plots/Data QC plots/Color QC 2019 plots')
QClist<-(c('BA','BO','BR','CB','CR','EL','FE','HB','MO','NG','PA','PE','TU','WA','WL','ME')) #pulls only data from QC lakes
data<-dbTable('COLOR',lakeID=QClist)

#changes depth class "MidEpi" to "PML" and "EL" to "FE" because they get QC'd the same way.
#changes EL to FE for all sites except the outlet, which is sent to ME.
#don't need to change back because the SITES, SAMPLES, and TABLE tables are just newdata, not all data
for(i in 1:nrow(data)){
  if(data$depthClass[i]=='MidEpi'){
    data$depthClass[i]<-("PML")
  }
  if(data$lakeID[i]=='EL' & data$siteName[i]!="Outlet"){
    data$lakeID[i]<-("FE")
  }
  if(data$lakeID[i]=='EL' & data$siteName[i]=="Outlet"){
    data$lakeID[i]<-("ME")}
}


#brings in new year's data and appends it onto the all previous data
newdata<-read.csv("Water/Output/compiledData/color_20210211.csv",header = TRUE, stringsAsFactors = F) #comes from the CompileChla.R script
#newdata<-newdata[,-1]
#want= c("projectID", "sampleID","lakeID", "siteName", "dateTimeSample", "depthClass", "depthTop", "depthBottom", "runID","chl","replicate", "flag")#nutrients
#data<-data[want]
data<-subset(data, data$flag == 0)
#newdata<-newdata[want]
#newdata$dateTimeSample=strptime(x = as.character( newdata$dateTimeSample ), format = "%m/%d/%Y %H:%M")
newdata$dateTimeSample=as.POSIXct(newdata$dateTimeSample,tz="America/Chicago")
data<-rbind(data,newdata)
#newdata$siteName<-as.character(newdata$siteName)
data<-subset(data, lakeID %in% QClist) #pulls non-QC data out of 'data' that was added in 2019


dbColor<-dbTable("COLOR",lakeID="FE",depthClass="Surface")
library("dplyr")
#data %>% group_by(data$depthClass) %>% summarize(count=n())
#sub<-subset(data,data$depthClass=="Meta" |data$depthClass=="Epi")
#Decisions: 
  #Ignoring for color QC:
    #Epi:   Only 2 from 2017. Potentially a log file data entry error?
    #Meta:  Only 22 from 2013 and 2 from 2017
    #point: Only 33 from 2015/2018
  #Need to include Surface for color QC
  #Excluded BA_Inlet1 because we only have one sample
  #Merged FE_Outlet with ME_Outlet because they're the same thing

data<-subset(data, depthClass %in% c("PML","Hypo","Surface")) #include "surface" here for DOC and color samples (inlets/outlets)
data<-subset(data, siteName %in% c("DeepHole","Inlet1","Inlet2","Inlet3","Outlet"))


#for loop that  
#1)subsets all data into unique lake/depthClass (lakeDepth) combinations
#2)calculates the mean and sd for each lakeDepth
#3)flags any new data points outside +/- 2*s and adds them to a df called "check"
#4)creates a plot of all data for that lake/depth over time, saves it to a list of plots for every unique lake/depth combo
#5)prints and saves pairs of figures for each lake (PML and Hypo)
i=2
j=2
check<-data.frame()
#unique_lakeDepths<-unique(data[c("lakeID","depthClass")])
#unique_lakeDepths<-unique_lakeDepths[order(unique_lakeDepths$lakeID),] #Can't rely on "Surface" for MO and EL/FE that have an inlet and an outlet
unique_lakeDepths<-unique(data[c("lakeID","depthClass","siteName")])
unique_lakeDepths<-unique_lakeDepths[order(unique_lakeDepths$lakeID),]
  #Problems:  BA_Inlet1 only has one data point. Error?
            # ME has the Outlet in 2019 but EL>>FE has it <2019  
unique_lakeDepths<-unique_lakeDepths[!(unique_lakeDepths$lakeID=="BA"&unique_lakeDepths$siteName=="Inlet1"),]
unique_lakeDepths<-unique_lakeDepths[!(unique_lakeDepths$depthClass=="Surface"&unique_lakeDepths$siteName=="DeepHole"),]
unique_lakeDepths$m<-NA
unique_lakeDepths$s<-NA
unique_lakeDepths<-unique_lakeDepths[
  with(unique_lakeDepths, order(unique_lakeDepths$lakeID, unique_lakeDepths$depthClass)),]
unique_lakeDepths$Num<-seq(1,nrow(unique_lakeDepths),1)
plot_list = list()

j=13  
r=2
for(j in 1:nrow(unique_lakeDepths)){
  #pulls out the unique lake/depthClass data for all years
  lakedepth<-subset(data,data$lakeID==unique_lakeDepths$lakeID[j] & data$depthClass==unique_lakeDepths$depthClass[j] & data$siteName==unique_lakeDepths$siteName[j])
  
  #calculates the all years mean and sd for just one lake/depth pair at a time
  m<-mean(lakedepth$abs440)
  s<-sd(lakedepth$abs440)
  
  #adds the lake/depth's mean and sd to the lakeDepths df for record keeping
  unique_lakeDepths$m[j]<-m
  unique_lakeDepths$s[j]<-s
  
  
  #flags any new data points that have a value greater than 2*s in a column called lakedepth$check
  for(r in 1:nrow(lakedepth)){
    if(lakedepth$abs440[r]>(m+s*2) | lakedepth$abs440[r]<(m-s*2)){
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
  testplot1 <- ggplot(data = lakedepth, aes(x = dateTimeSample, y = abs440)) + geom_point() + geom_line() +
    geom_hline(yintercept = m) + geom_hline(yintercept = m + 2*s, linetype= 'dashed') + geom_hline(yintercept = m - 2*s, linetype= 'dashed') + 
    ggtitle(paste(unique_lakeDepths$lakeID[j],unique_lakeDepths$siteName[j],unique_lakeDepths$depthClass[j],"abs440")) + scale_y_continuous(paste("abs440")) + theme(legend.position = "none") + theme_bw()
  
  plot_list[[j]] = testplot1
}
plot(plot_list[[2]]) #how to plot just one plot from the list

#write.csv(check,"Water/Output/checkCSVs/color_check.csv")
###### End of big for loop
###### Don't need to continue if you're not re-printing the plots to PDF

lakepairs<-data.frame(PML=seq(1,30,2),HYPO=seq(2,30,2),lakeID=unique(unique_lakeDepths$lakeID))
#sub<-subset(data,data$depthClass=="Surface" & data$siteName=="DeepHole")
counts<-data.frame(unique_lakeDepths %>% group_by(unique_lakeDepths$lakeID) %>% summarize(count=n()), PML=1, HYPO=1, Inlet1=0, Inlet2=0, Inlet3=0, Outlet=0)
counts<-counts[order(counts$count),]
i=1
for (i in 1:nrow(unique_lakeDepths)){
  r<-unique_lakeDepths$lakeID[i]
  if(unique_lakeDepths$siteName[i]=="Inlet1"){
    counts$Inlet1[counts$unique_lakeDepths.lakeID==r]<-1
  }
  if(unique_lakeDepths$siteName[i]=="Inlet2"){
    counts$Inlet2[counts$unique_lakeDepths.lakeID==r]<-1
  }
  if(unique_lakeDepths$siteName[i]=="Inlet3"){
    counts$Inlet3[counts$unique_lakeDepths.lakeID==r]<-1
  }
  if(unique_lakeDepths$siteName[i]=="Outlet"){
    counts$Outlet[counts$unique_lakeDepths.lakeID==r]<-1

  }
 
}

#lake plots
lakePML<-subset(unique_lakeDepths,unique_lakeDepths$depthClass=="PML")
lakeHYPO<-subset(unique_lakeDepths,unique_lakeDepths$depthClass=="Hypo")
lakes<-merge(lakePML,lakeHYPO,by="lakeID")

p=1
for(p in 1:nrow(lakes)){
  plots<-cowplot::plot_grid(plot_list[[lakes$Num.x[p]]],plot_list[[lakes$Num.y[p]]],nrow=2,ncol=1)
  cowplot::save_plot(file.path("Water/QC plots/Color", paste0(lakes$lakeID[p], "_Color_plots.pdf")), plots, base_height = 10, base_width = 10)
}


#Surface plots
surf<-subset(unique_lakeDepths,unique_lakeDepths$depthClass=="Surface")

p=7
for (p in 1:nrow(surf)){
  if(surf$lakeID[p]!="MO"){
    plot<-cowplot::plot_grid(plot_list[[surf$Num[p]]],nrow=2,ncol=1)
    cowplot::save_plot(file.path("Water/QC plots/Color",paste0(surf$lakeID[p],surf$siteName[p],"_Color_plot.pdf")),plot, base_height = 10, base_width = 10)
  }
}

#MO I1 and I2
plots<-cowplot::plot_grid(plot_list[[23]],plot_list[[24]], nrow=2, ncol=1)
cowplot::save_plot(file.path("Water/QC plots/Color","MO_Surface_Inlets_1-2_Color_plots.pdf"), plots, base_height = 10, base_width = 10)  

#MO I3 and O
plots<-cowplot::plot_grid(plot_list[[25]],plot_list[[26]], nrow=2, ncol=1)
cowplot::save_plot(file.path("Water/QC plots/Color","MO_Surface_Inlet_3_and_Outlet_Color_plots.pdf"), plots, base_height = 10, base_width = 10)

############ Flag newdata
#After evaluating the "check" output csv and flagging samples in the csv, continue below: 
#check<-read.csv("C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/Color/color_check.csv",stringsAsFactors = F)
check<-read.csv("Water/Output/checkCSVs/color_check_StuartComments.csv", header = T, stringsAsFactors = F)
check$dateSample<-as.Date(check$dateSample,format="%m/%d/%Y")


i=7
for (i in 1:nrow(check)){
  newdata$flag[newdata$sampleID==check$sampleID[i]]<-check$flag[i]
  #newdata$flag[newdata$sampleID==check$sampleID[i] & newdata$replicate==check$replicate[i]]<-check$flag[i]
  newdata$comments[newdata$sampleID==check$sampleID[i]]<-check$comments[i]
  #newdata$comments[newdata$sampleID==check$sampleID[i] & newdata$replicate==check$replicate[i]]<-check$comments[i]
}
nrow(subset(newdata,newdata$flag==1)) #Check that this matches the number in df "check"

#remove last Jones Lake JO sample because it was never run
newdata<-newdata[-c(167,168),]
write.csv(newdata, "Water/Output/color2020_forBella_20210211.csv", row.names = F)

################################
#Making SITE, SAMPLE, and TABLE tables for export to "Current Database" folder
#Make these !!!!AFTER!!!! QC has been done and flags, comments, and updateIDs have been added to the newdata df

###siteColor_YYYYMMDD###
siteColor <-newdata[order(newdata$lakeID),]
siteColor$updateID<-"siteColor.20200203"
#need to add lat, long, and UTM to sites
lakes<-dbTable("LAKES")
siteLakes<-data.frame(unique(siteColor$lakeID))
names(siteLakes)[1]<-"lakeID"
siteLakes<-merge(lakes,siteLakes)
siteLakes<-siteLakes[c("lakeID","lat","long","UTM")]
siteColor<-merge(siteColor,siteLakes,by="lakeID")
siteColor$siteID<-paste(siteColor$lakeID,"_",siteColor$siteName,sep="")
wantSite<-c("siteID","lakeID","siteName","lat","long","UTM","updateID")
siteColor<-siteColor[wantSite]
#check that vector data formats are the same between siteColor and SITES table
str(dbTable("SITES"))
str(siteColor)
db<-dbTable("SITES")
df<-data.frame("vector"=(names(db)))
i=1
for(i in 1:ncol(siteColor)){
  df$site[i]<-class(siteColor[,i])
}
for(i in 1:ncol(db)){
  df$DB[i]<-class(db[,i])
}

i=2
df$DB<-as.character(df$DB)
for(i in 1:nrow(df)){
  if (df$DB[i]=="character"){
    siteColor[,paste(df$vector[i])]<-as.character(siteColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="numeric"){
    siteColor[,paste(df$vector[i])]<-as.numeric(siteColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="integer"){
    siteColor[,paste(df$vector[i])]<-as.integer(siteColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="factor"){
    siteColor[,paste(df$vector[i])]<-as.factor(siteColor[,paste(df$vector[i])])
  }
}
write.csv(siteColor,paste(dbdir,"siteColor_20200203.csv",sep=""),row.names=FALSE)

###sampleColor_YYYYMMDD###

    ## Note: The samplesIS table was incorrect for samples collected on 6/3 and 6/4.I changed the sampleIS table momentarily to the redo values, then changed it back. 
    ## Would cause problems (show up in fix) if the sampleColor table portion of code is rerun.
    ## See (C:\Users\notter\Google Drive\Randi\Sample Analysis\raw data needing calculations\2019\Color\color sample table corrections) for corrected values.
sampleColor<-newdata[order(newdata$lakeID),]
sampleColor$updateID<-"sampleColor.20200203"
log<-read.csv("C:/Users/notter/Google Drive/Summer 2019/Limno/limnoEntryTool/logFiles2019/samplesIS.csv")
log<-log[c("sampleID","crew","weather","comments")]
sampleColor<-merge(sampleColor,log,by="sampleID",all.x=TRUE) #all samples taken, including those that don't have sample info logs
fix<-sampleColor[is.na(sampleColor$crew),] #checks to make sure that all rows have sample data from the sampleIS.csv. If this returns with 0 entries, then all data matches. Rows that populate do not have matching sampleIDs in the sampleIS.csv and chlLogFile.csv (and all derivitive data frames)

sampleColor$siteID<-paste(sampleColor$lakeID,"_",sampleColor$siteName,sep="")
wantSample<-c("siteID","sampleID","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","crew","weather","comments.y","metadataID","updateID")
sampleColor<-sampleColor[wantSample]
names(sampleColor)[10]<-"comments"
#check that vector data formats are the same between sampleColor and SAMPLES table
str(dbTable("SAMPLES"))
str(sampleColor)
db<-dbTable("SAMPLES")
df<-data.frame("vector"=(names(db)))
i=1
for(i in 1:ncol(sampleColor)){
  df$sample[i]<-class(sampleColor[,i])
}
for(i in 1:ncol(db)){
  df$DB[i]<-class(db[,i])
}

i=2
df$DB<-as.character(df$DB)
for(i in 1:nrow(df)){
  if (df$DB[i]=="character"){
    sampleColor[,paste(df$vector[i])]<-as.character(sampleColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="numeric"){
    sampleColor[,paste(df$vector[i])]<-as.numeric(sampleColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="integer"){
    sampleColor[,paste(df$vector[i])]<-as.integer(sampleColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="factor"){
    sampleColor[,paste(df$vector[i])]<-as.factor(sampleColor[,paste(df$vector[i])])
  }
}
sampleColor$dateSample<-as.Date(sampleColor$dateSample,format="%Y-%m-%d")
write.csv(sampleColor,paste(dbdir,"sampleColor_20200203.csv",sep=""),row.names=FALSE)

###tableColor_YYYYMMDD###
tableColor<-newdata[order(newdata$lakeID),]
tableColor$updateID<-"tableColor.20210221"
checkCols(dbTable("COLOR"),tableColor)
wantTable<-c("projectID","sampleID","lakeID","siteName","dateSample","dateTimeSample","depthClass","depthTop","depthBottom","runID","chl","replicate","metadataID","comments","flag","updateID")
tableColor<-tableColor[wantTable]

str(dbTable("Color"))
str(tableColor)
db<-dbTable("Color")
df<-data.frame("vector"=(names(db)))
i=1
for(i in 1:ncol(tableColor)){
  df$table[i]<-class(tableColor[,i])
}
for(i in 1:ncol(db)){
  df$DB[i]<-class(db[,i])
}

i=2
df$DB<-as.character(df$DB)
for(i in 1:nrow(df)){
  if (df$DB[i]=="character"){
    tableColor[,paste(df$vector[i])]<-as.character(tableColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="numeric"){
    tableColor[,paste(df$vector[i])]<-as.numeric(tableColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="integer"){
    tableColor[,paste(df$vector[i])]<-as.integer(tableColor[,paste(df$vector[i])])
  }
  if (df$DB[i]=="factor"){
    tableColor[,paste(df$vector[i])]<-as.factor(tableColor[,paste(df$vector[i])])
  }
}
tableColor$dateSample<-as.Date(tableColor$dateSample)
write.csv(tableColor,"Water/Output/compiledData/tableColor_20210221.csv",row.names=FALSE)










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


####################Remember to go through all the usual data QC like duplicates, etc. in whatever script Katie used for that. 
# probably the WritingUpdatingDBtables.R?
