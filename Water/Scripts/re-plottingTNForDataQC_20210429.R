#KS 2019-05-16; RNN 2020-03-02

## load data and connect multiple files
rm(list=ls())  #clear variables
library(ggplot2)
library(tidyverse)

# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("C:/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210423.db" #Change name of db file with every update
dbTableList()

QClist<-(c('BA','BO','BR','CB','CR','EL','FE','HB','MO','NG','PA','PE','TU','WA','WL','ME')) #pulls only data from QC lakes
data<-dbTable("WATER_CHEM",lakeID=QClist) %>% filter(parameter == "TN")


#Add back sample info removed in transition from NUTRIENTS to WATER_CHEM
data<- data %>% 
  add_column(depthClass = word(data$sampleID,5,5,sep="_")) %>% 
  add_column(siteName = word(data$sampleID,2,2,sep="_"))

#changes depth class "MidEpi" to "PML" and "EL" to "FE" because they get QC'd the same way.
#don't need to change back because the SITES, SAMPLES, and TABLE tables are just newdata, not all data
for(i in 1:nrow(data)){
  if(data$depthClass[i]%in%c('MidEpi','midEpi')){
    data$depthClass[i]<-("PML")
  }
  if(data$lakeID[i]=='EL'){
    data$lakeID[i]<-("FE")
  }  
} 

#brings in new year's data and appends it onto the all previous data
newdata<-read.csv("Water/Output/compiledData/tnCorrected_20210429.csv",header = TRUE, stringsAsFactors = F) #comes from the CompileXYZ.R script
newdata$flag[is.na(newdata$parameterValue)]=1 #need to flag NA samples here so they can be removed from QC analysis
newdata$dateSample<-as.Date(newdata$dateSample, format = "%m/%d/%Y")
#newdata$dateTimeSample<-as.POSIXct(newdata$dateTimeSample)
checkCols(data,newdata)

#formatting TN-only input into WATER_CHEM format: (Typically indv. nutrients go through a processing script to make tableNutrients)
newdata<- newdata %>% 
  add_column(parameter = "TN") %>% 
  rename(parameterValue = "TN") %>% 
  add_column(QCcode = NA) %>% 
  add_column(flag = 0) %>% 
  add_column(metadataID = "Nutrients.N.20110601") %>% 
  add_column(updateID = "fixedTN_20210429") %>% 
  add_column(siteName = word(newdata$sampleID,2,2,sep="_")) %>% 
  select(colnames(data))
checkCols(data,newdata)

#depthClass changes to WATER_CHEM
unique(newdata$depthClass)
unique(data$depthClass)
newdata<- newdata %>% 
  mutate(depthClass = case_when(depthClass == "Hypo" ~ "hypo", depthClass == "Surface" ~ "surface", TRUE ~ depthClass))

data<-rbind(data,newdata)
data<-subset(data, data$flag == 0)
data<-subset(data, lakeID %in% QClist) #pulls non-QC data out of 'data' that was added in 2019
data<-subset(data, depthClass %in% c("PML","hypo","surface")) #include "surface" here for DOC and color samples (inlets/outlets)
sites<-as.character(unique(newdata$siteName))
dataSites<-subset(data, siteName %in% c(sites)) #need this for Nutrients because there are additional sites from old experiments
#removed<-subset(data,!(data$sampleID%in%dataSites$sampleID))
data<-dataSites




#for loop that  
#1)subsets all data into unique lake/depthClass (lakeDepth) combinations
#2)calculates the mean and sd for each lakeDepth
#3)flags any new data points outside +/- 2*s and adds them to a df called "check"
#4)creates a plot of all data for that lake/depth over time, saves it to a list of plots for every unique lake/depth combo
#5)prints and saves pairs of figures for each lake (PML and hypo)
check<-data.frame()

#options for creating list of unique sites

unique_lakeDepths<-unique(newdata[c("lakeID","depthClass","siteName","parameter")])
unique_lakeDepths<-unique_lakeDepths[order(unique_lakeDepths$lakeID),]
unique_lakeDepths<-unique_lakeDepths[unique_lakeDepths$lakeID %in% QClist,]
unique_lakeDepths<-unique_lakeDepths %>% filter(depthClass %in% c("PML","hypo","Surface"))
# newSites<-c("U5","P15","U4")
# unique_lakeDepths<-unique_lakeDepths[!(unique_lakeDepths$siteName %in% newSites), ] #U5 only has nutrient data from this year

    unique_lakeDepths$m<-NA
    unique_lakeDepths$s<-NA

unique_lakeDepths<-unique_lakeDepths[with(unique_lakeDepths, order(unique_lakeDepths$lakeID, unique_lakeDepths$depthClass)),]
unique_lakeDepths$Num<-seq(1,nrow(unique_lakeDepths),1)
unique_lakeDepths$parameter<-as.character(unique_lakeDepths$parameter)
#remove BA_Inlet1 (only one datapoint)
# unique_lakeDepths<- unique_lakeDepths %>% filter(!(lakeID=="BA" & siteName=="Inlet1"))

plot_list = list()

j=1  
r=1
for(j in 1:nrow(unique_lakeDepths)){
  #pulls out the unique lake/depthClass data for all years
  lakedepth<-subset(data,data$lakeID==unique_lakeDepths$lakeID[j] & data$depthClass==unique_lakeDepths$depthClass[j] & data$siteName==unique_lakeDepths$siteName[j] & data$parameter==unique_lakeDepths$parameter[j])
  lakedepth$check<-"not done yet"
  lakedepth$plotNum<-unique_lakeDepths$Num[j]
    m<-mean(lakedepth$parameterValue)
    s<-sd(lakedepth$parameterValue)


    unique_lakeDepths$m[j]<-m
    unique_lakeDepths$s[j]<-s
  
  
  #flags any new data points that have a value greater than 2*s in a column called lakedepth$check
    for(r in 1:nrow(lakedepth)){
    if(lakedepth$parameterValue[r]>(m+s*2) | lakedepth$parameterValue[r]<(m-s*2)){
      lakedepth$check[r]=1
    }else{
      lakedepth$check[r]=0
    }
  }
    
  #adds all the sample information from any flagged rows into the df "check"
  #colnames(check1)<-colnames(lakedepth) #gives column names equal to data
  check1<-lakedepth[lakedepth$check==1,]
  check<-rbind(check, check1)
  check<-subset(check,dateSample>="2020-01-01")
  
  #Summarize how many parameters from each check sample exist. >parameters == >likelihood that there's something wrong with the sample/pull
  check$parameter<-as.factor(check$parameter)
  check_samples<-check %>% 
    group_by(sampleID) %>% 
    summarize(n_analyte = n_distinct(parameter))
  
  #creates a plot of all data at that lake/depthClass combo
  testplot1 <- ggplot(data = lakedepth, aes(x = dateSample, y = parameterValue)) + geom_point() + geom_line() +
    geom_hline(yintercept = m) + geom_hline(yintercept = m + 2*s, linetype= 'dashed') + geom_hline(yintercept = m - 2*s, linetype= 'dashed') +
    ggtitle(paste(unique_lakeDepths$lakeID[j],unique_lakeDepths$depthClass[j], unique_lakeDepths$siteName[j],unique_lakeDepths$parameter[j])) + 
    scale_y_continuous(paste(unique_lakeDepths$parameter[j], "(ug/L)",sep=" ")) + theme(legend.position = "none") + theme_bw()

  plot_list[[j]] = testplot1
}

#nutrients_check.csv
#write.csv(check,"Water/Output/checkCSVs/TNcorrected_check.csv")




plot(plot_list[[7]]) #how to plot just one plot from the list
###### End of big for loop
###### Don't need to continue if you're not re-printing the plots to PDF  

#plot by site
site<- unique_lakeDepths %>% 
  distinct(lakeID,depthClass,siteName) %>% 
  mutate(lakeID = as.character(lakeID)) %>% 
  mutate(depthClass = as.character(depthClass)) %>% 
  mutate(siteName = as.character(siteName))


i=1
for(i in 1:nrow(site)){
  # site$tp[i]<-unique_lakeDepths$Num[unique_lakeDepths$lakeID==site$lakeID[i]&
  #                                     unique_lakeDepths$depthClass==site$depthClass[i]&
  #                                     unique_lakeDepths$siteName==site$siteName[i]&
  #                                     unique_lakeDepths$parameter=="TP"]
  # 
  # site$srp[i]<-unique_lakeDepths$Num[unique_lakeDepths$lakeID==site$lakeID[i]&
  #                                     unique_lakeDepths$depthClass==site$depthClass[i]&
  #                                     unique_lakeDepths$siteName==site$siteName[i]&
  #                                     unique_lakeDepths$parameter=="SRP"]
    
  site$tn[i]<-unique_lakeDepths$Num[unique_lakeDepths$lakeID==site$lakeID[i]&
                                      unique_lakeDepths$depthClass==site$depthClass[i]&
                                      unique_lakeDepths$siteName==site$siteName[i]&
                                      unique_lakeDepths$parameter=="TN"]
      
  # site$no3[i]<-unique_lakeDepths$Num[unique_lakeDepths$lakeID==site$lakeID[i]&
  #                                     unique_lakeDepths$depthClass==site$depthClass[i]&
  #                                     unique_lakeDepths$siteName==site$siteName[i]&
  #                                     unique_lakeDepths$parameter=="nitrate"]
}

p=1
for(p in 1:nrow(site)){
  #plots<-cowplot::plot_grid(plot_list[[site$tp[p]]],plot_list[[site$srp[p]]],plot_list[[site$tn[p]]],plot_list[[site$no3[p]]],nrow=2,ncol=2)
  plots<-plot_list[[site$tn[p]]]
  cowplot::save_plot(file.path("Water/QC plots/TNcorrected",paste(site$lakeID[p], site$depthClass[p],site$siteName[p],"Nutrient_plots.pdf",sep="_")), plots, base_height = 5, base_width = 10)
  #cowplot::save_plot(file.path("Water/QC plots/Nutrients",paste(site$lakeID[p],site$siteName[p],"Nutrient_plots.pdf",sep="_")), plots, base_height = 15, base_width = 20)
  
  }




# #Make rerun list
# #go to the "nutrients_check.csv" and review the plots to determine which ones need to be reran. 
# #mark them in the spreadsheet
# #produce "missing" table for reruns list:
# #is there a TN, TP, SRP, and nitrate value for every sampleID in the log?
# missing<-newdata %>% group_by(sampleID) %>% count() %>% filter(n<4) %>% pull(sampleID)
# have<- newdata %>% filter(sampleID %in% missing) %>% arrange(sampleID)
# have$item<-paste(have$sampleID, have$parameter, sep="_")
# need<-expand.grid(sampleID=missing, parameter=c("TN", "TP", "SRP", "nitrate"))
# need$item<-paste(need$sampleID, need$parameter, sep="_")
# find<- need %>% filter(!item %in% have$item) 
# find$rerun<- "y"
# 
# #read it back in: 
# run<-read.csv("Water/Output/checkCSVs/nutrients_check_StuartComments.csv", header = T, stringsAsFactors = F)
# run<-run[,1:19]
# run<-bind_rows(run, find)
# run<-subset(run,run$rerun=="y")
# run<-run[order(run$parameter),]
# i=1
# for(i in 1:nrow(run)){
#   if(run$parameter[i] %in% c("TP","TN")){
#     run$type[i]<-"unfiltered"
#   }else{
#     run$type[i]<-"filtered"
#   }
# }
# 
# runf<-subset(run,run$type=="filtered")
# fLog<-read.csv("logFiles2020/correctedFiles/filteredLogFile.csv",header=T, stringsAsFactors = F)
# runf<-merge(runf,fLog[,c("sampleID","filteredID","timeSample")],by="sampleID",all.x=TRUE)
# runf<- rename(runf, ID = "filteredID")
# 
# runu<-subset(run,run$type=="unfiltered")
# uLog<-read.csv("logFiles2020/correctedFiles/unfilteredLogFile.csv",header=T, stringsAsFactors = F)
# runu<-merge(runu,uLog[c("sampleID","unfilteredID","timeSample")],by="sampleID",all.x=TRUE)
# runu<- rename(runu, ID = "unfilteredID")
# checkCols(runf,runu)
# run<-bind_rows(runf,runu) %>% select(sampleID, ID, parameter)
# 
# 
# dupList<-run[duplicated(run$ID),] #lists duplicated samples (watch out for minimum volumes)
# dup<-run[run$sampleID%in%dupList$sampleID,] #lists duplicates and originals
# run<-run[order(run$parameter),]
# i=1
# # for(i in 1:nrow(run)){
# #   if(run$sampleID[i]%in%dupList$sampleID){
# #     run$dup[i]<-1
# #   }else{
# #     run$dup[i]<-0
# #   }
# # }
# # want<-c("dup","ID","sampleID","parameter","parameterValue","lakeID","siteName","dateSample","depthClass","plotNum","Review.comments",)
# # run<-run[,want]
# 
# 
# 
# 
# 
# #adding runIDs to see if anything was wonky in a particular run (there are a lot of TN samples)
# tp<-read.csv("Water/Output/compiledData/tp_20210216.csv",header=TRUE,stringsAsFactors = FALSE)
# srp<-read.csv("Water/Output/compiledData/srp_20210217.csv",header=TRUE,stringsAsFactors = FALSE)
# tn<-read.csv("Water/Output/compiledData/tn_20210216.csv",header=TRUE,stringsAsFactors = FALSE)
# no3<-read.csv("Water/Output/compiledData/no3_20210217.csv",header=TRUE,stringsAsFactors = FALSE)
# 
# tp<-tp[,c("sample","runID")]
# srp<-srp[,c("sample","runID")]
# tn<-tn[,c("sample","runID")]
# no3<-no3[,c("sample","runID")]
# 
# i=1
# for(i in 1:nrow(run)){
#   if(run$parameter[i]=="nitrate"){
#     run$runID[i]<-if(run$ID[i] %in% no3$sample){
#       no3$runID[no3$sample==run$ID[i]]
#       }else{"NA"}
#   }
#   if(run$parameter[i]=="TN"){
#     run$runID[i]<-if(run$ID[i] %in% tn$sample){
#       tn$runID[tn$sample==run$ID[i]]
#       }else{"NA"}
#   }
#   if(run$parameter[i]=="SRP"){
#     run$runID[i]<-if(run$ID[i] %in% srp$sample){
#       srp$runID[srp$sample==run$ID[i]]
#       }else{"NA"}
#   }
#   if(run$parameter[i]=="TP"){
#     run$runID[i]<-if(run$ID[i] %in% tp$sample){
#       tp$runID[tp$sample==run$ID[i]]
#       }else{"NA"}
#   }
# }
# str(run)
# # run$dateSample<-as.Date(as.character(run$dateSample),format="%m/%d/%Y")
# # run<-subset(run,run$dateSample>"2019-05-01") #remove's Brittni's April winter sample
# 
# 
# 
# write.csv(run,"Water/Output/checkCSVs/nutrients_run_list.csv",row.names=FALSE)
# 
# tn10<-subset(tn,tn$runID=="TN_2019_Run10_20200131.csv")
# add<-tn10[!(tn10$sample%in%run$ID),]
# add<-merge(add,uLog[,c("sampleID","unfilteredID")],by.x="sample",by.y="unfilteredID")
# runf<-merge(runf,fLog[,c("sampleID","filteredID","timeSample")],by="sampleID",all.x=TRUE)
# 
# 
# #Swap QC'd values for those that need replacing
# #I used the following method for QC changes in 2019/2020 but it could use some streamlining in the future.
# #Produced the re-run list as above. 
# #Reran each sample, leaving out samples that were empty and couldn't be rerun. 
# #Entered the run datasheets as "rerun"... .csv in with the rest of the data sheets per nutrient. 
# #Reran the compileXYZ.R script and use the dupList and dup sections to create lists of originals and rerun values. 
# #Pulled up the table "dup" one nutrient at a time and compared the originals and reruns.
#   #Had nutrients_check up, which had my annotations about why I was flagging it for rerun.
#   #Also had "nutrient QC codes.csv" open (just unique data$comments for flagged nutrients that applied to QC testing)
# #Decided which samples to keep originals vs. reruns. Consult "nutrient QC codes.csv" for specifics. 
#   #eg. Samples that had reruns withing 20% of original were reported as averages. 
#   #eg. Reruns that were very different either kept the original OR the new based on the original rerun flag comment. 
#   #eg. TN's run10 in 2019 was messed up, so all the samples were WAY too high. I didn't flag them, just replaced them. 
# #Once decided on 1)keep original, 2)keep rerun, or 3)average: officially flag and comment them using nutrient QC codes. 
#   #Try to use existing codes and only make new ones if you have to. 
#   #Made these flags in the nutrients_check.csv as well as a "kOrig" binary to help filter comments/flags in next step. 
# 
# 
# #Now you have:
#   #1) newdata (from this script, all the original run parameterValues)
#   #2) nutrients_check.csv with kOrig, flag, and comments completed
#       nutrients_check<-read.csv("C:/Users/notter/Google Drive/Randi/Sample Analysis/raw data needing calculations/2019/nutrients_check.csv",header=TRUE)
#       nutrients_check$parameterValue<-as.numeric(nutrients_check$parameterValue)
#       nutrients_check$rerun.value<-as.numeric(as.character(nutrients_check$rerun.value))
# #Begin to merge newdata and nutrients_check
#   #Remove observations where kOrig=0 (meaning you don't want to keep the original, want to replace)
#       rem<-subset(nutrients_check,nutrients_check$kOrig==0)
#       rem$comments<-as.character(rem$comments)
#       rem %>% group_by(parameter) %>% summarize(count=n_distinct(sampleID))
#       
#       newdata<-newdata1
#       newdata$comments<-as.character(newdata$comments)
#       newdata$long<-paste(newdata$sampleID,newdata$parameter,sep="_")
#       rem$long<-paste(rem$sampleID,rem$parameter,sep="_")
#       i=111
#       for(i in 1:nrow(newdata)){
#         if(newdata$long[i] %in% rem$long){
#           newdata$parameterValue[i]<-rem$rerun.value[rem$long==newdata$long[i]]
#           newdata$flag[i]<-rem$flag[rem$long==newdata$long[i]]
#           
#           if(newdata$comments[i]==""){
#             newdata$comments[i]<-rem$comments[rem$long==newdata$long[i]]
#           }else{
#             newdata$comments[i]<-paste(rem$comments[rem$long==newdata$long[i]],". ",newdata$comments[i],sep="")
#           }
#         }
#       }
#       
#       #make sure that these classes line up by checking dims
#       newdata %>% 
#         mutate(as.factor(newdata$comments)) %>% 
#         group_by(comments) %>% 
#         summarize(count=n_distinct(long))
#         
#       #visually check to make sure that the rem parameterValues transferred over correctly. 
#       newdata$long %in% rem$long
#       
#       #change updateID if necessary (makes sense since you're saving a new file)
#       newdata$updateID<-"tableNutrients.20200316"
#       newdata$long<-NULL
#       
#       add<-data.frame
#       add<-fLog[fLog$filteredID=="F137",]
#       need<-colnames(newdata)[!(colnames(newdata)%in%colnames(fLog))]
#       add$siteName<-"DeepHole"
#       add$dateSample<-as.Date(add$dateSample, format="%m/%d/%Y")
#       add$dateTimeSample<-as.POSIXct("2019-07-01 9:55",format="%Y-%m-%d %H:%M",tz="America/Chicago")
#       add$parameter<-"nitrate"
#       add$parameterValue<-last(no3$NO3)
#       add$metadataID<-newdata$metadataID[1]
#       add$flag<-"0"
#       add$updateID<-newdata$updateID[1]
#       add<-add %>% select(colnames(newdata))
#       
#       newdata<-rbind(newdata,add)
#       
#       
# #write.csv(newdata,"C:/Users/notter/Google Drive/Randi/Database/currentdb/tableNutrients_20200316.csv",row.names=FALSE)
# 
# #Bring this back into compileNutrientTables.R and update the site, sample, and table 
#       #It's possible that you added new samples with the reruns. 