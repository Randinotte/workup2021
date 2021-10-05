#checking for NFC fish
#RNN 2020-12-2
#Thought that maybe the 2019 FishScapes and 2020 Jones Lake data was only including positive fish samples in fishSamplesIS (leaving)
#out any NFC samples. Turns out this wasn't the case. Caught a different error in the 2020 Jones data, fixed it in compileFishSamples_20201124.R

####NFC Samples####----------------------------------------------------------------------------
###**FS19####

###**J20####### 
#!all samples WERE in fishSamplesIS, and there were no NFC.
files<-list.files("Fish/input/2020 Jones Lake/datasheetsCSV")
files<- files[grepl("_2020_",files)==FALSE]
samples<-data.frame()
#colnames(samples)<-names


for (i in 1:length(files)){
  cur<- read.csv(file.path("Fish/input/2020 Jones Lake/datasheetsCSV",files[i]),header = F, stringsAsFactors = F)
  cur<- cur[1:17,1:2]
  cur<- t(cur)
  if(i == 1){
    names<- cur[1,]
  }
  colnames(cur)<-names
  cur<-t(cur[2,])
  
  samples<-rbind(samples,cur)
}

###**FISH_SAMPLES
#check missing/extra columns#
missing<- colnames(fsamp)[!colnames(fsamp) %in% colnames(samples)] #updateID, lakeID
extra<- colnames(samples)[!colnames(samples) %in% colnames(fsamp)] #entryFile

#add/remove columns#
samples<- samples %>% 
  add_column(siteID = paste(samples$lakeID, samples$siteName, sep="_")) %>% 
  add_column(sampleID = NA) %>%
  add_column(dayOfYear = NA) %>%
  add_column(dateSet = NA) %>%
  add_column(dateSample = NA) %>%
  rename(dateTimeSet = "dataTimeSet") %>%
  rename(useCPUE = "UseCPUE") %>% 
  add_column(updateID = "sampleFishInfo.20201124") %>%
  select(c("lakeID",colnames(fsamp)))
samples$timeID<-gsub(":","",word(as.character(samples$dateTimeSample),2,2,sep=" "))
samples<- samples %>%
  mutate(timeID = case_when(nchar(samples$timeID)==3~ paste("0",samples$timeID,sep=""), TRUE ~ samples$timeID)) %>% 
  mutate(dateSet = as.Date(parse_date_time(samples$dateTimeSet, orders = c("mdy HM","mdY HM"), tz = "America/New_York"))) %>%
  mutate(dateSample = as.Date(parse_date_time(samples$dateTimeSample, orders = c("mdy HM","mdY HM"), tz = "America/New_York"))) %>%
  mutate(dateTimeSet = parse_date_time(samples$dateTimeSet, orders = c("mdy HM","mdY HM"), tz = "America/New_York")) %>%
  mutate(dateTimeSample = parse_date_time(samples$dateTimeSample, orders = c("mdy HM","mdY HM"), tz = "America/New_York")) %>% 
  add_column(dateID = as.character(gsub("-","",as.Date(parse_date_time(samples$dateTimeSample, orders = c("mdy HM","mdY HM"), tz = "America/New_York")))))
samples<- samples %>% 
  mutate(sampleID = paste(samples$siteID, samples$dateID, samples$timeID, samples$gear, "JonesLakeExperiment.20200607",sep="_")) %>% 
  mutate(dayOfYear = strftime(samples$dateSample, format = "%j")) 


#table fixes#
unique(samples$metadataID) %in% dbTable("METADATA")$metadataID #multiple metadataID spellings
unique(word(samples$sampleID,6,6,sep="_")) #multiple metadataID's in sampleID
samples<- samples %>% 
  mutate(siteID = gsub("wholeShoreline","WholeShoreline",samples$siteID)) %>%
  mutate(sampleID = gsub("wholeShoreline","WholeShoreline",samples$sampleID))


#see how j20samp compares to samples
NFC<-samples$sampleID[!samples$sampleID %in% j20samp$sampleID] #none!
nrow(j20samp)+length(NFC)==nrow(samples) #woo!
missing<- subset(samples,samples$sampleID %in% NFC)
FC<-subset(samples$sampleID,samples$sampleID %in% j20samp$sampleID) #201
dup<-samples[duplicated(samples$sampleID),] #yay no dups

#did duplicate raw csv's result in duplicate fish in fishInfoIS->j20info?
dup<-j20info[duplicated(j20info$fishID),] #no

#did these sampleID problems (CR_WholeShoreline_20200717) occur in fishInfoIS->j20info?
unique(j20info$sampleID[grepl("00200717",j20info$sampleID)]) #yes 
nrow(j20info[grepl("00200717",j20info$fishID),])
#fixed this
