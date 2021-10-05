#RNN 2020-12-01 (somehow it's already DECEMBER)
#compiles fishSamplesIS.csv

dir<-"Fish/input/2020 Long Lake/minnowTraps/datasheetsCSV"
files<-list.files(dir)
samples<-data.frame()
ll20sampMT<-data.frame()

i=1
for (i in 1:length(files)){
  csv<- read.csv(file.path(dir,files[i]),header = F, stringsAsFactors = F,fileEncoding="UTF-8-BOM") #fileEncoding fixes the ï»¿ at the beginning of the file
  samp<- csv[1:16,1:2]
  samp<- t(samp)
  colnames(samp)<- samp[1,]
  samp<-t(samp[2,])
  
  samples<-rbind(samples,samp)
  
  mts<- gsub("T","T.",csv[18:nrow(csv),1])
  full<- samp
  full<- as.data.frame(full[rep(1,length(mts)),])
  for(m in 1:nrow(full)){
    full$siteName[m]<-mts[m]
  }
  
  ll20sampMT<-rbind(ll20sampMT,full)
}

#get column names/format
ll20sampAN<-read.csv("Fish/input/2020 Long Lake/angling/fishSamplesIS.csv", header = T, stringsAsFactors = F)

missing<- colnames(ll20sampAN)[!colnames(ll20sampAN) %in% colnames(samples)] #updateID, lakeID
extra<- colnames(samples)[!colnames(samples) %in% colnames(ll20sampMT)]

#add/remove columns#
ll20sampMT<- ll20sampMT %>% 
  add_column(siteID = paste(ll20sampMT$lakeID, ll20sampMT$siteName, sep="_")) %>% 
  add_column(sampleID = NA) %>%
  add_column(dayOfYear = NA) %>%
  add_column(dateSet = NA) %>%
  add_column(dateSample = NA) %>%
  add_column(entryFile = NA) %>% 
  rename(dateTimeSet = "dataTimeSet") %>%
  rename(useCPUE = "UseCPUE") %>% 
  add_column(updateID = "sampleFishInfo.20201124") %>%
  select(colnames(ll20sampAN))
ll20sampMT$timeID<-gsub(":","",word(as.character(ll20sampMT$dateTimeSample),2,2,sep=" "))
ll20sampMT<- ll20sampMT %>%
  mutate(timeID = case_when(nchar(ll20sampMT$timeID)==3~ paste("0",ll20sampMT$timeID,sep=""), TRUE ~ ll20sampMT$timeID)) %>%
  mutate(dateSet = as.Date(parse_date_time(ll20sampMT$dateTimeSet, orders = c("mdy HM","mdY HM"), tz = "America/New_York"))) %>%
  mutate(dateSample = as.Date(parse_date_time(ll20sampMT$dateTimeSample, orders = c("mdy HM","mdY HM"), tz = "America/New_York"))) %>%
  mutate(dateTimeSet = parse_date_time(ll20sampMT$dateTimeSet, orders = c("mdy HM","mdY HM"), tz = "America/New_York")) %>%
  mutate(dateTimeSample = parse_date_time(ll20sampMT$dateTimeSample, orders = c("mdy HM","mdY HM"), tz = "America/New_York")) %>%
  add_column(dateID = as.character(gsub("-","",as.Date(parse_date_time(ll20sampMT$dateTimeSample, orders = c("mdy HM","mdY HM"), tz = "America/New_York")))))
ll20sampMT<- ll20sampMT %>%
  mutate(sampleID = paste(ll20sampMT$siteID, ll20sampMT$dateID, ll20sampMT$timeID, ll20sampMT$gear, metadataID,sep="_")) %>%
  mutate(dayOfYear = strftime(ll20sampMT$dateSample, format = "%j"))


#table fixes#
unique(ll20sampMT$metadataID) %in% dbTable("METADATA")$metadataID #multiple metadataID spellings
unique(word(ll20sampMT$sampleID,6,6,sep="_")) #multiple metadataID's in sampleID

ll20sampMT<- ll20sampMT %>% select(colnames(ll20sampAN))

write.csv(ll20sampMT,"Fish/input/2020 Long Lake/minnowTraps/fishSamplesIS.csv",row.names = F)





#might be worth checking her fishInfo.csv to make sure it got everything
info<- data.frame()

i=1
for (i in 1:length(files)){
  fish<- read.csv(file.path(dir,files[i]),header = F, stringsAsFactors = F,fileEncoding="UTF-8-BOM")
  fish<- fish[17:nrow(fish),]
  colnames(fish)<-fish[1,]
  fish<-fish[-1,]
  fish$file<-i
  
  info<-rbind(info,fish)
}

info<- info[!info$species == "NFC",] #good, matches fishInfoIS.csv given



