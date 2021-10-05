#FISH_INFO$sampleID's not in FISH_SAMPLES
#*many FISH_SAMPLES$sampleID's not in FISH_INFO when NFC, that's ok

fsamp<-dbTable("FISH_SAMPLES")
finfo<-dbTable("FISH_INFO")

#does FISH_SAMPLES always include NFC samples, or is that just new? What happened in 2018?
fsamp2018<-subset(fsamp,fsamp$dateSample<"2019-01-01"&fsamp$dateSample>"2018-01-01")
finfo2018<-subset(finfo,grepl("_2018",finfo$sampleID))
miss18<-fsamp2018$sampleID[!fsamp2018$sampleID %in% finfo2018$sampleID] #452 fsamp not in finfo, 

length(unique(fsamp2018$sampleID)) #780
length(unique(finfo2018$sampleID)) #354
length(unique(fsamp2018$sampleID))-length(unique(finfo2018$sampleID)) #=426
s18<-unique(fsamp2018$sampleID)
i18<-unique(finfo2018$sampleID)
i18[!i18 %in% s18]
#many have sampleID issue where WholeShoreline2018MMDD instead of WholeShoreline_2018MMDD
fsamp2018error<-subset(s18,grepl("WholeShoreline20",fsamp2018$sampleID))
correct<-gsub("WholeShoreline","WholeShoreline_",fsamp2018error)
missing<-i18[!i18 %in% s18]
left<- missing[!missing %in% correct] #error in fsamp2018 metadataID spelling for blgMorphologyDOC.20180626
#moral of the story: there are samples not in info, which means that NFC's were included in SAMPLES 

#what about the whole tables?
fsampCorr<- fsampCorr %>% 
  mutate(sampleID = gsub("WholeShoreline2","WholeShoreline_2",fsamp$sampleID, ignore.case = T))

notInSamples<- data.frame(sampleID = fsampCorr$sampleID[!fsampCorr$sampleID %in% finfo$sampleID]) #8852 sampleID's in finfo aren't in fsamp
notInSamples$year<- substr(word(notInSamples$sampleID,3,3,sep="_"),1,4)
notInSamples %>% count(year)
notInSamples$metadataID<- word(notInSamples$sampleID, 6,6, sep="_")
notInSamples %>% count(metadataID) #majority (4411) from YOYSurv.20162017
