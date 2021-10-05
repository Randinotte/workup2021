sites<- dbTable("SITES")
ws<-subset(sites, sites$siteName %in% c("WholeShoreline","wholeShoreline")) %>% arrange(siteName, lakeID, updateID)

#wholeShoreline didn't exist until updateID siteFishInfo2019.20200529 which included updates to FISH_SAMPLES and FISH_INFO.
#since I'm going in and redoing them I can fix this mistake in the meantime. 

#FISH_SAMPLES: siteID and sampleID
#FISH_INFO: sampleID and fishID

#Let's do some checks: 
#1) does wholeShoreline appear in any non-2019 samples in FISH_SAMPLES or FISH_INFO (would have been by accident and not caught by writingUpdatingDBtables.R)?
fsamp<-dbTable("FISH_SAMPLES")
lower<-subset(fsamp,grepl("wholeShoreline",fsamp$siteID, ignore.case = F))
nrow(lower[lower$dateSample<"2019-01-01",]) #no non-2019 samples in fsamp

finfo<-dbTable("FISH_INFO")
finfo$siteID<-word(finfo$sampleID,2,2,sep="_")
lower<-subset(finfo,grepl("wholeShoreline",finfo$siteID, ignore.case = F))
nrow(lower[lower$dateinfole<"2019-01-01",]) #no non-2019 infoles in finfo


#2) does wholeShoreline appear in any other tables (esp. CREEL)?
tables<-dbTableList()
check<-data.frame()
none<-data.frame()
i=8
for (i in 1:length(tables)){
  if("sampleID" %in% colnames(dbTable(tables[i]))){
    table<-dbTable(tables[i])
    siteNames<- unique(word(table$sampleID,2,2,sep="_"))
    if("wholeShoreline" %in% siteNames){
      check<-rbind(check,tables[i])
    }
  }
}

table<-dbTable("CREEL_TRAILER_SAMPLES")
unique(word(table$sampleID,2,2,sep="_"))

#doesn't exist anywhere else
