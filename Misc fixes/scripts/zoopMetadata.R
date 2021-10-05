#zoop metadata confusion
#"ZoopCounts.20110507" doesn't have metadata but has samples

zsub<-dbTable("ZOOPS_SUBSAMPLE")
zlen<-dbTable("ZOOPS_LENGTHS")
zabun<-dbTable("ZOOPS_ABUND_BIOMASS")

findID("ZoopCounts.20110601")
findID("ZoopCounts.20110517") #this string only exists in METADATA and ZOOPS_LENGTHS

#Is it possible that the date was just incorrectly copy/pasted from ZoopSurv.Sample.20110517?
findID("ZoopSurv.Sample.20110517")

#Why is "ZoopCounts.20110601" in the sampleIDs?
zlen2<-subset(zlen, grepl("ZoopCounts.20110517", zlen$metadataID))

#How often does the metadataID in the sampleID match the metadataID column?
zlen2<-zlen %>% 
  add_column(match = case_when(word(zlen$sampleID,7,7,sep="_")==zlen$metadataID ~ "yes", TRUE ~ "no"))

zlen2 %>% 
  group_by(metadataID) %>% 
  count(match)
View(zlen2$match)

#zsub
zsub2<-zsub %>% 
  add_column(match = case_when(word(zsub$sampleID,7,7,sep="_")==zsub$metadataID ~ "yes", TRUE ~ "no"))

zsub2 %>% 
  group_by(metadataID) %>% 
  count(match)

#zabun
zabun2<-zabun %>% 
  add_column(match = case_when(word(zabun$sampleID,7,7,sep="_")==zabun$metadataID ~ "yes", TRUE ~ "no"))

zabun2 %>% 
  group_by(metadataID) %>% 
  count(match)


#how many zabun have metadataID "20110517"
library(lubridate)
zlen %>% 
  add_column(year = year(zlen$dateSample)) %>% 
  group_by(metadataID) %>% 
  count(year)
