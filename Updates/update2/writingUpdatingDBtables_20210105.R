#update2 for workup2020
#4.2.1- quick fix for some stray NA's that were entered as updateIDs by RN and KG
  #Going to fix by just forcing them in, will make change in my script that caused them

#Sources: 
  #A) 4.2.0 writingUpdatingDBtables_20201207.R: Stray case_when() in FISH_SAMPLES; caused all non-new updateIDs to "NA"
      #Will need to pull in previous update's version of FISH_SAMPLES to get correct updateIDs, plus change "fish.fix.20201211" ---> fish.fix.20210105 (needs added to UPDATE_METADATA)
  #B) UNITS: 
      #) "KG forgot to add an updateID for units updates in script meta_4.1.0 for db version 4.1.0." ---> units_20201125 (needs added to UPDATE_METADATA)
      #) "RN forgot to add an updateID for units with CREEL update for db version 3.5.1" ---> units_20201012 (original updateID in UPDATE_METADATA already)

#Need to update these tables: FISH_SAMPLES (x), UNITS (x), UPDATE_METADATA (x), and VERSION_HISTORY (x)

rm(list=ls())  #clear variables
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201215.db"

library(tidyverse)
library(lubridate)
library(arsenal)
dbTableList()
last(dbTable("VERSION_HISTORY")$dbTitle)
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")

####A) FISH_SAMPLES updateIDs ####
#get previous version of table for old updateIDs
fsamp<-dbTable("FISH_SAMPLES")
fsamp_old<- read.table("C:/Users/notter/Box/MFE/Database/Archived DB Files/MFEdb_20201125/FISH_SAMPLES.txt", header = F, sep = '|', col.names = colnames(fsamp), stringsAsFactors = F)
fsamp %>% group_by(updateID) %>% count() #12226 old updateID's need to be added + 7 "fish.fix.20201211 = 12233 (length of sample after fixes below, yay!)

  #Since there were so many fixes made to the previous version of the table before working with it, merging in the old updateIDs
  #by sampleID or anything would be really hard. I think I'm just going to recreate the table update, so we'll see how that goes. 

  #lines 126-263 from writingUpdatingDBtables_20201207.R from update1 4.2.0
###---###---### SAMPLE TABLE UPDATE ###---###---###
sample<- fsamp_old #reads in most recent text file of SAMPLES table
#remove erroneous fish samples from the updateID (for update1)
#Have to separate sections that manipulate sampleID or they override each other!
sample<- sample %>% 
  #fix sampleID errors:
  mutate(updateID = case_when(grepl("line2",sample$sampleID)~ "fish.fix.20210105", TRUE ~ sample$updateID)) %>% 
  mutate(sampleID = gsub("WholeShoreline2","WholeShoreline_2",sample$sampleID)) 

sample<- sample %>% 
  #fix "21070818" date:
  mutate(updateID = case_when(grepl("21070818", sample$sampleID)~ "fish.fix.20210105", TRUE ~ sample$updateID)) %>% 
  mutate(sampleID = gsub("21070818", "20170818",sample$sampleID)) %>% 
  mutate(dateSample = gsub("2107-08-18", "2017-08-18", sample$dateSample))

error2019<-subset(sample, sample$dateSample>"2019-01-01")

sample<- sample %>% 
  mutate(updateID = case_when(sample$metadataID == "hyperstability.20180604" ~ "fish.fix.20210105", TRUE ~ sample$updateID)) %>% #this was the stray case_when() that caused the NA's that needed to be fixed in 4.2.1 
  mutate(sampleID = gsub("hyperstability.20180604","hyperstability.20180521", sample$sampleID)) %>% 
  mutate(metadataID = gsub("hyperstability.20180604","hyperstability.20180521", sample$metadataID))

sample<- sample %>%   
  #fix siteID spelling errors:
  mutate(updateID = case_when(sample$siteID %in% c("SR _FNm1", "SR _FN7") ~ "fish.fix.20210105", TRUE ~ sample$updateID)) %>% 
  mutate(siteID = case_when(sample$siteID == "SR _FNm1"~ "SR_FNm1", 
                            sample$siteID == "SR _FN7" ~ "SR_FN7",
                            TRUE ~ sample$siteID)) 

sample<- sample %>% 
  #fix blgMorphologyDOC.201806(27-30) metadataID drag down error:
  mutate(updateID = case_when(sample$metadataID %in% c("blgMorphologyDOC.20180627", "blgMorphologyDOC.20180628",
                                                       "blgMorphologyDOC.20180629", "blgMorphologyDOC.20180630")
                              ~ "fish.fix.20210105", TRUE ~ sample$updateID)) %>% 
  mutate(sampleID = case_when(sample$metadataID %in% c("blgMorphologyDOC.20180627", "blgMorphologyDOC.20180628",
                                                       "blgMorphologyDOC.20180629", "blgMorphologyDOC.20180630")
                              ~ paste(word(sample$sampleID,1,5,sep="_"),"blgMorphologyDOC.20180626",sep="_"), TRUE ~ sample$sampleID)) %>% 
  mutate(metadataID = case_when(sample$metadataID %in% c("blgMorphologyDOC.20180627", "blgMorphologyDOC.20180628",
                                                         "blgMorphologyDOC.20180629", "blgMorphologyDOC.20180630")
                                ~ "blgMorphologyDOC.20180626", TRUE ~ sample$metadataID)) %>% 
  
  #filter out bad samples from bad update
  filter(dateSample < "2019-01-01")

sample<- sample %>% 
  #remove duplicate rows for 	SQ_FN8_20180613_1520_FN_blgMorphologyDOC.20180626
  distinct()


#generate csv for rawCSV file storage
outDir<- "Updates/update2"
write.csv(sample, file.path(outDir,"samples.fish.fix.20210105.csv"),row.names = F)


sample_N <- read.csv("Fish/output/fishSAMPLES.20201204.csv", header=TRUE,stringsAsFactors = F)
#sample_N$updateID<- "sampleFishInfo.20201124"

#match date/time formats before binding:
#Note: set sample_N to have the same timezone as SAMPLES or your POSIXct times won't match the sampleID
#sample:
sample$dateSample<-as.Date(sample$dateSample)
sample$dateSet<-as.Date(sample$dateSet)
sample$dateTimeSet<-as.POSIXct(sample$dateTimeSet, tz = "America/Chicago")
sample$dateTimeSample<-as.POSIXct(sample$dateTimeSample, tz = "America/Chicago")
sample$dayOfYear<-as.character(sample$dayOfYear)
sampleTZ<-tz(sample$dateTimeSample)

#sample_N:
sample_N$dateSet<-as.Date(sample_N$dateSet)
sample_N$dateSample<-as.Date(sample_N$dateSample)
sample_N$dateTimeSample<-as.POSIXct(sample_N$dateTimeSample, tz = sampleTZ)
sample_N$dateTimeSet<-as.POSIXct(sample_N$dateTimeSet, tz = sampleTZ)

#check what's already in SAMPLES
sample_names <- unique(sample$sampleID) # unique sample names from existing data
sample_N_unique <- sample_N[!sample_N$sampleID %in% sample_names,] # ADD; New sampleIDs that need to be added to sampleS table
sample_N_nonUnique <- sample_N[sample_N$sampleID %in% sample_names,] # DON'T ADD; sampleIDs that are already in the sampleS table (do a quick calculation on the dims of each table to make sure things lineup )


#normal way below:
sample_current<- rbind(sample,unique(sample_N_unique)) #combine data sets. If header names are different (typo/wrong table) this will not work
#!!! Check that times in POSIXct columns match sampleID
str(sample_current)
sample_current %>% add_column(year = year(sample_current$dateSample)) %>% count(year)
fix<-sample_current[duplicated(sample_current$sampleID),] # Last duplicate check (no rows=no duplicates)


summary(comparedf(fsamp, sample_current)) #compares all differences between last version's table and new table 
recreateSampleIDs(sample_current, fish = TRUE)

# additional check to make sure siteID is in the new sites table. JAZ
sample_sites<-unique(sample_current$siteID) #all unique sites listed in samples table 
site_current<-dbTable("SITES")
if(length(sample_sites[!sample_sites%in%site_current$siteID])>0){ # checks if there are site names from the sample table not in the site names 
  print('Sites listed in sample table not in site table:') # if there are sites that need to be added, add those first. If there are mispellings in the samples table, fix them before writing new table 
  sample_sites[!sample_sites%in%site_current$siteID]
}else{
  print('All good') # proceed with writing new samples table 
}


checkDuplicates(sample_current, "sampleID")


outDir<-'Updates/update2' #Will be unique for each user 
write.table(sample_current,file.path(outDir,paste('FISH_SAMPLES','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F) # should be the same format that Access creates 
write.csv(sample_current,file.path(outDir,'FISH_SAMPLES.20210105.csv'), row.names = F) #for rawCSV file storage


#Update Metadata Table
#updates <- dbTable('update_metadata')
updates <- read.csv("Updates/update2/update_metadata_fix.csv", header = TRUE, stringsAsFactors = F)
colnames(updates) <- names(dbTable("update_metadata"))
update_names<-unique(sample_current$updateID)  
if(length(update_names[!update_names%in%updates$updateID])>0){ 
  print('updateIDs listed in samples table not in update_metadata table:')
  update_names[!update_names%in%updates$updateID]
}else{
  print('All good') # proceed with writing new samples table 
}



#check metadataIDs are in metadata table
metadata<- dbTable("metadata")
sample_metadata<-unique(sample_current$metadataID) #all unique metadataIDs listed in current table 
if(length(sample_metadata[!sample_metadata%in%metadata$metadataID])>0){
  print('metadataID listed in "sample" table not in METADATA table:') 
  sample_metadata[!sample_metadata%in%metadata$metadataID]
}else{
  print('All good') # proceed with writing new tab table 
}

####B) UNITS updateIDs ####
u<-dbTable("UNITS")
u$pair<-paste(u$tableName,u$colName,sep="/")

rn<-c("CREEL_BOAT_SAMPLES","CREEL_BOATS","CREEL_TRAILER_SAMPLES","CREEL_TRAILERS","FISH_SAMPLES","FLIGHTS","FLIGHTS_SAMPLES","FLIGHTS_INFO")   
kg<-c("SAMPLES","BENTHIC_INVERT_SAMPLES","SED_TRAP_SAMPLES","WATER_CHEM","FISH_MORPHOMETRICS")

u_fix<- u %>% 
  mutate(updateID = case_when(updateID == "NA" & u$tableName %in% rn ~ "units_20201012",
                              updateID == "NA" & u$tableName %in% kg ~ "units_20201125",
                              TRUE ~ u$updateID)) %>% 
  select(colnames(dbTable("UNITS")))
unique(u_fix$updateID)


summary(comparedf(u,u_fix))

outDir<-'Updates/update2' #Will be unique for each user 
write.table(u_fix,file.path(outDir,paste('UNITS','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F) # should be the same format that Access creates 
write.csv(u_fix,file.path(outDir,'units.20210105.csv'), row.names = F) #for rawCSV file storage


#### Other tables ####
#UPDATE_METADATA
updates_N<-read.csv("Updates/update2/update_metadata_fix.csv", header = T, stringsAsFactors = F)
write.table(updates_N, file.path(outDir, paste('UPDATE_METADATA','.txt',sep="")),quote = F,sep= '|',row.names=F,col.names=F)
write.csv(updates_N, "Updates/update2/update_metadata.20210105.csv", row.names = F)

#VERSION_HISTORY
v<-dbTable("VERSION_HISTORY")
v[20,"status"]<- "archived"
v_N<-data.frame(dbDateCreate = "1/5/2021 15:00",
                verNumber = "4.2.1",
                updater = "RN",
                dbTitle = "(Hydro)xychloroquine",
                status = "current",
                updateInfo = "Quick fix to updateIDs that were left as \"NA\" in previous updates to FISH_SAMPLES (4.2.0) and UNITS(3.5.1 and 4.1.0)")
v_N<-rbind(v,v_N)

write.table(v_N, file.path(outDir, paste('VERSION_HISTORY','.txt',sep="")),quote = F,sep= '|',row.names=F,col.names=F)
write.csv(v_N, "Updates/update2/version_history.20210105.csv", row.names = F)


###*********************************************************************************###
###                      HOW TO WRITE NEW DB AFTER UPDATES                          ###
###*********************************************************************************###
# To write a new .db file that contains updated data (new and/or updated tables), 
# the next step involves SQLite. If any changes made to table headers, make sure the SQL script (text file) 
# code is up-to-date. This is especially true if you created a brand new data table. Run the 
# updated .sql script through Terminal (Mac) or Command Line (Windows), and voila! New DB object.
# Can then bring new DB back into R to query from
###*********************************************************************************###
###*********************************************************************************###

db="MFEdb_20210105.db" #make sure this script uses new database file
dbTableList() # Make sure that dbname is changed in this function (and in dbTable)
View(dbTable("VERSION_HISTORY"))
NEW_met<-dbTable("UPDATE_METADATA") #Make sure that dims match in new tables and those noted in the updates
NEW_sample <- dbTable("FISH_SAMPLES")
NEW_units<-dbTable("UNITS")
recreateSampleIDs(NEW_sample, fish = TRUE)
checkDuplicates(NEW_sample, "sampleID")
checkMet(NEW_sample)
