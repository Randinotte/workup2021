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
db="MFEdb_20210402.db"

library(tidyverse)
library(lubridate)
library(arsenal)
dbTableList()
ver<-(dbTable("VERSION_HISTORY"))
last(dbTable("VERSION_HISTORY")$dbTitle)
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")


###---###---### SAMPLE TABLE UPDATE ###---###---###
sample_current<- read.csv("Updates/update3/LIMNO_PROFILES_20210414.csv", header = T, stringsAsFactors = F)



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


# outDir<-'Updates/update3' #Will be unique for each user 
# write.table(sample_current,file.path(outDir,paste('FISH_SAMPLES','.txt',sep='')),quote = F,sep = '|',row.names=F,
#             col.names=F) # should be the same format that Access creates 
# write.csv(sample_current,file.path(outDir,'FISH_SAMPLES.20210105.csv'), row.names = F) #for rawCSV file storage


#Update Metadata Table
#updates <- dbTable('update_metadata')
#updates <- read.csv("Updates/update3/update_metadata_fix.csv", header = TRUE, stringsAsFactors = F)
#colnames(updates) <- names(dbTable("update_metadata"))
updates<- dbTable("UPDATE_METADATA")
update_names<-unique(sample_current$updateID)  
if(length(update_names[!update_names%in%updates$updateID])>0){ 
  print('updateIDs listed in samples table not in update_metadata table:')
  update_names[!update_names%in%updates$updateID]
}else{
  print('All good') # proceed with writing new samples table 
}

upNew<- updates %>% 
  mutate(updateDate = gsub("2021-04-02", "4/02/2021", updateDate)) %>% 
  add_row(updateID = "tableLimno_Profiles.20210414", 
          updateDate = "4/14/2021", 
          verNumber = "4.5.3",
          rawFile = "LIMNO_PROFILES_20210414.csv", 
          parentBoxFolder = "summer2020/update3", 
          sourceCode = "writingUpdatingDBtables_20210414.R", 
          updaterInitials = "RN", 
          updateMetadata = "Added 2019 (2626 rows) and 2020 (1401 rows) to LIMNO_PROFILES for a total of 24255 rows.")

write.table(upNew, file.path("Updates/update3", 'UPDATE_METADATA.txt'),quote = F,sep= '|',row.names=F,col.names=F)
write.csv(upNew, "Updates/update3/update_metadata.20210414.csv", row.names = F)

#check metadataIDs are in metadata table
metadata<- dbTable("metadata")
sample_metadata<-unique(sample_current$metadataID) #all unique metadataIDs listed in current table 
if(length(sample_metadata[!sample_metadata%in%metadata$metadataID])>0){
  print('metadataID listed in "sample" table not in METADATA table:') 
  sample_metadata[!sample_metadata%in%metadata$metadataID]
}else{
  print('All good') # proceed with writing new tab table 
}



#VERSION_HISTORY
v<-dbTable("VERSION_HISTORY")
v[nrow(v),"status"]<- "archived"
v_N<-data.frame(dbDateCreate = "4/14/2021 17:00",
                verNumber = "4.5.3",
                updater = "RN",
                dbTitle = "Pfizometer",
                status = "current",
                updateInfo = "Small update to LIMNO_PROFILES to add 2019 and 2020 data. No new sites or metadata added.")
v_N<-rbind(v,v_N)

write.table(v_N, file.path("Updates/update3", 'VERSION_HISTORY.txt'),quote = F,sep= '|',row.names=F,col.names=F)
write.csv(v_N, "Updates/update3/version_history.20210414.csv", row.names = F)


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

db="MFEdb_20210414.db" #make sure this script uses new database file
dbTableList() # Make sure that dbname is changed in this function (and in dbTable)
View(dbTable("VERSION_HISTORY"))
NEW_met<-dbTable("UPDATE_METADATA") #Make sure that dims match in new tables and those noted in the updates
NEW_prof <- dbTable("LIMNO_PROFILES")
NEW_units<-dbTable("UNITS")
recreateSampleIDs(NEW_sample, fish = TRUE)
checkDuplicates(NEW_prof, "sampleID")
checkMet(NEW_prof)
