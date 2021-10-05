###*** Relational database integrity test - Sites/Samples and extra tables ***###
# January 2016 - Alex Ross
# Edits made June 2017- Katie Saunders
#Copy used for 20210505 update by RN



rm(list=ls())  #clear variables
#setwd('C:/Users/notter/OneDrive/Randi/Database/currentDB') #set wd to where .db file is
source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
dbdir="C:/Users/notter/Box/MFE/Database/currentDB/"
db="MFEdb_20210423.db"
last(dbTable("VERSION_HISTORY")$dbTitle)
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")


library(tidyverse)
library(lubridate)

###***Important to go in order: Lake, Site, Sample, other tables.

#### SITE TABLE UPDATE #####
# Step 1: Load existing 'SITES' table, and read in the 'SITES' portion of new data that's being pulled in (use Excel Template)
##** Choose which "site" you want. Either from the db if it's the first analysis in this batch, OR from your most recent SITES_YYYYMMDD.txt if you're updating multiple tables/analyses in this db update.

site <-dbTable('SITES') # existing site data
#site<- read.table(file.path(dbdir,"SITES.txt"), header = F, sep = '|', col.names = c("siteID","lakeID","siteName","lat","long","UTM","updateID"), stringsAsFactors = F)
#site<- read.csv("Misc fixes/output/SITES.20201208.csv", header = T, stringsAsFactors = F)
site_N <- read.csv("Updates/update4/siteNutrients_20210505.csv", header=TRUE) # new data that is to be added to existing
checkCols(site,site_N)  
site_N<- site_N %>% 
    #mutate(updateID = case_when(is.na(site_N$updateID)~"siteFishInfo.20201124", TRUE ~ site_N$updateID))
    add_column(UTM = NA) %>% 
    select(colnames(site))

    
# Step 2: Identify unique siteIDs that are not in current database
site_names <- unique(site$siteID) # unique site names from existing data
  #make sure site_N has a siteID:
  #site_N$siteID<-paste(site_N$lakeID,site_N$siteName,sep="_") 
site_N_unique <- site_N[!site_N$siteID %in% site_names,] # ADD; New siteIDs that need to be added to SITES table
site_N_nonUnique <- site_N[site_N$siteID %in% site_names,] # DON'T ADD; siteIDs that are already in the SITES table (do a quick calculation on the dims of each table to make sure things lineup)


# Step 3: Copy new siteIDs to existing siteIDs and check one last time for duplicates
site_current<- rbind(site,unique(site_N_unique)) #combine data sets. If header names are different (typo/wrong table) this will not work
#Check dims again to make sure proper number of rows are in new table (original siteIDs + new unique IDs)
site_current[duplicated(site_current$siteID),] # Last duplicate check (no rows=no duplicates)
nrow(site_current)-nrow(site) #number of new sites added to "SITES" (for dim count)
added<-subset(site_current[!site_current$siteID %in% site$siteID,])


# Step 4: Additional check to make sure lakeID is in the lakes Table: copied from JAZ
lakes <- dbTable('lakes')
# newLake<-data.frame(lakeID = "JO", lakeName = "Jones Lake", state = "WI", county = "Vilas", city = "Boulder Junction", surfaceArea = 21.8, 
#                     maxDepth = 29, lat = 46.18195, long = -89.51711, latLongSource = "Lake finder app", WBIC = "2765100", 
#                     comments = "Private lake associated with Rainbo Lodge off County HWY B", updateID = "lakeIDs.2020.20201209")
# lakes<- lakes %>% 
#   bind_rows(newLake) %>% 
#   arrange(lakeID)
#lakes_n<- read.table("LAKES.txt",header = F,sep = '|',col.names = c("lakeID","lakeName","state","county","city","surfaceArea","maxDepth","lat","long","UTM","comments","updateID"))
lake_names<-unique(site_current$lakeID) #all unique lakeIDs are listed in sites table 
if(length(lake_names[!lake_names%in%lakes$lakeID])>0){ # checks if there are lakeIDs from the sites table not in the lakes table 
  print('Lakes listed in site table not in lakes table:') # if there are sites that need to be added or typos that need to be fixed do that first before writing new table
  lake_names[!lake_names%in%lakes$lakeID]
}else{
  print('All good') # proceed with writing new samples table 
}

#Lakes Table
outDir<-'Updates/update1'
write.table(lakes,file.path(outDir,paste('LAKES','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F) # should be the same format that Access creates
write.csv(lakes,file.path(outDir,'LAKES.20201214.csv'), row.names = F) #for rawCSV file storage
  
# Step 5: Additional check to make sure updateID is in the UPDATE_METADATA Table
# See Step 6 to add new metadata to this table
updates <- dbTable('update_metadata')
update_names<-unique(site_current$updateID)  
if(length(update_names[!update_names%in%updates$updateID])>0){ 
  print('updateID listed in site table not in update_metadata table:')
  update_names[!update_names%in%updates$updateID]
}else{
  print('All good') # proceed with writing new samples table 
}

  # Step 5b: If mistakes are found write a csv of site_current; make fixes; bring back to R
  #write.csv(site_current, file="site_current_fix.csv", row.names=F)
  #site_current <- read.csv("site_current_fix.csv")

#Step 6: Update the "UPDATE_METADATA" csv in excel, bring it in to R to re-write your UPDATE_METADATA table
#write.csv(updates, "Updates/update1/update_metadata_fix.csv", row.names = F)
update_N <- read.csv("Updates/update1/update_metadata_fix.csv", header = TRUE)
updates<- update_N

# Step 7: Overwrite new SITES table to database folder (using the same name as before)
outDir<-'Updates/update4'
#Sites Table
write.table(site_current,file.path(outDir,paste('SITES','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F) # should be the same format that Access creates
write.csv(site_current,file.path(outDir,'SITES.20210505.csv'), row.names = F) #for rawCSV file storage
#Update Metadata Table
write.table(update_N,file.path(outDir,paste('UPDATE_METADATA','.txt',sep='')),quote = F,sep = '|',row.names=F,
col.names=F)

#### SAMPLE TABLE UPDATE ####
sample<- dbTable("SAMPLES") #reads in most recent text file of SAMPLES table
sample<- read.csv("Updates/update4/SAMPLES.20210505.csv",header = T, stringsAsFactors = F)  
#fix sample$dateTimesample
  sample<- sample %>% 
    add_column(times = paste(substr(word(sample$sampleID,4,4,sep="_"),1,2),substr(word(sample$sampleID,4,4,sep="_"),3,4),sep=":")) %>% 
    mutate(dateTimeSample = as.POSIXct(paste(dateSample, times), tz = "America/Chicago")) %>% 
    select(colnames(dbTable("SAMPLES")))
             
sample_N <- read.csv("Updates/update4/sampleNutrients_20210505.csv", header=TRUE,stringsAsFactors = F)

#match date/time formats before binding:
#Note: set sample_N to have the same timezone as SAMPLES or your POSIXct times won't match the sampleID
  #sample:
  sampleTZ<-tz(sample$dateTimeSample)
  sample$dateSample<- as.Date(sample$dateSample)
  
  #sample_N:
  sample_N$dateSample<- as.Date(sample_N$dateSample)
  sample_N$dateTimeSample<- as.POSIXct(sample_N$dateTimeSample, tz = sampleTZ)
  
#depthClass capitalization fix: new foreign key contstraints require specific spelling
  dbClass<-unique(sample$depthClass) 
  nClass<-unique(sample_N$depthClass)
  any(!nClass %in% dbClass)
  if(any(!nClass %in% dbClass)){
    sample_N<- sample_N %>% 
      mutate(depthClass = case_when(depthClass=="Hypo" ~ "hypo", 
                                    depthClass=="Surface" ~ "surface", 
                                    depthClass=="Point" ~ "point",
                                    TRUE~depthClass)) %>% 
      mutate(sampleID = case_when(grepl("Hypo",sampleID) ~ gsub("Hypo","hypo", sampleID), 
                                  grepl("Surface",sampleID) ~ gsub("Surface","surface", sampleID), 
                                  grepl("Point",sampleID) ~ gsub("Point","point", sampleID),
                                    TRUE~sampleID))
  }
  dbClass<-unique(sample$depthClass) 
  nClass<-unique(sample_N$depthClass)
  any(!nClass %in% dbClass)

#check what's already in SAMPLES
sample_names <- unique(sample$sampleID) # unique sample names from existing data
sample_N_unique <- sample_N[!sample_N$sampleID %in% sample_names,] # ADD; New sampleIDs that need to be added to sampleS table
sample_N_nonUnique <- sample_N[sample_N$sampleID %in% sample_names,] # DON'T ADD; sampleIDs that are already in the sampleS table (do a quick calculation on the dims of each table to make sure things lineup )


#normal way below:
checkCols(sample,sample_N_unique)
sample_current<- rbind(sample,unique(sample_N_unique)) #combine data sets. If header names are different (typo/wrong table) this will not work
  #!!! Check that times in POSIXct columns match sampleID
  str(sample_current)
  sample_current %>% add_column(year = year(sample_current$dateSample)) %>% count(year)
fix<-sample_current[duplicated(sample_current$sampleID),] # Last duplicate check (no rows=no duplicates)
  #This is where I'm stuck ####
    #1) some sampleID's are already in SAMPLES just with dateTimeSample and hour earlier and also in the sampleID
        #Ex. AQ_DeepHole_20190823
    #2) rows in "fix" are dupicated because sample_N_unique has multiple metadataIDs for the same sampleID
        #SAMPLES should only have one row per sampleID so I just need to pick one.

#Fix vector types (new problem for RNN: The SAMPLES.txt table reads in with factor and number formats and that's not what the SQL script is expecting)
#force variable structures to match
#str(sample_current)


# additional check to make sure siteID is in the new sites table. JAZ
sample_sites<-unique(sample_current$siteID) #all unique sites listed in samples table 
if(length(sample_sites[!sample_sites%in%site_current$siteID])>0){ # checks if there are site names from the sample table not in the site names 
  print('Sites listed in sample table not in site table:') # if there are sites that need to be added, add those first. If there are mispellings in the samples table, fix them before writing new table 
  sample_sites[!sample_sites%in%site_current$siteID]
}else{
  print('All good') # proceed with writing new samples table 
}
#If changes need to be made, write csv --> fix, bring back into R
#write.csv (sample_current, file= "sample_current_fix.csv", row.names=F) 
#sample_current <- read.csv("sample_current_fix.csv", head=T) #Run through checks again

checkDuplicates(sample_current, "sampleID")
recreateSampleIDs(sample_current,fish=FALSE)


outDir<-'Updates/update4' #Will be unique for each user 
write.table(sample_current,file.path(outDir,paste('SAMPLES','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F) # should be the same format that Access creates 
write.csv(sample_current,file.path(outDir,'SAMPLES.20210505.csv'), row.names = F) #for rawCSV file storage


#Update Metadata Table
updates <- dbTable('update_metadata')
updates <- read.csv("Updates/update1/update_metadata_fix.csv", header = TRUE, stringsAsFactors = F)
colnames(updates) <- names(dbTable("update_metadata"))
update_names<-unique(sample_current$updateID)  
if(length(update_names[!update_names%in%updates$updateID])>0){ 
  print('updateIDs listed in samples table not in update_metadata table:')
  update_names[!update_names%in%updates$updateID]
}else{
  print('All good') # proceed with writing new samples table 
}

outDir<-'Updates/update1' #Will be unique for each user
write.table(update_N,file.path(outDir,paste('UPDATE_METADATA','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F)


#check metadataIDs are in metadata table
metadata<- dbTable("metadata")
sample_metadata<-unique(sample_current$metadataID) #all unique metadataIDs listed in current table 
if(length(sample_metadata[!sample_metadata%in%metadata$metadataID])>0){
  print('metadataID listed in "sample" table not in METADATA table:') 
  sample_metadata[!sample_metadata%in%metadata$metadataID]
}else{
  print('All good') # proceed with writing new tab table 
}

#write.csv(metadata, "Updates/update1/metadata_fix.csv", row.names = F)
metadata_N <- read.csv("Updates/update1/metadata_fix.csv", header = TRUE)
metadata<- metadata_N



# ***If you need to replace sampleIDs from a whole table (including those IDs that currently in existing DB), use script 'replaceWholeDBtable.R'

#### TABLE UPDATES ####
tab<- dbTable('COLOR') #Choose table you're looking to update #73600
tab_N <- read.csv ('Updates/update4/tableNutrients_20210505.csv', header=TRUE, stringsAsFactors = F) # Pull in new table (make sure that csv is saved in working directory folder)

#depthClass capitalization fix: new foreign key contstraints require specific spelling
dbClass<-unique(sample$depthClass) 
nClass<-unique(tab_N$depthClass)
any(!nClass %in% dbClass)
if(any(!nClass %in% dbClass)){
  tab_N<- tab_N %>% 
    mutate(depthClass = case_when(depthClass=="Hypo" ~ "hypo", 
                                  depthClass=="Surface" ~ "surface", 
                                  depthClass=="Point" ~ "point",
                                  TRUE~depthClass)) %>% 
    mutate(sampleID = case_when(grepl("Hypo",sampleID) ~ gsub("Hypo","hypo", sampleID), 
                                grepl("Surface",sampleID) ~ gsub("Surface","surface", sampleID), 
                                grepl("Point",sampleID) ~ gsub("Point","point", sampleID),
                                TRUE~sampleID))
}
dbClass<-unique(sample$depthClass) 
nClass<-unique(tab_N$depthClass)
any(!nClass %in% dbClass)


checkCols(tab,tab_N)
str(tab)
str(tab_N)
tab_current <- rbind(tab, tab_N)
str(tab_current)

# ****QUICK CHECKS FOR TYPOS/ERRORS -- 
(depth_check<- tab_N[!(tab_N$depthClass %in% tab$depthClass), ]) #Check depthClass spelling; fix if need be
(siteName_check <- tab_current[!(tab_current$siteName %in% site_current$siteName), ]) #Check spelling; new siteNames may come up if a new site was just added
(depthOrder_check <- tab_current[(tab_current$depthTop > tab_current$depthBottom),])

#If errors update csv with proper information, bring back.****
#write.csv(tab_current, file="tab_current_fix.csv", row.names=F)
#tab_current <- read.csv("tab_current_fix.csv", header = TRUE)

# For DOC and Chla tables sampleIDs should be repeated twice (each sample, n=2 replicates) 
# Check if there are any sampleIDs occuring >2
#(check <- as.data.frame(tab_current %>% count(sampleID) %>% filter(n > 2))) #this is from dplyr
# Problem with above, not in dataframe format so don't know exact rows where info copied. Can go back
# into excel to 'Find' where samples are duplicated, remove them and then bring the csv back into R.
checkDuplicates(tab_current, "sampleID")



#If duplicate issues write csv, make necessary adjustments and then bring back in
#write.csv(tab, file="tab_current_fix.csv", row.names = F)
#Fix issues, bring back in and run ifelse function once more to make sure duplicates are taken care of
#tab_current <- read.csv("tab_current_fix.csv", header = TRUE)

#if you need to look at the merged duplicates in a csv, export here
#write.csv(duplicated,"merged_tab_current_duplicates.csv", row.names = FALSE)

#Check to make sure that other ID keys are in associated parent tables
 # SampleID check
tab_sample<-unique(tab_current$sampleID) #all unique sampleIDs listed in table
  if(length(tab_sample[!tab_sample%in%sample_current$sampleID])>0){ # checks if tab table sampleIDs are in SAMPLES table
    print('SampleID listed in "tab" table not in SAMPLES table:') # if there are sites that need to be added, add those first. If there are mispellings in the tab table, fix them before writing new table
    tab_sample[!tab_sample%in%sample_current$sampleID]
  }else{
    print('All good') # proceed with writing new tab table
  }

checkINFO(tab_current,sample_current)

#lakeID
tab_lake<-unique(tab_current$lakeID) #all unique lakeIDs listed in  table 
if(length(tab_lake[!tab_lake%in%lakes$lakeID])>0){ 
  print('lakeID listed in "tab" table not in LAKES table:') 
  tab_lake[!tab_lake%in%lakes$lakeID]
}else{
  print('All good') # proceed with writing new tab table 
}

#projectID
projects<- dbTable("projects")
tab_project<-unique(tab_current$projectID) #all unique project IDs in table 
if(length(tab_project[!tab_project%in%projects$projectID])>0){
  print('projectID listed in "tab" table not in PROJECTS table:') 
  tab_project[!tab_project%in%projects$projectID]
}else{
  print('All good') # proceed with writing new tab table 
}

#updateID
tab_update<-unique(tab_current$updateID) #all unique updateIDs listed in current table 
if(length(tab_update[!tab_update%in%updates$updateID])>0){
  print('updateID listed in "tab" table not in UPDATE_METADATA table:') 
  tab_update[!tab_update%in%updates$updateID]
}else{
  print('All good') # proceed with writing new tab table 
}

update_N <- read.csv("Updates/update1/update_metadata_fix.csv", header = TRUE)
updates<- update_N

#metadataID
metadata<- dbTable("metadata")
tab_metadata<-unique(tab_current$metadataID) #all unique metadataIDs listed in current table 
if(length(tab_metadata[!tab_metadata%in%metadata$metadataID])>0){
  print('metadataID listed in "tab" table not in METADATA table:') 
  tab_metadata[!tab_metadata%in%metadata$metadataID]
}else{
  print('All good') # proceed with writing new tab table 
}

metadata_N <- read.csv("Updates/update1/metadata_fix.csv", header = TRUE)
metadata<- metadata_N

#any(tab_current$species %in% finfo$species==FALSE) #all species in FISH_INFO


######## !!!! Remember to change the name of the table below or you'll overwrite the last tableXYZ.txt that you made !!!! ###
#!
#!  Hold up, don't overwrite. SAVE YOURSELF THE HEARTACHE
#!
outDir<-'Updates/update4' #Will be unique for each user
write.table(tab_current,file.path(outDir,paste('COLOR','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F)
write.csv(tab_current,file.path(outDir,'COLOR.20210505.csv'), row.names = F) #for rawCSV file storage




#Misc. table updates necessary: 
metadata<-dbTable("METADATA")
metadata_N<- read.csv("Updates/update1/metadata_fix.csv", header = T, stringsAsFactors = F)
metadata_N$metadataID[!metadata_N$metadataID %in% metadata$metadataID]
metadata$metadataID[!metadata$metadataID %in% metadata_N$metadataID]
write.table(metadata_N,file.path(outDir,'METADATA.txt'),quote = F,sep = '|',row.names=F,col.names=F)
write.csv(metadata_N, file.path(outDir,'METADATA.20201214.csv'), row.names = F)
  
  
update_N <- read.csv("Updates/update1/update_metadata_fix.csv", header = TRUE)
outDir<-'Updates/update1' #Will be unique for each user
write.table(update_N,file.path(outDir,'UPDATE_METADATA.txt'),quote = F,sep = '|',row.names=F,col.names=F)


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

###****** ASIDE ******###
###---###---### OTU RELATION TABLE UPDATE ###---###---###
# Any table that uses an otu (i.e., zoop abund, zoop length, benthic inverts, etc.)
# Check to make sure spelling is right; taxa is in OTU table
otu <- dbTable('OTU')
write.csv(otu, "otu_fix_20170727.csv", row.names = FALSE)
otu <- read.csv("otu_fix_20170727.csv", head=T)

otu_check <- tab_current # Table with OTU; otu code in this table(zoop lengths) is "taxa"

(name_check<- otu_check[!(otu_check$taxa %in% otu$otu), ]) 

outDir<-'/Users/ksaunde1/Documents/Regular Database/Current Database' #Will be unique for each user
write.table(otu,file.path(outDir,paste('OTU','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F)

#If incoming taxa data are not found in OTU a df will be populated with incompatable results
# This also includes mystery "spaces" at the end or beginning of taxa names (i.e., 
# "cyclopoid "; the third observation in populated dataset)
# If no errors, should be a df with zero observations

###****** ASIDE OVER ******###

#*******************************************************************************************************#
### fix errors in a table
fix<- dbTable("metadata")
write.csv(fix, file="metadata_current_fix.csv", row.names = FALSE) # open csv, make updates/changes, bring back to R to write txt file

fix_current <- read.csv("metadata_current_fix.csv")
write.table(fix_current,file.path(outDir,paste('METADATA','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F)

#*******************************************************************************************************#
### How to write a new table to be used in existing database

# In Excel, set up a spreadsheet w/ proper header, IDs, data, etc., bring this into R
units_current <- read.csv("units_fix_20170726.csv", header = TRUE) #writing the 'UPDATE_METADATA' table
# Now, write a text file into the same folder where other text files for the DB are located
outDir<-'/Users/ksaunde1/Documents/Regular Database/Current Database'
write.table(gc,file.path(outDir,paste('UNITS','.txt',sep='')),quote = F,sep = '|',row.names=F,
            col.names=F)



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

#2020- need to copy/paste the folder contents from Updates/update1 into currentDB, "replacing" current versions so that SQL pulls from same folder for everything

### With new database written load it back in and make sure that additions have been properly made
#setwd('C:/Users/notter/Google Drive/Randi/Database/currentDB') #set wd to where .db file is
source('C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R') # Load 'dbTable' and 'dbTableList' functions
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
dbdir="C:/Users/notter/Box/MFE/Database/currentDB"
db="MFEdb_20201215.db" #make sure this script uses new database file
dbTableList() # Make sure that dbname is changed in this function (and in dbTable)
View(dbTable("VERSION_HISTORY"))
NEW_met<-dbTable("UPDATE_METADATA") #Make sure that dims match in new tables and those noted in the updates
NEW_site <- dbTable("SITES") 
NEW_sample <- dbTable("FISH_SAMPLES")
recreateSampleIDs(NEW_sample, fish = TRUE)
checkDuplicates(NEW_sample, "sampleID")
checkMet(NEW_sample)


NEW_tab <- dbTable("FISH_INFO")
checkDuplicates(NEW_tab, "fishID")


chl<-dbTable("CHLOROPHYLL")
col<-dbTable("COLOR")
wc<-dbTable("WATER_CHEM")
update<-dbTable("UPDATE_METADATA")
met<-dbTable("METADATA")




#Upload the data to Box, then save the folder to the desktop and try here:
setwd('C:/Users/notter/OneDrive/Randi/Database/currentDB/') #set wd to where .db file is
source('C:/Users/notter/OneDrive/Randi/Database/R/dbUtil.R') # Load 'dbTable' and 'dbTableList' functions
dbdir="C:/Users/notter/Desktop" 
db="MFEdb_20200530.db" #make sure this script uses new database file
dbTableList() # Make sure that dbname is changed in this function (and in dbTable)
NEW_met<-dbTable("UPDATE_METADATA") #Make sure that dims match in new tables and those noted in the updates
NEW_site <- dbTable("SITES") 



NEW_tab <- dbTable("FISH_INFO")
chl<-dbTable("CHLOROPHYLL")
col<-dbTable("COLOR")
wc<-dbTable("WATER_CHEM")
met<-dbTable("UPDATE_METADATA")

