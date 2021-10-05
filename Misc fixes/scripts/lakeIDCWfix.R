#While making the fish update I found that lakeID CW existed in SITES but not in LAKES.
#Kaija worked on LAKES and removed CW because there was no info in the table and neither Chris/Stuart knew of a lake CW. 
#They guessed it was a mispelling of CY, so they removed it from LAKES because CY already existed. 
#Data was associated with CW and "CW" was never changed to "CY" in their sampleIDs. 

source("/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")
source("/Users/notter/Google Drive/Randi/Database/R/QCfuns.R")
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" 
db="MFEdb_20201125.db"
db="MFEdb_20210402.db"

findID("CW") #CHLOROPHYLL, COLOR, SAMPLES, SITES, WATER_CHEM
  #need to change in siteID in SAMPLES and SITES too


colnames(dbTable("CHLOROPHYLL"))
colnames(dbTable("COLOR"))
colnames(dbTable("SAMPLES"))
colnames(dbTable("SITES"))
colnames(dbTable("WATER_CHEM"))


changeSomething("CW", "CY", "CHLOROPHYLL", columns = c("sampleID","lakeID"),"20201208", "lakeID.fix.20201208")
changeSomething("CW", "CY", "COLOR", columns = c("sampleID","lakeID"),"20201208", "lakeID.fix.20201208")
changeSomething("CW", "CY", "SAMPLES", columns = c("sampleID","lakeID", "siteID"),"20201208", "lakeID.fix.20201208")
changeSomething("CW", "CY", "SITES", columns = c("siteID","lakeID", "siteName"),"20201208", "lakeID.fix.20201208")
changeSomething("CW", "CY", "WATER_CHEM", columns = c("sampleID","lakeID"),"20201208", "lakeID.fix.20201208")

