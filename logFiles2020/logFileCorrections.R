#Log files from 2020 have "PML" instead of the correct siteName (Inlet1, Inlet2, Inlet3, Outlet) in their sampleID's 
  #whenever the siteName=="Surface". PML, Hypo, and point are ok.

#Script corrects log files and writes new csv's to a folder called "correctedFiles"
library(tidyverse)

wrong<- "logFiles2020/originalFiles"
right<- "logFiles2020/correctedFiles"
files<-list.files(wrong)
files<-files[grep('.csv',files)]

#checking which log files have this problem: 
samples<- read.csv("logFiles2020/originalFiles/samplesIS.csv", header = T, stringsAsFactors = F) #was ok originally
profiles<- read.csv("logFiles2020/originalFiles/profilesIS.csv", header = T, stringsAsFactors = F) #does not have inlet/outlet
#also no inlet/outlet samples: bp, chl, or zoop

i=2
r=32

for (i in c(3:6,9)){
  orig<- read.csv(file.path(wrong,files[i])) #read in log file
  new<- orig %>% 
    mutate(sampleID = as.character(sampleID)) %>% 
    mutate(sampleID = case_when(site == "Inlet1" ~ gsub("PML", "Inlet1", orig$sampleID), 
                                site == "Inlet2" ~ gsub("PML", "Inlet2", orig$sampleID),
                                site == "Inlet3" ~ gsub("PML", "Inlet3", orig$sampleID),
                                site == "Outlet" ~ gsub("PML", "Outlet", orig$sampleID),
                                TRUE ~ sampleID))
  write.csv(new, file.path(right, files[i]), row.names = F)
}

#checking output: (all good)
newColor<- read.csv("logFiles2020/correctedFiles/colorLogFile.csv", header = T, stringsAsFactors = F)
newDOC<- read.csv("logFiles2020/correctedFiles/docLogFile.csv", header = T, stringsAsFactors = F)
newfiltered<- read.csv("logFiles2020/correctedFiles/filteredLogFile.csv", header = T, stringsAsFactors = F)
newunfiltered<- read.csv("logFiles2020/correctedFiles/unfilteredLogFile.csv", header = T, stringsAsFactors = F)
newpoc<- read.csv("logFiles2020/correctedFiles/pocLogFile.csv", header = T, stringsAsFactors = F)

#moving unaffected log files over to the correctedFiles folder: 
move<-files[c(1:2, 7:8, 10)]
i=1
for (i in 1:length(move)){
  file.copy(file.path("logFiles2020/originalFiles",move[i],sep=""),file.path("logFiles2020/correctedFiles",move[i],sep=""))
}



