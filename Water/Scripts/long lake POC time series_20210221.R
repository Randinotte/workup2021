#from KS plotting POC,PON Long over time.R

rm(list=ls())
#setwd('C:/Users/notter/Google Drive/Randi/Database/plots/Long Lake plots/2020 winter meeting')
# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
dir<-'Water/Long Lake Plots'

library(tidyverse)

#poc<-dbTable('POC',lakeID = c('EL','WL','FE')) #function that grabs table from database 
poc<-dbTable("WATER_CHEM", lakeID = c('EL','WL','FE')) 
poc<- poc%>% 
  add_column(siteName = word(poc$sampleID,2,2,sep="_")) %>% 
  add_column(depthClass = word(poc$sampleID,5,5,sep="_")) %>% 
  #filter(parameter %in% c("POC","PON"))  
  filter(parameter == "particulateP")
  #select(-metadataID, -updateID)
pocFull<- poc %>% 
  spread(parameter, parameterValue)
poc<-pocFull

poc2020<- read.csv("Water/Output/compiledData/particulate_forBella.20200220.csv", header = T, stringsAsFactors = F)
poc2020<- poc2020%>% 
  add_column(siteName = word(poc2020$sampleID,2,2,sep="_")) %>% 
  add_column(depthClass = word(poc2020$sampleID,5,5,sep="_")) %>% 
  #filter(parameter %in% c("POC","PON")) 
  filter(parameter == "particulateP")
poc2020Full<- poc2020 %>% 
  spread(parameter, parameterValue)
# poc2020<-poc2020Full %>% 
#   mutate(PON = as.numeric(poc2020Full$PON))
poc2020<-poc2020Full

#want= c("projectID", "lakeID", "siteName", "dateSample", "depthClass",  "POC", "PON", "comments", "flag")
want= c("projectID", "lakeID", "siteName", "dateSample", "depthClass",   "particulateP", "comments", "flag")
poc<-poc[want]
poc2020$dateSample<-as.POSIXct(poc2020$dateSample)
#desired = c("projectID", "lakeID", "siteName", "dateSample", "depthClass", "POC", "PON", "comments", "flag")
desired = c("projectID", "lakeID", "siteName", "dateSample", "depthClass", "particulateP", "comments", "flag")
poc2020<-poc2020[desired]
colnames(poc2020)<-colnames(poc)
poc2020<-poc2020[poc2020$lakeID%in%c('EL','WL','FE'),]
poc<-rbind(poc,poc2020)
poc$lakeID<- replace(poc$lakeID,poc$lakeID== "FE", "EL")
#poc$lakeID<- replace(poc$lakeID,poc$lakeID== "ME", "EL")

#plot Hypo
hypo = subset(poc, poc$depthClass == "Hypo")
#hypo = hypo[complete.cases(hypo[,9]),]
hypo = hypo[complete.cases(hypo[,6]),]
#hypo<-subset(hypo, hypo$POC<2)
# hypo<-subset(hypo, hypo$PON>0)

lake1 = subset(hypo, hypo$lakeID == "EL")
lake2 = subset(hypo, hypo$lakeID == "WL")

plot(POC~dateSample, data = hypo, main= "Long Lake Hypo POC", xlab= "Date", ylab = "POC ug/L")
lines(POC~dateSample, data = lake1, col = "red")
lines(POC~dateSample, data = lake2, col = "blue")
legend("topleft", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

#plot Epi
epi = subset(poc, poc$depthClass == "PML")
#epi = epi[complete.cases(epi[,9]),]
epi = epi[complete.cases(epi[,6]),]
# epi<-subset(epi, epi$PON<.3)
# epi<-subset(epi, epi$TN>0)

lake1 = subset(epi, epi$lakeID == "EL")
lake2 = subset(epi, epi$lakeID == "WL")

plot(particulateP~dateSample, data = epi, main= "Long Lake Epi POP", xlab= "Date", ylab = "POP ug/L")
lines(particulateP~dateSample, data = lake1, col = "red")
lines(particulateP~dateSample, data = lake2, col = "blue")
legend("topleft", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

library(cowplot)
testplot1 <- ggplot(data = hypo, aes(x = dateSample, y = particulateP, color = lakeID)) + geom_point() + geom_line() + 
  ggtitle("Long Lake Hypo POP") + scale_y_continuous("POP ug/L", limits = c(0,40))
testplot1 <- ggplot(data = hypo, aes(x = dateSample, y = particulateP, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake Hypo POP") + scale_y_continuous("POP ug/L", limits = c(0,40)) + scale_shape_manual(values = c(16,1))+ theme_classic()
testplot1 <- ggplot(data = hypo, aes(x = dateSample, y = POC, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake Hypo POC") + scale_y_continuous("POC ug/L", limits = c(0,2000)) + scale_shape_manual(values = c(16,1))+ theme_classic()
testplot1 <- ggplot(data = hypo, aes(x = dateSample, y = PON, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake Hypo PON") + scale_y_continuous("PON ug/L", limits = c(0,300)) + scale_shape_manual(values = c(16,1))+ theme_classic()

testplot <- ggplot(data = epi, aes(x = dateSample, y = particulateP, color = lakeID)) + geom_point() + geom_line() + 
  ggtitle("Long Lake PML POP") + scale_y_continuous("POP ug/L", limits = c(0,40))
testplot <- ggplot(data = epi, aes(x = dateSample, y = particulateP, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake PML POP") + scale_y_continuous("POP ug/L", limits = c(0,40))+ scale_shape_manual(values = c(16,1))+ theme_classic()
testplot <- ggplot(data = epi, aes(x = dateSample, y = POC, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake PML POC") + scale_y_continuous("POC ug/L", limits = c(0,2000)) + scale_shape_manual(values = c(16,1))+ theme_classic()
testplot <- ggplot(data = epi, aes(x = dateSample, y = PON, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake PML PON") + scale_y_continuous("PON ug/L", limits = c(0,300)) + scale_shape_manual(values = c(16,1))+ theme_classic()

pocplots<-cowplot::plot_grid(testplot1, testplot, nrow = 2, ncol = 1)
cowplot::save_plot(file.path(dir,"POPplots.pdf"), pocplots, base_height = 9, base_width = 10)
