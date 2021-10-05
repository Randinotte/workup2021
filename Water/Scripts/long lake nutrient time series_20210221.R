#from KS plotting Long nutrients time series.R

rm(list=ls())
#setwd('C:/Users/notter/Google Drive/Randi/Database/plots/Long Lake plots/2020 winter meeting')
# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
dir<-'Water/Long Lake Plots'

library(tidyverse)

## doc time series 
nut<-dbTable('WATER_CHEM',lakeID = c('EL','WL','FE')) #function that grabs table from database 
param<-"SRP"
anal<-subset(nut,nut$parameter == param)
anal<- anal %>% 
  add_column(siteName = word(anal$sampleID,2,2,sep="_")) %>% 
  add_column(depthClass = word(anal$sampleID,5,5,sep="_"))
nut2020<- read.csv('Water/Output/compiledData/nutrients_forPresentation.20210221.csv',header=TRUE,stringsAsFactors = F) #read in 2014-2016 profile data 
nut2020<- nut2020 %>% 
  add_column(siteName = word(nut2020$sampleID,2,2,sep="_")) %>% 
  add_column(depthClass = word(nut2020$sampleID,5,5,sep="_"))

want= c("projectID", "lakeID", "siteName", "dateSample", "parameter","parameterValue", "flag", "depthClass")
anal<-anal[want]
nut2020$dateSample<-as.POSIXct(nut2020$dateSample)
desired = c("projectID", "lakeID", "siteName", "dateSample",  "parameter","parameterValue", "flag", "depthClass")
nut2020<-nut2020[desired]
nut2020<-subset(nut2020,nut2020$parameter == param)
colnames(nut2020)<-colnames(anal)
nut2020<-nut2020[nut2020$lakeID%in%c('FE','WL','EL'),]
anal<-rbind(anal,nut2020)
anal$lakeID<- replace(anal$lakeID,anal$lakeID== "FE", "EL")
# anal$lakeID<- replace(anal$lakeID,anal$lakeID== "ME", "EL")
anal<-subset(anal,anal$siteName == 'DeepHole')


#plot Hypo
hypo = subset(anal, anal$depthClass == "Hypo")

#average replicates for chl and DOC

#hypo<-subset(hypo, hypo$parameterValue<50)

lake1 = subset(hypo, hypo$lakeID == "EL" | hypo$lakeID == 'FE')
#lake1$lakeID= rep("EL", nrow(lake1))
lake2 = subset(hypo, hypo$lakeID == "WL")

plot(parameterValue~dateSample, data = hypo, main= paste("Long Lake Hypo",param), xlab= "Date", ylab = paste(param,"ug/L"))
lines(parameterValue~dateSample, data = lake1, col = "red")
lines(parameterValue~dateSample, data = lake2, col = "blue")
legend("top", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

#plot Epi
epi = subset(anal, anal$depthClass == "PML")
epi$parameterValue<-replace(epi$parameterValue,epi$parameterValue<0,0)

#epi<-subset(epi, epi$g440<11)

lake1 = subset(epi, epi$lakeID == "EL")
lake2 = subset(epi, epi$lakeID == "WL")

plot(parameterValue~dateSample, data = epi, main= paste("Long Lake Epi",param), xlab= "Date", ylab = paste(param,"ug/L"))
lines(parameterValue~dateSample, data = lake1, col = "red")
lines(parameterValue~dateSample, data = lake2, col = "blue")
legend("topleft", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

#change flags
hypo$flag<- replace(hypo$flag,hypo$flag==2,1)

epi$flag<- replace(epi$flag,epi$flag==2,1)

library(cowplot)
testplot1 <- ggplot(data = hypo, aes(x = dateSample, y = parameterValue, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle(paste("Long Lake Hypo",param)) + scale_y_continuous(paste(param,"ug/L"), limits = c(0,120)) + scale_shape_manual(values = c(16,1)) + theme_classic()
testplot2 <- ggplot(data = epi, aes(x = dateSample, y = parameterValue, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle(paste("Long Lake PML",param)) + scale_y_continuous(paste(param,"ug/L"), limits = c(0,15))+ scale_shape_manual(values = c(16,1)) + theme_classic()
colorplots<-cowplot::plot_grid(testplot1, testplot2, nrow = 2, ncol = 1)
cowplot::save_plot(file.path(dir,paste(param,"plots.pdf",sep="")), colorplots, base_height = 9, base_width = 10)


