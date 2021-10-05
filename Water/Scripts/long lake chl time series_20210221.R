#Katie's script with path changes so I can use it on my data.

## chl time series 
rm(list=ls())
#setwd('C:/Users/notter/Google Drive/Randi/Database/plots/Long Lake plots/2020 winter meeting')
# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
dir<-'Water/Long Lake Plots'


CHL<-dbTable('chlorophyll',lakeID = c('EL','FE','WL')) #function that grabs table from database 
CHL<-CHL[,c(1,3,4,6:12,14,15)]
#chl2020<- read.csv('C:/Users/notter/Google Drive/Randi/Database/currentDB/tableChlorophyll_20200203.csv',header=TRUE,stringsAsFactors = F)
chl2020<- read.csv("Water/Output/compiledData/tableChlorophyll_20210219.csv", header = T, stringsAsFactors = F)
#chl2020$dateTimeSample<-as.POSIXct(chl2020$dateTimeSample, format="%m/%d/%Y %H:%M")
chl2020$dateTimeSample<-as.POSIXct(chl2020$dateTimeSample)
desired = c("projectID", "lakeID", "siteName", "dateTimeSample", "depthClass", "depthTop", "depthBottom", "runID","chl", "replicate", "comments", "flag")
chl2020<-chl2020[desired]
colnames(chl2020)<-colnames(CHL)
chl2020<-chl2020[chl2020$lakeID%in%c('EL','WL','FE'),]
CHL<-rbind(CHL,chl2020)
CHL$lakeID<- replace(CHL$lakeID,CHL$lakeID== "FE", "EL")
CHL<-as.data.frame(CHL)
CHL<-subset(CHL,CHL$siteName=="DeepHole")
CHL<-subset(CHL, CHL$depthClass %in% c("Hypo","PML"))



#plot Hypo
newdf<- subset(CHL, CHL[["depthClass"]] == "Hypo")

#average replicates for chl and DOC
avg<-aggregate(newdf$chl,by=list(newdf$dateTimeSample),FUN='mean')
colnames(avg)<-c('dateTimeSample','CHL')
m=merge(avg,newdf,by="dateTimeSample", all.x = TRUE)
hypo<- subset(m, m$replicate == "1")
#hypo<-subset(hypo, hypo$chl<35)

lake1 = subset(hypo, hypo$lakeID == "EL" | hypo$lakeID == "FE") 
lake2 = subset(hypo, hypo$lakeID == "WL")

plot(CHL~dateTimeSample, data = hypo, main= "Long Lake Hypo Chl", xlab= "Date", ylab = "Chl ug/L", ylim = c(0,32))
lines(CHL~dateTimeSample, data = lake1, col = "red")
lines(CHL~dateTimeSample, data = lake2, col = "blue")
legend("topleft", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

#plot Epi
newdf2 = subset(CHL, CHL$depthClass == "PML")
newdf2 = subset(newdf2, newdf2$siteName == "DeepHole")
#average replicates for chl and CHL
avg<-aggregate(newdf2$chl,by=list(newdf2$dateTimeSample),FUN='mean')
colnames(avg)<-c('dateTimeSample','CHL')
m=merge(avg,newdf2,by="dateTimeSample", all.x = TRUE)
epi<- subset(m, m$replicate == "1")
#epi<-subset(epi, epi$CHL<35)

lake1 = subset(epi, epi$lakeID == "EL")
lake2 = subset(epi, epi$lakeID == "WL")

plot(CHL~dateTimeSample, data = epi, main= "Long Lake Epi Chl", xlab= "Date", ylab = "Chl ug/L", ylim = c(0,32))
lines(CHL~dateTimeSample, data = lake1, col = "red")
lines(CHL~dateTimeSample, data = lake2, col = "blue")
legend("topleft", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

library(cowplot)
testplot1 <- ggplot(data = hypo, aes(x = dateTimeSample, y = CHL, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake Hypo Chl") + scale_y_continuous("Chl ug/L", limits = c(0,32)) + scale_shape_manual(values = c(16,1)) + theme_classic()
testplot <- ggplot(data = epi, aes(x = dateTimeSample, y = CHL, color = lakeID)) + geom_point(aes(shape = as.factor(flag))) + geom_line() + 
  ggtitle("Long Lake PML Chl") + scale_y_continuous("Chl ug/L", limits = c(0,32)) + scale_shape_manual(values = c(16,1))+ theme_classic()
docplots<-cowplot::plot_grid(testplot1, testplot, nrow = 2, ncol = 1)
cowplot::save_plot(file.path(dir,"chlplots.pdf"), docplots, base_height = 9, base_width = 10)
