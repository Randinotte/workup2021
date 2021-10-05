#fom KS plotting Long color over time.R
#RNN 2020-03-16

## color time series 
rm(list=ls())
#setwd('C:/Users/notter/Google Drive/Randi/Database/plots/Long Lake plots/2020 winter meeting')
# Source the database functions
source("C:/Users/notter/Google Drive/Randi/Database/R/dbUtil.R")

# Set up objects to locate database. Functions in dbUtil require these to be defined ahead of use.
dbdir="C:/Users/notter/Google Drive/Randi/Database/currentDB/" #folder in Database folder that contains the most recent .db file (below)
db="MFEdb_20210112.db" #Change name of db file with every update
dir<-'Water/Long Lake Plots'

color<-dbTable('color',lakeID = c('EL','FE','WL','ME')) #function that grabs table from database 
color2020<- read.csv('Water/Output/compiledData/tableColor_20210221.csv',header=TRUE,stringsAsFactors = F) #read in 2014-2016 profile data 
want= c("projectID", "lakeID", "siteName", "dateTimeSample", "depthClass", "depthTop", "depthBottom", "g440", "comments")
color<-color[want]
color2020$dateTimeSample<-as.POSIXct(color2020$dateTimeSample)
desired = c("projectID", "lakeID", "siteName", "dateTimeSample", "depthClass", "depthTop", "depthBottom", "g440", "comments")
color2020<-color2020[desired]
colnames(color2020)<-colnames(color)
color2020<-color2020[color2020$lakeID%in%c('FE','ME','WL','EL'),]
color<-rbind(color,color2020)
color$lakeID<- replace(color$lakeID,color$lakeID== "FE", "EL")
color<-as.data.frame(color)
color<-subset(color,color$siteName=="DeepHole")
color<-subset(color, color$depthClass %in% c("Hypo","PML"))



#plot Hypo
hypo = subset(color, color$depthClass == "Hypo")
hypo$lakeID<- replace(hypo$lakeID,hypo$lakeID== "FE", "EL")

#average replicates for chl and DOC

hypo<-subset(hypo, hypo$g440<20)

lake1 = subset(hypo, hypo$lakeID == "EL" | hypo$lakeID == 'FE')
lake1$lakeID= rep("EL", nrow(lake1))
lake2 = subset(hypo, hypo$lakeID == "WL")

plot(g440~dateTimeSample, data = hypo, main= "Long Lake Hypo Color", xlab= "Date", ylab = "Color g440 1/m")
lines(g440~dateTimeSample, data = lake1, col = "red")
lines(g440~dateTimeSample, data = lake2, col = "blue")
legend("top", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

#plot Epi
epi = subset(color, color$depthClass == "PML" | color$depthClass == 'Epi')
epi$lakeID<- replace(epi$lakeID,epi$lakeID== "FE", "EL")

epi<-subset(epi, epi$g440<11)

lake1 = subset(epi, epi$lakeID == "EL")
lake2 = subset(epi, epi$lakeID == "WL")

plot(g440~dateTimeSample, data = epi, main= "Long Lake Epi Color", xlab= "Date", ylab = "Color g440 1/m")
lines(g440~dateTimeSample, data = lake1, col = "red")
lines(g440~dateTimeSample, data = lake2, col = "blue")
legend("topleft", legend=c("EL","WL"), pch=22, pt.bg=c('red','blue'), ncol=1,cex=1.5,bty='n')

library(cowplot)
library(ggplot2)
testplot1 <- ggplot(data = hypo, aes(x = dateTimeSample, y = g440, color = lakeID)) + geom_point() + geom_line() + 
  ggtitle("Long Lake Hypo Color") + scale_y_continuous("Color g440 1/m", limits = c(0,20)) + theme_classic()
testplot2 <- ggplot(data = epi, aes(x = dateTimeSample, y = g440, color = lakeID)) + geom_point() + geom_line() + 
  ggtitle("Long Lake PML Color") + scale_y_continuous("Color g440 1/m", limits = c(0,12)) + theme_classic()
colorplots<-cowplot::plot_grid(testplot1, testplot2, nrow = 2, ncol = 1)
cowplot::save_plot(file.path(dir,"colorplots.pdf"), colorplots, base_height = 9, base_width = 10)
