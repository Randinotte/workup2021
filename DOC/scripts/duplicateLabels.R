library(ggplot2)

#read in mergeDOCData.csv
all.doc<-read.csv("DOC/output/mergeDOCData.csv", sep="")

#try to figure out which duplicates are which
#subset all.doc to just Long Lake for the whole summer
longDOC<-subset(all.doc,all.doc$lakeID %in% c("FE","WL"))
longDOC$dateSample<-as.Date(longDOC$dateSample,format="%m/%d/%Y")

#plot DOC over time per lake
long<-ggplot(longDOC, aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=lakeID))
nonInlet<-ggplot(longDOC[longDOC$depthClass %in% c("PML","Hypo"),],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass))


#plot DOC per depth class
feDH<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$depthClass %in% c("PML","Hypo"),],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass)) + ggtitle("FE PML & Hypo") #+scale_x_date(date_breaks = "1 week",date_labels=("%m-%d"))
fePML<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$depthClass =="PML",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color= runID))+ ggtitle("FE PML")
feHypo<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$depthClass =="Hypo",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color= runID))+ ggtitle("FE Hypo")

wlDH<-ggplot(longDOC[longDOC$lakeID=="WL"& longDOC$depthClass %in% c("PML","Hypo"),],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass))+ ggtitle("WL PML & Hypo") #+scale_x_date(date_breaks = "1 week",date_labels=("%m-%d"))
wlPML<-ggplot(longDOC[longDOC$lakeID=="WL"& longDOC$depthClass =="PML",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color= runID)) + ggtitle("WL PML")#+ scale_x_date(date_breaks = "1 week",date_labels=("%m-%d"))
wlHypo<-ggplot(longDOC[longDOC$lakeID=="WL"& longDOC$depthClass =="Hypo",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color= runID)) + ggtitle("WL Hypo")

feI<-ggplot(longDOC[longDOC$lakeID=="FE"& longDOC$site =="Inlet1",],aes(x=dateSample, y=DOC_mgL)) +geom_point(aes(color=depthClass)) +ggtitle("FE Inlet1")
