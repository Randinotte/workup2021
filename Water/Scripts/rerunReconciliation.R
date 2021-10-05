#Nutrient rerun reconciliation script
#RNN 2021-04-28
  #Pulls in rerun data for TP, TN, SRP, NO3, POC/PON, and POP
  #Compares duplicates/reruns
  #Makes changes to datasets
  #Writes new files for compileNutrientTables.R

# TP ----
#Post-QC reconciliation:
#find previous TP comments: 
wc<-dbTable("WATER_CHEM")
TP<-wc %>% filter(parameter=="TP" & flag=="1")
View(unique(TP$comments))