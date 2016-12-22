### CHANGED
### First prepare one large dataset which has all of the climate data in
### Then we can go through this and take out the 
setwd("C:/Alison/Chapter 1/Analysis")
library(stringr)

## READ IN THE CLIMATE DATA  
climate<-read.table("worldgrid_realm_climate.txt",sep="\t",header=T) 
colnames(climate)
climate<-climate[,c(-1,-28,-36)]
names(climate)



# Go through from January to December. We want to extra
Months<-str_pad(1:12,2,pad="0")
## EXTRACT THE Data for each month 
for (i in seq(along=Months)) {
 #i<-1
  print(Months[i])
  Months[i]
  col.i<-names(climate[grep(paste(Months[i]),colnames(climate))])
  month.i<-climate[,col.i]
  month.i$WM_ID<-climate$WM_ID
  month.i$Realm<-climate$Realm
  month.i$Region<-climate$Regions
  month.i$Month<-Months[i]
  colnames(month.i)
  colnames(month.i)<-c("tmax","tmin","rham","rhpm","rad","pre","WM_ID","Realm","Region","Month") 
  
  if (i==1) (month.i.all <- month.i) else (month.i.all <- rbind(month.i.all, month.i))
  
}


table(month.i.all$Month)
### SAVE THIS 
### THIS IS THE TABLE THAT I THEN USE TO EXTRACT THE APPROPRIATE DATA FOR A CLADE 
write.table(month.i.all, "Climate_month_realm.txt", sep="\t", col.names=TRUE, row.names=FALSE)

