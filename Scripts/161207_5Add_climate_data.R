##################################################################################################
#### DESCRIPTION: 
#### Add the climate data to the data copenhagen grid 
#### Grid ID and Realm 

### READ IN CLIMATE DATA
### Climate data is saved as workspace
load("C:/Users/aeyres/Desktop/Alison_Environment/Climate_copenhagen.RData")
str(climate_copenhagen, max=2) ## 97 variables,  64800 polygons 
names(climate_copenhagen@data) # 12* 6 climate variables and then some ID squares
summary(climate_copenhagen@data$WM_ID) ## This is the ID square that we use to link things

### Copenhagen grid with Realm info
realm<-read.table("Copenhagen_realms_grid.txt",sep="\t",header=T,) 
str(realm,max=2)
summary(realm$WM_ID)

## Add climate data to the copenhagen world grid
# realm$NEW<-tmax01h
# check 
# which(climate_copenhagen$tmax01=="30") # 3 grid squares have 30 degrees
# climate_copenhagen@data[which(climate_copenhagen$tmax01=="30"),] # 3 grid squares have 30 degrees
# These are 
# WMID: 39470, 34135,30544, centroid =49.5,114,5, 124.5
# realm$WM_ID[which(realm$NEW=="30")]
# WMIDs MATCH #  OK JUST TO ADD IT IN ROW BY ROW 
# realm$Centroid_X[which(realm$NEW=="30")] # Centroids  x also match 


## Add all climate data to the grid
names(climate_copenhagen)
names(climate_copenhagen)
climate<-(climate_copenhagen@data[24:97])
realm2<-cbind(realm,climate)

### Now do a sanity check ## DO THE values match ok?? 
which(realm2$WM_ID=="64800")
realm2[360,]

## SAVE 
write.table(realm2, "worldgrid_realm_climate.txt", sep="\t", col.names=TRUE)

