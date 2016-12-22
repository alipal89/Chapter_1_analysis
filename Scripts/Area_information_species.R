############### Need to calculate the area for occupied for all species 
############### Annual area
############### Seasonal area
rm(list = ls())
setwd("C:/Alison/Chapter 1/Analysis")

# This is the species occurences for all species 
species_occ<-read.table("AllGridded_FDB_CPH_filtered.txt",sep = "\t", header=T)


splist<-unique(species_occ$IOC3_1_Binomial)

splist[1]

#### range area
#### NEED TO CHECK 

#### Which unique grid cells are occupied in the whole year (Br, Non-br and annual)
for (i in seq(along=splist)) {

species_occ.i<-species_occ[which(species_occ$IOC3_1_Binomial==paste(splist[i])),]
species_occ.i
br_occ.i<-length(which(species_occ.i$Range_type!="non-breeding"))
wi_occ.i<-length(which(species_occ.i$Range_type!="breeding"))
Annual.i<-sum(br_occ.i,wi_occ.i)

Hypo_br<-species_occ[which(species_occ$IOC3_1_Binomial==paste(splist[i]) & species_occ$Range_type!="breeding"),]
Hypo_br<-length(unique(Hypo_br$WM_ID))*2

Hypo_wi<-species_occ[which(species_occ$IOC3_1_Binomial==paste(splist[i]) & species_occ$Range_type!="non-breeding"),]
Hypo_wi<-length(unique(Hypo_wi$WM_ID))*2

Annual_area<- data.frame(Species=splist[i],Area=Annual.i, Hypo_br_area=Hypo_br, Hypo_wi_area=Hypo_wi)

if (i==1) (Annual_area_all <- Annual_area) else (Annual_area_all <- rbind(Annual_area_all, Annual_area))


}


#### I THINK THIS IS RIGHT ###########

##### NEED TO CHECK 

#### SAVE INFORMATION WITH THE MIGRATION INFORMATION
Months_clade<-read.table("Months_clade_migration.txt",sep = "\t", header=T)
Months_clade_migration_area<-merge(Months_clade,Annual_area_all)

head(Months_clade_migration_area)
write.table(Months_clade_migration_area, "Months_clade_migration_area.txt", sep="\t", col.names=TRUE, row.names=FALSE)
