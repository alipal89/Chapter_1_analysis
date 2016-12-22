##################THIS SCRIPT NEEDS TO BE SUPER TIDIED UP ###################


### IS A BIRD SPECIES A MIGRANT OR NOT? 

species_occ<-read.table("AllGridded_FDB_CPH_filtered.txt",sep = "\t", header=T)

length(unique(species_occ$Range_type))
# You are a migrant if you have more than 1 type of Range type? Is this right? 

splist<-unique(species_occ$IOC3_1_Binomial)
splist
i<-1
splist[i]

for (i in seq(along=splist)) {
season.i<-length(unique(species_occ$Range_type[which(species_occ$IOC3_1_Binomial==paste(splist[i]))]))

seasons<- data.frame(Species=splist[i],Season=season.i)

if (i==1) (seasons_all <- seasons) else (seasons_all <- rbind(seasons_all, seasons))

}

head(seasons_all)
### Add in a column which says if it is a migrant
seasons_all$Migration<-ifelse(seasons_all$Season==1,"res","mig")



### IS THERE A WAY OF DOING WHETHER IT IS LONG DISTANCE OR SHORT DISTANCE?

### I THINK IF THEY OCCUPY ANY OF THE SAME GRID SQUARES IN EACH SEASON IT COULD BE SAID TO BE SHORT.
### THIS IS SUPER CRUDE

for (i in seq(along=splist)) {
  

species.occ.i<-species_occ[which(species_occ$IOC3_1_Binomial==paste(splist[i])),]
species.br.i<-species.occ.i$WM_ID[which(species.occ.i$Range_type!="non-breeding")]
species.wi.i<-species.occ.i$WM_ID[which(species.occ.i$Range_type!="breeding")]
distance<-length(intersect(species.br.i,species.wi.i))

movement<- data.frame(Species=splist[i],distance=distance)

if (i==1) (movement_all <- movement) else (movement_all <- rbind(movement_all, movement))


} 





## Add this to the Migration 

head(seasons_all)
head(movement_all)

total<-merge(seasons_all,movement_all)

total$new<-NA
total$new[total$distance!="0"]<-"short"
total$new[total$Migration=="res"] <- NA
total$new[total$Migration=="mig" & total$distance==0]<-"long"
head(total)
#### NOW I HAVE A TABLE WHICH HAS THE SPECIES NAME
#### WHETHER IT IS A MIGRANT OR A RESIDENT
#### AND WHETHER IT IS LONG OR SHORT DISTANCE MIGRANT 
colnames(total)
total<-total[,c(1,3,5)]
colnames(total)<-c("Species","Migration","Distance")
head(total)


### I THINK I WILL ADD THIS INFORMATION TO THE MONTHS CLADE DATASET 

Months_clade<-read.table("Species_realm_months.txt",sep = "\t", header=T)
head(Months_clade)

Months_clade_migration<-merge(Months_clade,total)
head(Months_clade_migration)


########### save this table 
write.table(Months_clade_migration, "Months_clade_migration.txt", sep="\t", col.names=TRUE, row.names=FALSE)
head(Months_clade_migration)
