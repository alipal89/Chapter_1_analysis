################################################ DESCRIPTION ################################################################

### This script is designed to work out which data is needed to be included in each clade's PCA
### For each species in a clade we need to include:
### Breeding background (climatic data for all the biogeographical reams a species breeds in, for all 3 months it breeds)
### Non-breeding background (climatic data for all the biogeographical realms a species non-breeding range occupies, for all 3 months of non-breeding period
### Hypothetical breeding-background (climate for all the biogeographical realms a species' non-breeding range occupies, for the breeding months)
### Hypothetical non-breeding background (climate for all the bgr a species' breeding range occupies, for the non-breeding months)


#############################################################################################################################
### For each clade read in the species information (when it breeds and in which continents)
### For each species we list the months it breeds and the biogeographic realms
### Then we remove duplicates within the clade (so if 2 species breed in Africa in Jan this is only included once in PCA)***** 
##############################################################################################################################


setwd("C:/Alison/Chapter 1/Analysis")

# Read in Breeding and wintering months
Months<-read.table("months_clade.txt",sep="\t",header=T) 
head(Months)
# Read in Breeding and wintering realms 
Realms<-read.table("realms_.txt",sep="\t",header=T) 

### WHICH MONTHS DOES EACH SPECIES BREED IN
### WHICH REALMS DOES EACH SPECIES BREED IN
### WHICH MONTHS DOES EACH SPECIES WINTER IN
### WHICH REALMS DOES EACH SPECIES WINTER IN

splist<-unique(Months$Species)

for(i in seq(along=splist)){
  print(paste("Number", i, "Species", splist[i], sep=" ")) # print off where the loop is
  
#  i<-52
  ## Which months breeds
  row.i.br<-Months[which(Months$Species==paste(splist[i])& Months$Season=="Br"),]
  species.i<-as.character(row.i.br[,1]) ## species' name
  clade.i<-as.character(row.i.br$EN_Clade)
  season.br<-as.character(row.i.br$Season)
  months.br<-as.matrix(row.i.br[3:5], header=F) ## Months that the species is present in
  #which realms is a species present in during breeding
  realms.i.br<-Realms[which(Realms$IOC3_1_Binomial==paste(splist[i]) & Realms$Range_type=="breeding"),]
  realms.i.br<-unique(realms.i.br$Realm)
  ## All Combinations for breeding (months and Realms)
  breeding_month_realm<-expand.grid(species.i,clade.i,season.br,months.br,realms.i.br) ## For each species find all the combinations of month and realm that exist
  breeding_month_realm
  ### Which months winters
  row.i.wi<-Months[which(Months$Species==paste(splist[i])& Months$Season=="Wi"),]
  season.wi<-as.character(row.i.wi$Season)
  months.wi<-as.matrix(row.i.wi[3:5], header=F) ## Months that the species is present in
  #which realms is a species present in during Wintering season
  realms.i.wi<-Realms[which(Realms$IOC3_1_Binomial==paste(splist[i]) & Realms$Range_type=="non-breeding"),]
  realms.i.wi<-unique(realms.i.wi$Realm)
  wintering_month_realm<-expand.grid(species.i,clade.i,season.wi,months.wi,realms.i.wi) ## For each species find all the combinations of month and realm that exist
  wintering_month_realm
 
  ## HYPO-BREEDING BG
  ## REALMS OF WINTERING
  ## MONTHS OF BREEDING
  print(paste("Number", i, "Species", splist[i], sep=" ")) # print off where the loop is
  

  hypo_br_month_realm<-expand.grid(species.i,clade.i,paste("Hypo",season.br,sep="_"),months.br,realms.i.wi) ## For each species find all the combinations of month and realm that exist
  hypo_br_month_realm
  
  ## HYPO-WINTERING BG
  ## REALMS OF BREEDING 
  ## MONTHS OF WINTERING
 
  hypo_wi_month_realm<-expand.grid(species.i,clade.i,paste("Hypo",season.wi,sep="_"),months.wi,realms.i.br) ## For each species find all the combinations of month and realm that exist
   
  species_month_realm<-rbind(breeding_month_realm,wintering_month_realm,hypo_br_month_realm,hypo_wi_month_realm)
 
  if (i==1) (clade_wide <- species_month_realm) else (clade_wide <- rbind(clade_wide, species_month_realm))
  colnames(hypo_br_month_realm)

}

colnames(clade_wide)<-c("Species","ENClade","Season","Month","Realm")
head(clade_wide)

write.table(clade_wide, "Species_realm_months.txt", sep="\t", col.names=TRUE, row.names=FALSE)

#################################################################################

##### THIS SHOULD BE NOW SPLIT INTO CLADES #####################################
##### I NEED TO KNOW WHICH DATA NEEDS TO BE IN EACH PCA FOR A CLADE
##### BUT LATER I NEED TO BE ABLE TO PULL OUT THE BACKGROUND FOR EACH SEASON/ EACH SPECIES 

Cd_list<-unique(clade_wide$ENClade)
for(i in seq(along=Cd_list)){
#i<-1
clade.i<-clade_wide[which(clade_wide$ENClade==paste(Cd_list[i])),]
head(clade.i)

#### REMOVE DUPLICATES WITHIN A CLADE
### FOR CLADE x WE NEED THE FOLLOWING CLIMATE DATA FOR THE FOLLOWING REALMS
### CLIMATE DATA IS MONTHLY
### SO GO THROUGH THE MONTHS
### TAKE THE CORRECT DATA

PCA_Clade_i<-clade.i[!duplicated(clade.i[c("Realm","Month")]),] ## Remove duplicates for month and realm combinations
PCA_Clade_i<-PCA_Clade_i[,c(2,4,5)]

if (i==1) (Clade_PCA <- PCA_Clade_i) else (Clade_PCA <- rbind(Clade_PCA, PCA_Clade_i))

}

write.table(Clade_PCA, "Clade_realm_months.txt", sep="\t", col.names=TRUE, row.names=FALSE)

##### NOW I NEED TO ACCESS THE CLIMATE DATA
##### I NEED TO BE ABLE TO EXTRACT THE COLUMNS WHICH ARE EACH REALM 



