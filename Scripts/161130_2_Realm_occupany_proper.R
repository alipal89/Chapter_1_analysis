#### DESCRIPTION 
#### FOR EACH SPECIES WE DETERMINE 
#### WHICH REALMS IT BREEDS IN
#### WHICH REALMS ARE NON - BREEDING



# 1st job make a clade species list 
# Need to know which EN group it is in 

setwd("C:/Alison/Chapter 1/Analysis")
rm(list = ls())
# RANGES
ranges<-read.table("AllGridded_FDB_CPH_filtered.txt",sep="\t",header=T) # No clade info here

## TAXONOMY INFO (WHICH CLADE)
Taxonomy<-read.table("SpeciesList3_1_TaxonomySummary_ENgroups.txt",sep="\t",header=T)
colnames(Taxonomy)

### Realm information
realm<-read.table("Copenhagen_realms_grid.txt",sep="\t",header=T,) 
colnames(realm)

realm$Regions[realm$Regions==""] <- NA
realm$Realm[realm$Realm==""] <- NA

## I AM CONCERNED THAT SOME OF OUR GRID CELLS DO NOT HAVE A REALM? 
## WHY IS THIS? 
## Checked- these are grid cells on the edge of land which the realms did not create a whole grid cell for them. But for which we have a bird species record (and have kept the cell beacuse there is some land)



range_taxonomy<-merge(ranges,Taxonomy,by.x="IOC3_1_Binomial",by.y="IOC3_1_Binomial")
head(range_taxonomy)
colnames(Taxonomy)
## Basically I want to add the ENGROUP 
## And the Realm and Region data to the ranges

## THEY SHARE THE IOC_Species 

## Now I add the Realm and Region data
## These share the GridCell IDs
range_taxonomy_realm<-merge(range_taxonomy,realm, by.x="WM_ID",by.y="WM_ID")
# Note there are some NAS NEED TO FIX


##################### NOW FOR EACH CLADE
##################### I NEED TO KNOW FOR EACH SPECIES
##################### WHICH REALMS ARE OCCUPIED IN BR
##################### WHICH REALMS ARE OCCUPIED IN NON-BR


### 1. MAKE A CLADE LIST WHICH WE CAN FILTER THROUGH 
Cd_list<-unique(range_taxonomy_realm$EN_groups)

## We will loop through each clade

for (i in seq(along=Cd_list)) {
 #i<-3
  print(paste("Number", i, "Clade", Cd_list[i], sep=" ")) # print off where the loop is
  
# subset the data so we are working with clade i
  row.clade.i<-range_taxonomy_realm[which(range_taxonomy_realm$EN_groups==paste(Cd_list[i])),]

# within this I want to loop around the species
  
  splist<-unique(row.clade.i$IOC3_1_Binomial)
  for (j in seq(along=splist)){
  # j<-21
    print(paste("Number", j, "splist", splist[j], sep=" ")) # print off where the loop is
    species.i<-range_taxonomy_realm[which(range_taxonomy_realm$IOC3_1_Binomial==paste(splist[j])),]
    br.i<-species.i[which(species.i$Range_type=="breeding"|species.i$Range_type=="year-round"),]
    realm.br.i<-unique(br.i$Realm)
    region.br.i<-unique(br.i$Regions)
    wi.i<-species.i[which(species.i$Range_type=="non-breeding"|species.i$Range_type=="year-round"),]
    realm.wi.i<-unique(wi.i$Realm)
    region.wi.i<-unique(wi.i$Regions)
  
    realms<-data.frame(IOC3_1_Binomial=paste(splist[j]),Range_type="breeding",Realm=realm.br.i,EN_clade=paste(Cd_list[i]))
    tmp <-data.frame(IOC3_1_Binomial=paste(splist[j]),Range_type="non-breeding",Realm=realm.wi.i,EN_clade=paste(Cd_list[i]))
    realms<-rbind(realms,tmp)

    regions<-data.frame(IOC3_1_Binomial=paste(splist[j]),Range_type="breeding",Region=region.br.i,EN_clade=paste(Cd_list[i]))
    tmp <-data.frame(IOC3_1_Binomial=paste(splist[j]),Range_type="non-breeding",Region=region.wi.i,EN_clade=paste(Cd_list[i]))
    regions<-rbind(regions,tmp)
  
    
    
    if (j==1) (regions_all <- regions) else (regions_all <- rbind(regions_all, regions))
    if (j==1) (realms_all <- realms) else (realms_all <- rbind(realms_all, realms))

# save this information for each species 

  }
# Add in a column with clade ID

  if (i==1) (regions_all_clade<-regions_all) else (regions_all_clade<-rbind(regions_all_clade,regions_all))
  if (i==1) (realms_all_clade<-realms_all) else (realms_all_clade<-rbind(realms_all_clade,realms_all))


}
regions_all_clade<-regions_all_clade[!is.na(regions_all_clade$Region),]
realms_all_clade<-realms_all_clade[!is.na(realms_all_clade$Realm),]

length(unique(regions_all_clade$IOC3_1_Binomial)) ### FOR ALL SPECIES IT GIVE US A LIST OF WHICH REALMS IT BREEDS 
dim(regions_all_clade)
dim(realms_all_clade)


## for each species it gives us a list of which realms it breeds in
## And each realm it non-breeds in 
# save this so I can just work from this from now on

write.table(regions_all_clade, "regions_.txt", sep="\t", col.names=TRUE)
write.table(realms_all_clade, "realms_.txt", sep="\t", col.names=TRUE)



## NEXT I NEED TO COMBINE THIS WITH THE DATA ON THE MONTHS FOR BREEDING AND NON-BREEDING




## I AM CONCERNED THAT SOME OF OUR GRID CELLS DO NOT HAVE A REALM? 
## WHY IS THIS? 
## Checked- these are grid cells on the edge of land which the realms did not create a whole grid cell for them. But for which we have a bird species record (and have kept the cell beacuse there is some land)
NA_cells<-unique(range_realm$WM_ID[which(is.na(range_realm$Regions))])
length(NA_cells)
head(NA_cells)


