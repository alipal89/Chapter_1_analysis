## I have a list of 499 species
## Which fall into 8 clades


## For PCA of the clade: I need to know which realms each species occupies (in each season)
## I also need to know which months it occupies 

# 1st job make a clade species list 
# Need to know which EN group it is in 

setwd("H:/Chapter 1/Analysis")
ranges<-read.table("AllGridded_FDB_CPH_filtered.txt",sep="\t",header=T) # No clade info here
species_filtered<-data.frame(unique(ranges$IOC3_1_Binomial))
species_filtered # THIS IS A LIST OF ALL THE SPECIES I WANT
colnames(species_filtered)<-"IOC3_1_Binomial"
# FOR THESE 484 SPECIES I WANT TO KNOW WHAT ENGROUP THEY ARE IN
Taxonomy<-read.table("SpeciesList3_1_TaxonomySummary_ENgroups.txt",sep="\t",header=T)

# This has a list of all the species (including ones I have filtered out)
# It also has the EN_groups that the species is from
# So I just need to merge these based on Species name (omit the ones which are missing)
Clade_ID<-Taxonomy[,c(1,8)]
res<-merge(species_filtered,Clade_ID)
total<-merge(ranges,res,by.x="IOC3_1_Binomial",by.y="IOC3_1_Binomial")

## Species 1
## which squares are breeding or annual
## Which realms are these in? 
## Which EN group is it so I can split them up? 

# make a vector of the clades

cdlist<-unique(total$EN_groups)

cdlist[1]
row.clade.i<-which(total$EN_groups==paste(cdlist[i]))
# now make a vector of species
splist.i<-unique(total$IOC3_1_Binomial[row.clade.i])



# Then make a loop through the species 
splist
