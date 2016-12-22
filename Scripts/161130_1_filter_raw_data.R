### Tidying up the first set of data - range maps 

### AllGridded_FDB_CPH_raw2_ENgroups.txt:
### Version 0 of range maps for Emmy Noethe project
### Note: There are some taxonomy problems/bugs

### Description
### Here I remove the problematic species from the version 0 of the range maps
### Problemspecies
### Marine species
### Small ranges (<3)

### Set up 
setwd("C:/Alison/Chapter 1/Analysis")


# 1. Remove problematic species 
# 16.11.29_species_omit_V0 - list of problematic species- with decision whether they should be omitted
problem<-read.table("16.11.30_species_omit_V0.txt",sep = "\t", header=T)
problem<-problem[which(problem$Decision=="remove"),]

# Ranges Version 0
ranges_raw<-read.table("AllGridded_FDB_CPH_raw2_ENgroups.txt",sep="\t",header=T)
dim(ranges_raw) # 249095 occurences
length(unique(ranges_raw$IOC3_1_Binomial))
ranges<-merge(ranges_raw,problem, by.x="IOC3_1_Binomial",by.y="Species",all.x=TRUE)
summary(ranges$Decision)

# Remove problematic species
ranges <-ranges[is.na(ranges$Decision),] 
length(unique(ranges$IOC3_1_Binomial)) # now 499 species
  
# 2. REMOVE GRID SQUARES WHICH ARE JUST MARINE 
# This information should be in the worldgrid
# Read in the Copenhagen grid 
# A spatial polygons file which divides the world into 1 degree grid squares
# These have the same IDs as the species distribution data
# Justification- sometimes miss-match between the two datasets- so some species have marine range- really just coastal?
library(sp)
library(maptools)
library(maps)
mygrid <- readShapePoly("Worldgrid/GlobalWorldMapGrid.shp")
summary(mygrid@data$PropLand)
# Things to consider: 
# should we use threshold for omitting a cell 
# Currently- I just chuck out cells which have zero land (maybe we should check the effect of changing this value)
row.marine<-(which(mygrid@data$PropLand==0))
marine_ID<-mygrid@data$WM_ID[row.marine] # What is the WM_ID for those cells with zero land
mydsmarine<-which(marine_ID %in% ranges$WM_ID) # Which of these ID squares are acutually in the dataset
length(mydsmarine) 
mydsmarine<-unique(mydsmarine) # there are 383 squares for which we have bird range which do not have any land.

newds<-data.frame(ET_ID=mydsmarine,marine="Marine")
marine_cells<-merge(ranges,newds,by.x="WM_ID",by.y="ET_ID",all.x=TRUE)
summary(marine_cells)
length(unique(marine_cells$WM_ID[which(marine_cells$marine=="Marine")])) # CHECK DID I MAKE A MISTAKE
dim(marine_cells)
length(unique(marine_cells$IOC3_1_Binomial))
# REMOVE THESE CELLS 
ranges_terrestrial <-ranges[is.na(marine_cells$marine),] 
dim(ranges_terrestrial)
length(unique(ranges_terrestrial$IOC3_1_Binomial)) # No species lost . But some grid squares


# 3. REMOVE SPECIES WITH ONLY ONE GRID SQUARE IN ANY ONE SEASON (BREEDING OR WINTERING)
splist<-unique(ranges$IOC3_1_Binomial)
# Add a new column for identifying if it has too small range


for (i in seq(along=splist)) {

#i<-498
print(paste("Number", i, "Species", splist[i], sep=" ")) 

# Get the rows for species i 
row.i<-which(ranges$IOC3_1_Binomial==paste(splist[i]))
# Now how many of these rows are year-round
length.yr<-length(which(ranges$Range_type[row.i]=="year-round"))
range_size <- data.frame(Species=splist[i], Range_type="year_round", Range_size=length.yr)
# how many of these rows are breeding
length.br<-length(which(ranges$Range_type[row.i]=="breeding"))
tmp <- data.frame(Species=splist[i], Range_type="breeding", Range_size=length.br)
range_total<-rbind(range_size,tmp)
range_total
# how many of these rows are wintering
length.wi<-length(which(ranges$Range_type[row.i]=="non-breeding"))
unique(ranges$Range_type)
tmp <- data.frame(Species=splist[i], Range_type="non-breeding", Range_size=length.wi)
range_total<-rbind(range_total,tmp)

if (i==1) (all_species_range <- range_total) else (all_species_range <- rbind(all_species_range, range_total))
}


# For each species we have a number of grid cells occupied
# Make a new table which has breeding and non-breeding total cells occupied 
# Breeding == year round + Breeding
# Wintering== year round + Wintering
# Residents (will have identical and wintering ranges)

for (i in seq(along=splist)) {
  
#i<-498
# which rows are species i
  row.i<-which(all_species_range$Species==paste(splist[i]))
  row.yr<-row.i[1]
  row.br<-row.i[2]
  row.wi<-row.i[3]

  yr.i<-all_species_range$Range_size[row.yr]
  br.i<-all_species_range$Range_size[row.br]
  wi.i<-all_species_range$Range_size[row.wi]

  br.i<-sum(yr.i,br.i)
  wi.i<-sum(yr.i,wi.i)

# Make a table

  seasonal_range <- data.frame(Species=splist[i], Range_type="breeding", Range_size=br.i)
  temp <- data.frame(Species=splist[i], Range_type="wintering", Range_size=wi.i)
  seasonal_range<-rbind(seasonal_range,temp)

  if (i==1) (all_seasonal_range <- seasonal_range) else (all_seasonal_range <- rbind(all_seasonal_range, seasonal_range))

}

# NOW I HAVE A LIST OF THE NUMBER OF GRID CELLS OCCUPIED IN EACH SEASON FOR ALL SPECIES
all_seasonal_range
# Resident species occupy the same number of grid squares in each season

# I want to know which species have only 1 grid cell in one of these seasons
# We will omit these species from the analysis
# We might change this number? I am not sure what is the minumum number of grid squares required to carry out overlap and breadth analysis

sample_1<-which(all_seasonal_range$Range_size<=3)
unique(all_seasonal_range$Species[sample_1]) # 15 species which occupy only 1 grid cell in one of the seasons # 

# Check if these 15 are correct: yep seems to make sense they are all the vulnerable island endemics
# Add a column which says this is a small species which we will remove


newds<-data.frame(Species=unique(all_seasonal_range$Species[sample_1]), Small="TRUE")
head(newds)

range_small<-merge(ranges_terrestrial,newds,by.x="IOC3_1_Binomial",by.y="Species",all.x=TRUE )
# Now omit these cells 
final_ranges<-range_small[is.na(range_small$Small),]
length(unique((final_ranges$IOC3_1_Binomial)))

## Now I only have 461 species
head(final_ranges)
final_ranges<-final_ranges[,1:5]

# save this so I can just work from this from now on
write.table(final_ranges, "AllGridded_FDB_CPH_filtered.txt", sep="\t", col.names=TRUE)


