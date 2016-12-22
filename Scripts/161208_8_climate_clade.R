###

### Carry out the analysis on a clade


## DESCRIPTION
## Modification of my original script since ecospat has been now developed as a package
## This script investigates ecological niche overlap from occurrence and spatial environmental data

## Full description of the script

# carry out a PCA on the entire environmental space available (climate data for all continents that the clade breeds or winters in, for all months that the whole year)
# for Oeananthe this is "Eurasia" for the entire year (12 months)
# extracts relevant PCA scores for overlap, equivalency and similarity tests 
# 1. real breeding vs real wintering
# 2. hypothetical breeding vs real wintering
# 3. real breeding vs hypothetical wintering 



#################################################################################################
############################## load functions and packages ######################################
#################################################################################################


library(ecospat)
library(biomod2)
library(ade4)
library(adehabitatHS)
library(MASS)

rm(list = ls())

setwd("C:/Alison/Chapter 1/Analysis")


## Read in the climate data
## This is the climate data for 12 months of the year
## All biogeographic realms 
climate<-read.table("Climate_month_realm.txt",sep="\t",header=T) 

### FOR EACH CLADE I NEED TO DO THE PCA ON THE WHOLE CLADE INCLUDING BACKGROUNDS & HYPO BACKGROUNDS
clade_month_realm<-read.table("Clade_realm_months.txt",sep="\t",header=T) 


pdf("Results/Niche_overlap_ENClades.pdf", width=8, height=10)
# layout of graphs on the page
layout(matrix(c(1,1,2,2,1,1,2,2,3,3,4,5,3,3,6,7), 4, 4, byrow = TRUE))


## Loop through each clade
## Just take the first clade
Cdlist<-unique(clade_month_realm$ENClade)
Cdlist[2]

for (i in seq(along=Cdlist)){
#i<-3
print(Cdlist[i])
clade.i<-clade_month_realm[which(clade_month_realm$ENClade==Cdlist[i]),]
clade.i
## I want to know which climate rows match each column
## e.g. for first one 
## which one is Month 5 and which is Neotropical 
## For now work with realms. They are bigger and less of them

for (j in seq(1:dim(clade.i)[1])){

#j<-124
print(j)
realm_month.j<-climate[which(climate$Realm==paste(clade_month_realm$Realm[j]) & climate$Month==paste(clade_month_realm$Month[j])),]
realm_month.j
realm_month.j$ENClade<-Cdlist[i]

if (j==1) (realm_month_clade <- realm_month.j) else (realm_month_clade <- rbind(realm_month_clade, realm_month.j))



}


if(i==1) (realm_month_ENClade<-realm_month_clade) else (realm_month_ENClade<- rbind(realm_month_ENClade,realm_month_clade))
}

### SAVE THIS AS AN R OBJECT

### THEN I RUN THROUGH THIS DOING PCAs for each clade
save(realm_month_ENClade, file="realm_month_ENClade.Rdata")
