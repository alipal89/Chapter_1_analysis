
## Full description of the script

# carry out a PCA on the entire environmental space available (climate data for all continents that the clade breeds or winters in, for all months that the whole year)

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
table(climate$Month,climate$Realm) ## ONLY 52 GRID SQUARES FOR MADAGASCAR # IS THIS SOMETHING TO DO WITH IT GOING WRONG?


### FOR EACH CLADE I NEED TO DO THE PCA ON THE WHOLE CLADE INCLUDING BACKGROUNDS & HYPO BACKGROUNDS
clade_month_realm<-read.table("Clade_realm_months.txt",sep="\t",header=T) 
head(clade_month_realm)


Cdlist<-unique(clade_month_realm$ENClade)
## Loop through each clade
for (i in seq(along=Cdlist)){
#i<-2
print(Cdlist[i])
clade.i<-clade_month_realm[which(clade_month_realm$ENClade==Cdlist[i]),]
clade.i  
## WE NEED ALL OF THIS DATA FOR THE PCA 
## I want to know which climate rows match each column
## e.g. for first one 
## which one is Month 5 and which is Neotropical 
## For now work with realms. They are bigger and less of them

for (j in seq(1:dim(clade.i)[1])){
print(j)
realm_month.j<-climate[which(climate$Realm==paste(clade.i$Realm[j]) & climate$Month==paste(clade.i$Month[j])),]
realm_month.j$ENClade<-Cdlist[i]
if (j==1) (realm_month_clade <- realm_month.j) else (realm_month_clade <- rbind(realm_month_clade, realm_month.j))



}


if(i==1) (realm_month_ENClade<-realm_month_clade) else (realm_month_ENClade<- rbind(realm_month_ENClade,realm_month_clade))
}
table(realm_month_ENClade$ENClade)

### SAVE THIS AS AN R OBJECT
dim(realm_month_ENClade)
### THEN I RUN THROUGH THIS DOING PCAs for each clade
save(realm_month_ENClade, file="realm_month_ENClade.Rdata")
