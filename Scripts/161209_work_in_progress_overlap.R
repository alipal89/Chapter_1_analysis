#### 

For a clade

## Loop through the species of interest 

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
install.packages("adehabitatHS")
library(adehabitatHS)
library(MASS)

rm(list = ls())

setwd("C:/Alison/Chapter 1/Analysis")




#################################################################################################
############################## preparation of datasets ##########################################
#################################################################################################

## Load in species occurences for clade 

## Load in PCscores for each clade
clade_pca<-load("PCs_ENClade.Rdata")
clade_pca
## Then I need to somehow extract the right PC scores for each species
species_occ<-read.table("AllGridded_FDB_CPH_filtered.txt",sep = "\t", header=T)
table(species_occ$IOC3_1_Binomial)

## ACTUALLY WHAT I NEED IS ACTUALLY THE BACKGROUND INFORMATION 

Months_clade<-read.table("Species_realm_months.txt",sep = "\t", header=T)
#################################################################################################
############################## ANALYSIS - selection of parameters ###############################
#################################################################################################

# selection of the type of analysis.
# If PROJ =F, the models are calibrated on both ranges.
# If PROJ =T, the models are calibrated on species 1 range only and projected to range 2. 
# Analyses where both ranges are needed (ex: LDA) are not done
PROJ = F
# PROJ = T


#number of interation for the tests of equivalency and similarity (potentially irrelevant)
iterations<-100

#resolution of the gridding of the climate space
R=100

# Functions #
se <- function (x) (sqrt(var(x)/length(x))) # function to get the standard error of a mean








### MAYBE I SHOULD NOT RUN THE ANALYSES FOR RESIDENTS #  IT TAKES MORE COMPUTER POWER.
### WE COULD MAKE A VECTOR OF MIGRANT VS RESIDENT 
### THIS COULD BE DONE AUTOMATICALLY

#################################################################################################
#################################### PCA-ENV ####################################################
#################################################################################################

####################################### PCA #####################################################
#### Description: We perform 1 PCA accross all species in one clade
#### In this PCA we include the background for all species in a clade
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE BREEDS
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE NON-BREEDS
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE HYPO-BREEDS
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE HYPO-NON-BREEDS

#### THIS HAS ALREADY BEEN DONE FOR EACH CLADE
#### SO FOR EACH CLADE WE HAVE THE PC SCORES FOR THE ENVIRONMENTAL SPACE



###########################################################################


pdf("Results/Niche_overlap_Oenanthe_ecospat.pdf", width=8, height=10)
# layout of graphs on the page
layout(matrix(c(1,1,2,2,1,1,2,2,3,3,4,5,3,3,6,7), 4, 4, byrow = TRUE))

#################################################################################################
#################################### PCA-ENV ####################################################
##############Loop through species in clade to carry out overlap and equivalancy ################


############ Take one clade example


head(pca.clade.ENclade)
Cdlist<-unique(pca.clade.ENclade$ENClade)
Cdlist[1]

#### SUBSET THE PCA DATA SO THAT WE ONLY WORK WITH CLADE 1
pca.clade.i<-pca.clade.ENclade[which(pca.clade.ENclade$ENclade==Cdlist[i]),]
head(pca.clade.i)
summary(pca.clade.i)

#### Make a species list for clade 
splist<-Months_clade$Species[which(Months_clade$ENClade==Cdlist[i])]

## Now just do it for one species as an example
i<-1
splist[i]
print(paste("Number", i, "Species", splist[i], sep=" ")) # print off where the loop is
  
  #### 1. Overlap between REAL niches ####
  # grouping factors
  # predict the scores on the axes
  #background climate 
  # BREEDING   
br.bg.i<-Months_clade[which(Months_clade$Species==paste(splist[i]) & Months_clade$Season=="Br"),]
    for (j in seq(1:dim(br.bg.i)[1])){
    #print(j)
     #j<-3  
    temp<-pca.clade.i[which(pca.clade.i$Month==paste(br.bg.i$Month[j])),]
    temp<-temp[which(temp$Realm==paste(br.bg.i$Realm[j])),]  
    if (j==1) (row.clim.br<- temp) else (row.clim.br <- rbind(row.clim.br, temp))
  head(row.clim.br)
    }
  
wi.bg.i<-Months_clade[which(Months_clade$Species==paste(splist[i]) & Months_clade$Season=="Wi"),]
for (j in seq(1:dim(wi.bg.i)[1])){
  #print(j)
  #j<-3  
  temp<-pca.clade.i[which(pca.clade.i$Month==paste(br.bg.i$Month[j])),]
  temp<-temp[which(temp$Realm==paste(wi.bg.i$Realm[j])),]  
  if (j==1) (row.clim.wi<- temp) else (row.clim.wi <- rbind(row.clim.wi, temp))
  
}
colnames(species_occ)
summary(species_occ$Range_type)
### NOW I NEED TO GET THE BREEDING OCCURENCES

### TAKE FROM THE BREEDING BACKGROUND 
## Check this 
WMID_BR.i<-species_occ$WM_ID[which(species_occ$IOC3_1_Binomial==paste(splist[i])& species_occ$Range_type!="non-breeding")]
species_occ[which(species_occ$IOC3_1_Binomial==paste(splist[i])),]
occ.br<-row.clim.br[WMID_BR.i %in% row.clim.br$WM_ID,]
names(occ.br)

### SAME FOR THE WINTERING RANGE ### 
WMID_WI.i<-species_occ$WM_ID[which(species_occ$IOC3_1_Binomial==paste(splist[i]) & species_occ$Range_type!="breeding")]
occ.wi<-row.clim.wi[WMID_WI.i %in% row.clim.wi$WM_ID,]


### Pull out the required PCA scores
scores.clim.br<-row.clim.br[,c(12,13)]
scores.clim.wi<-row.clim.wi[,c(12,13)]
scores.clim.brwi<-rbind(scores.clim.br,scores.clim.wi)
scores.br<-occ.br[,c(12,13)] ### Pulls out scores for Breeding distribution
scores.wi<- occ.wi[,c(12,13)]#### Pulls out scores for wintering distribution
  
  # THIS IS THE SECTION THAT CHANGES- I NOW USE ECOSPAT RATHER THAN SOURCING THE FUNCTIONS
  # calculation of occurence density and test of niche equivalency and similarity 
  z1<- ecospat.grid.clim.dyn(scores.clim.brwi,scores.clim.br,scores.br,R)
  z2<- ecospat.grid.clim.dyn(scores.clim.brwi,scores.clim.wi,scores.wi,R)
  a<-ecospat.niche.equivalency.test(z1,z2,rep=2)# test of niche equivalency and similarity according to Warren et al. 2008
  b<-ecospat.niche.similarity.test(z1,z2,rep=2)
  b2<-ecospat.niche.similarity.test(z2,z1,rep=2)
  
  
  
  # save results for equivalency test: make a results dataframe with one line, where we 
  # save the species name, the type of niche comparison, the observed D value, and then:
  # the results of equivalency testing: P value, mean D of the simulations, and the std error of that mean D
  # the results of similarity br to wi: ---
  # the results of similarity wi to br: ---
  
  
  resds <- data.frame(Species=splist[i], Comparison="real", D.obs=a$obs$D, eq.P=a$p.D, eq.mean.sim=mean(a$sim$D), eq.se.sim=se(a$sim$D), si12.P=b$p.D, si12.mean.sim=mean(b$sim$D), si12.se.sim=se(b$sim$D), si21.P=b2$p.D, si21.mean.sim=mean(b2$sim$D), si21.se.sim=se(b2$sim$D))
  simds <- data.frame(eq=a$sim$D, si12=b$sim$D, si21=b2$sim$D)
  names(simds) <- paste(splist[i], "real", names(simds), sep="_")
  
  
  ## Plotting is also changed- uses ecospat package rather than sourcing functions
  
  ecospat.plot.niche(z1,title=paste(splist[i],"PCA-env -Real- Br niche"),name.axis1="PC1",name.axis2="PC2")
  ecospat.plot.niche(z2,title=paste(splist[i],"PCA-env -Real- Wi niche"),name.axis1="PC1",name.axis2="PC2")
  ecospat.plot.contrib(pca.cal$co,pca.cal$eig)
  #plot.new(); text(0.5,0.5,paste("niche overlap:","\n","D=",round(as.numeric(ecospat.niche.overlap(z1,z2,cor=T)[1]),3)))
  ecospat.plot.overlap.test(a,"D","Equivalency")
  ecospat.plot.overlap.test(b,"D","Similarity 2->1")
  ecospat.plot.overlap.test(b2,"D","Similarity 1->2")
  
  
  #####################################################################################################################################
  
  #### Hypothetical niche overlaps (if migration doesn't happen) ####
  
  # only carry out for migratory species(ie those with different areas wi and br)
  if (resmig[i]=="migrant") {
    
    #### 2. Overlap between HYPOTHETICAL breeding and REAL wintering niches
    #Grouping factors 
    
    #background climate
    #wi background is as above
    back.wi.i <- which(names(complete)==paste(splist[i], "wi_back", sep="_")) # which column is (species i), wi_back #
    row.clim.wi<-which(complete[,back.wi.i] == 1) # Which rows are the wintering background for species i
    
    #background hypo br is grid cell IDs for background wi BUT in months of br
    back.br.i <- which(names(complete)==paste(splist[i], "br_back", sep="_"))  # which column is the breeding background for species i
    back.br.i.months <- unique(complete$Month[which(complete[,back.br.i]==1)]) # extract months for which species i breeds (breeding bg)
    back.wi.i.cells <- complete$ED_IT[which(complete[,back.wi.i]==1)] # extract grid cell IDs in which species i has its wintering background really
    row.clim.br <- which(is.element(complete$ED_IT, back.wi.i.cells) & is.element(complete$Month, back.br.i.months)) # background hypo br is the real wi background in the br months
    #summary(as.factor(complete$Month[row.clim.br])) # to check 
    
    
    #hypothetical breeding occurences and real wintering occurences#
    #real wintering occurences
    real.wi.i <- which(names(complete)==paste(splist[i], "wi","real", sep="_")) # which column is species i real winter occurence #
    row.wi<-which(complete[,real.wi.i] == 1) #which cells does species i occur in during the winter (winter occurence)
    #hypothetical breeding occurences
    hypo.br.i <- which(names(complete)==paste(splist[i],"br","hypo", sep="_")) # which column name has species i, then just "br_hypo"?
    row.br<-which(complete[,hypo.br.i] == 1) #Which rows does hispanica breed in hypothetically
    
    
    #Pull out the required PCA scores
    scores.clim.br<- complete[row.clim.br,c("Axis1","Axis2")] ### Pulls out the scores for Breeding background
    scores.clim.wi<- complete[row.clim.wi,c("Axis1","Axis2")] #### Pulls out the scores for Wintering background
    scores.clim.brwi<- complete[union(row.clim.br, row.clim.wi),c("Axis1","Axis2")] ### PULLS OUT THE SCORES FOR THE WHOLE BACKGROUND (Br and Wi)
    scores.br<- complete[row.br,c("Axis1","Axis2")] ### Pulls out scores for Breeding distribution
    scores.wi<- complete[row.wi,c("Axis1","Axis2")] #### Pulls out scores for wintering distribution
    
    #### calculation of occurence density and test of niche equivalency and similarity #### 
    #### Note this has changed to now use Ecospat ####
    z1<- ecospat.grid.clim.dyn(scores.clim.brwi,scores.clim.br,scores.br,R)
    z2<- ecospat.grid.clim.dyn(scores.clim.brwi,scores.clim.wi,scores.wi,R)
    a<-ecospat.niche.equivalency.test(z1,z2,rep=100)# test of niche equivalency and similarity according to Warren et al. 2008
    b<-ecospat.niche.similarity.test(z1,z2,rep=100)
    b2<-ecospat.niche.similarity.test(z2,z1,rep=100)
    
    
    
    
    #### add a line to the results table with results from the hypo br to real wi comparison ####
    tmp <- data.frame(Species=splist[i], Comparison="hypo_br", D.obs=a$obs$D, eq.P=a$p.D, eq.mean.sim=mean(a$sim$D), eq.se.sim=se(a$sim$D), si12.P=b$p.D, si12.mean.sim=mean(b$sim$D), si12.se.sim=se(b$sim$D), si21.P=b2$p.D, si21.mean.sim=mean(b2$sim$D), si21.se.sim=se(b2$sim$D))
    resds<-rbind(resds,tmp)
    
    simds2 <- data.frame(eq=a$sim$D, si12=b$sim$D, si21=b2$sim$D)
    names(simds2) <- paste(splist[i], "hypo_br", names(simds2), sep="_")
    simds <- cbind(simds, simds2)
    
    
    #plot
    
    
    ecospat.plot.niche(z1,title=paste(splist[i],"PCA-env Hypo- Br niche"),name.axis1="PC1",name.axis2="PC2")
    ecospat.plot.niche(z2,title=paste(splist[i],"PCA-env Real- Wi niche"),name.axis1="PC1",name.axis2="PC2")
    ecospat.plot.contrib(pca.cal$co,pca.cal$eig)
    plot.new(); text(0.5,0.5,paste("niche overlap:","\n","D=",round(as.numeric(ecospat.niche.overlap(z1,z2,cor=T)[1]),3)))
    ecospat.plot.overlap.test(a,"D","Equivalency")
    ecospat.plot.overlap.test(b,"D","Similarity 2->1")
    ecospat.plot.overlap.test(b2,"D","Similarity 1->2")
    
    
    
    
    ###### 3. Overlap between REAL breeding and HYPOTHETICAL wintering niches
    #grouping factors
    
    #background climate 
    #background br is as in REAL comparisons
    back.br.i <- which(names(complete)==paste(splist[i], "br_back", sep="_"))
    row.clim.br<-which(complete[,back.br.i] == 1) # breeding background 
    
    #background hypo wi is grid cell IDs for background br BUT in months of wi
    back.wi.i <- which(names(complete)==paste(splist[i], "wi_back", sep="_")) # which column name has species i, then just "wi_back"?
    back.br.i.cells <- complete$ED_IT[which(complete[,back.br.i]==1)] # extract grid cell IDs in which hispanica breeds really
    back.wi.i.months <- unique(complete$Month[which(complete[,back.wi.i]==1)]) # extract months in which hispanica winters really
    row.clim.wi <- which(is.element(complete$ED_IT, back.br.i.cells) & is.element(complete$Month, back.wi.i.months)) # background hypo wi is the real br background in the wi months
    summary(as.factor(complete$Month[row.clim.wi])) # to check 
    
    
    #### real breeding and hypothetical wintering occurences####
    real.br.i <- which(names(complete)==paste(splist[i],"br","real", sep="_")) # which column name has species i, then just "br_real"?
    row.br<-which(complete[,real.br.i] == 1) #Which rows does hispanica breed in really (breeding dist in breeding months)
    hypo.wi.i <- which(names(complete)==paste(splist[i], "wi","hypo", sep="_"))
    row.wi<-which(complete[,hypo.wi.i] == 1) #which rows does hispanica winter in hypothetically  
    
    ##### PULL OUT THE REQUIRED PCA SCORES ####
    
    scores.clim.br<- complete[row.clim.br,c("Axis1","Axis2")] ### Pulls out the scores for Breeding background
    scores.clim.wi<- complete[row.clim.wi,c("Axis1","Axis2")] #### Pulls out the scores for Wintering background
    scores.clim.brwi<- complete[union(row.clim.br, row.clim.wi),c("Axis1","Axis2")] ### PULLS OUT THE SCORES FOR THE WHOLE BACKGROUND (Br and Wi)
    scores.br<- complete[row.br,c("Axis1","Axis2")] ### Pulls out scores for Breeding distribution
    scores.wi<- complete[row.wi,c("Axis1","Axis2")] #### Pulls out scores for wintering distribution
    
    #### calculation of occurence density and test of niche equivalency and similarity #### 
    #### This has been changed to include ecospat
    
    z1<- ecospat.grid.clim.dyn(scores.clim.brwi,scores.clim.br,scores.br,R)
    z2<- ecospat.grid.clim.dyn(scores.clim.brwi,scores.clim.wi,scores.wi,R)
    a<-ecospat.niche.equivalency.test(z1,z2,rep=100)# test of niche equivalency and similarity according to Warren et al. 2008
    b<-ecospat.niche.similarity.test(z1,z2,rep=100)
    b2<-ecospat.niche.similarity.test(z2,z1,rep=100)
    
    
    
    #### add a line to the results table with results from the hypo br to real wi comparison ####
    tmp <- data.frame(Species=splist[i], Comparison="hypo_wi", D.obs=a$obs$D, eq.P=a$p.D, eq.mean.sim=mean(a$sim$D), eq.se.sim=se(a$sim$D), si12.P=b$p.D, si12.mean.sim=mean(b$sim$D), si12.se.sim=se(b$sim$D), si21.P=b2$p.D, si21.mean.sim=mean(b2$sim$D), si21.se.sim=se(b2$sim$D))
    resds<-rbind(resds,tmp)
    
    simds2 <- data.frame(eq=a$sim$D, si12=b$sim$D, si21=b2$sim$D)
    names(simds2) <- paste(splist[i], "hypo_wi", names(simds2), sep="_")
    simds <- cbind(simds, simds2)
    
    
    #plot- change to use ecospat 
    
    ecospat.plot.niche(z1,title=paste(splist[i],"PCA-env Real- Br niche"),name.axis1="PC1",name.axis2="PC2")
    ecospat.plot.niche(z2,title=paste(splist[i],"PCA-env Hypo- Wi niche"),name.axis1="PC1",name.axis2="PC2")
    ecospat.plot.contrib(pca.cal$co,pca.cal$eig)
    plot.new(); text(0.5,0.5,paste("niche overlap:","\n","D=",round(as.numeric(ecospat.niche.overlap(z1,z2,cor=T)[1]),3)))
    ecospat.plot.overlap.test(a,"D","Equivalency")
    ecospat.plot.overlap.test(b,"D","Similarity 2->1")
    ecospat.plot.overlap.test(b2,"D","Similarity 1->2")
    
    
  } # end of "if" statement for migratory species
  
  # when running the loop on to next species, make sure to keep all results so far
  # growing datasets
  if (i==1) (resdsall <- resds) else (resdsall <- rbind(resdsall, resds))
  if (i==1) (simdsall <- simds) else (simdsall <- cbind(simdsall, simds))
  
  
  
}
dev.off() # close the pdf with all the plots in

resdsall$migration <- resmig[match(resdsall$Species, splist)] # make a column which tells you whether species is resident or migratory

##### save resds
save(resdsall, file="resds_ecospat_sig.Rdata")
write.table(resdsall,"resds_ecospat_sig.txt",sep='\t',quote=FALSE,row.names=F,col.names=T)
##### save simds
save(simdsall, file="simds_ecospat_sig.Rdata")