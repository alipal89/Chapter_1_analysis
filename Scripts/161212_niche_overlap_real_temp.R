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

rm(list = ls())

setwd("C:/Alison/Chapter 1/Analysis")




#################################################################################################
############################## preparation of datasets ##########################################
#################################################################################################

## Load in PCscores for each clade
#clade_pca<-load("PCs_ENClade.Rdata")
clade_pca<-load("temperature_PCs_ENClade.Rdata")
clade_pca
pca.clade.ENclade
colnames(pca.clade.ENclade)

# Description: climate variables & PC scores for each clade (backgrounds)
# From this I need to be able to extract species specific PC scores

# Filtered occurence data from Susanne- Need to check seems to include a species which has just one occurence
species_occ<-read.table("AllGridded_FDB_CPH_filtered.txt",sep = "\t", header=T)
summary(as.factor(species_occ$IOC3_1_Binomial))
colnames(species_occ) # species,gridcell, range type

# Checking the data
table(species_occ$IOC3_1_Binomial, species_occ$Range_type)


## In order to extract the right PCs for a species. I need to know which months it breeds and non-breeds in 
Months_clade<-read.table("Months_clade_migration.txt",sep = "\t", header=T)
colnames(Months_clade) # Which months br & which realm br (have also added in migration now)



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



## Shannon diversity function for niche breadth 
# Shannon diversity index function 
shannon<-function(x){
  log<-log(x/sum(x))
  if !is.na(log) log <- sapply(log, is.infinite) ### replace infinite values with 0s
  -1*sum((log*(x/sum(x))),na.rm=TRUE)
}


####################################### PCA #####################################################
#### Description: We perform 1 PCA accross all species in one clade
#### In this PCA we include the background for all species in a clade
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE BREEDS
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE NON-BREEDS
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE HYPO-BREEDS
#### ALL THE MONTHS AND REALMS FOR WHICH THE CLADE HYPO-NON-BREEDS

#### THIS HAS ALREADY BEEN DONE FOR EACH CLADE
#### SO FOR EACH CLADE WE HAVE THE PC SCORES FOR THE ENVIRONMENTAL SPACE

#################################################################################################
#################################### PCA-ENV ####################################################
##############Loop through species in clade to carry out overlap and equivalancy ################


############ Take one clade example
head(pca.clade.ENclade)
Cdlist<-unique(pca.clade.ENclade$ENClade)
Cdlist[3]
i<-3
#### SUBSET THE PCA DATA SO THAT WE ONLY WORK WITH CLADE 1: EXAMPLE XOLMIINI
pca.clade.i<-pca.clade.ENclade[which(pca.clade.ENclade$ENclade==Cdlist[i]),]
summary(pca.clade.i)
#### Make a species list for clade 

splist<-unique(Months_clade$Species[which(Months_clade$ENClade==Cdlist[i])])
length(splist)
### Make a vector(resmig - which has the identity of whether a species is a migrant)
resmig<-Months_clade[which(Months_clade$ENClade==Cdlist[i]),]
resmig<-resmig[!duplicated(resmig$Species), ]
resmig<-resmig$Migration

###########################################################################
pdf("Results/161212Niche_overlap_clade_3_rep1_temp.pdf", width=8, height=10)
# layout of graphs on the page

layout(matrix(c(1,1,2,2,3,3,4,5), 2, 4, byrow = TRUE))
#################################################################################################
#################################### PCA-ENV ####################################################
##############Loop through species in clade to carry out overlap and equivalancy ################


for (i in seq(along=splist)) {
if(i==16) next 

  
  print(paste("Number", i, "Species", splist[i], sep=" ")) # print off where the loop is
  
 
  #################################################################################################
  ############################## FOR EACH SPECIES PULL OUT REQUIRED PC SCORES #####################
  #################################################################################################
  
  
  
  #### Scores that we need to pull out
  # 1. Real breeding background
  # 2. Real wintering background
  # 3. hypothetical breeding background
  # 4. hypothetical wintering background
  # 5. Real breeding occurrences
  # 6. Real wintering occurences
  # 7. hypothetical breeding occurences
  # 8. hypothetical wintering occurences
  

  
  # 1. PCA SCORES FOR THE REAL BREEDING BACKGROUND
  row.br.i<-Months_clade[which(Months_clade$Species==paste(splist[i]) & Months_clade$Season=="Br"),]
  row.br.i<-unique(row.br.i)
  
  for (j in seq(1:dim(row.br.i)[1])){
    #print(j)
    #j<-1  
    temp<-pca.clade.i[which(pca.clade.i$Month==paste(row.br.i$Month[j])),]
    temp<-temp[which(temp$Realm==paste(row.br.i$Realm[j])),]  
    if (j==1) (row.clim.br<- temp) else (row.clim.br <- rbind(row.clim.br, temp))
  }
  
  real.clim.br<-row.clim.br[c("Axis1","Axis2")]
  
  # 2. PCA SCORES FOR THE REAL WINTERING BACKGROUND
  row.wi.i<-Months_clade[which(Months_clade$Species==paste(splist[i]) & Months_clade$Season=="Wi"),]
  row.wi.i<-unique(row.wi.i)
  
  for (j in seq(1:dim(row.wi.i)[1])){
    #print(j)
    #j<-3  
    temp<-pca.clade.i[which(pca.clade.i$Month==paste(row.wi.i$Month[j])),]
    temp<-temp[which(temp$Realm==paste(row.wi.i$Realm[j])),]  
    if (j==1) (row.clim.wi<- temp) else (row.clim.wi <- rbind(row.clim.wi, temp))
    
  }
  
  real.clim.wi<-row.clim.wi[c("Axis1","Axis2")]
  
  # 3. Hypothetical breeding background
  # Species stays in Wintering range
  # In breeding month 
  row.hypo.br.i<-Months_clade[which(Months_clade$Species==paste(splist[i]) & Months_clade$Season=="Hypo_Br"),]
  row.hypo.br.i<-unique(row.hypo.br.i)                          
  for (j in seq(1:dim(row.hypo.br.i)[1])){
    temp<-pca.clade.i[which(pca.clade.i$Month==paste(row.hypo.br.i$Month[j])),]
    temp<-temp[which(temp$Realm==paste(row.hypo.br.i$Realm[j])),]  
    if (j==1) (row.clim.hypo.br<- temp) else (row.clim.hypo.br <- rbind(row.clim.hypo.br, temp))  
  }

hypo.clim.br<-row.clim.hypo.br[c("Axis1","Axis2")]

# 4. Hypothetical wintering background
  # Species stays in Breeding range
  # In wintering months
  row.hypo.wi.i<-Months_clade[which(Months_clade$Species==paste(splist[i]) & Months_clade$Season=="Hypo_Wi"),]
  row.hypo.wi.i<-unique(row.hypo.wi.i)
  for (j in seq(1:dim(row.hypo.wi.i)[1])){
    #j<-1
    temp<-pca.clade.i[which(pca.clade.i$Month==paste(row.hypo.wi.i$Month[j])),]  # Winter months
    temp<-temp[which(temp$Realm==paste(row.hypo.wi.i$Realm[j])),]  
    
    
    if (j==1) (row.clim.hypo.wi<- temp) else (row.clim.hypo.wi <- rbind(row.clim.hypo.wi, temp))
    
    }
  hypo.clim.wi<-row.clim.hypo.wi[c("Axis1","Axis2")]
  
  # 5. Real breeding occurences
  # These need to be selected from the real breeding background for the species
  # Which grid cells does species i occupy in breeding season (br+year round)
  real.br.i<-species_occ$WM_ID[which(species_occ$IOC3_1_Binomial==paste(splist[i]) & species_occ$Range_type!="non-breeding")]
  length(real.br.i)
  real.br.i<-row.clim.br[row.clim.br$WM_ID %in% real.br.i,] # Think this is right. We are missing a couple but these might be the cells which had no climate?
  dim(real.br.i) # 69 missing - I think these are grid cells which do not have climate data. Check
  real.scores.br<- real.br.i[,c("Axis1","Axis2")] ### Pulls out scores for Breeding distribution
  
  # 6. Real wintering occurences 
  real.wi.i<-species_occ$WM_ID[which(species_occ$IOC3_1_Binomial==paste(splist[i]) & species_occ$Range_type!="breeding")]
  length(real.wi.i) # 262 occurences. Should be 786 in total
  # Now i want these grid squares but only for the months that it winters
  real.wi.i<-row.clim.wi[row.clim.wi$WM_ID %in% real.wi.i,] # Think this is right. We are missing a couple but these might be the cells which had no climate?
  dim(real.wi.i) ## 
  real.scores.wi<- real.wi.i[,c("Axis1","Axis2")] ### Pulls out scores for Breeding distribution
 
 # 7. Hypo-breeding occurences
  # Non-breeding occurences in the breeding months
  # SO SELECT THIS FROM THE HYPO BREEDING BG
  hypo.br.i<-species_occ$WM_ID[which(species_occ$IOC3_1_Binomial==paste(splist[i]) & species_occ$Range_type!="breeding")]
  head(hypo.br.i)
# Now i want these grid squares but only for the months that it breeds
  hypo.br.i<-row.clim.hypo.br[row.clim.hypo.br$WM_ID %in% hypo.br.i,] # Think this is right. We are missing a couple but these might be the cells which had no climate?
  hypo.scores.br<- hypo.br.i[,c("Axis1","Axis2")] ### Pulls out scores for Breeding distribution
  
  # 8. Hypo wintering occurences
  # Breeding occurences in the non-breeding months
  # So select from the non-breeding background
  hypo.wi.i<-species_occ$WM_ID[which(species_occ$IOC3_1_Binomial==paste(splist[i]) & species_occ$Range_type!="non-breeding")]
  dim(hypo.br.i)
  # Now i want these grid squares but only for the winter months 
  hypo.wi.i<-row.clim.hypo.wi[row.clim.hypo.wi$WM_ID %in% hypo.wi.i,] # Think this is right. We are missing a couple but these might be the cells which had no climate?
  
  hypo.scores.wi<- hypo.wi.i[,c("Axis1","Axis2")] ### Pulls out scores for Breeding distribution
  
  # In addition 
  # Also need to have the global climates for 3 senarios:
  # A. Real Breeding + Real Wintering
  real.clim.brwi<- rbind(real.clim.br,real.clim.wi)
  
# B. Real Breeding + hypo wintering
  realhypo.clim.brwi<- rbind(real.clim.br, hypo.clim.wi)
  # C. Hypo Breeding + Real Wintering 
  hyporeal.clim.brwi<- rbind(hypo.clim.br, real.clim.wi)
  
  
 
 
  ##############################################################################################################
  #################################################################################################
  ############################## GRID AND KERNAL SMOOTHING ###########################################
  #################################################################################################

  #### REAL BREEDING AND REAL NON-BREEDING CLIMATIC NICHES
  # Gridding the breeding niche
  
  z1<- ecospat.grid.clim.dyn(real.clim.brwi,real.clim.br,real.scores.br,R)
  # Gridding the wintering niche
  z2<- ecospat.grid.clim.dyn(real.clim.brwi,real.clim.wi,real.scores.wi,R)
  
  
  ##############################################################################################################
  #################################################################################################
  ############################## NICHE QUANTIFICATION  ###########################################
  #################################################################################################
  
  
  # Niche overlap
  D.overlap <-  ecospat.niche.overlap(z1,z2, cor=T)$D
  
  # Perform the Niche Equivalency Test with according to Warren et al. (2008)
  # H1: Is the overlap between the breeding and wintering niche higher than 2 random niches.  
  a<-ecospat.niche.equivalency.test(z1,z2,rep=1,alternative="greater")
  
  # Perform the Niche Similarity test-  
  #H1: Is the overlap between the breeding and winter higher than when the winter niche is randomly chosen from the winter study area
  # If rand.type = 1, both z1 and z2 are randomly shifted, if rand.type =2, only z2 is randomly shifted
  # alternative specifies if you want to test for niche conservatism (alternative = "greater", 
  # i.e.  the niche overlap is more equivalent/similar than random) or for niche divergence (alternative = "lower",
  # i.e. the niche overlap is less equivalent/similar than random).
  b<-ecospat.niche.similarity.test(z1,z2,rep=1, alternative="greater", rand.type=2)
  b2<-ecospat.niche.similarity.test(z2,z1,rep=1, alternative="greater", rand.type=2)
  
  # Niche Breadth
  # This example calculates the breadth for Breeding and winter niche separately and then sums them to make annual breadth
  # Justification: If you do grid clim for these two together- the breeding niche is generally smaller so might be biased against.
  # I have another code doing grid clim for annual. I should check whether there is a difference in results
  

  
  Br_breadth<-z1$z.unco@data@values
  Br_breadth<-shannon(Br_breadth)
  Wi_breadth<-z2$z.unco@data@values
  Wi_breadth<-shannon(Wi_breadth)
  An_breadth<-sum(Br_breadth,Wi_breadth)
  
  ### Niche expansion, Stability and unfilling   
  # Niche expansion: The proportion of climatic space only colonised in invaded range (so e.g. climatic space occupied in winter that is not occupied in Breeding)
  # Niche Stability: Portion of climatic space occupied in Both breeding and wintering season
  # Niche unfilling: Portion of niche space only colonised in the native range. (e.g in Breeding season)
  # These indices were measured in climatic space shared between the native and invaded extents (breeding and wintering so that they dont just detect shifts in the availability of climatic conditions btw seasons)  
  filling.i<-ecospat.niche.dyn.index (z1, z2, intersection=NA)
  
  # save results for niche quantification: make a results dataframe with one line, where we 
  # save the species name, the type of niche comparison, the observed D value, and then:
  # the results of equivalency testing: P value, mean D of the simulations, and the std error of that mean D
  # the results of similarity br to wi: ---
  # the results of similarity wi to br: ---
  
  resds <- data.frame(Species=splist[i], Comparison="real", D.obs=a$obs$D, eq.P=a$p.D,eq.mean.sim=mean(a$sim$D), eq.se.sim=se(a$sim$D), si12.P=b$p.D, si12.mean.sim=mean(b$sim$D), si12.se.sim=se(b$sim$D), si21.P=b2$p.D, si21.mean.sim=mean(b2$sim$D), si21.se.sim=se(b2$sim$D), an_breadth=An_breadth,Br_breadth=Br_breadth,Wi_breadth=Wi_breadth, Niche_expansion=unname(filling.i$dynamic.index.w[1]),Niche_stability=unname(filling.i$dynamic.index.w[2]), Niche_unfilling=unname(filling.i$dynamic.index.w[3]))
  simds <- data.frame(eq=a$sim$D, si12=b$sim$D, si21=b2$sim$D)
  names(simds) <- paste(splist[i], "real", names(simds), sep="_")
  
  
  #################################################################################################
  #################################################################################################
  ########################################### PLOTTING REAL########################################
  #################################################################################################
  ## I think that breeding is Green
  ## Wintering is Red 
  
  ecospat.plot.niche.dyn (z1,z2, quant=0.25, interest=2, title= paste(splist[i]," seasonal overlap",sep=""), name.axis1="PC1", name.axis2="PC2")
  ecospat.shift.centroids (real.scores.br, real.scores.wi, real.clim.br, real.clim.wi)
  plot.new(); text(0.5,0.5,paste("niche overlap:","\n","D=",round(as.numeric(ecospat.niche.overlap(z1,z2,cor=T)[1]),3)))
  ecospat.plot.overlap.test(a,"D","Equivalency")
  ecospat.plot.overlap.test(b,"D","Similarity 2->1")
  ecospat.plot.overlap.test(b2,"D","Similarity 1->2")
  
  ###########################################################################################################  
  #### Calculate hypothetical niche overlaps (if migrations doesn't happen)
  #### This is only carried out for migratory species(ie those with different areas wi and br)
  if (resmig[i]=="mig") {
  
    # A.  OVERLAP BETWEEN HYPOTHETICAL BREEDING NICHE and REAL WINTERING NICHE
    # i.e the species stays in the wintering range all year round
    # predict the scores on the axes
    
    # Need to include
    # Gridding
    # z calcs
    
    ##############################################################################################################
    #################################################################################################
    ############################## GRID AND KERNAL SMOOTHING ###########################################
    #################################################################################################
    
    #### HYPO BREEDING AND REAL NON-BREEDING CLIMATIC NICHES
    # Gridding the breeding niche
    z1<- ecospat.grid.clim.dyn(hyporeal.clim.brwi,hypo.clim.br,hypo.scores.br,R)
    # Gridding the wintering niche
    z2<- ecospat.grid.clim.dyn(hyporeal.clim.brwi,real.clim.wi,real.scores.wi,R)
    
    
    ##############################################################################################################
    #################################################################################################
    ############################## NICHE QUANTIFICATION  ###########################################
    #################################################################################################
    
    
    # Niche overlap
    D.overlap <-  ecospat.niche.overlap(z1,z2, cor=T)$D
    
    # Perform the Niche Equivalency Test with according to Warren et al. (2008)
    # H1: Is the overlap between the breeding and wintering niche higher than 2 random niches.  
    a<-ecospat.niche.equivalency.test(z1,z2,rep=1,alternative="greater")
    
    # Perform the Niche Similarity test-  
    #H1: Is the overlap between the breeding and winter higher than when the winter niche is randomly chosen from the winter study area
    # If rand.type = 1, both z1 and z2 are randomly shifted, if rand.type =2, only z2 is randomly shifted
    # alternative specifies if you want to test for niche conservatism (alternative = "greater", 
    # i.e.  the niche overlap is more equivalent/similar than random) or for niche divergence (alternative = "lower",
    # i.e. the niche overlap is less equivalent/similar than random).
    b<-ecospat.niche.similarity.test(z1,z2,rep=1, alternative="greater", rand.type=2)
    b2<-ecospat.niche.similarity.test(z2,z1,rep=1, alternative="greater", rand.type=2)
    
    
    ### Niche expansion, Stability and unfilling   
    # Niche expansion: The proportion of climatic space only colonised in invaded range (so e.g. climatic space occupied in winter that is not occupied in Breeding)
    # Niche Stability: Portion of climatic space occupied in Both breeding and wintering season
    # Niche unfilling: Portion of niche space only colonised in the native range. (e.g in Breeding season)
    # These indices were measured in climatic space shared between the native and invaded extents (breeding and wintering so that they dont just detect shifts in the availability of climatic conditions btw seasons)  
    filling.i<-ecospat.niche.dyn.index (z1, z2, intersection=NA)
    
    
  
    # add a line to the results table with results from the hypo br to real wi comparison 
    tmp <- data.frame(Species=splist[i], Comparison="hypo_br", D.obs=a$obs$D, eq.P=a$p.D, eq.mean.sim=mean(a$sim$D), eq.se.sim=se(a$sim$D), si12.P=b$p.D, si12.mean.sim=mean(b$sim$D), si12.se.sim=se(b$sim$D), si21.P=b2$p.D, si21.mean.sim=mean(b2$sim$D), si21.se.sim=se(b2$sim$D),an_breadth=NA,Br_breadth=NA,Wi_breadth=NA,Niche_expansion=unname(filling.i$dynamic.index.w[1]),Niche_stability=unname(filling.i$dynamic.index.w[2]), Niche_unfilling=unname(filling.i$dynamic.index.w[3]))
    resds<-rbind(resds,tmp)
    simds2 <- data.frame(eq=a$sim$D, si12=b$sim$D, si21=b2$sim$D)
    names(simds2) <- paste(splist[i], "hypo_br", names(simds2), sep="_")
    simds <- cbind(simds, simds2)
    
    
    
    #################################################################################################
    #################################################################################################
    ########################################### PLOTTING HYPO-BR REAL WI#############################
    #################################################################################################
    ## I think that breeding is Green
    ## Wintering is Red 
    
    ecospat.plot.niche.dyn (z1,z2, quant=0.25, interest=2, title= paste(splist[i]," seasonal overlap-hypo.br",sep=""), name.axis1="PC1", name.axis2="PC2")
    ecospat.shift.centroids (hypo.scores.br, real.scores.wi, hypo.clim.br, real.clim.wi)
    plot.new(); text(0.5,0.5,paste("niche overlap:","\n","D=",round(as.numeric(ecospat.niche.overlap(z1,z2,cor=T)[1]),3)))
    ecospat.plot.overlap.test(a,"D","Equivalency")
    ecospat.plot.overlap.test(b,"D","Similarity 2->1")
    ecospat.plot.overlap.test(b2,"D","Similarity 1->2")
    
    
    # b. Overlap between REAL breeding and HYPOTHETICAL wintering niches
    # i.e. if a species stayed in the breeding site all year
 
    ##############################################################################################################
    #################################################################################################
    ############################## GRID AND KERNAL SMOOTHING ###########################################
    #################################################################################################
    
    #### REAL BREEDING AND HYPO NON-BREEDING CLIMATIC NICHES
    # Gridding the breeding niche
    z1<- ecospat.grid.clim.dyn(realhypo.clim.brwi,real.clim.br,real.scores.br,R)
    # Gridding the wintering niche
    z2<- ecospat.grid.clim.dyn(realhypo.clim.brwi,hypo.clim.wi,hypo.scores.wi,R)
    
    
    ##############################################################################################################
    #################################################################################################
    ############################## NICHE QUANTIFICATION  ###########################################
    #################################################################################################
    
    
    # Niche overlap
    D.overlap <-  ecospat.niche.overlap(z1,z2, cor=T)$D
    
    # Perform the Niche Equivalency Test with according to Warren et al. (2008)
    # H1: Is the overlap between the breeding and wintering niche higher than 2 random niches.  
    a<-ecospat.niche.equivalency.test(z1,z2,rep=1,alternative="greater")
    
    # Perform the Niche Similarity test-  
    #H1: Is the overlap between the breeding and winter higher than when the winter niche is randomly chosen from the winter study area
    # If rand.type = 1, both z1 and z2 are randomly shifted, if rand.type =2, only z2 is randomly shifted
    # alternative specifies if you want to test for niche conservatism (alternative = "greater", 
    # i.e.  the niche overlap is more equivalent/similar than random) or for niche divergence (alternative = "lower",
    # i.e. the niche overlap is less equivalent/similar than random).
    b<-ecospat.niche.similarity.test(z1,z2,rep=1, alternative="greater", rand.type=2)
    b2<-ecospat.niche.similarity.test(z2,z1,rep=1, alternative="greater", rand.type=2)
    
  
    ### Niche expansion, Stability and unfilling   
    # Niche expansion: The proportion of climatic space only colonised in invaded range (so e.g. climatic space occupied in winter that is not occupied in Breeding)
    # Niche Stability: Portion of climatic space occupied in Both breeding and wintering season
    # Niche unfilling: Portion of niche space only colonised in the native range. (e.g in Breeding season)
    # These indices were measured in climatic space shared between the native and invaded extents (breeding and wintering so that they dont just detect shifts in the availability of climatic conditions btw seasons)  
    filling.i<-ecospat.niche.dyn.index (z1, z2, intersection=NA)
    
    
    #### add a line to the results table with results from the hypo br to real wi comparison ####
    tmp <- data.frame(Species=splist[i], Comparison="hypo_wi", D.obs=a$obs$D, eq.P=a$p.D, eq.mean.sim=mean(a$sim$D), eq.se.sim=se(a$sim$D), si12.P=b$p.D, si12.mean.sim=mean(b$sim$D), si12.se.sim=se(b$sim$D), si21.P=b2$p.D, si21.mean.sim=mean(b2$sim$D), si21.se.sim=se(b2$sim$D),an_breadth=NA,Br_breadth=NA,Wi_breadth=NA,Niche_expansion=unname(filling.i$dynamic.index.w[1]),Niche_stability=unname(filling.i$dynamic.index.w[2]), Niche_unfilling=unname(filling.i$dynamic.index.w[3]))
    resds<-rbind(resds,tmp)
    
    simds2 <- data.frame(eq=a$sim$D, si12=b$sim$D, si21=b2$sim$D)
    names(simds2) <- paste(splist[i], "hypo_wi", names(simds2), sep="_")
    simds <- cbind(simds, simds2)
    
    #################################################################################################
    #################################################################################################
    ########################################### PLOTTING REAL BR HYPO WI#############################
    #################################################################################################
    ## I think that breeding is Green
    ## Wintering is Red 
    
    ecospat.plot.niche.dyn (z1,z2, quant=0.25, interest=2, title= paste(splist[i]," seasonal overlap-hypo wi",sep=""), name.axis1="PC1", name.axis2="PC2")
    ecospat.shift.centroids (real.scores.br, hypo.scores.wi, real.clim.br, hypo.clim.wi)
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
dev.off()
length(unique(resdsall$Species))
length(resmig) # need to omit 44 and 48 
# For clade 2
resmig2<-resmig[c(-16)]
length(resmig2)
resdsall$migration <- resmig2[match(resdsall$Species, splist)] # make a column which tells you whether species is resident or migratory
resdsall$clade<-paste(Cdlist[3])

head(resdsall)
##### save resds
save(resdsall, file="Results/Clade2_Hirundinidae/resds_ecospat_realm_niche_clade2_temp.Rdata")
write.table(resdsall,"Results/Clade2_Hirundinidae/resds_ecospat_realm_clade2_temp.txt",sep='\t',quote=FALSE,row.names=F,col.names=T)
##### save simds
#save(simdsall, file="Results/Clade2_Turdus/simds_ecospat_realm_clade8.Rdata")
