####

#### Carry out PCA on the climatic variables for each clade individually
#### Plot it
#### Save the PC scores

setwd("C:/Alison/Chapter 1/Analysis")
library(ecospat)
source("Scripts/plotPCAfunction.R") ## Modified from ecospat so that you can add in title

### load data ### 
clade_pca<-load("realm_month_ENClade.Rdata") 
summary(realm_month_ENClade) ## All the climate that needs to be included for each clade


## Loop through each clade and carry out PCA

Cdlist<-unique(realm_month_ENClade$ENClade)


pdf("Results/Niche_overlap_ENClades_temperature.pdf", width=8, height=10)

for (i in seq(along=Cdlist)){
#  i<-2
  print(paste("Number", i, "Clade", Cdlist[i], sep=" "))
  clade.i<-realm_month_ENClade[which(realm_month_ENClade$ENClade==Cdlist[i]),]
  summary(clade.i) # There are 30 NAs? 
  row.has.na <- apply(clade.i, 1, function(x){any(is.na(x))})
  sum(row.has.na)
  clade.i <- clade.i[!row.has.na,]

  Xvar<-c(1:2)
  nvar<-length(Xvar)
  data.env.occ<-(clade.i)[,Xvar]#
  
  pca.cal <-dudi.pca(data.env.occ, center = TRUE, scale = T, scannf = F, nf = 2)
  PCA_plot_function(pca.cal$co,pca.cal$eig, title=paste(Cdlist[i]))
  pca.clade.i<-cbind(clade.i,pca.cal$li)
  pca.clade.i$ENclade<-Cdlist[i]
  
  if (i==1) (pca.clade.ENclade <- pca.clade.i) else (pca.clade.ENclade <- rbind(pca.clade.ENclade, pca.clade.i))
  
  
}
dev.off()


summary(pca.clade.ENclade)

### Save the PC scores for each of the clades 
save(pca.clade.ENclade, file="temperature_PCs_ENClade.Rdata")
