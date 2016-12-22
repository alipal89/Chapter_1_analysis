#### I HAVE THE 3 MONTHS OF BREEDING FOR ALL OF MY SPECIES 
#### I WOULD LIKE TO KNOW WHICH 3 MONTHS THEY WINTER IN

setwd("C:/Alison/Chapter 1/Analysis")


#### Breeding months table for species
Months<-read.table("161202_Br_months_ENClades.txt",sep="\t",header=T) 

#### Problem: Breeding months were found for IOC5.1 
#### Range maps are in IOC3.1 
#### I need to change the breeding months species to match IOC3.1
#### I have a matching table
Months$IOC5_1_Binomial<-paste(Months$Genus, Months$Species, sep="_")

Match_table<-read.table("Taxonomy_IOC_5.1_3.1_for_R.txt",sep="\t",header=T) 
head(Match_table)
Match_table$IOC_5.1_binomial<-paste(Match_table$IOC_5.1_genus, Match_table$IOC_5.1_species, sep="_")
Match_table$IOC_3.1_binomial<-paste(Match_table$IOC_3.1_genus, Match_table$IOC_3.1_species, sep="_")
match<-Match_table[,c(1,4,12,13)]
head(match)
head(Months)
matched<-merge(Months,match,by.x="IOC5_1_Binomial", by.y="IOC_5.1_binomial",all =T )
head(matched)
colnames(matched)
#### Determine the 3 months that species are non-breeding 
#### This is the 3 months opposite the breeding months

### BREEDING MONTH NUMBERS
colnames(matched)
# Some species have no breeding months
# I will omit these for now

matched<-matched[!is.na(matched$B1),]

month.fun<-function(x){grep(x,month.name);}
colnames(matched)
br<-matched[5:7] ## 3 columns of breeding months


vector<-1:dim(br)[2] # Vector of the number of columns (will always be 3 columns)
for (i in seq(along=vector)){
  #i<-1
  br.i<-br[,i] ## Extracts 1 column 
  name<-paste("B",i,"_number",sep="") # Name is B#_number
  results<-sapply(br.i,month.fun)
  assign(name, results) ## We have 3 new strings of integers which are the breeding month numbers
  
}

Br_numbers<-cbind(B1_number,B2_number,B3_number)

## Sanity check
head(matched)
head(Br_numbers)
Months[322,]
Br_numbers[322,]


##### Wintering months function #####
##### Determine the months directly opposite the breeding month 
##### If you breed in January you winter 6 months later in July 
##### If the breeding month is between 1 and 6, you add 6
##### If the breeding month is between 7-12, you subtract 6

wi_months <- function(x) {
  if(x<=6){
    w1<-x+6
  }else{
    w1<-x-6
  }
  return(w1)
} 

vector<-1:dim(Br_numbers)[2] ### 3 breeding columns to apply function accross  
for (i in seq(along=vector)){
  #i<-1
  Breeding.i<-Br_numbers[,i] 
  name<-paste("W",i,"_number",sep="")
  results<-sapply(Breeding.i,wi_months)
  assign(name,results)
}

Wi_numbers<-cbind(W1_number,W2_number,W3_number)

#####################################################################
##### Save breeding and wintering months with the species info

colnames(matched)
################ Breeding months/season ID ############################# 
Breeding<-data.frame(matched$IOC_3.1_binomial,matched$EN_Clade,Br_numbers)
head(Breeding)
Breeding$New<-NA
Breeding$New<-"Br"
head(Breeding)
names(Breeding)<-c("Species","EN_Clade","1","2","3","Season")


################ wintering months/season ID ############################# 
Wintering<-data.frame(matched$IOC_3.1_binomial,matched$EN_Clade,Wi_numbers)
head(Wintering)
Wintering$New<-NA
Wintering$New<-"Wi"
head(Wintering)
names(Wintering)<-c("Species","EN_Clade","1","2","3","Season")
head(Wintering)


#### Bind them together and save 
clade_months<-rbind(Breeding,Wintering)
clade_months
write.table(clade_months, "months_clade.txt", sep="\t", col.names=TRUE)

