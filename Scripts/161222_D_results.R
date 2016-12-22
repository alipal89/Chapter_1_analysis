# PLotting D values
setwd("C:/Alison/Chapter 1/Analysis/Results/16.12.21.Results_2")

# Read in the data
Clade_1<-read.table("resds_ecospat_realm_clade1.txt",sep = "\t", header=T)
Clade_2<-read.table("resds_ecospat_realm_clade2.txt",sep = "\t", header=T)
Clade_3<-read.table("resds_ecospat_realm_clade3.txt",sep = "\t", header=T)
Clade_4<-read.table("resds_ecospat_realm_clade4.txt",sep = "\t", header=T)
Clade_5<-read.table("resds_ecospat_realm_clade5.txt",sep = "\t", header=T)
Clade_6<-read.table("resds_ecospat_realm_clade6.txt",sep = "\t", header=T)
Clade_7<-read.table("resds_ecospat_realm_clade7.txt",sep = "\t", header=T)
Clade_8<-read.table("resds_ecospat_realm_clade8.txt",sep = "\t", header=T)

ENClade_results<-rbind(Clade_1,Clade_2,Clade_3,Clade_4,Clade_5,Clade_6,Clade_7,Clade_8)

head(ENClade_results)
summary(ENClade_results$clade)

### Note I am missing some species: no breeding information, dodgy range, too small range 
length(unique(ENClade_results$Species)) # 394 species currently 

### I need to add in information about migration 
### Add in information about area 
setwd("C:/Alison/Chapter 1/Analysis")
Range_migration<-read.table("Months_clade_migration_area.txt",sep = "\t", header=T)
Range_migration<-unique(Range_migration[,c(1,2,6,7,8,9,10)])
head(Range_migration)

ENClade_results_range<-merge(ENClade_results,Range_migration)
dim(ENClade_results_range)


### Add in Susannes data on migration 
Migration<-read.table("SpeciesList3_1_TaxonomyMigrationSummary_ENgroups.txt",sep = "\t", header=T)
head(Migration)
colnames(Migration)
length(Migration$IOC3_1_Binomial)

ENClade_results_range_migration<-merge(ENClade_results_range,Migration, by.x="Species",by.y="IOC3_1_Binomial")
dim(ENClade_results_range_migration)
length(unique(ENClade_results_range_migration$Species))

### check it matched up ok
head(ENClade_results_range_migration)
colnames(ENClade_results_range_migration)
ENClade_results_range_migration[,c(1,17,25)]

### SAVE THIS DATA TO CARRY OUT ANALYSIS ON!
write.table(ENClade_results_range_migration, "Results/16.12.21.Results_2/161222_ENClade_results_v2.txt", sep="\t", col.names=TRUE, row.names=FALSE)

