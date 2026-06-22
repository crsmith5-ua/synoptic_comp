#Load Packages
library(tidyverse)
library(readxl)
# All raw data is uploaded to hydroshare. Specific links for download will be added post publication to
# anonymity for review process
#SE####
#import data####
##load reads from all primers ####
AIMS_SE_MACR <- read_excel("Data/Raw_metabarcoding/AIMS_SE_MACR_20211027_20230213_V1.0.xlsx", 
                           sheet = "Finalized Data")
glimpse(AIMS_SE_MACR)

#Pull just approach 3 for now and split by primer COIBE and COIF230
AIMS_SE_MACR_apr3_COIBE<-AIMS_SE_MACR%>%
  filter(appr3==1, testid=="COIBE")

AIMS_SE_MACR_apr3_COIF230<-AIMS_SE_MACR%>%
  filter(appr3==1, testid=="COIF230")

##import taxa sheet ####
coarse_aquatic_taxa <- read_excel("Data/coarse_aquatic_taxa.xlsx")

#clean####
##BE####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIBE_SE_apr3_filtered<-AIMS_SE_MACR_apr3_COIBE%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIBE_SE_apr3_terr<-COIBE_SE_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda")|
           family %in% coarse_aquatic_taxa[-c(1:5),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(COIBE_SE_apr3_filtered$phylum[!(COIBE_SE_apr3_filtered$phylum %in% COIBE_SE_apr3_terr$phylum)])
unique(COIBE_SE_apr3_filtered$class[!(COIBE_SE_apr3_filtered$class %in% COIBE_SE_apr3_terr$class)])
unique(COIBE_SE_apr3_filtered$order[!(COIBE_SE_apr3_filtered$order %in% COIBE_SE_apr3_terr$order)])
unique(COIBE_SE_apr3_filtered$family[!(COIBE_SE_apr3_filtered$family %in% COIBE_SE_apr3_terr$family)])

#write csv post cleaning
write.csv(COIBE_SE_apr3_terr, "Data/Cleaned_metabarcoding/COIBE_SE_apr3_terr.csv", na= "", row.names = F)

##F230####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIF230_SE_apr3_filtered<-AIMS_SE_MACR_apr3_COIF230%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIF230_SE_apr3_terr<-COIF230_SE_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda")|
           family %in% coarse_aquatic_taxa[-c(1:5),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(COIF230_SE_apr3_filtered$phylum[!(COIF230_SE_apr3_filtered$phylum %in% COIF230_SE_apr3_terr$phylum)])
unique(COIF230_SE_apr3_filtered$class[!(COIF230_SE_apr3_filtered$class %in% COIF230_SE_apr3_terr$class)])
unique(COIF230_SE_apr3_filtered$order[!(COIF230_SE_apr3_filtered$order %in% COIF230_SE_apr3_terr$order)])
unique(COIF230_SE_apr3_filtered$family[!(COIF230_SE_apr3_filtered$family %in% COIF230_SE_apr3_terr$family)])

#write csv post cleaning
write.csv(COIBE_SE_apr3_terr, "Data/Cleaned_metabarcoding/COIBE_SE_apr3_terr.csv", na= "", row.names = F)

#combine BE and F230####
SE_apr3_read_join<-full_join(COIBE_SE_apr3_terr[,c(2,3,11,16:22,25)],
                          COIF230_SE_apr3_terr[,c(2,3,11,16:22,25)],relationship="many-to-many")
#combine reads by ID-
SE_apr3_read_join_taxa<-SE_apr3_read_join%>%
  group_by(site, date, watershed,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))
#are all the sites there?
unique(SE_apr3_read_join_taxa$site)

#make read before and after list####
before_read_join<-full_join(AIMS_SE_MACR_apr3_COIBE[,c(2,3,11,16:22,25)],
                            AIMS_SE_MACR_apr3_COIF230[,c(2,3,11,16:22,25)],relationship="many-to-many")
before_read_join<-before_read_join%>%
  group_by(date,site,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))

read_before<-before_read_join%>%
  group_by(date, site)%>%
  summarise(reads=sum(tot_read, na.rm=F))

read_after<-SE_apr3_read_join_taxa%>%
  group_by(date, site)%>%
  summarise(reads_after=sum(tot_read, na.rm=F))

read_change_apr3<-left_join(read_before, read_after, by=c("date","site"))
read_change_apr3$diff<-read_change_apr3$reads-read_change_apr3$reads_after
read_change_apr3$per<-round(read_change_apr3$reads_after/read_change_apr3$reads*100,0)

#write final change table to csv ####
#write csv post cleaning
write.csv(read_change_apr3, "Data/Ancillary_cleaning_data/read_change_SE_apr3.csv", na= "", row.names = F)

#Cleaned Keeping undescribed species####
##add lowest id name column for later analysis####
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa%>%
  mutate(Lowest_ID=species)
#remove taxa within samples that are identified at higher level but are present at lower level####
#remove genus that have species present by counting number of times a a unique lowest id appears within a sample
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus)%>%
  mutate(dup=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without species id and a duplicate id>1 and keeping those with species ID
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  filter(dup>1 & !is.na(species) | dup==1)

#remove family that have genus present by counting number of times a a unique lowest id appears within a sample
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class,order,family)%>%
  mutate(dup2=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  filter(dup2>1 & !is.na(genus) | dup2==1)

#remove order that have family present by counting number of times a a unique lowest id appears within a sample
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class,order)%>%
  mutate(dup3=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  filter(dup3>1 & !is.na(family) | dup3==1)

#remove class that have order present by counting number of times a a unique lowest id appears within a sample--only 1 of these
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class)%>%
  mutate(dup4=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  filter(dup4>1 & !is.na(order) | dup4==1)
#remove dup columns
SE_apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all[,-c(13:16)]

##Finish fixing lowest id
#add genus if no species
SE_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(SE_apr3_read_join_taxa_all$Lowest_ID)==T,
                                          SE_apr3_read_join_taxa_all$genus, 
                                          SE_apr3_read_join_taxa_all$Lowest_ID)
#add family if no genus
SE_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(SE_apr3_read_join_taxa_all$Lowest_ID)==T,
                                          SE_apr3_read_join_taxa_all$family, 
                                          SE_apr3_read_join_taxa_all$Lowest_ID)

#add order if no family
SE_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(SE_apr3_read_join_taxa_all$Lowest_ID)==T,
                                          SE_apr3_read_join_taxa_all$order, 
                                          SE_apr3_read_join_taxa_all$Lowest_ID)

#add class if no order
SE_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(SE_apr3_read_join_taxa_all$Lowest_ID)==T,
                                          SE_apr3_read_join_taxa_all$class, 
                                          SE_apr3_read_join_taxa_all$Lowest_ID)
##check if any one at higher level missing
sum(is.na(SE_apr3_read_join_taxa_all$Lowest_ID))
#write csv post combining
write.csv(SE_apr3_read_join_taxa_all, "Data/Final_metabarcoding/SE_apr3_read_join_taxa_all.csv", na= "", row.names = F)

#MW####
#import data####
##load reads from all primers ####
#had to remove NAs from excel sheet
AIMS_MW_MACR <- read_excel("Data/Raw_metabarcoding/AIMS_MW_MACR_20220319_20230630_V1.0.xlsx", 
                           sheet = "Finalized_Data")[,-1]
glimpse(AIMS_MW_MACR)


#Pull just approach 3 for now and split by primer COIBE and COIF230
AIMS_MW_MACR_apr3_COIBE<-AIMS_MW_MACR%>%
  filter(appr3==1, testid=="COIBE")

AIMS_MW_MACR_apr3_COIF230<-AIMS_MW_MACR%>%
  filter(appr3==1, testid=="COIF230")

##import taxa sheet ####
coarse_aquatic_taxa <- read_excel("Data/coarse_aquatic_taxa.xlsx")

#clean####
##BE####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIBE_MW_apr3_filtered<-AIMS_MW_MACR_apr3_COIBE%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIBE_MW_apr3_terr<-COIBE_MW_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda", "Amphipoda")|
           family %in% coarse_aquatic_taxa[-c(1:5),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(COIBE_MW_apr3_filtered$phylum[!(COIBE_MW_apr3_filtered$phylum %in% COIBE_MW_apr3_terr$phylum)])
unique(COIBE_MW_apr3_filtered$class[!(COIBE_MW_apr3_filtered$class %in% COIBE_MW_apr3_terr$class)])
unique(COIBE_MW_apr3_filtered$order[!(COIBE_MW_apr3_filtered$order %in% COIBE_MW_apr3_terr$order)])
unique(COIBE_MW_apr3_filtered$family[!(COIBE_MW_apr3_filtered$family %in% COIBE_MW_apr3_terr$family)])

#write csv post cleaning
write.csv(COIBE_MW_apr3_terr, "Data/Cleaned_metabarcoding/COIBE_MW_apr3_terr.csv", na= "", row.names = F)

##F230####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIF230_MW_apr3_filtered<-AIMS_MW_MACR_apr3_COIF230%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIF230_MW_apr3_terr<-COIF230_MW_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda", "Amphipoda")|
           family %in% coarse_aquatic_taxa[-c(1:5),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(COIF230_MW_apr3_filtered$phylum[!(COIF230_MW_apr3_filtered$phylum %in% COIF230_MW_apr3_terr$phylum)])
unique(COIF230_MW_apr3_filtered$class[!(COIF230_MW_apr3_filtered$class %in% COIF230_MW_apr3_terr$class)])
unique(COIF230_MW_apr3_filtered$order[!(COIF230_MW_apr3_filtered$order %in% COIF230_MW_apr3_terr$order)])
unique(COIF230_MW_apr3_filtered$family[!(COIF230_MW_apr3_filtered$family %in% COIF230_MW_apr3_terr$family)])

#write csv post cleaning
write.csv(COIBE_MW_apr3_terr, "Data/Cleaned_metabarcoding/COIBE_MW_apr3_terr.csv", na= "", row.names = F)

#combine BE and F230####
MW_apr3_read_join<-full_join(COIBE_MW_apr3_terr[,c(2,3,11,16:22,25)],
                             COIF230_MW_apr3_terr[,c(2,3,11,16:22,25)],relationship="many-to-many")
#combine reads by ID-
MW_apr3_read_join_taxa<-MW_apr3_read_join%>%
  group_by(site, date, watershed,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))
#are all the sites there?
unique(MW_apr3_read_join_taxa$site)

#make read before and after list####
before_read_join<-full_join(AIMS_MW_MACR_apr3_COIBE[,c(2,3,11,16:22,25)],
                            AIMS_MW_MACR_apr3_COIF230[,c(2,3,11,16:22,25)],relationship="many-to-many")
before_read_join<-before_read_join%>%
  group_by(date,site,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))

read_before<-before_read_join%>%
  group_by(date, site)%>%
  summarise(reads=sum(tot_read, na.rm=F))

read_after<-MW_apr3_read_join_taxa%>%
  group_by(date, site)%>%
  summarise(reads_after=sum(tot_read, na.rm=F))

read_change_apr3<-left_join(read_before, read_after, by=c("date","site"))
read_change_apr3$diff<-read_change_apr3$reads-read_change_apr3$reads_after
read_change_apr3$per<-round(read_change_apr3$reads_after/read_change_apr3$reads*100,0)

#write final change table to csv ####
#write csv post cleaning
write.csv(read_change_apr3, "Data/Ancillary_cleaning_data/read_change_MW_apr3.csv", na= "", row.names = F)

#Cleaned Keeping undescribed species####
##add lowest id name column for later analysis####
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa%>%
  mutate(Lowest_ID=species)
#remove taxa within samples that are identified at higher level but are present at lower level####
#remove genus that have species present by counting number of times a a unique lowest id appears within a sample
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus)%>%
  mutate(dup=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without species id and a duplicate id>1 and keeping those with species ID
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  filter(dup>1 & !is.na(species) | dup==1)

#remove family that have genus present by counting number of times a a unique lowest id appears within a sample
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class,order,family)%>%
  mutate(dup2=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  filter(dup2>1 & !is.na(genus) | dup2==1)

#remove order that have family present by counting number of times a a unique lowest id appears within a sample
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class,order)%>%
  mutate(dup3=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  filter(dup3>1 & !is.na(family) | dup3==1)

#remove class that have order present by counting number of times a a unique lowest id appears within a sample--only 1 of these
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class)%>%
  mutate(dup4=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all%>%
  filter(dup4>1 & !is.na(order) | dup4==1)
#remove dup columns
MW_apr3_read_join_taxa_all<-MW_apr3_read_join_taxa_all[,-c(13:16)]

##Finish fixing lowest id
#add genus if no species
MW_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(MW_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             MW_apr3_read_join_taxa_all$genus, 
                                             MW_apr3_read_join_taxa_all$Lowest_ID)
#add family if no genus
MW_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(MW_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             MW_apr3_read_join_taxa_all$family, 
                                             MW_apr3_read_join_taxa_all$Lowest_ID)

#add order if no family
MW_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(MW_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             MW_apr3_read_join_taxa_all$order, 
                                             MW_apr3_read_join_taxa_all$Lowest_ID)

#add class if no order
MW_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(MW_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             MW_apr3_read_join_taxa_all$class, 
                                             MW_apr3_read_join_taxa_all$Lowest_ID)
##check if any one at higher level missing
sum(is.na(MW_apr3_read_join_taxa_all$Lowest_ID))
#write csv post combining
write.csv(MW_apr3_read_join_taxa_all, "Data/Final_metabarcoding/MW_apr3_read_join_taxa_all.csv", na= "", row.names = F)

#GP####
#import data####
##load reads from all primers ####
#remove NAs from excel before hand
AIMS_GP_MACR <- read_excel("Data/Raw_metabarcoding/AIMS_GP_MACR_20210604_20221111_V2.0.xlsx", 
                           sheet = "Finalized_Data")
glimpse(AIMS_GP_MACR)

#Pull just approach 3 for now and split by primer COIBE and COIF230
AIMS_GP_MACR_apr3_COIBE<-AIMS_GP_MACR%>%
  filter(appr3==1, testid=="COIBE")

AIMS_GP_MACR_apr3_COIF230<-AIMS_GP_MACR%>%
  filter(appr3==1, testid=="COIF230")

##import taxa sheet ####
coarse_aquatic_taxa <- read_excel("Data/coarse_aquatic_taxa.xlsx")

#clean####
##BE####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIBE_GP_apr3_filtered<-AIMS_GP_MACR_apr3_COIBE%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIBE_GP_apr3_terr<-COIBE_GP_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda")|
           family %in% coarse_aquatic_taxa[-c(1:5),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(COIBE_GP_apr3_filtered$phylum[!(COIBE_GP_apr3_filtered$phylum %in% COIBE_GP_apr3_terr$phylum)])
unique(COIBE_GP_apr3_filtered$class[!(COIBE_GP_apr3_filtered$class %in% COIBE_GP_apr3_terr$class)])
unique(COIBE_GP_apr3_filtered$order[!(COIBE_GP_apr3_filtered$order %in% COIBE_GP_apr3_terr$order)])
unique(COIBE_GP_apr3_filtered$family[!(COIBE_GP_apr3_filtered$family %in% COIBE_GP_apr3_terr$family)])

#write csv post cleaning
write.csv(COIBE_GP_apr3_terr, "Data/Cleaned_metabarcoding/COIBE_GP_apr3_terr.csv", na= "", row.names = F)

##F230####
#Filter matches >90%, then >=95 family, >=98 genus, and >=99 species
COIF230_GP_apr3_filtered<-AIMS_GP_MACR_apr3_COIF230%>%
  filter(is.na(family) & is.na(genus) & is.na(species) & `% match`>=90 |
           is.na(genus) & is.na(species) & `% match`>=95 |
           is.na(species) & `% match`>=98 |
           !is.na(species) & `% match`>=99)

#remove terrestrial taxa-listing phylum, class and orders without families in taxa list,
#also 5 big all aquatic groups in case families are missed
#note orders at 90% match that are a mixed batch are excluded with this
COIF230_GP_apr3_terr<-COIF230_GP_apr3_filtered%>%
  filter(phylum %in% c("Nematoda", "Nematomorpha","Nemertea", "Platyhelminthes","Malacostraca", "Mollusca") |
           class %in% c("Clitellata", "Collembola", "Hexanauplia","Ostracoda")|
           order %in% c("Ephemeroptera", "Plecoptera","Trichoptera","Megaloptera","Odonata","Lumbriculida",
                        "Haplotaxida","Sarcoptiformes","Trombidiformes","Mesostigmata", "Tubificida","Decapoda")|
           family %in% coarse_aquatic_taxa[-c(1:5),]$Family)

#random checking of what's in each data set to build above filter and make sure there aren't any taxa that are getting missed
unique(COIF230_GP_apr3_filtered$phylum[!(COIF230_GP_apr3_filtered$phylum %in% COIF230_GP_apr3_terr$phylum)])
unique(COIF230_GP_apr3_filtered$class[!(COIF230_GP_apr3_filtered$class %in% COIF230_GP_apr3_terr$class)])
unique(COIF230_GP_apr3_filtered$order[!(COIF230_GP_apr3_filtered$order %in% COIF230_GP_apr3_terr$order)])
unique(COIF230_GP_apr3_filtered$family[!(COIF230_GP_apr3_filtered$family %in% COIF230_GP_apr3_terr$family)])

#write csv post cleaning
write.csv(COIBE_GP_apr3_terr, "Data/Cleaned_metabarcoding/COIBE_GP_apr3_terr.csv", na= "", row.names = F)

#combine BE and F230####
GP_apr3_read_join<-full_join(COIBE_GP_apr3_terr[,c(2,3,11,16:22,25)],
                             COIF230_GP_apr3_terr[,c(2,3,11,16:22,25)],relationship="many-to-many")
#combine reads by ID-
GP_apr3_read_join_taxa<-GP_apr3_read_join%>%
  group_by(site, date, watershed,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))
#are all the sites there?
unique(GP_apr3_read_join_taxa$site)

#make read before and after list####
before_read_join<-full_join(AIMS_GP_MACR_apr3_COIBE[,c(2,3,11,16:22,25)],
                            AIMS_GP_MACR_apr3_COIF230[,c(2,3,11,16:22,25)],relationship="many-to-many")
before_read_join<-before_read_join%>%
  group_by(date,site,kingdom,phylum,class,order,family,genus,species)%>%
  summarise(tot_read=sum(reads))

read_before<-before_read_join%>%
  group_by(date, site)%>%
  summarise(reads=sum(tot_read, na.rm=F))

read_after<-GP_apr3_read_join_taxa%>%
  group_by(date, site)%>%
  summarise(reads_after=sum(tot_read, na.rm=F))

read_change_apr3<-left_join(read_before, read_after, by=c("date","site"))
read_change_apr3$diff<-read_change_apr3$reads-read_change_apr3$reads_after
read_change_apr3$per<-round(read_change_apr3$reads_after/read_change_apr3$reads*100,0)

#write final change table to csv ####
#write csv post cleaning
write.csv(read_change_apr3, "Data/Ancillary_cleaning_data/read_change_GP_apr3.csv", na= "", row.names = F)

#Cleaned Keeping undescribed species####
##add lowest id name column for later analysis####
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa%>%
  mutate(Lowest_ID=species)
#remove taxa within samples that are identified at higher level but are present at lower level####
#remove genus that have species present by counting number of times a a unique lowest id appears within a sample
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus)%>%
  mutate(dup=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without species id and a duplicate id>1 and keeping those with species ID
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  filter(dup>1 & !is.na(species) | dup==1)

#remove family that have genus present by counting number of times a a unique lowest id appears within a sample
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class,order,family)%>%
  mutate(dup2=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  filter(dup2>1 & !is.na(genus) | dup2==1)

#remove order that have family present by counting number of times a a unique lowest id appears within a sample
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class,order)%>%
  mutate(dup3=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  filter(dup3>1 & !is.na(family) | dup3==1)

#remove class that have order present by counting number of times a a unique lowest id appears within a sample--only 1 of these
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  group_by(site,date, watershed,kingdom,phylum,class)%>%
  mutate(dup4=length(unique(Lowest_ID)))

#remove higher classification if lower exist by filtering rows without genus id and a duplicate id>1 and keeping those with genus ID
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all%>%
  filter(dup4>1 & !is.na(order) | dup4==1)
#remove dup columns
GP_apr3_read_join_taxa_all<-GP_apr3_read_join_taxa_all[,-c(13:16)]

##Finish fixing lowest id
#add genus if no species
GP_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(GP_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             GP_apr3_read_join_taxa_all$genus, 
                                             GP_apr3_read_join_taxa_all$Lowest_ID)
#add family if no genus
GP_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(GP_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             GP_apr3_read_join_taxa_all$family, 
                                             GP_apr3_read_join_taxa_all$Lowest_ID)

#add order if no family
GP_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(GP_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             GP_apr3_read_join_taxa_all$order, 
                                             GP_apr3_read_join_taxa_all$Lowest_ID)

#add class if no order
GP_apr3_read_join_taxa_all$Lowest_ID<-ifelse(is.na(GP_apr3_read_join_taxa_all$Lowest_ID)==T,
                                             GP_apr3_read_join_taxa_all$class, 
                                             GP_apr3_read_join_taxa_all$Lowest_ID)
##check if any one at higher level missing
sum(is.na(GP_apr3_read_join_taxa_all$Lowest_ID))
#write csv post combining
write.csv(GP_apr3_read_join_taxa_all, "Data/Final_metabarcoding/GP_apr3_read_join_taxa_all.csv", na= "", row.names = F)

#Combine all region approach 3 data into one####
apr3_read_join_taxa_all<-SE_apr3_read_join_taxa_all%>%
  bind_rows(GP_apr3_read_join_taxa_all)%>%
  bind_rows(MW_apr3_read_join_taxa_all)
#write csv post combining
write.csv(apr3_read_join_taxa_all, "Data/Final_metabarcoding/apr3_read_join_taxa_all.csv", na= "", row.names = F)
