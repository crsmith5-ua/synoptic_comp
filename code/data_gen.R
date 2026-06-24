# apr3_read_join_taxa_all was generated in the cleaning_apr3 code
# sampled sites was generated in hydro_summary
# load packages ####
library(pacman)
p_load(tidyverse, readxl, lubridate, adespatial, vegan)
# Import data and clean a bit ####
sampled_sites <- read_csv("generated_data/sampled_sites.csv")
apr3_read_join_taxa_all <- read_csv("generated_data/Final_metabarcoding/apr3_read_join_taxa_all.csv")
apr3_read_join_taxa_all<- apr3_read_join_taxa_all%>%
  filter(tot_read>0)
#remove extra sample and those with less than 4 taxa
apr3_read_join_taxa_all <- apr3_read_join_taxa_all %>%
  semi_join(sampled_sites, join_by(site==siteId)) %>%
  filter(!(site=="04M06" & date=="20210607"))
#number of lowest id and number of genera in each sample
apr3_read_join_taxa_all %>%
  group_by(site) %>%
  reframe(lowest_n <- length(unique(Lowest_ID)),
          genera_n <- length(unique(genus))) %>%
  print(n=86)
#remove those with fewer than 4 unique genera- also 02M09 had 0 reads after cleaning and need to remove STIC70
apr3_read_join_taxa_nmds<- apr3_read_join_taxa_all %>%
  filter(!site%in%c("20M05","GPZ05","TLM22","STIC70"))
# Make overall nmds dataset at genus level and save ####
#transpose for NMDS to site by low.name matrix with presence
apr3_nmds_data<-apr3_read_join_taxa_nmds%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = Lowest_ID, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))
#convert watershed to region codes
apr3_nmds_data <- apr3_nmds_data %>%
  mutate(watershed = case_when (watershed == "TAL"~"SE",
                                watershed == "KNZ" ~"GP",
                                watershed == "GBJ" ~ "MW"))
write.csv(apr3_nmds_data, "generated_data/apr3_nmds_data.csv", row.names = F)

apr3_nmds_data_gen<-apr3_read_join_taxa_nmds%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus)%>%
  summarise(tot_read=sum(tot_read))%>%
  filter(!is.na(genus), !is.na(family),!is.na(order))%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = genus, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

apr3_nmds_data_gen <- apr3_nmds_data_gen %>%
  mutate(watershed = case_when (watershed == "TAL"~"SE",
                                watershed == "KNZ" ~"GP",
                                watershed == "GBJ" ~ "MW"))

write.csv(apr3_nmds_data_gen, "generated_data/apr3_nmds_data_gen.csv", row.names = F)
# Make diversity metric dataset to save ####
## Richness ####
apr3_taxa_div<-apr3_read_join_taxa_all%>%
  filter(site!="STIC70")%>%
  group_by(watershed, site,date)%>%
  reframe(rich=length(unique(Lowest_ID)), rich_gen=length(unique(genus, na.rm=T)))

## Calculate LCBD at lowest and gen level and add ####
apr3_read_join_taxa_SE<-apr3_read_join_taxa_all%>%
  filter(watershed=="TAL")%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus, species, Lowest_ID)%>%
  reframe(tot_read=sum(tot_read))%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = genus, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

apr3_read_join_taxa_GP<-apr3_read_join_taxa_all%>%
  filter(watershed=="KNZ")%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus, species, Lowest_ID)%>%
  reframe(tot_read=sum(tot_read))%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = genus, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

apr3_read_join_taxa_MW<-apr3_read_join_taxa_all%>%
  filter(watershed=="GBJ" & site!="STIC70")%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus, species, Lowest_ID)%>%
  summarise(tot_read=sum(tot_read))%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = genus, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

bd_as_comp_SE<-beta.div.comp(apr3_read_join_taxa_SE[,-c(1:3)], coef="J", quant=F)

#calculate local contribution for each matrix from bd.div.comp
LCBD_rep<-LCBD.comp(bd_as_comp_SE$repl, sqrt.D = F)
LCBD_rich<-LCBD.comp(bd_as_comp_SE$rich, sqrt.D = F)
LCBD_D<-LCBD.comp(bd_as_comp_SE$D, sqrt.D = T)

SE_div<-data.frame(first=apr3_read_join_taxa_SE[,1],second=apr3_read_join_taxa_SE[,3],
                   LCBD_rep=LCBD_rep$LCBD,  LCBD_rich=LCBD_rich$LCBD,
                   LCBD_D=LCBD_D$LCBD)


bd_as_comp_GP<-beta.div.comp(apr3_read_join_taxa_GP[,-c(1:3)], coef="J", quant=F)

#calculate local contribution for each matrix from bd.div.comp
LCBD_rep<-LCBD.comp(bd_as_comp_GP$repl, sqrt.D = F)
LCBD_rich<-LCBD.comp(bd_as_comp_GP$rich, sqrt.D = F)
LCBD_D<-LCBD.comp(bd_as_comp_GP$D, sqrt.D = T)

GP_div<-data.frame(first=apr3_read_join_taxa_GP[,1],second=apr3_read_join_taxa_GP[,3],
                   LCBD_rep=LCBD_rep$LCBD,  LCBD_rich=LCBD_rich$LCBD,
                   LCBD_D=LCBD_D$LCBD)

bd_as_comp_MW<-beta.div.comp(apr3_read_join_taxa_MW[,-c(1:3)], coef="J", quant=F)

#calculate local contribution for each matrix from bd.div.comp
LCBD_rep<-LCBD.comp(bd_as_comp_MW$repl, sqrt.D = F)
LCBD_rich<-LCBD.comp(bd_as_comp_MW$rich, sqrt.D = F)
LCBD_D<-LCBD.comp(bd_as_comp_MW$D, sqrt.D = T)

MW_div<-data.frame(first=apr3_read_join_taxa_MW[,1],second=apr3_read_join_taxa_MW[,3],
                   LCBD_rep=LCBD_rep$LCBD,  LCBD_rich=LCBD_rich$LCBD,
                   LCBD_D=LCBD_D$LCBD)

lcbd<-SE_div%>%
  bind_rows(GP_div)%>%
  bind_rows(MW_div)

#Also make overall beta diversity of each watershed 
bd_as_comp_SE<-data.frame(bd_as_comp_SE$part)
bd_as_comp_SE<-bd_as_comp_SE%>%
  rownames_to_column(var = "metric")%>%
  pivot_wider(names_from=metric,values_from = bd_as_comp_SE.part)%>%
  mutate(reg="SE")

bd_as_comp_GP<-data.frame(bd_as_comp_GP$part)
bd_as_comp_GP<-bd_as_comp_GP%>%
  rownames_to_column(var = "metric")%>%
  pivot_wider(names_from=metric,values_from = bd_as_comp_GP.part)%>%
  mutate(reg="GP")

bd_as_comp_MW<-data.frame(bd_as_comp_MW$part)
bd_as_comp_MW<-bd_as_comp_MW%>%
  rownames_to_column(var = "metric")%>%
  pivot_wider(names_from=metric,values_from = bd_as_comp_MW.part)%>%
  mutate(reg="MW")

bd_as_comp<-bd_as_comp_SE%>%
  bind_rows(bd_as_comp_GP)%>%
  bind_rows(bd_as_comp_MW)%>%
  pivot_longer(cols=BDtotal:'RichDif/BDtotal',names_to = "metric", values_to = "value")

write.csv(bd_as_comp, "generated_data/bd_as_comp.csv", row.names = F)
## Now at genus level ####
apr3_read_join_taxa_genus_SE<-apr3_read_join_taxa_all%>%
  filter(watershed=="TAL")%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus)%>%
  summarise(tot_read=sum(tot_read))%>%
  filter(!is.na(genus), !is.na(family),!is.na(order))%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = genus, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

apr3_read_join_taxa_genus_GP<-apr3_read_join_taxa_all%>%
  filter(watershed=="KNZ")%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus)%>%
  summarise(tot_read=sum(tot_read))%>%
  filter(!is.na(genus), !is.na(family),!is.na(order))%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = genus, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

apr3_read_join_taxa_genus_MW<-apr3_read_join_taxa_all%>%
  filter(watershed=="GBJ" & site!="STIC70")%>%
  group_by(site,date, watershed, kingdom,phylum,class,order,family,genus)%>%
  summarise(tot_read=sum(tot_read))%>%
  filter(!is.na(genus), !is.na(family),!is.na(order))%>%
  pivot_wider(id_cols = c(site,date,watershed), names_from = genus, values_from = tot_read, 
              values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))

bd_as_comp_SE<-beta.div.comp(apr3_read_join_taxa_genus_SE[,-c(1:3)], coef="J", quant=F)

#calculate local contribution for each matrix from bd.div.comp
LCBD_rep<-LCBD.comp(bd_as_comp_SE$repl, sqrt.D = F)
LCBD_rich<-LCBD.comp(bd_as_comp_SE$rich, sqrt.D = F)
LCBD_D<-LCBD.comp(bd_as_comp_SE$D, sqrt.D = T)

SE_div<-data.frame(first=apr3_read_join_taxa_genus_SE[,1],second=apr3_read_join_taxa_genus_SE[,3],
                   LCBD_rep_gen=LCBD_rep$LCBD,  LCBD_rich_gen=LCBD_rich$LCBD,
                   LCBD_D_gen=LCBD_D$LCBD)


bd_as_comp_GP<-beta.div.comp(apr3_read_join_taxa_genus_GP[,-c(1:3)], coef="J", quant=F)

#calculate local contribution for each matrix from bd.div.comp
LCBD_rep<-LCBD.comp(bd_as_comp_GP$repl, sqrt.D = F)
LCBD_rich<-LCBD.comp(bd_as_comp_GP$rich, sqrt.D = F)
LCBD_D<-LCBD.comp(bd_as_comp_GP$D, sqrt.D = T)

GP_div<-data.frame(first=apr3_read_join_taxa_genus_GP[,1],second=apr3_read_join_taxa_genus_GP[,3],
                   LCBD_rep_gen=LCBD_rep$LCBD,  LCBD_rich_gen=LCBD_rich$LCBD,
                   LCBD_D_gen=LCBD_D$LCBD)

bd_as_comp_MW<-beta.div.comp(apr3_read_join_taxa_genus_MW[,-c(1:3)], coef="J", quant=F)

#calculate local contribution for each matrix from bd.div.comp
LCBD_rep<-LCBD.comp(bd_as_comp_MW$repl, sqrt.D = F)
LCBD_rich<-LCBD.comp(bd_as_comp_MW$rich, sqrt.D = F)
LCBD_D<-LCBD.comp(bd_as_comp_MW$D, sqrt.D = T)

MW_div<-data.frame(first=apr3_read_join_taxa_genus_MW[,1],second=apr3_read_join_taxa_genus_MW[,3],
                   LCBD_rep_gen=LCBD_rep$LCBD,  LCBD_rich_gen=LCBD_rich$LCBD,
                   LCBD_D_gen=LCBD_D$LCBD)

lcbd_gen<-SE_div%>%
  bind_rows(GP_div)%>%
  bind_rows(MW_div)

#Also make overall beta diversity of each watershed 
bd_as_comp_SE<-data.frame(bd_as_comp_SE$part)
bd_as_comp_SE<-bd_as_comp_SE%>%
  rownames_to_column(var = "metric")%>%
  pivot_wider(names_from=metric,values_from = bd_as_comp_SE.part)%>%
  mutate(reg="SE")

bd_as_comp_GP<-data.frame(bd_as_comp_GP$part)
bd_as_comp_GP<-bd_as_comp_GP%>%
  rownames_to_column(var = "metric")%>%
  pivot_wider(names_from=metric,values_from = bd_as_comp_GP.part)%>%
  mutate(reg="GP")

bd_as_comp_MW<-data.frame(bd_as_comp_MW$part)
bd_as_comp_MW<-bd_as_comp_MW%>%
  rownames_to_column(var = "metric")%>%
  pivot_wider(names_from=metric,values_from = bd_as_comp_MW.part)%>%
  mutate(reg="MW")

bd_as_comp_gen<-bd_as_comp_SE%>%
  bind_rows(bd_as_comp_GP)%>%
  bind_rows(bd_as_comp_MW)%>%
  pivot_longer(cols=BDtotal:'RichDif/BDtotal',names_to = "metric", values_to = "value")

write.csv(bd_as_comp_gen, "generated_data/bd_as_comp_gen.csv", row.names = F)

#bind to apr3_taxa_div
apr3_taxa_div <- apr3_taxa_div %>%
  left_join(lcbd)%>%
  left_join(lcbd_gen)

write.csv(apr3_taxa_div, "generated_data/apr3_taxa_div.csv", row.names = F)

# Gather all useful environmental data ####
ENVI_SE_TAL <- read_excel("data/envi/all_but_knz/ENVI_SE_TAL.xlsx", 
                          sheet = "Final Data")

WASH_MW_GBJ_V2_0 <- read_excel("data/envi/all_but_knz/WASH_MW_GBJ_V2.0.xlsx", 
                               sheet = "Final Data")

ENVI_GP_KNZ <- read_excel("data/envi/ENVI_GP_approach3_20210603_20210812_V1.0.xlsx", 
                          sheet = "Final Data")

ENVI_GP_KNZ <- ENVI_GP_KNZ %>%
  mutate(distance_from_outlet=distance_from_outlet*1000)

## Reduce to needed data and combine ####
ENVI_SE_TAL_red <- ENVI_SE_TAL %>%
  select(siteId, elevation, long ,lat, distance_from_outlet, drainage_area_m) %>%
  mutate(distance_from_outlet=as.numeric(distance_from_outlet),
         drainage_area_m=as.numeric(drainage_area_m))

ENVI_MW_GBJ_red <- WASH_MW_GBJ_V2_0 %>%
  select(siteId, elevation, long, lat, distance_from_outlet, drainage_area_m)%>%
  mutate(long=as.numeric(long),
         lat=as.numeric(lat))

ENVI_GP_KNZ_red <- ENVI_GP_KNZ %>%
  select(siteId, elevation, long, lat, distance_from_outlet, drainage_area) %>%
  mutate(drainage_area_m= drainage_area*10000)%>%
  select(-drainage_area)

ENVI_all <- ENVI_SE_TAL_red %>%
  bind_rows(ENVI_GP_KNZ_red) %>%
  bind_rows(ENVI_MW_GBJ_red)%>%
  distinct()

#limit to sites sampled for macros
## Macro sites ####
#bring in macro mame data to filter to locations where samples were taken
MAME_GP <- read_excel("data/MAME/MAME_GP_KNZ_20210606_20221111_V3.0.xlsx", 
                      sheet = "Final Data")
MAME_GP$rep<-as.numeric(MAME_GP$rep)
MAME_GP$watershed <- rep("GP", nrow(MAME_GP))

MAME_MW <- read_excel("data/MAME/MAME_MW_GBJ_20220326_20230630_V2.0.xlsx", 
                      sheet = "Final Data")
MAME_MW$transectPosition_m<-as.numeric(MAME_MW$transectPosition_m)
MAME_MW$watershed <- rep("MW", nrow(MAME_MW))

MAME_SE <- read_excel("data/MAME/MAME_SE_TAL_20211027_20230131_V1.0.xlsx", 
                      sheet = "Final Data")

MAME_SE$watershed <- rep("SE", nrow(MAME_SE))

#Join datasets together and summarise to whether a site had a sample taken or not
MAME_all<-MAME_GP%>%
  bind_rows(MAME_MW)%>%
  bind_rows(MAME_SE)%>%
  filter(appr3==1)%>%
  distinct()

#seem to have double samples for some
MAME_all%>%
  group_by(siteId)%>%
  reframe(count=length(transectPosition_m))%>%
  filter(count>6)%>%
  print(n=20)
#need to remove extra row from STIC08 row 367 and second sample from 02M01 on 20210608
MAME_all<-MAME_all[-367,]%>%
  filter(!(siteId=="02M01"& date=="20210608"))
##need to find MAME info for outlet sampled 6/4/21 but will add in for now
sampled_sites<-MAME_all%>%
  group_by(watershed,siteId)%>%
  reframe(status=paste(locationWetDry, collapse=","))

sampled_sites<-sampled_sites%>%
  filter(str_detect(status,"W"))

sampled_sites<-sampled_sites%>%
  group_by(siteId)%>%
  mutate(status=tolower(status))%>%
  mutate(num_samples=str_count(status,"w"))%>%
  mutate(connected= case_when(str_detect(status,"d")~"Disconnected",
                              .default = "Connected"))%>%
  ungroup()

sampled_sites <- add_row(sampled_sites, siteId="SFM01", watershed="GP")

ENVI_all <- sampled_sites %>%
  left_join(ENVI_all) %>%
  select(!status)

## Calculate mean canopy cover and mean width 
MAME_all <- MAME_all %>%
  mutate(across(c(substrateSilt_percent:algaeFil_percent,
                  canopyCover_percent:depth5), as.numeric))

macro_reach <- MAME_all %>%
  group_by(siteId)%>%
  summarise(mean_canopy=mean(canopyCover_percent, na.rm = T),
            mean_width=mean(wettedWidth_m, na.rm=T)) %>%
  distinct()

#combine with ENVI_all
ENVI_all <- ENVI_all %>%
  left_join(macro_reach)

## Get YSI data for conductivity readings ####
YSI_TAL <- read_excel("data/YSI/YSIS_SE_TAL_20211007_20241004_V1.0.xlsx", 
                      sheet = "Final Data")
YSI_TAL <- YSI_TAL %>%
  rename(SpecCond_usCm = SpCond_uScm)%>%
  select(date, siteId,appr3, SpecCond_usCm)

YSI_KNZ <- read_excel("data/YSI/YSIS_GP_KNZ_20210603_20240818_V1.0.xlsx", 
                      sheet = "Final Data")%>%
  select(date, siteId,appr3, SpecCond_usCm)

YSI_GBJ <- read_excel("data/YSI/YSIS_MW_GBJ_20211124_20241019_V2.0.xlsx", 
                      sheet = "Final Data")%>%
  select(date, siteId,appr3, SpecCond_usCm)

#Combine, select conductivity and reduce to sampled sites
YSI_all <- YSI_TAL %>%
  bind_rows(YSI_KNZ) %>%
  bind_rows(YSI_GBJ) %>%
  filter(appr3==1) %>%
  semi_join(sampled_sites)%>%
  select(-appr3) %>%
  mutate(SpecCond_usCm= as.numeric(SpecCond_usCm)) %>%
  distinct()

#Join to ENVI_all
ENVI_all <- ENVI_all %>%
  left_join(YSI_all) 

ENVI_all <- ENVI_all[-c(46,103),]

##Pull all stics and PTS and calculate temp 7 days prior ####
temp_all_sampled <- read_csv("generated_data/temp_all_sampled.csv")

#combine with ENVI_all
ENVI_all <- ENVI_all %>%
  left_join(temp_all_sampled)%>%
  distinct()

#stic 67 doubled for some reason so remove
ENVI_all <- ENVI_all[-67,]

##add number of dry days####
wetdry_all_sampled <- read_csv("generated_data/wetdry_all_sampled.csv")
ENVI_all <- ENVI_all %>%
  left_join(wetdry_all_sampled)

###add number of dry days in alt wetdry dataset
wetdry_all_alt_sampled <- read_csv("generated_data/wetdry_all_alt_sampled.csv")
wetdry_all_alt_sampled <- wetdry_all_alt_sampled %>%
  rename_with(~paste0(.x, "_alt"))%>%
  rename(siteId=siteId_alt)

ENVI_all <- ENVI_all %>%
  left_join(wetdry_all_alt_sampled)

write.csv(ENVI_all, "generated_data/ENVI_all.csv", row.names = F)
