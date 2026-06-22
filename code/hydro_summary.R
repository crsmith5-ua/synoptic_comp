# load packages ####
library(pacman)
p_load(tidyverse, readxl, lubridate, ggthemes, plotly, sf, terra, tidyterra, mapview,
       ggspatial, rcartocolor, patchwork, usmap)
# All raw data is uploaded to hydroshare. Specific links for download will be added post publication to
# anonymity for review process
#Southeast Hydro ####
## import SE stics ####
stic_tal<-list.files(path= "data/stics_SE/", pattern="*.csv", full.names=T, recursive=T)%>%
  map_df(.,~read_csv(., col_types = cols(QAQC = col_character())), id="file_name")

#get date range for all stics
stic_tal%>%
  group_by(siteId)%>%
  reframe(num=length(wetdry), min=min(datetime), max=max(datetime),
          days=max(datetime)-min(datetime))%>%
  arrange(days)%>%
  print(n=70)
#looks like most short term stics go from 2022-05-12 to 2023-04-17, TLC04 stopped in January 2023

#clip dataframe to short term stic range
stic_tal_short <- stic_tal %>%
  filter(between(datetime,as.POSIXct("2022-05-12"), as.POSIXct("2023-04-17")))

#take a look
ggplot(stic_tal_short, aes(datetime, condUncal))+
  facet_wrap(~siteId)+
  geom_point(aes(color=wetdry))
#dry period range check
stic_tal_short%>%
  filter(between(datetime, as.POSIXct("2022-08-01"), as.POSIXct("2022-11-30")))%>%
  ggplot(aes(datetime, condUncal))+
  facet_wrap(~siteId)+
  geom_point(aes(color=factor(wetdry)))
#calculate a daily wetdry value, rules are that there needs to be at least half a days worth of 
#readings (at least 48) and it needs to be wet or dry for that length of time
stic_tal_short_day <- stic_tal_short %>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry)/sum(!is.na(wetdry))>=0.50~1,
                             .default = 0),
          num_read=sum(!is.na(wetdry)))%>%
  filter(use=="Y")

stic_tal_day_sum <- stic_tal_short_day %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)

#Alt look just at dry period**This was the metric used for the manuscript***
stic_tal_day_alt <- stic_tal_short %>%
  filter(between(datetime, as.POSIXct("2022-09-15"), as.POSIXct("2022-11-01")))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry)/sum(!is.na(wetdry))>=0.50~1,
                                 .default = 0),
          num_read=sum(!is.na(wetdry)))%>%
  filter(use=="Y")

stic_tal_day_sum_alt <- stic_tal_day_alt %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)%>%
  mutate(per_dry=num_dry_days/read_days*100)

stic_tal %>%
  filter(between(datetime, as.POSIXct("2022-10-01"), as.POSIXct("2022-10-12")))%>%
  filter(siteId=="TLM08")%>%
  ggplot(aes(datetime,condUncal))+
  geom_point()
#TLM20 and TLC04 have shorter record because of malfunctions
### Calculate mean temperature from the 7 days prior to sampling Sampled 6/9/2022 so 6/2/2022
stic_tal_temp <- stic_tal %>%
  filter(between(datetime,as.POSIXct("2022-06-02"), as.POSIXct("2022-06-08")))%>%
  group_by(siteId)%>%
  reframe(mean_temp=mean(tempC, na.rm=T), n=length(unique(date(datetime))))
## Import SE pressure transducers ####
pres_tal<-list.files(path= "data/pres_SE/", pattern="*.csv", full.names=T, recursive=T)%>%
  map_df(.,~read_csv(., id="file_name"))

#filter only file_names that contain SW
pres_tal <- pres_tal %>%
  filter(str_detect(file_name, "SW"))
         
#get date range for all LTMs
pres_tal%>%
  group_by(siteId)%>%
  reframe(num=length(waterHeight_m), min=min(datetime), max=max(datetime),
          days=max(datetime)-min(datetime))%>%
  arrange(days)%>%
  print(n=7)

#clip to stic date range
pres_tal_short <- pres_tal %>%
  filter(between(datetime,as.POSIXct("2022-05-12"), as.POSIXct("2023-04-17")))%>%
  filter(QAQC!="E" | is.na(QAQC))

#take a look at level and make sure nothing weird going on-some weird stuff in waterHeight that
#has been corrected in waterDepth so we'll use that
ggplot(pres_tal_short, aes(datetime, waterDepth_m))+
  facet_wrap(~siteId)+
  geom_point(aes(color=QAQC),size=0.5, alpha=0.5)+
  geom_hline(yintercept=0)+
  theme_few()

#we'll use water depth as our cut off for wet dry>0, need at least 48 readings and to be wet
#for half the day, issue of NA values being flagged as dry in the wet dry column let's take a look
#by site
ggplotly(pres_tal_short %>%
  filter(siteId=="TLA01") %>%
  filter(between(datetime, as.POSIXct("2022-08-01"), as.POSIXct("2022-10-01"))) %>%
  mutate(wetdry= case_when(wetdry=="dry"~0,
                           wetdry=="wet"~1))%>%
ggplot()+
  geom_point(aes(datetime, waterDepth_m,color=QAQC),size=0.5, alpha=0.5)+
  geom_point(aes(x=datetime, y=wetdry), shape=21)+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_few())
#TLM01 has NA when out of water marked as dry, TLM19, TLC01,TLA01 have single point early on marked as dry but
#should be ok since 1 point
pres_tal_short_day <- pres_tal_short %>%
  mutate(wetdry_new = case_when(waterDepth_m > 0 ~1,
                                (waterDepth_m <= 0 & siteId!="TLM01") | (wetdry == "dry" & siteId!="TLM01") ~0,
                                is.na(waterDepth_m)==F & waterDepth_m <= 0 & siteId=="TLM01"~0))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(waterDepth_m))>=0.50~1,
                                 .default = 0),
          num_read=length(waterDepth_m))%>%
  filter(use=="Y")

pres_tal_day_sum <- pres_tal_short_day %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = length(num_read))%>%
  mutate(num_dry_days=read_days-num_days)
#Alt look just at dry period
pres_tal_day_alt <- pres_tal_short %>%
  filter(between(datetime, as.POSIXct("2022-09-15"), as.POSIXct("2022-11-01")))%>%
  mutate(wetdry_new = case_when(waterDepth_m > 0 ~1,
                                (waterDepth_m <= 0 & siteId!="TLM01") | (wetdry == "dry" & siteId!="TLM01") ~0,
                                is.na(waterDepth_m)==F & waterDepth_m <= 0 & siteId=="TLM01"~0))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(waterDepth_m))>=0.50~1,
                                 .default = 0),
          num_read=length(waterDepth_m))%>%
  filter(use=="Y")

pres_tal_day_sum_alt <- pres_tal_day_alt %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)%>%
  mutate(per_dry=num_dry_days/read_days*100)

### Calculate mean temperature from the 7 days prior to sampling Sampled 6/9/2022 so 6/2/2022
pres_tal_temp <- pres_tal %>%
  filter(between(datetime,as.POSIXct("2022-06-02"), as.POSIXct("2022-06-08")))%>%
  filter(tempC>0)%>%
  group_by(siteId)%>%
  reframe(mean_temp=mean(tempC, na.rm=T), n=length(unique(date(datetime))))
#combine all together
tal_short_wetdry<-stic_tal_day_sum%>%
  bind_rows(pres_tal_day_sum)

#add percent dry
tal_short_wetdry<-tal_short_wetdry%>%
  mutate(per_dry=num_dry_days/read_days*100,
         watershed=rep("SE", length(tal_short_wetdry$siteId)))

#alt combine
tal_alt_short_wetdry<- stic_tal_day_sum_alt %>%
  bind_rows(pres_tal_day_sum_alt)%>%
  mutate(watershed=rep("SE", 56))
#combine temp
tal_temp<-stic_tal_temp%>%
  bind_rows(pres_tal_temp)
#Great Plains Hydro ####
## import GP stics ####
stic_knz<-list.files(path= "data/stics_GP/", pattern="*.csv", full.names=T, recursive=T)%>%
  map_df(.,~read_csv(., col_types = cols(QAQC = col_character())), id="file_name")

#grab LS for temp filling
stic_knz_ls<-stic_knz %>%
  filter(sublocation=="LS")
#konza had multiple stic types, for comparability, we will only use the HS or high stics
#filter only file_names that contain SW
stic_knz <- stic_knz %>%
  filter(sublocation=="HS")

#get date range for all stics
stic_knz%>%
  group_by(siteId, sublocation)%>%
  reframe(num=length(wetdry), min=min(datetime), max=max(datetime),
          days=max(datetime)-min(datetime))%>%
  arrange(days)%>%
  print(n=110)

stic_knz%>%
  filter(str_detect(siteId, "02M"))%>%
  group_by(siteId, sublocation)%>%
  reframe(num=length(wetdry), min=min(datetime), max=max(datetime),
          days=max(datetime)-min(datetime))%>%
  arrange(min)%>%
  print(n=110)
#knz kept their stics, so all are for the full record. Synoptic took place in June 2021, use surrounding 
#year to match TAL record: go from 2021-05-22 to 2022-04-21- skipping first day or so cause it looks like
#some were reading when they weren't quite out??

#clip dataframe to short term stic range
stic_knz_short <- stic_knz %>%
  filter(between(datetime,as.POSIXct("2021-05-22"), as.POSIXct("2022-04-21")))

#take a look
ggplot(stic_knz_short, aes(datetime, condUncal))+
  facet_wrap(~siteId)+
  geom_point(aes(color=wetdry))
#short range check
stic_knz_short%>%
  filter(between(datetime, as.POSIXct("2021-08-01"),as.POSIXct("2022-01-01")))%>%
  ggplot(aes(datetime, condUncal))+
  facet_wrap(~siteId)+
  geom_point(aes(color=factor(wetdry)))
#calculate a daily wetdry value, rules are that there needs to be at least half a days worth of 
#readings (at least 48) and it needs to be wet or dry for that length of time
stic_knz_short_day <- stic_knz_short %>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(length(wetdry)>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry)/sum(!is.na(wetdry))>=0.50~1,
                                 .default = 0),
          num_read=length(wetdry))%>%
  filter(use=="Y")

stic_knz_day_sum <- stic_knz_short_day %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)

#Alt look just at dry period
stic_knz_day_alt <- stic_knz_short %>%
  filter(between(datetime, as.POSIXct("2021-08-15"), as.POSIXct("2021-10-01")))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry)/sum(!is.na(wetdry))>=0.50~1,
                                 .default = 0),
          num_read=sum(!is.na(wetdry)))%>%
  filter(use=="Y")

stic_knz_day_sum_alt <- stic_knz_day_alt %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)%>%
  mutate(per_dry=num_dry_days/read_days*100)

### Calculate mean temperature from the 7 days prior to sampling Sampled 6/6/2021 so 5/30/2021
stic_knz_temp <- stic_knz %>%
  filter(between(datetime,as.POSIXct("2021-05-30"), as.POSIXct("2021-06-05")))%>%
  group_by(siteId)%>%
  reframe(mean_temp=mean(tempC, na.rm=T), n=length(unique(date(datetime))))
  
#some sites missing HS at synoptic---grab LS
stic_knz_temp_fill <- stic_knz_ls %>%
  filter(siteId %in% c("02M01","02M04","02M05","SFM03","SFM06"))%>%
  filter(between(datetime,as.POSIXct("2021-05-30"), as.POSIXct("2021-06-05")))%>%
  group_by(siteId)%>%
  reframe(mean_temp=mean(tempC, na.rm=T), n=length(unique(date(datetime))))

#many have short records so will need to compare to which were sampled for bugs and see
## Import GP pressure transducers####
pres_knz<-list.files(path= "data/pres_GP/", pattern="*.csv", full.names=T, recursive=T)%>%
  map_df(.,~read_csv(., id="file_name"))

#filter only file_names that contain SW
pres_knz <- pres_knz %>%
  filter(str_detect(file_name, "SW"))

#get date range for all LTMs
pres_knz%>%
  group_by(siteId)%>%
  reframe(num=length(waterHeight_m), min=min(datetime), max=max(datetime),
          days=max(datetime)-min(datetime))%>%
  arrange(days)%>%
  print(n=7)

#clip to stic date range
pres_knz_short <- pres_knz %>%
  filter(between(datetime,as.POSIXct("2021-05-22"), as.POSIXct("2022-04-21")))%>%
  filter(!QAQC%in%c("E","T","ZT") | is.na(QAQC))

#take a look at level and make sure nothing weird going on-some weird stuff in waterHeight that
#has been corrected in waterDepth so we'll use that
ggplot(pres_knz_short, aes(datetime, waterDepth_m))+
  facet_wrap(~siteId)+
  geom_point(aes(color=QAQC),size=0.5, alpha=0.5)+
  geom_hline(yintercept=0)+
  theme_few()

#we'll use water depth as our cut off for wet dry>0, need at least 48 readings and to be wet
#for half the day
ggplotly(pres_knz_short %>%
           filter(siteId=="SFM01") %>%
           mutate(wetdry= case_when(wetdry=="dry"~0,
                                    wetdry=="wet"~1))%>%
           ggplot()+
           geom_point(aes(datetime, waterDepth_m,color=QAQC),size=0.5, alpha=0.5)+
           geom_point(aes(x=datetime, y=wetdry), shape=21)+
           geom_hline(yintercept=0, linetype="dashed")+
           theme_few())

#Calculate daily wetdry
pres_knz_short_day <- pres_knz_short %>%
  mutate(wetdry_new = case_when(waterDepth_m > 0 ~1,
                                waterDepth_m <= 0  | wetdry == "dry" ~0))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(waterDepth_m))>=0.50~1,
                                 .default = 0),
          num_read=length(waterDepth_m))%>%
  filter(use=="Y")

pres_knz_day_sum <- pres_knz_short_day %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = length(num_read))%>%
  mutate(num_dry_days=read_days-num_days)

#Alt look just at dry period
pres_knz_day_alt <- pres_knz_short %>%
  filter(between(datetime, as.POSIXct("2021-08-15"), as.POSIXct("2021-10-01")))%>%
  mutate(wetdry_new = case_when(waterDepth_m > 0 ~1,
                                waterDepth_m <= 0  | wetdry == "dry" ~0))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(waterDepth_m))>=0.50~1,
                                 .default = 0),
          num_read=length(waterDepth_m))%>%
  filter(use=="Y")

pres_knz_day_sum_alt <- pres_knz_day_alt %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)%>%
  mutate(per_dry=num_dry_days/read_days*100)

#Konza has stics co-located with pressure transducers. For consistency we are going to remove those and 
#use the pres data to match other regions
stic_knz_day_sum<-stic_knz_day_sum%>%
  filter(!siteId%in%c(unique(pres_knz_day_sum$siteId)))

stic_knz_day_sum_alt<-stic_knz_day_sum_alt%>%
  filter(!siteId%in%c(unique(pres_knz_day_sum_alt$siteId)))

#pressure transducers weren't out until after synoptic so use co-located stics for temp.
#combine all together
knz_short_wetdry<-stic_knz_day_sum%>%
  bind_rows(pres_knz_day_sum)

#add percent dry
knz_short_wetdry<-knz_short_wetdry%>%
  mutate(per_dry=num_dry_days/read_days*100,
         watershed=rep("GP", length(knz_short_wetdry$siteId)))

#combine alt
knz_short_wetdry_alt <- stic_knz_day_sum_alt %>%
  bind_rows(pres_knz_day_sum_alt)%>%
  mutate(watershed=rep("GP", 47))

#combine temp
knz_temp<-stic_knz_temp%>%
  bind_rows(stic_knz_temp_fill)

#Mountain West Hydro ####
## import MW stics ####
stic_gbj<-list.files(path= "data/stics_MW/", pattern="*.csv", full.names=T, recursive=T)%>%
  map_df(.,~read_csv(., col_types = cols(QAQC = col_character())), id="file_name")


#get date range for all stics
stic_gbj%>%
  group_by(siteId)%>%
  reframe(num=length(wetdry), min=min(datetime), max=max(datetime),
          days=max(datetime)-min(datetime))%>%
  arrange(days)%>%
  print(n=75)
#gbj synoptic took place in the end of June 2023

#clip dataframe to short term stic range
stic_gbj_short <- stic_gbj %>%
  filter(between(datetime,as.POSIXct("2023-05-26"), as.POSIXct("2024-04-26")))

#take a look
ggplot(stic_gbj_short, aes(datetime, condUncal))+
  facet_wrap(~siteId)+
  geom_point(aes(color=wetdry))
#individual site check
ggplotly(stic_gbj_short%>%
           filter(siteId=="STIC64")%>%
           ggplot(aes(datetime, condUncal))+
           geom_point(aes(color=factor(wetdry))))
#look for alt short period when things dried
stic_gbj_short%>%
  filter(between(datetime,as.POSIXct("2023-08-01"), as.POSIXct("2023-09-15")))%>%
  ggplot(aes(datetime, condUncal))+
  facet_wrap(~siteId)+
  geom_point(aes(color=wetdry))

#calculate a daily wetdry value, rules are that there needs to be at least half a days worth of 
#readings (at least 48) and it needs to be wet or dry for that length of time
stic_gbj_short_day <- stic_gbj_short %>%
  group_by(siteId, date=date(datetime))%>%
  reframe(use = case_when(length(wetdry)>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry)/sum(!is.na(wetdry))>=0.50~1,
                                 .default = 0),
          num_read=length(wetdry))%>%
  filter(use=="Y")
#Assume that those still wet in last fall reading stayed wet usually dry at this point or in Mar-Jun
#go ahead and filter to stics we did bugs at to reduce and take a look
MAME_MW <- read_excel("data/MAME/MAME_MW_GBJ_20220326_20230630_V2.0.xlsx", 
                      sheet = "Final Data")
MAME_MW$transectPosition_m<-as.numeric(MAME_MW$transectPosition_m)

stic_gbj_short_day_bug <- stic_gbj_short_day %>%
  semi_join(MAME_MW, join_by(siteId == siteId))

#take a look
ggplot(stic_gbj_short_day_bug, aes(date, wetdry_day))+
  facet_wrap(~siteId)+
  geom_point(aes(color=wetdry_day))

#how many days of readings does each stic have
stic_gbj_short_day_bug %>%
  group_by(siteId)%>%
  reframe(n=n()) %>%
  print(n=26)
#look like using that criteria only 6 and 67 need to be filled as dry with an assumption of 336 total days
stic_gbj_day_sum <- stic_gbj_short_day_bug %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)
#stic 06 has 124 read days so add 212 dry days
stic_gbj_day_sum[6,4] <- 117+212
# stic 67 has 144 read days so add 192
stic_gbj_day_sum[22,4] <- 95 + 192
# also anomaly day looking at data for stic 78
stic_gbj_day_sum[26,4] <- 0

#Alt look just at dry period
stic_gbj_day_alt <- stic_gbj_short %>%
  filter(between(datetime,as.POSIXct("2023-06-15"), as.POSIXct("2023-08-01")))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry)/sum(!is.na(wetdry))>=0.50~1,
                                 .default = 0),
          num_read=sum(!is.na(wetdry)))%>%
  filter(use=="Y")

stic_gbj_day_sum_alt <- stic_gbj_day_alt %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)%>%
  mutate(per_dry=num_dry_days/read_days*100)

### Calculate mean temperature from the 7 days prior to sampling Sampled 6/27/2023 so 6/20/2023
stic_gbj_temp <- stic_gbj %>%
  filter(between(datetime,as.POSIXct("2023-06-20"), as.POSIXct("2023-06-26")))%>%
  group_by(siteId)%>%
  reframe(mean_temp=mean(tempC, na.rm=T), n=length(unique(date(datetime))))

## Import MW pressure transducers ####
pres_gbj<-list.files(path= "data/pres_MW/", pattern="*.csv", full.names=T, recursive=T)%>%
  map_df(.,~read_csv(., id="file_name"))

#filter only file_names that contain SW
pres_gbj <- pres_gbj %>%
  filter(sublocation=="SW")

#get date range for all LTMs
pres_gbj%>%
  group_by(siteId)%>%
  reframe(num=length(waterHeight_cm), min=min(datetime), max=max(datetime),
          days=max(datetime)-min(datetime))%>%
  arrange(days)%>%
  print(n=7)

#clip to stic date range
pres_gbj_short <- pres_gbj %>%
  filter(between(datetime,as.POSIXct("2023-05-26"), as.POSIXct("2024-04-26")))%>%
  filter(QAQC!="E" | is.na(QAQC))

#take a look at level and make sure nothing weird going on-some weird stuff in waterHeight that
#has been corrected in waterDepth so we'll use that
ggplot(pres_gbj_short, aes(datetime, waterDepth_cm))+
  facet_wrap(~siteId)+
  geom_point(aes(color=QAQC),size=0.5, alpha=0.5)+
  geom_hline(yintercept=0)+
  theme_few()

#we'll use water depth as our cut off for wet dry>0, need at least 48 readings and to be wet
#for half the day
ggplotly(pres_gbj_short %>%
           filter(siteId=="gpz05") %>%
           mutate(wetdry= case_when(QAQC=="D"~0,
                                    .default=1))%>%
           ggplot()+
           geom_point(aes(datetime, waterDepth_cm,color=QAQC),size=0.5, alpha=0.5)+
           geom_point(aes(x=datetime, y=wetdry), shape=21)+
           geom_hline(yintercept=0, linetype="dashed")+
           theme_few())

#Calculate daily wetdry
pres_gbj_short_day <- pres_gbj_short %>%
  mutate(wetdry_new = case_when(waterDepth_cm > 0 ~1,
                                waterDepth_cm <= 0  | QAQC == "D" ~0))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(waterDepth_cm))>=0.50~1,
                                 .default = 0),
          num_read=length(waterDepth_cm))%>%
  filter(use=="Y")

pres_gbj_day_sum <- pres_gbj_short_day %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = length(num_read))%>%
  mutate(num_dry_days=read_days-num_days)

#Alt look just at dry period
pres_gbj_day_alt <- pres_gbj_short %>%
  filter(between(datetime,as.POSIXct("2023-06-15"), as.POSIXct("2023-08-01")))%>%
  mutate(wetdry_new = case_when(waterDepth_cm > 0 ~1,
                                waterDepth_cm <= 0  | QAQC == "D" ~0))%>%
  group_by(siteId, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(waterDepth_cm))>=0.50~1,
                                 .default = 0),
          num_read=length(waterDepth_cm))%>%
  filter(use=="Y")

pres_gbj_day_sum_alt <- pres_gbj_day_alt %>%
  group_by(siteId) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)%>%
  mutate(per_dry=num_dry_days/read_days*100)

### Calculate mean temperature from the 7 days prior to sampling Sampled 6/27/2023 so 6/20/2023
pres_gbj_temp <- pres_gbj %>%
  filter(between(datetime,as.POSIXct("2023-06-20"), as.POSIXct("2023-06-26")))%>%
  filter(tempC>0)%>%
  group_by(siteId)%>%
  reframe(mean_temp=mean(tempC, na.rm=T), n=length(unique(date(datetime))))

#MW supersensor is in separate file
SUPERSENSOR_MW <- read_csv("data/SUPERSENSOR_MW.csv")
SUPERSENSOR_MW$datetime<-as.POSIXct(SUPERSENSOR_MW$datetime, format="%m/%d/%Y %H:%M")

SUPERSENSOR_MW%>%
  filter(siteID=="GSS01")%>%
  ggplot()+
  geom_point(aes(datetime, finalwaterlevel))+
  geom_hline(yintercept = 0)+
  theme_few()
#has multiple sites so filter to stic date range and to GSS01
sup_short <- SUPERSENSOR_MW %>%
  filter(siteID=="GSS01",
         between(datetime,as.POSIXct("2023-05-26"), as.POSIXct("2024-04-26")))

#take a look
ggplotly(sup_short %>%
           ggplot()+
           geom_point(aes(datetime, finalwaterlevel),size=0.5, alpha=0.5)+
           geom_point(aes(x=datetime, y=as.numeric(wetdry)*50), shape=21)+
           geom_hline(yintercept=0, linetype="dashed")+
           theme_few())

#Calculate daily wetdry
sup_short_day <- sup_short %>%
  mutate(wetdry_new = case_when(finalwaterlevel > 0 ~1,
                                finalwaterlevel <= 0  | wetdry == "0" ~0))%>%
  group_by(siteID, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(finalwaterlevel))>=0.50~1,
                                 .default = 0),
          num_read=length(finalwaterlevel))%>%
  filter(use=="Y")

sup_day_sum <- sup_short_day %>%
  group_by(siteID) %>%
  reframe(num_days = sum(wetdry_day), read_days = length(num_read))%>%
  mutate(num_dry_days=read_days-num_days)

#change siteID to match other dataframes
sup_day_sum<-sup_day_sum%>%
  rename(siteId=siteID)

#Alt look just at dry period
sup_short_day_alt <- sup_short %>%
  filter(between(datetime,as.POSIXct("2023-06-15"), as.POSIXct("2023-08-01")))%>%
  mutate(wetdry_new = case_when(finalwaterlevel > 0 ~1,
                                finalwaterlevel <= 0  | wetdry == "0" ~0))%>%
  group_by(siteID, date(datetime))%>%
  reframe(use = case_when(sum(!is.na(wetdry_new))>=48~"Y",
                          .default = "N"),
          wetdry_day = case_when(sum(wetdry_new, na.rm=T)/sum(!is.na(finalwaterlevel))>=0.50~1,
                                 .default = 0),
          num_read=length(finalwaterlevel))%>%
  filter(use=="Y")

sup_day_sum_alt <- sup_short_day_alt %>%
  group_by(siteID) %>%
  reframe(num_days = sum(wetdry_day), read_days = sum(!is.na(num_read)))%>%
  mutate(num_dry_days=read_days-num_days)%>%
  mutate(per_dry=num_dry_days/read_days*100)

#change siteID to match other dataframes
sup_day_sum_alt<-sup_day_sum_alt%>%
  rename(siteId=siteID)

### Calculate mean temperature from the 7 days prior to sampling Sampled 6/27/2023 so 6/20/2023
sup_gbj_temp <- SUPERSENSOR_MW %>%
  filter(between(datetime,as.POSIXct("2023-06-20"), as.POSIXct("2023-06-26")))%>%
  filter(tempC>0)%>%
  group_by(siteID)%>%
  reframe(mean_temp=mean(tempC, na.rm=T), n=length(unique(date(datetime))))%>%
  rename(siteId=siteID)

#combine all together
gbj_short_wetdry<-stic_gbj_day_sum%>%
  bind_rows(pres_gbj_day_sum)%>%
  bind_rows(sup_day_sum)

gbj_short_wetdry <- gbj_short_wetdry %>%
  mutate(watershed=rep("MW", length(gbj_short_wetdry$siteId)))

#not adding percent dry here since having to make some educated inferences
#percent 
#Combine temperatures
gbj_temp <- stic_gbj_temp %>%
  bind_rows(pres_gbj_temp) %>%
  bind_rows(sup_gbj_temp)

#combine alt days together
gbj_short_wetdry_alt <- stic_gbj_day_sum_alt%>%
  bind_rows(pres_gbj_day_sum_alt)%>%
  bind_rows(sup_day_sum_alt)%>%
  mutate(watershed=rep("MW", 79))

#combine into single dataframe and save-MW already limited to bug only sites
wetdry_all<-tal_short_wetdry%>%
  bind_rows(knz_short_wetdry)%>%
  bind_rows(gbj_short_wetdry)

write.csv(wetdry_all, "generated_data/wetdry_all.csv", row.names = F)

#alt
wetdry_all_alt<-tal_alt_short_wetdry%>%
  bind_rows(knz_short_wetdry_alt)%>%
  bind_rows(gbj_short_wetdry_alt)


write.csv(wetdry_all_alt, "generated_data/wetdry_all_alt.csv", row.names = F)
#temp
temp_all <- tal_temp %>%
  bind_rows(knz_temp) %>%
  bind_rows(gbj_temp)

write.csv(temp_all, "generated_data/temp_all.csv", row.names = F)

# Macro sites ####
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
  ungroup()

sampled_sites <- add_row(sampled_sites, siteId="SFM01", watershed="GP")

#remove sites with missing hydro data in the GP (13) and 1 from MW (GPZ08)
sampled_sites <- sampled_sites %>%
  semi_join(wetdry_all_alt_sampled)

write.csv(sampled_sites, "generated_data/sampled_sites.csv", row.names = F)
#filter wetdry to only sites sampled####
#first need to make all sites capital in wet dry to match sampled_sites
wetdry_all<-wetdry_all%>%
  mutate(siteId=toupper(siteId))

wetdry_all_sampled<-wetdry_all%>%
  semi_join(sampled_sites)

write.csv(wetdry_all_sampled, "generated_data/wetdry_all_sampled.csv", row.names = F)

#alt-dry period sample numbers
wetdry_all_alt<-wetdry_all_alt%>%
  mutate(siteId=toupper(siteId))

wetdry_all_alt_sampled<-wetdry_all_alt%>%
  semi_join(sampled_sites)

#reduce to keep only those which at least 80% of readings or 37 days
wetdry_all_alt_sampled <- wetdry_all_alt_sampled %>%
  mutate(per_dry=case_when(read_days<37~NA,.default=per_dry))%>%
  drop_na(per_dry)

write.csv(wetdry_all_alt_sampled, "generated_data/wetdry_all_alt_sampled.csv", row.names = F)

#temperature data ####
temp_all <- temp_all %>%
  mutate(siteId=toupper(siteId))

temp_all_sampled<-temp_all%>%
  semi_join(sampled_sites)

write.csv(temp_all_sampled, "generated_data/temp_all_sampled.csv", row.names = F)

