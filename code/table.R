library(tidyverse)
library(readxl)
library(sf)

#Make table of environmental data ####
#Exo Data ####
## Mean DO,Cond,Temp and range across watersheds from outlet ##
## GP ####
KNZ_exo <- read_excel("data/EXO_data/AIMS_KMZ_approach1_EXOS_V1.0.xlsx", 
                      sheet = "Final_Data")

#Clip to date range of interest
KNZ_exo <- KNZ_exo %>%
  filter(between(datetime_CST, as.POSIXct("2021-06-09"), as.POSIXct("2022-06-08")))

###DO ####
KNZ_exo_DO <- KNZ_exo %>%
  filter(flag_ODO_mg_L=="NA")

#rough check
ggplot(KNZ_exo_DO, aes(datetime_CST, ODO_mg_L))+
  geom_point()
#filter 0's randomly in middle
KNZ_exo_DO <- KNZ_exo_DO %>%
  filter(ODO_mg_L>0)

KNZ_exo_DO_sum <- KNZ_exo_DO %>%
  reframe(mean_ODOmgL=mean(ODO_mg_L, na.rm=T),
          min_ODOmgL=min(ODO_mg_L, na.rm=T),
          max_ODOmgL=max(ODO_mg_L, na.rm=T),
          n_days = case_when(is.na(ODO_mg_L)==F~length(unique(date(datetime_CST)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_ODOmgL:max_ODOmgL)

###SpCond ####
KNZ_exo_SpC <- KNZ_exo %>%
  filter(flag_Conductivity_uS_cm=="NA")

#rough check
ggplot(KNZ_exo_SpC, aes(datetime_CST, Conductivity_uS_cm))+
  geom_point()

KNZ_exo_SpC_sum <- KNZ_exo_SpC %>%
  reframe(mean_SpCugL=mean(Conductivity_uS_cm, na.rm=T),,
          min_SpCugL=min(Conductivity_uS_cm, na.rm=T),
          max_SpCugL=max(Conductivity_uS_cm, na.rm=T),
          n_days = case_when(is.na(Conductivity_uS_cm)==F~length(unique(date(datetime_CST)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_SpCugL:max_SpCugL)

### Temp ####
KNZ_exo_Temp <- KNZ_exo %>%
  filter(flag_Temp_EXO_C=="NA") 

#rough check
ggplot(KNZ_exo_Temp, aes(datetime_CST, Temperature_C))+
  geom_point()


KNZ_exo_Temp_sum <- KNZ_exo_Temp %>%
  reframe(mean_Temp=mean(Temperature_C, na.rm=T),,
          min_Temp=min(Temperature_C, na.rm=T),
          max_Temp=max(Temperature_C, na.rm=T),
          n_days = case_when(is.na(Temperature_C)==F~length(unique(date(datetime_CST)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_Temp:max_Temp)

#merge
KNZ_exo_sum <- KNZ_exo_DO_sum %>%
  bind_rows(KNZ_exo_SpC_sum)%>%
  bind_rows(KNZ_exo_Temp_sum)%>%
  mutate(watershed=rep("GP",9))%>%
  separate_wider_delim(name, delim="_", names = c("measure","type"))%>%
  pivot_wider( values_from = value, names_from = measure)%>%
  relocate(n_days, .after = max)%>%
  relocate(watershed, .before=type)

##SE####
TAL_exo <- read_excel("data/EXO_data/AIMS_TAL_EXOS_20210915_20241231_v1.0.xlsx", 
                         sheet = "Final_Data")

#Clip to date range of interest
TAL_exo <- TAL_exo %>%
  filter(between(datetime.local, as.POSIXct("2022-06-09"), as.POSIXct("2023-06-08")))

###DO ####
TAL_exo_DO <- TAL_exo %>%
  filter(Qf_ODO_mg_L=="NA") 

#rough check
ggplot(TAL_exo_DO, aes(datetime.local, ODO_mg_L))+
  geom_point()
#filter -100000 that was missed
TAL_exo_DO <- TAL_exo_DO %>%
  filter(ODO_mg_L>0)

TAL_exo_DO_sum <- TAL_exo_DO %>%
  reframe(mean_ODOmgL=mean(ODO_mg_L, na.rm=T),
          min_ODOmgL=min(ODO_mg_L, na.rm=T),
          max_ODOmgL=max(ODO_mg_L, na.rm=T),
          n_days = case_when(is.na(ODO_mg_L)==F~length(unique(date(datetime.local)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_ODOmgL:max_ODOmgL)

###SpCond ####
TAL_exo_SpC <- TAL_exo %>%
  filter(Qf_SpCond_uS_cm =="NA") 

#rough check
ggplot(TAL_exo_SpC, aes(datetime.local, Conductivity_uS_cm))+
  geom_point()

#filter -100000 that was missed
TAL_exo_SpC <- TAL_exo_SpC %>%
  filter(Conductivity_uS_cm>0)

TAL_exo_SpC_sum <- TAL_exo_SpC %>%
  reframe(mean_SpCugL=mean(Conductivity_uS_cm, na.rm=T),,
          min_SpCugL=min(Conductivity_uS_cm, na.rm=T),
          max_SpCugL=max(Conductivity_uS_cm, na.rm=T),
          n_days = case_when(is.na(Conductivity_uS_cm)==F~length(unique(date(datetime.local)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_SpCugL:max_SpCugL)

### Temp ####
TAL_exo_Temp <- TAL_exo %>%
  filter(Qf_Temp_EXO_C =="NA") 

#rough check
ggplot(TAL_exo_Temp, aes(datetime.local, Temp_C))+
  geom_point()

#filter 0's 
TAL_exo_Temp <- TAL_exo_Temp %>%
  filter(Temp_C>0)

TAL_exo_Temp_sum <- TAL_exo_Temp %>%
  reframe(mean_Temp=mean(Temp_C, na.rm=T),,
          min_Temp=min(Temp_C, na.rm=T),
          max_Temp=max(Temp_C, na.rm=T),
          n_days = case_when(is.na(Temp_C)==F~length(unique(date(datetime.local)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_Temp:max_Temp)

#merge
TAL_exo_sum <- TAL_exo_DO_sum %>%
  bind_rows(TAL_exo_SpC_sum)%>%
  bind_rows(TAL_exo_Temp_sum)%>%
  mutate(watershed=rep("SE",9))%>%
  separate_wider_delim(name, delim="_", names = c("measure","type"))%>%
  pivot_wider( values_from = value, names_from = measure)%>%
  relocate(n_days, .after = max)%>%
  relocate(watershed, .before=type)

##MW####
EXOS_MW_GBJ_2023 <- read_csv("data/EXO_data/EXOS_MW_GBJ_GSS01_SW_2023.csv")
EXOS_MW_GBJ_2024 <- read_csv("data/EXO_data/EXOS_MW_GBJ_GSS01_SW_2024.csv")

GBJ_exo <- EXOS_MW_GBJ_2023%>%
  bind_rows(EXOS_MW_GBJ_2024)

#Clip to date range of interest
GBJ_exo <- GBJ_exo %>%
  filter(between(datetime_local, as.POSIXct("2023-06-26"), as.POSIXct("2024-06-25")))

###DO ####
GBJ_exo_DO <- GBJ_exo %>%
  filter(is.na(flag_ODO_mg_L)) 

#rough check
ggplot(GBJ_exo_DO, aes(datetime_local, ODO_mg_L))+
  geom_point()

GBJ_exo_DO_sum <- GBJ_exo_DO %>%
  reframe(mean_ODOmgL=mean(ODO_mg_L, na.rm=T),
          min_ODOmgL=min(ODO_mg_L, na.rm=T),
          max_ODOmgL=max(ODO_mg_L, na.rm=T),
          n_days = case_when(is.na(ODO_mg_L)==F~length(unique(date(datetime_local)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_ODOmgL:max_ODOmgL)

###SpCond ####
GBJ_exo_SpC <- GBJ_exo %>%
  filter(is.na(flag_Conductivity_uS_cm)) 

#rough check
ggplot(GBJ_exo_SpC, aes(datetime_local, Conductivity_uS_cm))+
  geom_point()

GBJ_exo_SpC_sum <- GBJ_exo_SpC %>%
  reframe(mean_SpCugL=mean(Conductivity_uS_cm, na.rm=T),,
          min_SpCugL=min(Conductivity_uS_cm, na.rm=T),
          max_SpCugL=max(Conductivity_uS_cm, na.rm=T),
          n_days = case_when(is.na(Conductivity_uS_cm)==F~length(unique(date(datetime_local)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_SpCugL:max_SpCugL)

### Temp ####
GBJ_exo_Temp <- GBJ_exo %>%
  filter(is.na(flag_Temp_EXO_C))

#rough check
ggplot(GBJ_exo_Temp, aes(datetime_local, Temp_EXO_C))+
  geom_point()

#filter 0's 
GBJ_exo_Temp <- GBJ_exo_Temp %>%
  filter(Temp_EXO_C>0)

GBJ_exo_Temp_sum <- GBJ_exo_Temp %>%
  reframe(mean_Temp=mean(Temp_EXO_C, na.rm=T),,
          min_Temp=min(Temp_EXO_C, na.rm=T),
          max_Temp=max(Temp_EXO_C, na.rm=T),
          n_days = case_when(is.na(Temp_EXO_C)==F~length(unique(date(datetime_local)))))%>%
  distinct()%>%
  pivot_longer(cols = mean_Temp:max_Temp)

#merge
GBJ_exo_sum <- GBJ_exo_DO_sum %>%
  bind_rows(GBJ_exo_SpC_sum)%>%
  bind_rows(GBJ_exo_Temp_sum)%>%
  mutate(watershed=rep("MW",9))%>%
  separate_wider_delim(name, delim="_", names = c("measure","type"))%>%
  pivot_wider( values_from = value, names_from = measure)%>%
  relocate(n_days, .after = max)%>%
  relocate(watershed, .before=type)

##Merge all together ####
exo_sum <- KNZ_exo_sum%>%
  bind_rows(TAL_exo_sum)%>%
  bind_rows(GBJ_exo_sum)

#Nutrient and DOC samples ####
##GP ####
NUTR_GP <- read_excel("data/Nutrient_DOC/NUTR_GP_KNZ_20210713_20230717_V2.0_1.xlsx", 
                      sheet = "Final Data")
DOCS_GP <- read_excel("data/Nutrient_DOC/DOCS_GP_KNZ_20210606_20240819_V1.0.xlsx", 
                      sheet = "Final Data")

#Filter to time frame of reference
NUTR_GP <- NUTR_GP %>%
  mutate(date=as.Date(as.character(NUTR_GP$date), format="%Y%m%d"))%>%
  filter(between(date, as.Date("2021-06-09"), as.Date("2022-06-08")))%>%
  filter(siteId=="SFM01")

NUTR_GP_sum <- NUTR_GP %>%
  mutate(NH4ugL=as.numeric(NH4ugL), SRPugL=as.numeric(SRPugL))%>%
  reframe(mean_NH4 = mean(NH4ugL, na.rm=T),
          meanSRP=mean(SRPugL, na.rm=T))

DOCS_GP <- DOCS_GP %>%
  filter(between(dateReg, as.POSIXct("2021-06-09"), as.POSIXct("2022-06-08")))%>%
  filter(siteId=="SFM01")

DOCS_GP_sum <- DOCS_GP %>%
  reframe(mean_NPOC = mean(NPOCmgL, na.rm=T))

##SE ####
NUTR_SE <- read_excel("data/Nutrient_DOC/NUTR_SE_TAL_20210915_20241004_V1.0.xlsx", 
                      sheet = "Final Data")
DOCS_SE <- read_excel("data/Nutrient_DOC/DOCS_SE_TAL_20211007_20241004_V1.0.xlsx", 
                      sheet = "Final Data")

#Filter to time frame of reference
NUTR_SE <- NUTR_SE %>%
  filter(between(dateReg, as.POSIXct("2022-06-09"), as.POSIXct("2023-06-08")))%>%
  filter(siteId=="TLM01")

NUTR_SE_sum <- NUTR_SE %>%
  reframe(mean_NH4 = mean(NH4ugL, na.rm=T),
          meanSRP=mean(SRPugL, na.rm=T))

DOCS_SE <- DOCS_SE %>%
  filter(between(dateReg, as.POSIXct("2022-06-09"), as.POSIXct("2023-06-08")))%>%
  filter(siteId=="TLM01")

DOCS_SE_sum <- DOCS_SE %>%
  reframe(mean_NPOC = mean(DOCmgL, na.rm=T))

##MW ####
NUTR_MW <- read_excel("data/Nutrient_DOC/NUTR_MW_GBJ_20220326_20241019_V2.0.xlsx", 
                      sheet = "Final Data")
DOCS_MW <- read_excel("data/Nutrient_DOC/DOCS_MW_GBJ_20220326_20241019_V1.0.xlsx", 
                      sheet = "Final Data")

#Filter to time frame of reference
NUTR_MW <- NUTR_MW %>%
  mutate(date=as.Date(as.character(NUTR_MW$date), format="%Y%m%d"))%>%
  filter(between(date, as.POSIXct("2023-06-26"), as.POSIXct("2024-06-25")))%>%
  filter(siteId=="GSS01")

NUTR_MW_sum <- NUTR_MW %>%
  mutate(NH4ugL=as.numeric(NH4ugL), SRPugL=as.numeric(SRPugL))%>%
  reframe(mean_NH4 = mean(NH4ugL, na.rm=T),
          meanSRP=mean(SRPugL, na.rm=T))

DOCS_MW <- DOCS_MW %>%
  filter(between(watershed, as.POSIXct("2022-06-09"), as.POSIXct("2023-06-08")))%>%
  filter(siteId=="GSS01")

DOCS_MW_sum <- DOCS_MW %>%
  reframe(mean_NPOC = mean(DOCmgL, na.rm=T))

#Drainage Density- need watershed and stream files to calculate stream length/watershed area
#SE
tal_watershed <- st_read("data/map_layers/watershed_TAL.shp")
tal_stream <- st_read("data/map_layers/streamnetwork_TAL.shp")
st_area(tal_watershed)#925712 m2 or 0.925712 km2
sum(st_length(tal_stream))#6938.1m or 6.9381.1 km

6.9381/.925712#7.49

#GP
knz_watershed <- st_read("data/map_layers/Konza_Watershed_Boundary.gpkg")
knz_stream <- st_read("data/map_layers/Konza_StreamNetwork.shp")
knz_watershed <- st_transform(knz_watershed, crs(knz_stream))

st_area(knz_watershed)#5.307324
sum(st_length(knz_stream))#14.93507

14.93507/5.307324#2.814049

#MW
gbj_watershed <- st_read("data/map_layers/Gibson_Jack_Watershed_Boundary.gpkg")
gbj_stream <- st_read("data/map_layers/Gibson_network.shp")
gbj_watershed <- st_transform(gbj_watershed, crs(gbj_stream))

st_area(gbj_watershed)#16.702998
sum(st_length(gbj_stream))#21.67465

21.67465/16.702998#1.29765

#Calculate mean canopy cover across watershed at each synoptic
ENVI_all <- read_csv("generated_data/ENVI_all.csv")

macro_watershed <- ENVI_all %>%
  group_by(watershed)%>%
  reframe(mean_canopy=mean(mean_canopy, na.rm=T),
          mean_width=mean(mean_width, na.rm=T))
