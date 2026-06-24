library(pacman)
p_load(tidyverse, readxl, lubridate, ggthemes, vegan, ggpubr, rcartocolor, ggrepel, ggvenn, ggpmisc, 
       scales,emmeans, iNEXT, sf, terra, tidyterra, mapview,ggspatial,  patchwork, usmap)
#all data available in data folder ####
# Figure 1 ####
## list of sites sampled for macros ####
sampled_sites <- read_csv("generated_data/sampled_sites.csv")
## point data ####
ENVI_SE_TAL <- read_excel("data/envi/all_but_knz/ENVI_SE_TAL.xlsx", 
                          sheet = "Final Data")

WASH_MW_GBJ_V2_0 <- read_excel("data/envi/all_but_knz/WASH_MW_GBJ_V2.0.xlsx", 
                               sheet = "Final Data")

ENVI_GP_KNZ <- read_excel("data/envi/ENVI_GP_approach3_20210603_20210812_V1.0.xlsx", 
                          sheet = "Final Data")
##watershed layers MW####
###SE ####
tal_watershed <- st_read("data/map_layers/watershed_TAL.shp")

tal_stream <- st_read("data/map_layers/streamnetwork_TAL.shp")

points_tal_shp <- ENVI_SE_TAL %>%
  semi_join(sampled_sites) %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = '+proj=longlat +datum=WGS84 +no_defs') %>%
  st_transform(crs(tal_stream))

###GP ####
knz_watershed <- st_read("data/map_layers/Konza_Watershed_Boundary.gpkg")

knz_stream <- st_read("data/map_layers/Konza_StreamNetwork.shp")

points_knz_shp <- ENVI_GP_KNZ %>%
  select(siteId, long, lat)%>%
  distinct()%>%
  semi_join(sampled_sites) %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = '+proj=longlat +datum=WGS84 +no_defs') %>%
  st_transform(crs(knz_stream))

#transform watershed to match CRS
knz_watershed <- st_transform(knz_watershed, crs(knz_stream))

###MW ####
gbj_watershed <- st_read("data/map_layers/Gibson_Jack_Watershed_Boundary.gpkg")

gbj_stream <- st_read("data/map_layers/Gibson_network.shp")


points_gbj_shp <- WASH_MW_GBJ_V2_0 %>%
  semi_join(sampled_sites)%>%
  select(siteId, sublocation, long, lat) %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = '+proj=longlat +datum=WGS84 +no_defs') %>%
  st_transform(crs(gbj_stream))

#transform watershed to match CRS
gbj_watershed <- st_transform(gbj_watershed, crs(gbj_stream))

#ok quick check
mapview(knz_watershed) + mapview(knz_stream) + mapview(points_knz_shp, label=points_knz_shp$siteId)

##SE Maps ####
#make map with points colored by number of dry days in dry period following synoptic
#import number of days 
wetdry_all_alt_sampled <- read_csv("generated_data/wetdry_all_alt_sampled.csv")

points_tal_shp_alt <- points_tal_shp %>%
  left_join(wetdry_all_alt_sampled, , join_by(siteId == siteId))%>%
  semi_join(sampled_sites)

tal_shed_alt <- ggplot() +
  geom_sf(data=tal_watershed, fill=NA, linewidth = 1, color= "black") +
  geom_sf(data=tal_stream, linewidth = 1.1, color="royalblue4")+
  geom_sf(data=points_tal_shp_alt, aes(fill=per_dry), shape =21, size = 3)+
  scale_fill_carto_c(palette = "Earth", direction = -1)+
  labs(title = "Talladega Watershed", subtitle = "Percent Dry 9/15/22 - 11/01/22",
       fill = "Percent Dry")+
  annotation_scale(location = "tr")+
  annotation_north_arrow(pad_y = unit(1, "cm"), location = "tr")+
  theme_void(base_size = 8); tal_shed_alt

ggplot2::ggsave("./graphs/tal_watershed_alt.png",dpi=600, width=4, height=6) 


## GP Maps ####
#make map with points colored by number of dry days in surrounding year
points_knz_shp_alt <- points_knz_shp %>%
  left_join(wetdry_all_alt_sampled, join_by(siteId == siteId))

knz_shed_alt <- ggplot() +
  geom_sf(data=knz_watershed, fill=NA, linewidth = 1, color= "black") +
  geom_sf(data=knz_stream, linewidth = 1.1, color="royalblue4")+
  geom_sf(data=points_knz_shp_alt, aes(fill=per_dry), shape =21, size = 3)+
  scale_fill_carto_c(palette = "Earth", direction = -1, na.value="white")+
  labs(title = "Kings Creek Watershed", subtitle = "Percent Dry 8/15/21 - 10/01/22",
       fill = "Percent Dry")+
  annotation_scale(location="br")+
  theme_void(base_size = 8); knz_shed_alt

ggplot2::ggsave("./graphs/knz_watershed_alt.png",dpi=600, width=4, height=6) 

## MW Maps ####
#make map with points colored by number of dry days in surrounding year
points_gbj_shp_alt <- points_gbj_shp %>%
  left_join(wetdry_all_alt_sampled, join_by(siteId == siteId))%>%
  semi_join(sampled_sites)

gbj_shed_alt <- ggplot() +
  geom_sf(data=gbj_watershed, fill=NA, linewidth = 1, color= "black") +
  geom_sf(data=gbj_stream, linewidth = 1.1, color="royalblue4")+
  geom_sf(data=points_gbj_shp_alt, aes(fill=per_dry), shape =21, size = 3)+
  scale_fill_carto_c(palette = "Earth", direction = -1)+
  labs(title = "Gibson Jack Watershed", subtitle = "Percent Dry 6/15/23 - 8/01/24",
       fill = "Percent Dry")+
  annotation_scale(location="br")+
  theme_void(base_size = 8); gbj_shed_alt

ggplot2::ggsave("./graphs/gbj_watershed_alt.png",dpi=600, width=4, height=6)

## US Map ####
points_tal_outlet <- points_tal_shp_alt %>%
  filter(siteId=="TLM01") %>%
  st_transform(9311)

points_knz_outlet <- points_knz_shp_alt %>%
  filter(siteId=="SFM01") %>%
  st_transform(9311)

points_gbj_outlet <- points_gbj_shp_alt %>%
  filter(siteId=="GSS01") %>%
  st_transform(9311)
outlets <- points_gbj_outlet %>%
  bind_rows(points_knz_outlet)%>%
  bind_rows(points_tal_outlet)

plot_usmap(exclude=c("Alaska","Hawaii"))+
  geom_sf(data=outlets, aes(fill=siteId), shape=21, size=5)+
  scale_fill_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed",
                    label = c("MW","GP","SE"))+
  annotation_scale()+
  theme_map(base_size=14)

ggplot2::ggsave("./graphs/us_map.pdf",dpi=800, width=6, height=5)

## Box plots of number of dry days ####
wetdry_all_alt_sampled$watershed <- factor(wetdry_all_alt_sampled$watershed,ordered = T, levels = c("MW", "GP", "SE"))
#Remove STIC70 since we don't have any other data
wetdry_all_alt_sampled <- wetdry_all_alt_sampled %>%
  filter(siteId!="STIC70")

asin_trans <- trans_new(
  name = "asin_sqrt",
  transform = function(x) asin(sqrt(x / 100)),
  inverse   = function(x) (sin(x))^2 * 100)

box_wetdry <- ggplot(wetdry_all_alt_sampled, aes(watershed, per_dry, fill=watershed))+
  geom_boxplot()+
  scale_fill_manual(values = c("#78CCD6","#5C3B26","#89A030"))+
  labs(x="Waterhsed", y="Percent Dry")+
  scale_y_continuous(trans=asin_trans)+
  annotate("text",x="MW", y=98,label="a", size=5)+
  annotate("text",x="GP", y=100,label="b", size=5)+
  annotate("text",x="SE", y=100,label="a", size=5)+
  theme_few(base_size = 12)+
  theme(legend.position = "none");box_wetdry

ggplot2::ggsave("./graphs/wet_dry_box.pdf",dpi=800, width=5, height=4) 

## Model for boxplot ####
#Graphs were compiled in adobe illustrator for final figure

#Figure 2 ####
## a ####
###import synoptic data ####
ENVI_all <- read_csv("generated_data/ENVI_all.csv")

#import synoptic diversity data
apr3_nmds_data <- read_csv("generated_data/apr3_nmds_data.csv")
apr3_nmds_data_gen <- read_csv("generated_data/apr3_nmds_data_gen.csv")
apr3_taxa_div <- read_csv("generated_data/apr3_taxa_div.csv")


apr3_taxa_div <- apr3_taxa_div %>%
  left_join(ENVI_all, join_by(site==siteId))%>%
  select(-watershed.x)%>%
  relocate(watershed.y, .before = "site")%>%
  rename(watershed=watershed.y)%>%
  filter(is.na(watershed)==F)

apr3_taxa_div$watershed<-factor(apr3_taxa_div$watershed, ordered=T, levels=c("MW","GP","SE"))

###NMDS ####
#run NMDS apr 3 at genus level
NMDS<-metaMDS(apr3_nmds_data_gen[,-c(1:3)], distance = "jaccard", k=2, autotransform=F, trymax = 5000)
NMDS 
####now to graph, pull points from NMDS function and make seperate dataframe with original descriptors####
df_nmds<-data.frame(x=NMDS$points[,1],y=NMDS$points[,2])
df_nmds<-cbind(apr3_nmds_data[,c(1:3)],df_nmds)
df_nmds$watershed <- factor(df_nmds$watershed, ordered=T, levels=c("MW","GP","SE"))
####add species and subset to p-values less than  .03 ####
fit<-(envfit(NMDS,apr3_nmds_data_gen[,-c(1:3)], perm=999))
#these could also be in a seperate dataframe
#subset data from evnfit function into a dataframe
scrs<-as.data.frame(scores(fit,"vectors"))
#pull r2 and p values
scrs$r2<-fit$vectors$r
scrs$pvals<-fit$vectors$pvals
scrs%>%arrange(pvals)
scrs%>%arrange(desc(r2))
#You choice of how to choose significance, r2 or p values cutoffs
scrs.r2<-subset(scrs, r2>=.28)
scrs.r2$env.variables<-rownames(scrs.r2)

plot_nmds_vec <- ggplot(df_nmds, aes(x = x, y= y)) + 
  geom_point(aes(fill=watershed, shape=watershed),size=5,stroke=2) + 
  labs(x = "NMDS 1", y = "NMDS 2 ")+
  annotate("text", label = "Stress=0.09", x = 2.3, y = 2, fontface = 3, size=5)+
  scale_fill_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  scale_shape_manual(values=c(22,24,23), name="Watershed")+
  geom_segment(data = scrs.r2,
               aes(x = 0, xend =NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_label_repel(data = scrs.r2, aes(NMDS1, NMDS2, label=env.variables),
                   size = 4, fill=alpha("gray",.85), max.overlaps = 20, fontface="italic")+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1));plot_nmds_vec

ggplot2::ggsave("./graphs/plot_nmds_vec.png",dpi=800, width=10, height=5)

## b ####
####now make pie chart of %of each insect order and non-insects present for each watershed ####
apr3_read_join_taxa_all <- read_csv("generated_data/Final_metabarcoding/apr3_read_join_taxa_all.csv")
apr3_read_join_taxa_all<- apr3_read_join_taxa_all%>%
  filter(tot_read>0)%>%
  filter(site!="STIC70")

sampled_sites <- read_csv("generated_data/sampled_sites.csv")
#remove extra sample
apr3_read_join_taxa_all <- apr3_read_join_taxa_all %>%
  semi_join(sampled_sites, join_by(site==siteId)) %>%
  filter(!(site=="04M06" & date=="20210607"))

apr3_taxa_list<-apr3_read_join_taxa_all%>%
  group_by(watershed, kingdom, phylum, class, order, family, genus)%>%
  reframe()

TAL<- apr3_taxa_list%>%filter(watershed=="TAL")%>%arrange(genus)
GBJ<- apr3_taxa_list%>%filter(watershed=="GBJ")%>%arrange(genus)
KNZ<-apr3_taxa_list%>%filter(watershed=="KNZ")%>%arrange(genus)
#make list with all three
venn_approach <- list(
  'SE' = TAL$genus, 
  'MW' = GBJ$genus, 
  'GP' = KNZ$genus)
#make venn diagram
ven<-ggvenn(
  venn_approach, 
  fill_color = c("#89A030","#78CCD6","#5C3B26"),
  stroke_size = 0.5, set_name_size = 5,text_size=4.5,show_elements = F,
  show_percentage = F)+
  theme_map(base_size = 14);ven
ggplot2::ggsave("./graphs/venn.jpeg",dpi=400, width=6, height=4)

plot_nmds_vec+ven+plot_layout(widths=c(1.5,1))+plot_annotation(tag_levels = 'a', tag_suffix = ")")

ggplot2::ggsave("./graphs/nmds_ven.jpeg",dpi=800, width=10, height=7)

# Figure 3 ####
##a rarefaction ####
SE_inext<-apr3_read_join_taxa_all%>%
  filter(watershed=="TAL")%>%
  pivot_wider(id_cols = Lowest_ID, names_from = site, values_from = tot_read, values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  as.data.frame

rownames(SE_inext)<-SE_inext[,1]
SE_inext<-SE_inext[,-1]

GP_inext<-apr3_read_join_taxa_all%>%
  filter(watershed=="KNZ")%>%
  pivot_wider(id_cols = Lowest_ID, names_from = site, values_from = tot_read, values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  as.data.frame

rownames(GP_inext)<-GP_inext[,1]
GP_inext<-GP_inext[,-1]

MW_inext<-apr3_read_join_taxa_all%>%
  filter(watershed=="GBJ" & site!="STIC70")%>%
  pivot_wider(id_cols = Lowest_ID, names_from = site, values_from = tot_read, values_fn=sum, values_fill = 0)%>%
  mutate_if(is.numeric, ~1 * (. != 0))%>%
  as.data.frame

rownames(MW_inext)<-MW_inext[,1]
MW_inext<-MW_inext[,-1]

#combine into single data set
inext_data<-list(SE=SE_inext, GP=GP_inext, MW=MW_inext)

iraw<-iNEXT(inext_data, q=0, datatype = "incidence_raw")
iraw

rare<-ggiNEXT(iraw)+
  scale_x_continuous(breaks=seq(0,60,10))+
  labs(y="Taxa Richness")+
  scale_color_manual(values=c("#5C3B26","#78CCD6","#89A030"))+
  scale_fill_manual(values=c("grey50", "grey50", "grey50"))+
  labs(y=bquote(~gamma~"-diversity"))+
  theme_few(base_size = 14)+
  theme(legend.position = "none");rare

## b make richness mean for each sampling event ####
rich_graph<-ggplot(apr3_taxa_div, aes(watershed,rich, fill=watershed))+
  geom_boxplot()+
  geom_point(shape=21, stroke=1)+
  scale_fill_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  labs(x="", y="Taxa Richness")+
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5), expand=c(0,0))+
  geom_text(aes(x="SE", y=32, label="*"),size=10)+
  theme_few(base_size=14)+
  theme(legend.position = "none");rich_graph

rich_mod<- lm(rich~watershed, data=apr3_taxa_div)
plot(rich_mod)
anova(rich_mod)
pairs(emmeans(rich_mod, ~watershed))

## c make beta diversity for each campaign- to species level ####
bd_as_comp <- read_csv("generated_data/bd_as_comp.csv")

bd_as_comp$reg<-factor(bd_as_comp$reg,ordered = T, levels=c("MW","GP","SE"))

beta_graph<-bd_as_comp%>%
  filter(metric %in% c("Repl", "RichDif"))%>%
  ggplot(aes(fill=metric, x=reg, y=value))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#83A6AB","#B69462"), name="", label=c(bquote(beta~"repl"),bquote(beta~"rich")))+
  scale_y_continuous(limits=c(0,0.5), expand=c(0,0))+
  scale_x_discrete(labels = c("MW","GP","SE"))+
  labs(x="", y=bquote(beta~"Diversity"))+
  theme_few(base_size = 14)+
  theme(legend.position = "bottom",legend.margin = margin(t = -25, unit = "pt"));beta_graph

## Plot together ####
rare+rich_graph+beta_graph+plot_annotation(tag_levels = 'a', tag_suffix = ")")
ggplot2::ggsave("./graphs/fig2.jpeg",dpi=800, width=10, height=5)

#Figure 4 ####
## a Richness  ####
area_rich<-ggplot(apr3_taxa_div, aes(drainage_area_m/1000000,rich))+
  facet_wrap(~watershed, scales="free_x")+
  geom_point(aes(fill=watershed),shape=21,size=3)+
  scale_x_continuous(trans="log", breaks=c(0,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20))+
  scale_y_continuous(trans="log", breaks = c(0,5,10,15,30))+
  geom_smooth(method="lm",aes(linetype=watershed,color=watershed))+
  scale_linetype_manual(values=c("solid","dashed","solid"),name="Watershed",)+
  scale_fill_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  scale_color_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  labs(x=bquote(Drainage~Area~(km^2)),y="Taxa Richness")+
  stat_poly_eq(use_label("R2","p"))+
  theme_few(base_size=14)+
  theme(legend.position = "none");area_rich

## b LCBD ####
area_lcbd<-ggplot(apr3_taxa_div, aes(drainage_area_m/1000000,LCBD_D))+
  facet_wrap(~watershed, scales="free_x")+
  geom_point(aes(fill=watershed),shape=21,size=3)+
  scale_x_continuous(trans="log", breaks=c(0,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20))+
  scale_y_continuous(limits=c(0.01,0.05), breaks=seq(0.01,0.05,0.01))+
  geom_smooth(method="lm",aes(linetype=watershed,color=watershed))+
  scale_linetype_manual(values=c("solid","dashed","solid"),name="Watershed")+
  scale_fill_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  scale_color_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  labs(x=bquote(Drainage~Area~(km^2)),y="LCBD")+
  stat_poly_eq(use_label("R2","p"), label.x="left", label.y="bottom")+
  theme_few(base_size=14)+
  theme(legend.position="none");area_lcbd

area_rich+area_lcbd+plot_layout(ncol=1, axes = 'collect', axis_titles = 'collect')+
  plot_annotation(tag_levels = 'a', tag_suffix = ")")
ggplot2::ggsave("./graphs/area_comp_lowest.jpeg",dpi=800, width=8, height=5)

# Figure 5 ####
#look at residuals from watershed area vs alternative dry days ####
## a Richness ####
tax_model_MW <- lm(log(rich)~log(drainage_area_m/1000000), data=apr3_taxa_div%>%filter(watershed=="MW"))
tax_model_GP <- lm(log(rich)~log(drainage_area_m/1000000), data=apr3_taxa_div%>%filter(watershed=="GP"))
tax_model_SE <- lm(log(rich)~log(drainage_area_m/1000000), data=apr3_taxa_div%>%filter(watershed=="SE"))

res_MW <- data.frame(watershed= apr3_taxa_div%>%filter(watershed=="MW")%>%filter(!site%in%c("GPZ08","STIC70")) %>%
                       select(watershed),
                     site= apr3_taxa_div%>%filter(watershed=="MW")%>%filter(!site%in%c("GPZ08","STIC70")) %>%
                       select(site),
                     rich_res=residuals(tax_model_MW))
res_GP <- data.frame(watershed= apr3_taxa_div%>%filter(watershed=="GP")%>%
                       select(watershed),
                     site= apr3_taxa_div%>%filter(watershed=="GP")%>%
                       select(site),
                     rich_res=residuals(tax_model_GP))
res_SE <- data.frame(watershed= apr3_taxa_div%>%filter(watershed=="SE")%>%
                       select(watershed),
                     site= apr3_taxa_div%>%filter(watershed=="SE")%>%
                       select(site),
                     rich_res=residuals(tax_model_SE))

#create new data frame
res_rich<- res_MW %>%
  bind_rows(res_GP)%>%
  bind_rows(res_SE)%>% 
  left_join(apr3_taxa_div[,c(1,2,32)])
#transform percent dry
res_rich <- res_rich %>%
  mutate(per_dry_alt_arcs = asin(sqrt(per_dry_alt/100)))
#check models
tax_model_MW_dry <- lm(rich_res~per_dry_alt_arcs, data=res_rich%>%filter(watershed=="MW"))
tax_model_GP_dry <- lm(rich_res~per_dry_alt_arcs, data=res_rich%>%filter(watershed=="GP"))
tax_model_SE_dry <- lm(rich_res~per_dry_alt_arcs, data=res_rich%>%filter(watershed=="SE"))

#plot
transformation <- trans_new(
  name = "asin_sqrt",
  transform = function(x) asin(sqrt(x / 100)),
  inverse   = function(x) (sin(x))^2 * 100)

dry_rich_alt_res<-ggplot(res_rich, aes(per_dry_alt,rich_res))+
  facet_wrap(~watershed, scales="free_x")+
  geom_point(aes(fill=watershed),shape=21,size=3)+
  scale_x_continuous(trans=transformation)+
  geom_smooth(method="lm",aes(linetype=watershed,color=watershed))+
  scale_linetype_manual(values=c("solid","dashed","solid"),name="Watershed",)+
  scale_fill_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  scale_color_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  labs(x="Percent Dry",y="Residuals Taxa Richness")+
  stat_poly_eq(use_label("R2","p"))+
  theme_few(base_size=14)+
  theme(legend.position = "none");dry_rich_alt_res

## b LCBD ####
#calculate LCBD
LCBD_model_MW <- lm(LCBD_D~log(drainage_area_m/1000000), data=apr3_taxa_div%>%filter(watershed=="MW"))
LCBD_model_GP <- lm(LCBD_D~log(drainage_area_m/1000000), data=apr3_taxa_div%>%filter(watershed=="GP"))
LCBD_model_SE <- lm(LCBD_D~log(drainage_area_m/1000000), data=apr3_taxa_div%>%filter(watershed=="SE"))

res_MW_LCBD <- data.frame(watershed= apr3_taxa_div%>%filter(watershed=="MW")%>%filter(!site%in%c("GPZ08","STIC70")) %>%
                       select(watershed),
                     site= apr3_taxa_div%>%filter(watershed=="MW")%>%filter(!site%in%c("GPZ08","STIC70")) %>%
                       select(site),
                     LCBD_res=residuals(LCBD_model_MW))
res_GP_LCBD <- data.frame(watershed= apr3_taxa_div%>%filter(watershed=="GP")%>%
                       select(watershed),
                     site= apr3_taxa_div%>%filter(watershed=="GP")%>%
                       select(site),
                     LCBD_res=residuals(LCBD_model_GP))
res_SE_LCBD <- data.frame(watershed= apr3_taxa_div%>%filter(watershed=="SE")%>%
                       select(watershed),
                     site= apr3_taxa_div%>%filter(watershed=="SE")%>%
                       select(site),
                     LCBD_res=residuals(LCBD_model_SE))

#create new data frame
res_LCBD<- res_MW_LCBD %>%
  bind_rows(res_GP_LCBD)%>%
  bind_rows(res_SE_LCBD)%>% 
  left_join(apr3_taxa_div[,c(1,2,32)])
#transform percent dry
res_LCBD <- res_LCBD %>%
  mutate(per_dry_alt_arcs = asin(sqrt(per_dry_alt/100)))
#check models
LCBD_model_MW_dry <- lm(LCBD_res~per_dry_alt_arcs, data=res_LCBD%>%filter(watershed=="MW"))
LCBD_model_GP_dry <- lm(LCBD_res~per_dry_alt_arcs, data=res_LCBD%>%filter(watershed=="GP"))
LCBD_model_SE_dry <- lm(LCBD_res~per_dry_alt_arcs, data=res_LCBD%>%filter(watershed=="SE"))
#Now plot
dry_LCBD_alt_res<-ggplot(res_LCBD, aes(per_dry_alt,LCBD_res))+
  facet_wrap(~watershed, scales="free_x")+
  geom_point(aes(fill=watershed),shape=21,size=3)+
  geom_smooth(method="lm",aes(linetype=watershed,color=watershed))+
  scale_x_continuous(trans=transformation)+
  scale_linetype_manual(values=c("dashed","dashed","dashed"),name="Watershed",)+
  scale_fill_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  scale_color_manual(values=c("#78CCD6","#5C3B26","#89A030"), name="Watershed")+
  labs(x="Percent Dry",y="Residuals LCBD")+
  stat_poly_eq(use_label("R2","p"))+
  theme_few(base_size=14)+
  theme(legend.position = "none");dry_LCBD_alt_res

## plot together ####
dry_rich_alt_res+dry_LCBD_alt_res+plot_layout(ncol=1, axes = 'collect', axis_titles = 'collect')+
  plot_annotation(tag_levels = 'a', tag_suffix = ")")
ggplot2::ggsave("./graphs/dry_comp_resid_lowest_alt.jpeg",dpi=800, width=8, height=5)

