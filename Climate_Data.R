




meta <- read.csv("~/Dropbox/NutNet data/comb-by-plot-clim-soil-diversity-01-Nov-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
clim <- read.csv("~/Dropbox/Projects/NutNet/Data/site-worldclim-2-August-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
biogeo <- read.csv("~/Dropbox/Projects/NutNet/Data/biogeographic_realms.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

meta2<- distinct(meta, site_code, country, region, habitat, MAT_v2, MAP_VAR_v2)

clim2<-distinct(clim,site_code,NDep,latitude,latitude.p,longitude)

meta3<-left_join(meta2, clim2)


meta3$Realm <- ifelse(meta3$latitude.p > 23.5 & meta3$latitude.p < 60, 'Temperate',
                      ifelse(meta3$latitude.p >23.5 , 'Tropical',
                             ifelse(meta3$latitude.p  < 60, 'Polar', 'other')))


meta3$NDep.cats <- ifelse(meta3$NDep >= 30.01 & meta3$NDep <= 35.91, '30.01-35.91',
                          ifelse(meta3$NDep >= 25.01 & meta3$NDep <= 30.00, '25.01-30.00',
                                 ifelse(meta3$NDep >= 20.01 & meta3$NDep <= 25.00, '20.01-25.00',
                                        ifelse(meta3$NDep >= 15.01 & meta3$NDep <= 20.00, '15.01-20.00',
                                               ifelse(meta3$NDep >= 10.01 & meta3$NDep <= 15.00, '10.01-15.00',
                                                      ifelse(meta3$NDep >= 5.01 & meta3$NDep <= 10.00, '5.01-10.00',
                                                             ifelse(meta3$NDep >= 2.51 & meta3$NDep <= 5.00, '2.51-5.00',
                                                                    ifelse(meta3$NDep >= 1.00 & meta3$NDep <= 2.50, '1.00-2.50',
                                                                           ifelse(meta3$NDep <1.0 , '< 1', 'other')))))))))

head(biogeo)
head(meta3)
as.factor(as.character(meta3$NDep.cats))
View(meta3)

write.csv(meta3,"~/Dropbox/Projects/NutNet/Data/clim_dat.csv")

clim_dat <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat.csv", stringsAsFactors = FALSE)

# In order to intersect the study points with the Whittaker biomes polygons, we
# need to transform the climate data to spatial point object, forcing
# temperature and precipitation (cm) data as coordinates without a CRS.
points_sp <- sp::SpatialPoints(coords = clim_dat[, c("MAT_v2", "MAP")])


# Extract biomes for each study location. # Whittaker biomes as polygons (comes
# with the plotbiomes package)
Whittaker_biomes_df <- sp::over(x = points_sp,
                                y = plotbiomes::Whittaker_biomes_poly)

clim_dat <- cbind(clim_dat, Whittaker_biomes_df)

write.csv(clim_dat, file = "~/Dropbox/Projects/NutNet/Data/clim_dat_with_Whittaker_biomes.csv", row.names = FALSE)



library(tidyverse)
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
country_codes <- read.csv("~/Dropbox/Projects/NutNet/Data/country_codes.csv", stringsAsFactors = FALSE)
biogeo <- read.csv("~/Dropbox/Projects/NutNet/Data/biogeographic_realms.csv", stringsAsFactors = FALSE)
clim_dat <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat_with_Whittaker_biomes.csv", stringsAsFactors = FALSE)

head(clim_dat)


biogeo2 <- left_join(biogeo,country_codes)

View(biogeo2)


clim_dat2 <- left_join(clim_dat,biogeo2)


View(clim_dat2)

write.csv(clim_dat2,"~/Dropbox/Projects/NutNet/Data/clim_dat_3.csv" )


clim <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat_3.csv", stringsAsFactors = FALSE)
#plot dat
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )

pdeets<-distinct(plot,site_code,block,plot,trt,year_trt,rich,NAT_rich,INT_rich,UNK_rich,live_mass)
p.x<-pdeets[pdeets$year_trt %in% c('0'),]
p.y<-pdeets[pdeets$year_trt != "0",]
colnames(p.x)

colnames(p.x)[5] <-"year_trt.x"
colnames(p.x)[6] <-"rich.x"
colnames(p.x)[7] <-"NAT_rich.x"
colnames(p.x)[8] <-"INT_rich.x"
colnames(p.x)[9] <-"UNK_rich.x"
colnames(p.x)[10] <-"live_mass.x"

colnames(p.y)[5] <-"year_trt.y"
colnames(p.y)[6] <-"rich.y"
colnames(p.y)[7] <-"NAT_rich.y"
colnames(p.y)[8] <-"INT_rich.y"
colnames(p.y)[9] <-"UNK_rich.y"
colnames(p.y)[10] <-"live_mass.y"


p.xy<-left_join(p.x,p.y)
View(p.xy)

View(plot)
p2<-distinct(plot,elevation,latitude.p,latitude,longitude,continent,country,site_code,region,grazed,burned,managed,anthropogenic,habitat,site_richness,site_native_richness,site_introduced_richness)

p2$site_percent_native_rich<- (p2$site_native_richness/p2$site_richness) * 100
p2$site_percent_int_rich<- (p2$site_introduced_richness/p2$site_richness) * 100

p2$site_dom <- ifelse(p2$site_percent_native_rich >= p2$site_percent_int_rich , 'native dominated',
                      ifelse(p2$site_percent_int_rich  >= p2$site_percent_native_rich, 'introduced dominated','other'))

View(p.xy)
rr<-p.xy %>% group_by(site_code) %>%
  summarise(s.rich = mean(rich.x),
            r.rich = round(s.rich))

View(p2)
meta<-left_join(p2,rr)

View(meta)


# climate data longitude has some different decimal points than
# also a couple latitudes
# nut net site data (suspect rounded differently)
# so dont match by longitude or latitude

plot_clim <- left_join(meta,clim, by="site_code")


View(plot_clim)

write.csv(plot_clim,"~/Dropbox/Projects/NutNet/Data/plot_clim.csv" )
