

library(plotbiomes)
library(tidyverse)
library(sp)

meta <- read.csv("~/Dropbox/NutNet data/comb-by-plot-clim-soil-diversity-01-May-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
clim <- read.csv("~/Dropbox/NutNet data/site-worldclim-2-August-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
biogeo <- read.csv("~/Dropbox/Projects/NutNet/Data/biogeographic_realms.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(meta)
meta2<- distinct(meta, site_code, country, region, habitat, MAT_v2, MAP_v2)

clim2<-distinct(clim,site_code,NDep,latitude,longitude)

meta3<-left_join(meta2, clim2)

meta3$latitude.p<-abs(meta3$latitude)


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


library(tidyverse)
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
country_codes <- read.csv("~/Dropbox/Projects/NutNet/Data/country_codes.csv", stringsAsFactors = FALSE)
biogeo <- read.csv("~/Dropbox/Projects/NutNet/Data/biogeographic_realms.csv", stringsAsFactors = FALSE)

head(clim_dat)
head(country_codes)
head(biogeo)

biogeo2 <- left_join(biogeo,country_codes)

View(biogeo2)

clim_dat_country<- clim_dat %>% rename(countrycode=country)
clim_dat2 <- left_join(clim_dat_country,biogeo2)


View(clim_dat2)

write.csv(clim_dat2, file = "~/Dropbox/Projects/NutNet/Data/clim_dat_2.csv", row.names = FALSE)



clim <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat_2.csv", stringsAsFactors = FALSE)
#plot dat
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

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

p2<-p2 %>% mutate(site_percent_int_rich= ifelse(is.na(site_percent_int_rich), 0, site_percent_int_rich),
                  site_percent_native_rich= ifelse(is.na(site_percent_native_rich), 0, site_percent_native_rich) )

p2$site_dom <- ifelse(p2$site_percent_native_rich >= p2$site_percent_int_rich , 'native dominated',
                      ifelse(p2$site_percent_int_rich  >= p2$site_percent_native_rich, 'introduced dominated','other'))

View(p2)
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

head(meta)
head(clim)

meta_country<- meta %>% rename(countrycode=country)


clim_fix<- clim %>% rename(latitude.clim.dat=latitude,
                           longitude.clim.dat=longitude,
                           latitude.p.clim.dat=latitude.p)

plot_clim <- left_join(meta_country,clim_fix,by = c( "countrycode", "site_code", "region", "habitat"))


View(plot_clim)

write.csv(plot_clim,"~/Dropbox/Projects/NutNet/Data/plot_clim.csv" )


clim <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv", stringsAsFactors = FALSE)
start.rich <- read.csv("~/Dropbox/Projects/NutNet/Data/start.rich.csv", stringsAsFactors = FALSE)
fig1 <- read.csv("~/Dropbox/Projects/NutNet/Data/Figure1_dat.csv", stringsAsFactors = FALSE)
#plot dat



head(clim)


clim_select <- clim %>% select(site_code,country,habitat,site_richness, site_dom) %>%
  left_join(start.rich) %>% left_join(fig1, by = c("site_code", "starting.richness")) %>% select(-X.x,-X.y,-m.rich,-r.rich,-rich.start,-rich.end,-mass.start,-mass.end, -Experiment.Length,-Experiment.Length2) %>%
  rename(Experiment.Length=maxyr)

View(clim_select)


write.csv(clim_select,"~/Dropbox/Projects/NutNet/Data/site.inclusion.csv" )
