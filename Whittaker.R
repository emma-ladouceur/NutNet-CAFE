

library(plotbiomes)
library(ggplot2)
library(tidyverse)

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
                                                                          ifelse(meta3$NDep < 1.0 , '< 1', 'other')))))))))

head(biogeo)
head(meta3)

write.csv(meta3,"~/Dropbox/Projects/NutNet/Data/clim_dat.csv")


meta <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


meta$MAP_mm<-meta$MAP_v2 / 10


whittaker_base_plot() +
  geom_point(data = meta, 
             aes(x = MAT_v2, 
                 y = MAP_mm), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()







