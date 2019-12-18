

library(tidyverse)
library(priceTools)
library(dplyr)

sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
#plot <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sp$year_trt<-as.factor(sp$year_trt)
levels(sp$year_trt)
levels(sp$site_code)
sp2<-group_by(sp, site_code,year_trt)
head(sp2)

all<-unite_(sp2, "site.year.id", c("site_code","year_trt"), remove=FALSE)
all<-unite_(all, "trt_year", c("trt","year_trt"), remove=FALSE)
all<-unite_(all, "site_trt_year", c("site_code","trt","year_trt"), remove=FALSE)

group.vars <- c('site.year.id','plot','block')
treat.vars<-c('trt_year')

grouped.data <- all %>% group_by_(.dots=c(group.vars,treat.vars))

#takes a long time
res <- pairwise.price(grouped.data, species="Taxon", func="biomass.sp")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp<-res
pp<-group.columns(pp,gps=c(group.vars,treat.vars), drop=T)
