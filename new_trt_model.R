

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(bayesplot)
library(priceTools)

sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



#price stuff

p.all2<-p.all[p.all$trt.x %in% c('NPK'),]
p.all3<-p.all2[p.all2$trt.y %in% c('NPK'),]
levels(p.all3$trt_year)
View(p.all3)
p.all4<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all4$f.year.y<-as.factor(p.all4$year.y)
p.all4$plot<-as.factor(p.all4$plot)
p.all4$site_code<-as.factor(p.all4$site_code)
nrow(p.all4)
#price vectors
p.all.c<-p.all4[complete.cases(p.all4), ]

# REMOVE ALL 0'S
#START PLOTS FROM YEAR 1 
#BECAUSE YEAR 1 IS A COMPARISON TO 0 YOU FOOL
p.all4<-p.all4 %>% filter(year.y != "0" )

p.all4<-p.all4[p.all4$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]

p.all5<-p.all4 %>% group_by(site_code) %>% top_n(1, year.y)


ctl<-p.all[p.all$trt_year %in% c('NPK_0 Control_0'),]
ctl2<-ctl[ctl$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]
View(ctl2)
ctl3<-separate(ctl2,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all4<-bind_rows(p.all5,ctl3)

p.all4$SL.p<-abs(p.all4$SL)



View(p.all4)

SL.trt.m <- brm(SL.p ~   treat + (treat | site.year.id/block/plot/site.year.id.y), 
                data = price.tplot, cores = 4, chains = 4)

SG.trt.m <- brm(SG ~   treat + (treat | site.year.id/block/plot/site.year.id.y), 
                data = price.tplot, cores = 4, chains = 4)

CDE.trt.m <- brm(CDE ~   treat + (treat | site.year.id/block/plot/site.year.id.y), 
                 data = price.tplot, cores = 4, chains = 4)



p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(p)
NPK.dat<-p[p$trt %in% c('NPK'),]

CTL.dat<-p[p$trt %in% c('Control'),]

NPK.dat2<-NPK.dat %>% group_by(site_code) %>% top_n(1, year.y)
CTL.dat2<-CTL.dat %>% group_by(site_code) %>% top_n(1, year.y)

CTL.zero<-CTL.dat[CTL.dat$year_trt %in% c('0'),]
NPK.zero<-NPK.dat[NPK.dat$year_trt %in% c('0'),]


CTL.dat2$treat<-'No treat'
NPK.dat2$treat<-'NPK'
CTL.zero$treat<-'Control'
NPK.zero$treat<-'Control'

tplot<-bind_rows(CTL.dat2,NPK.dat2,CTL.zero,NPK.zero)

plot.rich.trt.m <- brm(rich ~  trt + (trt | site_code/block/plot), 
                       data = year.three, cores = 4, chains = 4)

plot.bm.trt.m <- brm(live_mass ~  trt + (trt | site_code/block/plot), 
                     data =year.three, cores = 4, chains = 4)


