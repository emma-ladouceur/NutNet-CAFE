
library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)
library(bayesplot)


plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )

colnames(plot)
plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$log.rich<-log(plot$rich)
#bm
plot$log.live.mass<-log(plot$live_mass)



load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')

plot.rich_fitted.npk

plot.rich_fitted.npk$starting.richness <- factor(plot.rich_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.rich_coef3$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

plot.rich_fitted.npk$Model<-"Species Richness"
plot.rich_fitted.ctl$Model<-"Species Richness"
plot.rich_fitted.npk <- plot.rich_fitted.npk %>% rename(Treatment = trt) 
plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% rename(Treatment = trt) 
fitted.rich<-bind_rows(plot.rich_fitted.npk,plot.rich_fitted.ctl)

fitted.rich

fitted.rich$Treatment <- factor(fitted.rich$Treatment , levels=c("NPK","Control"))



rich.s<-plot.rich_fitted.npk %>% distinct(site_code)
rich.s$Rich<-"1"
bm.s<-plot.bm_fitted.npk %>% distinct(site_code) 
bm.s$Bm<-"1"

rich.bm<- rich.s %>% full_join(bm.s)
View(rich.bm)

gain.s<-sgain.trt_fitted.npk %>% distinct(site_code) 
gain.s$Gain<-"1"
loss.s<-sloss.trt_fitted.npk %>% distinct(site_code) 
loss.s$Loss<-"1"
loss.gain<- gain.s %>% full_join(loss.s)
View(loss.gain)

sg.s<-sg.trt_fitted.npk %>% distinct(site_code) 
sg.s$SG<-"1"
sl.s<-sl.trt_fitted.npk %>% distinct(site_code) 
sl.s$SL<-"1"
sg.sl<- sg.s %>% full_join(sl.s)
View(sg.sl)

cde.s<-cde_fitted.npk %>% distinct(site_code) 
cde.s$CDE<-"1"
sg.sl.cde<-sg.sl%>%  full_join(cde.s)

price<- loss.gain %>%  full_join(sg.sl.cde)
View(price)

raw<- plot13 %>% distinct(site_code)
raw$ raw.dat<- "1"

all.mods<- rich.bm %>% full_join(price)
View(all.mods)

all<-all.mods %>% full_join(raw)
View(all)


write.csv(all,"~/Desktop/site.inclusion.csv")





plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

site <- read.csv("~/Dropbox/Projects/NutNet/Data/site.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



head(site)

site.smol<-site %>% filter(!is.na(Loss)) 

head(site.smol,n=20)

nrow(site.smol)

nrow(site)


plot.smol<-site.smol %>% left_join(plot)


plot.smol %>% distinct(site_code)


write.csv(all,"~/Dropbox/Projects/NutNet/Data/smol.plot.csv")

