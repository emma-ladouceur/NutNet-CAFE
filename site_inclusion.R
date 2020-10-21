
library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)
library(bayesplot)


plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(plot)
plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sg_dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


View(plot.rich_fitted.npk)

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

raw<- plot %>% distinct(site_code)
raw$ raw.dat<- "1"

all.mods<- rich.bm %>% full_join(price)
View(all.mods)

all<-all.mods %>% full_join(raw)
View(all)


write.csv(all,"~/Desktop/site.inclusion.csv")







library(tidyverse)
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
comb <- read.csv("~/Dropbox/NutNet data/comb-by-plot-31-August-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
country_codes <- read.csv("~/Dropbox/Projects/NutNet/Data/country_codes.csv", stringsAsFactors = FALSE)
biogeo <- read.csv("~/Dropbox/Projects/NutNet/Data/biogeographic_realms.csv", stringsAsFactors = FALSE)
contacts <- read.csv("~/Dropbox/Projects/NutNet/Data/pi-contact-list-8-Nov-2019.csv", stringsAsFactors = FALSE)

colnames(plot)
colnames(comb)
head(country_codes)

site_deets <- comb %>% dplyr::select(site_name,site_code)


contacts <- contacts %>% select(-country,-site_name,-institution, -email)


site.include <- plot %>% dplyr::select(site_code,country,habitat,max.year) %>% 
  distinct() %>% mutate(countrycode = country) %>%
  dplyr::select(-country) %>%
  left_join(country_codes) %>% left_join(site_deets) %>% filter(max.year >= 3) %>%
  dplyr::select(site_code,site_name,country,habitat, max.year) %>% distinct() %>%
  left_join(contacts, by= c("site_code"))


View(site.include)


site.include.names <- site.include %>% unite( name , firstname:lastname, remove=TRUE, sep=" ") %>%
  group_by(site_code,site_name, country, max.year) %>%
  summarise(name = toString(name)) %>% ungroup() %>%
  rename('data contributor(s)' = name)
  

View(site.include.names)

write.csv(site.include,"~/Dropbox/Projects/NutNet/Data/Table_S1.csv")


