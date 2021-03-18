
library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)
library(bayesplot)


plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(plot)
plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/bm.mod.dat.Rdata')

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl.n.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sg_dat.Rdata')

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/cde.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain_dat.Rdata')

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


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

write.csv(all,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/site.inclusion.csv")


# Table S1
comb <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/comb-by-plot-31-August-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
country_codes <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/country_codes.csv", stringsAsFactors = FALSE)
pis <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/pi-contact-list-8-Nov-2019.csv", stringsAsFactors = FALSE)
# produce under meta_data code
quads <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv", stringsAsFactors = FALSE)

all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/site.inclusion.csv", stringsAsFactors = FALSE)


colnames(comb)
head(country_codes)

prep <- comb %>% distinct(site_code, site_name, country,habitat) 

prep <- comb %>%
  group_by(site_code) %>%
  summarise(`Experiment Length` = max(year_trt)) %>%
  filter(`Experiment Length` >= 3) %>% left_join(prep) %>%
  mutate(countrycode = country) %>% select(-country) %>% left_join(country_codes) %>%
  select(-countrycode)

head(prep)

pis_prep <- pis %>% select(site_code, firstname, lastname) %>%
  unite(`Site Manager`, firstname:lastname, remove=TRUE, sep=" ") %>%
  group_by(site_code) %>%
  summarise(
    `Site Managers` = paste(`Site Manager`, collapse = ', ') ) 

head(pis_prep)


site_info <- prep %>% left_join(pis_prep)

head(site_info)

table_s1 <- all %>% select(site_code) %>% left_join(site_info) %>% left_join(quads)


View(table_s1)

write.csv(table_s1,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_S1.csv")

