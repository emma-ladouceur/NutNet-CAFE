


# load packages
library(tidyverse)
library(brms)

meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta$site_code<- as.factor(meta$site_code)
levels(meta$site_code)

colnames(meta)
 chil.plot <- meta %>% filter(site_code == "chilcas.ar") %>% 
   select(site_code,block ,plot, year_trt,trt, all.div, int.div,nat.div, plot.cover, plot.mass) %>% 
   filter( block == "1",
           plot == "8")
 # pesky block 1 , plot 8 !!
 
 View(chil.plot)
 
 colnames(p.all)
 chil.price <- p.all %>% filter(site_code == "chilcas.ar") %>%
   select(site_code, block, plot, trt_year,x.rich, y.rich,s.loss.n,s.gain) %>%
   filter( block == "1 1",
           plot == "8 8")
 
 View(chil.price)
 
 
 cover.april   <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/NutNet data/full-cover-02-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
 comb.april  <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/NutNet data/comb-by-plot-02-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
 biomass.april <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/NutNet data/full-biomass-02-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
 

 chil.april <- cover.april %>% left_join(comb.april) %>%
   filter(site_code == "chilcas.ar") %>% 
   select(site_code,block ,plot, year_trt,trt, rich) %>% 
   filter( block == "1",
           plot == "8")
View(chil.april)


write.csv(chil.april, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/chilcas.ar.csv')


 