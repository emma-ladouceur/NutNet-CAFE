
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
p.all <- read.csv(paste0(path, '/price_time_only.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)



s.loss.i <- brm(s.loss.p.log ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
                data = p.all, cores = 4, chains = 4)



save(s.loss.i,
     file=Sys.getenv('OFILE'))



