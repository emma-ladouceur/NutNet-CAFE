
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
p.all <- read.csv(paste0(path, '/cumulative_time_only2.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)



p.CDE.trt.i <- brm(CDE ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
                 data = p.all, family=asym_laplace(),cores = 4, chains = 4)


save(p.CDE.trt.i,
     file=Sys.getenv('OFILE'))



