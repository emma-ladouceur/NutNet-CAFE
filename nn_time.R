
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
p.all <- read.csv(paste0(path, '/price_time_only.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.all$f.year.y<-as.factor(p.all$year.y)
p.all$plot<-as.factor(p.all$plot)
p.all$block<-as.factor(p.all$block)
p.all$site_code<-as.factor(p.all$site_code)


sl.trt.i <- brm(SL.p ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
                data = p.dat5, family=hurdle_lognormal(),cores = 4, chains = 4)


sg.trt.i <- brm(SG ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
                data = p.dat5, family=hurdle_lognormal(),cores = 4, chains = 4)


CDE.trt.i <- brm(CDE ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
                 data = p.dat5, family=asym_laplace(),cores = 4, chains = 4)


s.loss.i <- brm(s.loss.p.log ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
                data = p.dat3, cores = 4, chains = 4)


s.gain.i <- brm(s.gain.log ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
                data = p.dat3, cores = 4, chains = 4)


s.change.i <- brm(s.change ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
                  data = p.dat3, family=asym_laplace(),cores = 4, chains = 4)

save(sl.trt.i,sg.trt.i,CDE.trt.i,s.loss.i,s.gain.i,s.change.i,
     file=Sys.getenv('OFILE'))



