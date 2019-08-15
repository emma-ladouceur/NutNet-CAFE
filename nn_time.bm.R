
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot_calc.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$log.live.mass<-log(plot$live_mass)



plot.bm.im <- brm(log.live.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                  data = plot , cores = 4, chains = 4)


save(plot.bm.im,
     file=Sys.getenv('OFILE'))



