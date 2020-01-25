
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot_calc.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$log.live.mass<-log(plot$live_mass)

plot <- plot %>% drop_na(live_mass)

plot.bm.log <- brm(live_mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                  data = plot ,family=lognormal(), cores = 4, chains = 4)
#Error: Family 'lognormal' requires responses to be positive.



save(plot.bm.log,
     file=Sys.getenv('OFILE'))



