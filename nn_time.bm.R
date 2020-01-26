
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot_calc.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot2<-plot %>% drop_na(live_mass)
plot2$log.live.mass<-log(plot2$live_mass)


plot.bm.logd <- brm(live_mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                  data = plot2 , family=lognormal(), cores = 4, chains = 4)

save(plot.bm.logd,
     file=Sys.getenv('OFILE'))

