
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot_calc.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )

plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot2<-plot %>% drop_na(live_mass)


plot.bm.s <- brm(live_mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                  data = plot2 , family=student(),  cores = 4,iter=6000, warmup = 1000, chains = 4)

save(plot.bm.s,
     file=Sys.getenv('OFILE'))

