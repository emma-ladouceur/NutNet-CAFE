
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot_calc.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)



plot.rich.im <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
                    data = plot, cores = 4, chains = 4)


save(plot.rich.im,
     file=Sys.getenv('OFILE'))



