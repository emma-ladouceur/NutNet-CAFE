
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$log.rich<-log(plot$all.div)

 plot <- plot %>% group_by(site_code) %>% filter(max.year >= 5) %>%
 ungroup()


plot.rich.5 <- brm(all.div ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
                    data = plot,cores = 4,iter=6000, warmup = 1000, chains = 4)

save(plot.rich.5,
     file=Sys.getenv('OFILE'))



