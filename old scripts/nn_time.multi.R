
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

nn.multi.3 <- brm(cbind(all.div, plot.mass) ~ trt * year_trt + (trt * year_trt  | p | site_code), 
                     data = plot,family=student(),  cores = 4, iter=6000, warmup = 1000,chains = 4)


save(nn.multi.3,
     file=Sys.getenv('OFILE'))



