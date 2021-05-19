


library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
p.all <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all$site_code<-as.factor(p.all$site_code)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
# 
p.all <- p.all %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup() %>% unite("site_block_plot", c(site_code, block, plot), sep="_")


sp.multi <- brm(mvbind(rich, strip.mass) ~ trt * year_trt + (trt * year_trt  | p | site_block_plot),
                data = p.all, family=student(),  cores = 4, iter=6000, warmup = 1000,chains = 4)


save(sp.multi,
     file=Sys.getenv('OFILE'))



