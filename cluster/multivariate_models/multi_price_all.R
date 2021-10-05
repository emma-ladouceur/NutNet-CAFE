


library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
p.all <- read.csv(paste0(path, '/nutnet_cumulative_time.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
# 

p.all <- p.all %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup() 


pp.multi_all <- brm( mvbind(SL,SG,CDE,s.loss.n,s.gain) ~ trt.y * year.y.m + (trt.y * year.y.m | p | site_code), 
                data = p.all,
                family=student(),
                cores = 4, iter = 6000, warmup = 1000, chains = 4,
                control = list(adapt_delta = 0.99) )


save(pp.multi_all,
     file=Sys.getenv('OFILE'))

