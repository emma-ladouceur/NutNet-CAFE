
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
p.all <- read.csv(paste0(path, '/nutnet_cumulative_time.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)


p.all <- p.all %>% group_by(site_code) %>% filter(year_max >= 6) %>%
  ungroup()

 # sloss.3<- brm(s.loss.n ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
 #                 data = p.all,family=student(), cores = 4, iter = 15000, warmup = 1000,chains = 4,
 #                 control = list(adapt_delta = 0.99))


sloss.6 <- brm(s.loss.n ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
                 data = p.all, family=student(), cores = 4, chains = 4,
                 iter=5000, warmup = 1000,
                 prior = c(
                   prior(normal(-4,3), class = Intercept),
                   prior(normal(3,1), class = b, coef = trt.yNPK),
                   prior(normal(0,1), class = b, coef = year.y.m),
                   prior(normal(0,1), class = b, coef = trt.yNPK:year.y.m),
                   prior(normal(0,1), class = sd),
                   prior(normal(0,1), class = sigma),
                   prior(constant(1), class = nu)),
                 control = list(adapt_delta = 0.99)#,
                 #sample_prior = 'only',
                 #backend = 'cmdstanr'
)


save(sloss.6,
     file=Sys.getenv('OFILE'))



