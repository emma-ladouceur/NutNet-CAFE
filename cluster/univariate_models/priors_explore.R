
library(tidyverse)
library(brms)
library(rstan)
library(rstanarm)

# price partitions
p.all <- read.csv('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv', header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)

p.all <- p.all %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()

head(p.all)

sd(p.all$SL, na.rm=T)

get_prior(SL ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


# same as sl_sb
sl.3_test2_nu10 <- brm(SL ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
                 data = p.all, family=student(), cores = 4, chains = 4#,
                 # prior = c(
                 #   prior(normal(-22,30), class = Intercept),
                 #   prior(normal(-30,10), class = b, coef = trt.yNPK),
                 #           prior(normal(0,10), class = b, coef = year.y.m),
                 #           prior(normal(0,10), class = b, coef = trt.yNPK:year.y.m),
                 #           prior(normal(0,10), class = sd),
                 #           prior(normal(0,10), class = sigma),
                 #           prior(constant(10), class = nu)),
                 # control = list(max_treedepth = 12),
                 # sample_prior = 'only'
                 )

#pp_check(sl.3_test2_nu10)


get_prior(SG ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


get_prior(CDE ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


get_prior(s.loss.n ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


get_prior(s.gain ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())



# biomass richness plot level stuff
plot <- read.csv('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv', header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



get_prior(rich ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
          data = plot)


get_prior(strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
          data = plot,
          family = student())





library(tidyverse)
library(brms)
library(cmdstanr)

p.all <- read.csv('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv', header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)

p.all <- p.all %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()


sl.3_test <- brm(SL ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
                 data = p.all, family=student(), cores = 8, chains = 8,
                 iter = 1000,
                 prior = c(
                   prior(normal(-22,30), class = Intercept),
                   prior(normal(-30,10), class = b, coef = trt.yNPK),
                   prior(normal(0,10), class = b, coef = year.y.m),
                   prior(normal(0,10), class = b, coef = trt.yNPK:year.y.m),
                   prior(normal(0,10), class = sd),
                   prior(normal(0,10), class = sigma),
                   prior(constant(7), class = nu)),
                 #sample_prior = 'only',
                 backend = 'cmdstanr')

mcmc_plot(sl.3_test, type = 'nuts_divergence')
pp_check(sl.3_test)






