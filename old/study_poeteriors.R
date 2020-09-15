



rm(list=ls())

library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")


load('~/Dropbox/Projects/NutNet/Model_fits/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/rich.Rdata') # plot.rich.g


# site level meta data for posrteriors
# calculated to site level details found in Climate_Data.R
# latitude and longitude dont match due to decimal rounding
# lat.x long.x is nutnet site, lat.y long.y is world clim
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


meta <- meta %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()


study_levels <- plot.rich.3$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

study_sample_posterior <- study_levels %>%
  mutate( rich.ctl = purrr::map(data, ~posterior_samples(plot.rich.3,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.ctl = purrr::map(data, ~posterior_samples(plot.bm.3,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
             rich.trt = purrr::map(data, ~posterior_samples(plot.rich.3,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.trt = purrr::map(data, ~posterior_samples(plot.bm.3,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))

View(study_sample_posterior)




rich_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(rich.ctl,rich.trt)


View(rich_posterior)
study.rich.p.ctl <-  rich_posterior %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(rich.ctl),
          eff_lower = quantile(rich.ctl, probs=0.025),
          eff_upper = quantile(rich.ctl, probs=0.975))  %>%
  select(-c(rich.ctl,rich.trt)) %>% distinct()   


study.rich.p.npk <-  rich_posterior %>% group_by(site_code) %>%
  mutate( rich.trt.add=(rich.ctl+rich.trt)) %>%
  mutate( response="NPK", eff = mean(rich.trt.add),
          eff_lower = quantile(rich.trt.add, probs=0.025),
          eff_upper = quantile(rich.trt.add, probs=0.975))  %>%
  select(-c(rich.ctl,rich.trt,rich.trt.add)) %>% distinct()  


study.rich.p <- bind_rows(study.rich.p.ctl,study.rich.p.npk)

study.rich.p

bm_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(bm.ctl,bm.trt)


study.bm.p.ctl <-  bm_posterior %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(bm.ctl),
          eff_lower = quantile(bm.ctl, probs=0.025),
          eff_upper = quantile(bm.ctl, probs=0.975))  %>%
  select(-c(bm.ctl,bm.trt)) %>% distinct()   


study.bm.p.npk <-  bm_posterior %>% group_by(site_code) %>%
  mutate( bm.trt.add=(bm.ctl+bm.trt)) %>%
  mutate( response="NPK", eff = mean(bm.trt.add),
          eff_lower = quantile(bm.trt.add, probs=0.025),
          eff_upper = quantile(bm.trt.add, probs=0.975))  %>%
  select(-c(bm.ctl,bm.trt,bm.trt.add)) %>% distinct()  


study.bm.p <- bind_rows(study.bm.p.ctl,study.bm.p.npk)

study.bm.p



setwd('~/Dropbox/Projects/NutNet/Data/')
save(study.rich.p, study.bm.p, file = 'study.p.effs.Rdata')


