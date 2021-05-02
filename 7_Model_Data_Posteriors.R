
# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 5 Pull Data out of models and prep for visualization
# models have been run on a cluster and each have an R script and a submit script (.sh)
# see R scripts for code to run models
# models are large and take a few hours to run

# load packages
library(tidyverse)
library(brms)


# load models
# model object names follow each model
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # plot.bm.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # plot.rich.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.3
load('~/Desktop/test_mods/ps.Rdata') # ps.3_sigma

meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- meta %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

# STUDY LEVEL POSTERIORS
study_levels <- plot.rich.3$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

# extract 1000 study-level posterior samples for each site, and treatment  (Control, NPK) for each model  
study_sample_posterior <- study_levels %>%
  mutate( rich.ctl.study = purrr::map(data, ~posterior_samples(plot.rich.3,
                                                         pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                         exact = TRUE,
                                                         subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          bm.ctl.study = purrr::map(data, ~posterior_samples(plot.bm.3,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          rich.npk.study = purrr::map(data, ~posterior_samples(plot.rich.3,
                                                         pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                         exact = TRUE,
                                                         subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          bm.npk.study = purrr::map(data, ~posterior_samples(plot.bm.3,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          sl.ctl.study = purrr::map(data, ~posterior_samples(sl.3,
                                                             pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000,
                                                                                  min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sg.ctl.study = purrr::map(data, ~posterior_samples(sg.3,
                                                             pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000,
                                                                                  min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sloss.ctl.study = purrr::map(data, ~posterior_samples(s.loss.3,
                                                                pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                                exact = TRUE,
                                                                subset = floor(runif(n = 1000,
                                                                                     min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sgain.ctl.study = purrr::map(data, ~posterior_samples(s.gain.3,
                                                                pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                                exact = TRUE,
                                                                subset = floor(runif(n = 1000,
                                                                                     min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          ps.ctl.study = purrr::map(data, ~posterior_samples(ps.3_sigma,
                                                             pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000,
                                                                                  min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          cde.ctl.study = purrr::map(data, ~posterior_samples(CDE.3,
                                                              pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                              exact = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          sl.npk.study = purrr::map(data, ~posterior_samples(sl.3, 
                                                             pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000,min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sg.npk.study = purrr::map(data, ~posterior_samples(sg.3,
                                                             pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000,
                                                                                  min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sloss.npk.study = purrr::map(data, ~posterior_samples(s.loss.3, 
                                                                pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                                exact = TRUE,
                                                                subset = floor(runif(n = 1000,min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sgain.npk.study = purrr::map(data, ~posterior_samples(s.gain.3,
                                                                pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                                exact = TRUE,
                                                                subset = floor(runif(n = 1000,
                                                                                     min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          ps.npk.study = purrr::map(data, ~posterior_samples(ps.3_sigma,
                                                             pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                             exact = TRUE,
                                                             subset = floor(runif(n = 1000,
                                                                                  min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          cde.npk.study = purrr::map(data, ~posterior_samples(CDE.3,
                                                              pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                              exact = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          
          )

View(study_sample_posterior)


rich_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(rich.ctl.study,rich.npk.study) %>%
  mutate( rich.trt.study = (rich.ctl.study + rich.npk.study))

head(rich_study_posterior)
nrow(rich_study_posterior)

bm_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(bm.ctl.study,bm.npk.study) %>%
  mutate( bm.trt.study = (bm.ctl.study + bm.npk.study)) 


head(bm_study_posterior)

sl_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(sl.ctl.study,sl.npk.study) %>%
  mutate( sl.trt.study = (sl.ctl.study + sl.npk.study)) 

head(sl_study_posterior)

sg_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(sg.ctl.study,sg.npk.study) %>%
  mutate( sg.trt.study = (sg.ctl.study + sg.npk.study)) 

head(sg_study_posterior)

cde_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(cde.ctl.study,cde.npk.study) %>%
  mutate( cde.trt.study = (cde.ctl.study + cde.npk.study)) 

head(cde_study_posterior)


sloss_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(sloss.ctl.study,sloss.npk.study) %>%
  mutate( sloss.trt.study = (sloss.ctl.study + sloss.npk.study)) 

head(sloss_study_posterior)

sgain_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(sgain.ctl.study,sgain.npk.study) %>%
  mutate( sgain.trt.study = (sgain.ctl.study + sgain.npk.study)) 

head(sgain_study_posterior)

ps_study_posterior <- study_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(ps.ctl.study,ps.npk.study) %>%
  mutate( ps.trt.study = (ps.ctl.study + ps.npk.study)) 

head(sg_study_posterior)

# Extract 1000 posterior samples from Fixed Effects (Overall/Population/Global Effects) 
# richness and biomass
rich.fixed.p<-posterior_samples(plot.rich.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))
bm.fixed.p<-posterior_samples(plot.bm.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
# price  reponses
sl.fixed.p<-posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p<-posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )

sloss.fixed.p<-posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )
ps.fixed.p <- posterior_samples(ps.3_sigma, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 


# select columns of interests and give meaningful names
rich_global_posterior <-  rich.fixed.p %>% dplyr::select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(rich.ctl.global =`b_year_trt`,
         rich.npk.global =`b_trtNPK:year_trt`,
         rich.trt.global = (`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  dplyr::select(-c(`b_year_trt`,`b_trtNPK:year_trt`))

head(rich_global_posterior)
nrow(rich_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
rich.p.npk <-  rich_global_posterior %>% 
  mutate( response="NPK", eff = mean(rich.trt.global),
          eff_lower = quantile(rich.trt.global, probs=0.025),
          eff_upper = quantile(rich.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

rich.p.ctl <-  rich_global_posterior %>% 
  mutate( response="Control", eff = mean(rich.ctl.global),
          eff_lower = quantile(rich.ctl.global, probs=0.025),
          eff_upper = quantile(rich.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.rich.p <- bind_rows(rich.p.npk,rich.p.ctl)

global.rich.p

# biomass
bm_global_posterior <-  bm.fixed.p %>% dplyr::select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(bm.ctl.global =`b_year_trt`,
         bm.npk.global=`b_trtNPK:year_trt`,
         bm.trt.global=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  dplyr::select(-c(`b_year_trt`,`b_trtNPK:year_trt`)) 

head(bm_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
bm.p.npk <-  bm_global_posterior %>% 
  mutate( response="NPK", eff = mean(bm.trt.global),
          eff_lower = quantile(bm.trt.global, probs=0.025),
          eff_upper = quantile(bm.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

bm.p.ctl <-  bm_global_posterior %>% 
  mutate( response="Control", eff = mean(bm.ctl.global),
          eff_lower = quantile(bm.ctl.global, probs=0.025),
          eff_upper = quantile(bm.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.bm.p <- bind_rows(bm.p.npk,bm.p.ctl)

global.bm.p


# SL : biomass change associated with species loss
sl_global_posterior <-  sl.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sl.ctl.global =`b_year.y.m`,
         sl.npk.global=`b_trt.yNPK:year.y.m`,
         sl.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sl_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
sl.p.npk <-  sl_global_posterior %>% 
  mutate( response="NPK", eff = mean(sl.trt.global),
          eff_lower = quantile(sl.trt.global, probs=0.025),
          eff_upper = quantile(sl.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

sl.p.ctl <-  sl_global_posterior %>% 
  mutate( response="Control", eff = mean(sl.ctl.global),
          eff_lower = quantile(sl.ctl.global, probs=0.025),
          eff_upper = quantile(sl.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.sl.p <- bind_rows(sl.p.npk,sl.p.ctl)

global.sl.p


# SG : biomass change associated with species gains
sg_global_posterior <-  sg.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sg.ctl.global =`b_year.y.m`,
         sg.npk.global=`b_trt.yNPK:year.y.m`,
         sg.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sg_global_posterior)


# take the mean quantiles of the fixed effects posteriors for each treatment
sg.p.npk <-  sg_global_posterior %>% 
  mutate( response="NPK", eff = mean(sg.trt.global),
          eff_lower = quantile(sg.trt.global, probs=0.025),
          eff_upper = quantile(sg.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

sg.p.ctl <-  sg_global_posterior %>% 
  mutate( response="Control", eff = mean(sg.ctl.global),
          eff_lower = quantile(sg.ctl.global, probs=0.025),
          eff_upper = quantile(sg.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.sg.p <- bind_rows(sg.p.npk,sg.p.ctl)

global.sg.p

# species loss
sloss_global_posterior <-  sloss.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sloss.ctl.global =`b_year.y.m`,
         sloss.npk.global=`b_trt.yNPK:year.y.m`,
         sloss.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sloss_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
sloss.p.npk <-  sloss_global_posterior %>% 
  mutate( response="NPK", eff = mean(sloss.trt.global),
          eff_lower = quantile(sloss.trt.global, probs=0.025),
          eff_upper = quantile(sloss.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

sloss.p.ctl <-  sloss_global_posterior %>% 
  mutate( response="Control", eff = mean(sloss.ctl.global),
          eff_lower = quantile(sloss.ctl.global, probs=0.025),
          eff_upper = quantile(sloss.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.sloss.p <- bind_rows(sloss.p.npk,sloss.p.ctl)

global.sloss.p

# species gain
sgain_global_posterior <-  sgain.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sgain.ctl.global =`b_year.y.m`,
         sgain.npk.global=`b_trt.yNPK:year.y.m`,
         sgain.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sgain_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
sgain.p.npk <-  sgain_global_posterior %>% 
  mutate( response="NPK", eff = mean(sgain.trt.global),
          eff_lower = quantile(sgain.trt.global, probs=0.025),
          eff_upper = quantile(sgain.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

sgain.p.ctl <-  sgain_global_posterior %>% 
  mutate( response="Control", eff = mean(sgain.ctl.global),
          eff_lower = quantile(sgain.ctl.global, probs=0.025),
          eff_upper = quantile(sgain.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.sgain.p <- bind_rows(sgain.p.npk,sgain.p.ctl)

global.sgain.p

# persistent species
head(ps.fixed.p)

ps_global_posterior <-  ps.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ps.ctl.global = (`b_year.y.m` ),
         ps.npk.global= (`b_trt.yNPK:year.y.m` ),
         ps.trt.global=(`b_year.y.m` + `b_sigma_trt.yControl`) ) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`))

head(ps_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
ps.p.npk <-  ps_global_posterior %>% 
  mutate( response="NPK", eff = mean(ps.trt.global),
          eff_lower = quantile(ps.trt.global, probs=0.025),
          eff_upper = quantile(ps.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

ps.p.ctl <-  ps_global_posterior %>% 
  mutate( response="Control", eff = mean(ps.ctl.global),
          eff_lower = quantile(ps.ctl.global, probs=0.025),
          eff_upper = quantile(ps.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.ps.p <- bind_rows(ps.p.npk,ps.p.ctl)

global.ps.p

# cde or biomass change associated with persistent species
cde_global_posterior <-  cde.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.global =`b_year.y.m`,
         cde.npk.global=`b_trt.yNPK:year.y.m`,
         cde.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(cde_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
cde.p.npk <-  cde_global_posterior %>% 
  mutate( response="NPK", eff = mean(cde.trt.global),
          eff_lower = quantile(cde.trt.global, probs=0.025),
          eff_upper = quantile(cde.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

cde.p.ctl <-  cde_global_posterior %>% 
  mutate( response="Control", eff = mean(cde.ctl.global),
          eff_lower = quantile(cde.ctl.global, probs=0.025),
          eff_upper = quantile(cde.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.cde.p <- bind_rows(cde.p.npk,cde.p.ctl)

global.cde.p

# use the mean and quantiles for :
# Inset effect plots in Figure 2 a & b,
# In Figure 2c for overall effects
# Inset effect plots in Figure 3 b-f
setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(global.rich.p, global.bm.p,global.sl.p,global.sg.p,global.cde.p,global.sloss.p,global.sgain.p,global.ps.p, file = 'global.p.effs.Rdata')


# combine 1000 posterior samples with 1000 study levels posteriors to get study level effects
rich.site.p <- rich_study_posterior %>% group_by(site_code) %>%
  nest() 

rich.p <- rich.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(rich_global_posterior)))) %>%
  unnest() %>% mutate(rich.study.trt.effect = (rich.trt.study + rich.trt.global)) %>%
  mutate(rich.study.ctl.effect = (rich.ctl.study + rich.ctl.global))

# biomass
bm.site.p <- bm_study_posterior %>% group_by(site_code) %>%
  nest() 

bm.p <- bm.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(bm_global_posterior)))) %>%
  unnest() %>% mutate(bm.study.trt.effect = (bm.trt.study + bm.trt.global))  %>%
  mutate(bm.study.ctl.effect = (bm.ctl.study + bm.ctl.global))


sl.site.p <- sl_study_posterior %>% group_by(site_code) %>%
  nest() 

sl.p <- sl.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sl_global_posterior)))) %>%
  unnest() %>% mutate(sl.study.trt.effect = (sl.trt.study + sl.trt.global))  %>%
  mutate(sl.study.ctl.effect = (sl.ctl.study + sl.ctl.global))


sg.site.p <- sg_study_posterior %>% group_by(site_code) %>%
  nest() 

sg.p <- sg.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sg_global_posterior)))) %>%
  unnest() %>% mutate(sg.study.trt.effect = (sg.trt.study + sg.trt.global))  %>%
  mutate(sg.study.ctl.effect = (sg.ctl.study + sg.ctl.global))



sloss.site.p <- sloss_study_posterior %>% group_by(site_code) %>%
  nest() 

sloss.p <- sloss.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sloss_global_posterior)))) %>%
  unnest() %>% mutate(sloss.study.trt.effect = (sloss.trt.study + sloss.trt.global))  %>%
  mutate(sloss.study.ctl.effect = (sloss.ctl.study + sloss.ctl.global))


sgain.site.p <- sgain_study_posterior %>% group_by(site_code) %>%
  nest() 

sgain.p <- sgain.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sgain_global_posterior)))) %>%
  unnest() %>% mutate(sgain.study.trt.effect = (sgain.trt.study + sgain.trt.global))  %>%
  mutate(sgain.study.ctl.effect = (sgain.ctl.study + sgain.ctl.global))


head(sgain.p)

ps.site.p <- ps_study_posterior %>% group_by(site_code) %>%
  nest() 


ps.p <- ps.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(ps_global_posterior)))) %>%
  unnest() %>% mutate(ps.study.trt.effect = (ps.trt.study + ps.trt.global))  %>%
  mutate(ps.study.ctl.effect = (ps.ctl.study + ps.ctl.global))


head(ps.p)

cde.site.p <- cde_study_posterior %>% group_by(site_code) %>%
  nest() 

cde.p <- cde.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(cde_global_posterior)))) %>%
  unnest() %>% mutate(cde.study.trt.effect = (cde.trt.study + cde.trt.global))  %>%
  mutate(cde.study.ctl.effect = (cde.ctl.study + cde.ctl.global))


head(cde.p)


# calculate the mean and quantiles for every study and keep only the effects we are interested in
study.rich.p.npk <-  rich.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(rich.study.trt.effect),
          eff_lower = quantile(rich.study.trt.effect, probs=0.025),
          eff_upper = quantile(rich.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.rich.p.ctl <-  rich.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(rich.study.ctl.effect),
          eff_lower = quantile(rich.study.ctl.effect, probs=0.025),
          eff_upper = quantile(rich.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.rich.p <- bind_rows(study.rich.p.npk,study.rich.p.ctl)

head(study.rich.p)

study.bm.p.npk <-  bm.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(bm.study.trt.effect),
          eff_lower = quantile(bm.study.trt.effect, probs=0.025),
          eff_upper = quantile(bm.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.bm.p.ctl <-  bm.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(bm.study.ctl.effect),
          eff_lower = quantile(bm.study.ctl.effect, probs=0.025),
          eff_upper = quantile(bm.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.bm.p <- bind_rows(study.bm.p.npk,study.bm.p.ctl)

head(study.bm.p)



study.sl.p.npk <-  sl.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(sl.study.trt.effect),
          eff_lower = quantile(sl.study.trt.effect, probs=0.025),
          eff_upper = quantile(sl.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sl.p.ctl <-  sl.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(sl.study.ctl.effect),
          eff_lower = quantile(sl.study.ctl.effect, probs=0.025),
          eff_upper = quantile(sl.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sl.p <- bind_rows(study.sl.p.npk,study.sl.p.ctl)

head(study.sl.p)



study.sg.p.npk <-  sg.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(sg.study.trt.effect),
          eff_lower = quantile(sg.study.trt.effect, probs=0.025),
          eff_upper = quantile(sg.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sg.p.ctl <-  sg.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(sg.study.ctl.effect),
          eff_lower = quantile(sg.study.ctl.effect, probs=0.025),
          eff_upper = quantile(sg.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sg.p <- bind_rows(study.sg.p.npk,study.sg.p.ctl)

head(study.sg.p)

study.cde.p.npk <-  cde.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(cde.study.trt.effect),
          eff_lower = quantile(cde.study.trt.effect, probs=0.025),
          eff_upper = quantile(cde.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.cde.p.ctl <-  cde.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(cde.study.ctl.effect),
          eff_lower = quantile(cde.study.ctl.effect, probs=0.025),
          eff_upper = quantile(cde.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.cde.p <- bind_rows(study.cde.p.npk,study.cde.p.ctl)

study.sloss.p.npk <-  sloss.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(sloss.study.trt.effect),
          eff_lower = quantile(sloss.study.trt.effect, probs=0.025),
          eff_upper = quantile(sloss.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sloss.p.ctl <-  sloss.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(sloss.study.ctl.effect),
          eff_lower = quantile(sloss.study.ctl.effect, probs=0.025),
          eff_upper = quantile(sloss.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sloss.p <- bind_rows(study.sloss.p.npk,study.sloss.p.ctl)

View(study.sloss.p)

study.sgain.p.npk <-  sgain.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(sgain.study.trt.effect),
          eff_lower = quantile(sgain.study.trt.effect, probs=0.025),
          eff_upper = quantile(sgain.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sgain.p.ctl <-  sgain.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(sgain.study.ctl.effect),
          eff_lower = quantile(sgain.study.ctl.effect, probs=0.025),
          eff_upper = quantile(sgain.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.sgain.p <- bind_rows(study.sgain.p.npk,study.sgain.p.ctl)

View(study.sgain.p)

study.ps.p.npk <-  ps.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(ps.study.trt.effect),
          eff_lower = quantile(ps.study.trt.effect, probs=0.025),
          eff_upper = quantile(ps.study.trt.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.ps.p.ctl <-  ps.p %>% group_by(site_code) %>%
  mutate( response="Control", eff = mean(ps.study.ctl.effect),
          eff_lower = quantile(ps.study.ctl.effect, probs=0.025),
          eff_upper = quantile(ps.study.ctl.effect, probs=0.975))  %>%
  dplyr::select(c(site_code,eff,eff_upper,eff_lower,response)) %>% distinct()  

study.ps.p <- bind_rows(study.ps.p.npk,study.ps.p.ctl)

# we use this in:
# Figure 2c study level effects
setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(study.rich.p, study.bm.p,study.sl.p,study.sg.p,study.cde.p,study.sloss.p,study.sgain.p, study.ps.p, file = 'study.p.effs.Rdata')

# Use these same data to calculate categories for:
# Figure 2c, Figure 5, Figure S1,  Figure S5, Table S1
study.rich.p2 <- study.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) 

study.bm.p2 <- study.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) 

study.effs.p <- study.rich.p2 %>% left_join(study.bm.p2) %>% filter(response == "NPK")

colnames(study.effs.p)

study.effs.p$Quadrant <- ifelse(study.effs.p$r.eff < 0 & study.effs.p$b.eff > 0, '+biomass -rich',
                                ifelse(study.effs.p$r.eff < 0 & study.effs.p$b.eff < 0,  '-biomass -rich',
                                       ifelse(study.effs.p$r.eff  > 0 & study.effs.p$b.eff > 0,  '+biomass +rich',
                                              ifelse(study.effs.p$r.eff > 0 & study.effs.p$b.eff < 0, '-biomass +rich','other'))))

study.effs.p$Quadrant <- factor(study.effs.p$Quadrant, levels= c("+biomass -rich",  "+biomass +rich", "-biomass -rich", "-biomass +rich"))

Quads <- study.effs.p %>% select(site_code, Quadrant)

write.csv(Quads,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv" )


# use same data again for:

# label the posteriors from each model in a column, 'Model
study.rich.p$Model<- "Species Richness"
study.bm.p$Model<- "Biomass"
study.sloss.p$Model<- "Species Loss"
study.sgain.p$Model<- "Species Gain"
study.ps.p$Model<- "Persistent Species"
study.sl.p$Model<- "Change Biomass Due to Species Loss"
study.sg.p$Model<- "Change in Biomass Due to Species Gain"
study.cde.p$Model<- "Persistent Species Change in Biomass"


# bind all the posteriors together
p.study.all <- study.rich.p %>% bind_rows(study.bm.p) %>% bind_rows(study.sloss.p) %>% bind_rows(study.sgain.p) %>%
  bind_rows(study.ps.p) %>%bind_rows(study.sl.p) %>% bind_rows(study.sg.p) %>% bind_rows(study.cde.p) %>%
  mutate(Treatment = response,
         Estimate = eff,
         Upper_CI = eff_upper,
         Lower_CI = eff_lower) %>%
  select(-response,-eff,-eff_upper,-eff_lower)


View(p.study.all)

# For Shiny App Study Level Stats and Study-Level Table
setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(p.study.all, file = 'study.p.all.Rdata')



# Figure 5 & Shiny App
sloss.t <- study.sloss.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sloss.trt.rate.p = eff) %>%
  select(-response,-eff)

sloss.c <- study.sloss.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sloss.ctl.rate.p = eff) %>%
  select(-response,-eff)

sloss.eff <- left_join(sloss.t,sloss.c)

sgain.t <- study.sgain.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sgain.trt.rate.p = eff) %>%
  select(-response,-eff)

sgain.c <- study.sgain.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sgain.ctl.rate.p = eff) %>%
  select(-response,-eff)

sgain.eff <- left_join(sgain.t,sgain.c)

ps.t <- study.ps.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(ps.trt.rate.p = eff) %>%
  select(-response,-eff)

ps.c <- study.ps.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(ps.ctl.rate.p = eff) %>%
  select(-response,-eff)

ps.eff <- left_join(ps.t,ps.c)


sl.t <- study.sl.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sl.trt.rate.p = eff) %>%
  select(-response,-eff)

sl.c <- study.sl.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sl.ctl.rate.p = eff) %>%
  select(-response,-eff)

sl.eff <- left_join(sl.t,sl.c)

sg.t <- study.sg.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sg.trt.rate.p = eff) %>%
  select(-response,-eff)

sg.c <- study.sg.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sg.ctl.rate.p = eff) %>%
  select(-response,-eff)

sg.eff <- left_join(sg.t,sg.c)

cde.t <- study.cde.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(cde.trt.rate.p = eff) %>%
  select(-response,-eff)

cde.c <- study.cde.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(cde.ctl.rate.p = eff) %>%
  select(-response,-eff)

cde.eff <- left_join(cde.t,cde.c)

sloss.sgain.effs<- left_join(sloss.eff,sgain.eff) %>% left_join(ps.eff)

sg.sl.eff<-left_join(sg.eff,sl.eff)

price.eff<-left_join(sg.sl.eff,cde.eff)

all.effs <- left_join(price.eff,sloss.sgain.effs)

View(all.effs)

# For : Figure 5 to plot mean NPK study-level effects
 all.effs <- all.effs %>% left_join(Quads) %>% select(-X)

head(all.effs)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(all.effs, file = 'study.price.p.effs.Rdata')

