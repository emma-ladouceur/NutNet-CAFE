
# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 5 Data Prep: Figure 2
# This workflow pulls Data out of model objects and preps data for visualization in Figures
# models have been run on a cluster and each have an R script and a submit script (.sh)
# see R scripts for code to run models
# models are large and take a few hours to run
# this script prepares all data needed for Figure 2 a), b) & c)

# packages
library(tidyverse)
library(brms)

# plot level data
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- plot %>% filter(max.year >= 3)

View(plot)

# model objects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # plot.bm.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # plot.rich.3

# species richness model


summary(plot.rich.3)


# each of these steps takes a few minutes because the model objects are large
# fixed effects
plot.rich_fitted <- cbind(plot.rich.3$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(plot.rich.3, re_formula = NA)) %>% 
  as_tibble() 

head(plot.rich_fitted)

# fixed effect coefficients 
plot.rich_fixef <- fixef(plot.rich.3)

# predict estimates for each site across a sequence of year_trt's
# this takes ~ 5 minutes
obs_nest.rich <- plot %>% 
  mutate(site_group = site_code) %>%
  group_by(site_group, site_code, trt) %>% 
  summarise(year_trt = seq(min(year_trt), max(year_trt), length.out = 13 )) %>%
  nest(data = c(site_code,year_trt,trt)) %>%
  mutate(predicted = map(data, ~predict(plot.rich.3, newdata= .x, re_formula = ~(trt * year_trt | site_code) ))) 

View(obs_nest.rich)

# coefficients for site-level (random) effects
plot.rich_coef <- coef(plot.rich.3)

plot.rich_coef2 <-  bind_cols(plot.rich_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(plot.rich_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'year_trt'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK:year_trt'] %>% 
                                as_tibble() %>% 
                                mutate(TESlope = Estimate,
                                       TESlope_lower = Q2.5,
                                       TESlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code') 

head(plot.rich_coef2)

plot.rich_fitted.npk <- plot.rich_fitted %>% filter(trt %in% c('NPK'))
plot.rich_fitted.ctl <- plot.rich_fitted %>% filter(trt %in% c('Control'))

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(plot.rich_fitted,plot.rich_fixef,plot.rich_fitted.npk,plot.rich_fitted.ctl,obs_nest.rich,plot.rich_coef2,file = 'rich.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich.mod.dat.Rdata')


# biomass model 
# fixed effects
plot.bm_fitted <- cbind(plot.bm.3$data,
                        # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                        fitted(plot.bm.3, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.bm_fixef <- fixef(plot.bm.3)

# predict estimates for each site across a sequence of year_trt's
# this takes ~ 5 minutes
obs_nest.bm <- plot %>% 
  mutate(site_group = site_code) %>%
  group_by(site_group, site_code,trt) %>% 
  summarise(year_trt = seq(min(year_trt), max(year_trt), length.out = 13 )) %>%
  nest(data = c(site_code,year_trt,trt)) %>%
  mutate(predicted = map(data, ~predict(plot.bm.3, newdata= .x, re_formula = ~(trt * year_trt | site_code) ))) 

View(obs_nest.bm)

# coefficients for experiment-level (random) effects
plot.bm_coef <- coef(plot.bm.3)


plot.bm_coef2 <-  bind_cols(plot.bm_coef$site_code[,,'Intercept'] %>% 
                              as_tibble() %>% 
                              mutate(Intercept = Estimate,
                                     Intercept_lower = Q2.5,
                                     Intercept_upper = Q97.5,
                                     site_code = rownames(plot.bm_coef$site_code[,,'Intercept'])) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            plot.bm_coef$site_code[,,'year_trt'] %>% 
                              as_tibble() %>% 
                              mutate(ISlope = Estimate,
                                     ISlope_lower = Q2.5,
                                     ISlope_upper = Q97.5) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            plot.bm_coef$site_code[,,'trtNPK'] %>% 
                              as_tibble() %>% 
                              mutate(TE = Estimate,
                                     TE_lower = Q2.5,
                                     TE_upper = Q97.5) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            plot.bm_coef$site_code[,,'trtNPK:year_trt'] %>% 
                              as_tibble() %>% 
                              mutate(TESlope = Estimate,
                                     TESlope_lower = Q2.5,
                                     TESlope_upper = Q97.5) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code') 



plot.bm_fitted.npk <- plot.bm_fitted %>% filter(trt %in% c('NPK'))
plot.bm_fitted.ctl <- plot.bm_fitted %>% filter(trt %in% c('Control'))


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(plot.bm_fitted,plot.bm_fixef,plot.bm_fitted.npk,plot.bm_fitted.ctl,obs_nest.bm,plot.bm_coef3,file = 'bm.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')


# need p.effs



