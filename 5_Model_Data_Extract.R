
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
library(bayesplot)

# plot level data
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- plot %>% filter(year_max >= 3) 

plot %>% distinct(site_code,year_trt)

head(plot)

plot$site_code <- as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

# model objects
#load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # plot.bm.3
#load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # plot.rich.3

#selected best mods
load('~/Desktop/mods/3/bm_sigmai.Rdata') # bm.3_sigmai 
load('~/Desktop/mods/3/rich_sigmai.Rdata') # rich.3_sigmai


# species richness model

#  model summary
summary(rich.3_sigmai)
# caterpillar plots
plot(rich.3_sigmai)
# predicted values vs. observed
color_scheme_set("darkgray")
fig_s3a <- pp_check(rich.3_sigmai) + theme_classic() + 
  labs(x= "Species richness", y = "Density")

fig_s3a

# residuals (this take a minute)
rich.m <- residuals(rich.3_sigmai)
rich.m <-as.data.frame(rich.m)
rr.plot <- cbind(plot,rich.m$Estimate)
head(rr.plot)

par(mfrow=c(2,2))
with(rr.plot, plot(site_code, rich.m$Estimate))
with(rr.plot, plot(block, rich.m$Estimate))
with(rr.plot, plot(year_trt, rich.m$Estimate))
with(rr.plot, plot(plot, rich.m$Estimate))


# each of these steps may take a few minutes because the model objects are large
# fixed effects
plot.rich_fitted <- cbind(rich.3_sigmai$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(rich.3_sigmai, re_formula = NA)) %>% 
  as_tibble() 

head(plot.rich_fitted)

# fixed effect coefficients 
plot.rich_fixef <- fixef(rich.3_sigmai)

# predict estimates for each site across a sequence of year_trt's
# this takes ~ 5 minutes
# obs_nest.rich <- plot %>% 
#   mutate(site_group = site_code) %>%
#   group_by(site_group, site_code, trt) %>% 
#   summarise(year_trt = seq(min(year_trt), max(year_trt), length.out = 13 )) %>%
#   nest(data = c(site_code,year_trt,trt)) %>%
#   mutate(predicted = map(data, ~predict(rich.3_sigmai, newdata= .x, re_formula = ~(trt * year_trt | site_code) ))) 
# 
# View(obs_nest.rich)

# coefficients for site-level (random) effects
plot.rich_coef <- coef(rich.3_sigmai)


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

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(plot.rich_fitted,plot.rich_fixef,plot.rich_fitted.npk,plot.rich_fitted.ctl,plot.rich_coef2,file = 'rich.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich.mod.dat.Rdata')


# biomass model 


#  model summary
summary(bm.3_sigmai)
# caterpillar plots
plot(bm.3_sigmai)
# predicted values vs. observed
#color_scheme_set("darkgray")
fig_s3b <- pp_check(bm.3_sigmai) + theme_classic() + 
  labs( x = expression(paste('Biomass (g/',m^2, ')')) , y = "Density") + 
  scale_x_continuous(limits = c(-1000, 2000))

fig_s3b

# residuals (this take a minute)
colnames(plot)
plot.bm <- plot %>% filter(!is.na(plot.mass))
bm.m <- residuals(bm.3_sigmai)
bm.m <- as.data.frame(bm.m)
br.plot <- cbind(plot.bm, bm.m$Estimate)
head(br.plot)

par(mfrow=c(2,2))
with(br.plot, plot(site_code, bm.m$Estimate))
with(br.plot, plot(block, bm.m$Estimate))
with(br.plot, plot(year_trt, bm.m$Estimate))
with(br.plot, plot(plot, bm.m$Estimate))


# fixed effects
plot.bm_fitted <- cbind(bm.3_sigmai$data,
                        # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                        fitted(bm.3_sigmai, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.bm_fixef <- fixef(bm.3_sigmai)

# predict estimates for each site across a sequence of year_trt's
# this takes ~ 5 minutes
# obs_nest.bm <- plot %>% 
#   mutate(site_group = site_code) %>%
#   group_by(site_group, site_code,trt) %>% 
#   summarise(year_trt = seq(min(year_trt), max(year_trt), length.out = 13 )) %>%
#   nest(data = c(site_code,year_trt,trt)) %>%
#   mutate(predicted = map(data, ~predict(bm.3_sigmai, newdata= .x, re_formula = ~(trt * year_trt | site_code) ))) 

View(obs_nest.bm)

# coefficients for experiment-level (random) effects
plot.bm_coef <- coef(bm.3_sigmai)


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


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(plot.bm_fitted,plot.bm_fixef,plot.bm_fitted.npk,plot.bm_fitted.ctl,plot.bm_coef2,file = 'bm.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')



