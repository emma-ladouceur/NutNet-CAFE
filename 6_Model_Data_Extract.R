

# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 5 Data Prep: Figure 3
# This workflow pulls Data out of model objects and preps data for visualization in Figures
# models have been run on a cluster and each have an R script and a submit script (.sh)
# see R scripts for code to run models
# models are large and take a few hours to run
# this script prepares all data needed for Figure 3 a)-f)

# packages
library(tidyverse)
library(brms)

# data
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# load model objects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.3


# Species Loss model
summary(s.loss.3)

# fixed effects
sloss.trt_fitted <- cbind(s.loss.3$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(s.loss.3, re_formula = NA)) %>% 
  as_tibble()


sloss.trt_fitted.npk <- sloss.trt_fitted %>% filter(trt.y %in% c('NPK'))
sloss.trt_fitted.ctl <- sloss.trt_fitted  %>% filter(trt.y %in% c('Control'))


# fixed effect coefficients 
sloss.trt_fixef <- fixef(s.loss.3)

# predict estimates for each site across a sequence of year.y (comparison plots age)
# this takes ~ 5 minutes

head(p.all)

obs_nest.sloss <- plot %>% 
  mutate(site_group = site_code) %>%
  group_by(site_group, site_code, trt.y) %>% 
  summarise(year.y = seq(min(year.y), max(year.y), length.out = 13 ),
            year.y.m = seq(min(year.y.m), max(year.y.m), length.out = 13) ) %>%
  nest(data = c(site_code,year.y, year.y.m, trt.y)) %>%
  mutate(predicted = map(data, ~predict(s.loss.3, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code) )))

View(obs_nest.sloss)

# coefficients for study-level (random) effects
sloss.trt_coef <- coef(s.loss.s)

sloss.trt_coef2 <-  bind_cols(sloss.trt_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(sloss.trt_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sloss.trt_coef$site_code[,,'year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sloss.trt_coef$site_code[,,'trt.yNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sloss.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(TESlope = Estimate,
                                       TESlope_lower = Q2.5,
                                       TESlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(sloss.trt_fitted,sloss.trt_fitted.npk,sloss.trt_fitted.ctl,obs_nest.sloss,sloss.trt_coef3,file = 'sloss.n.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


# Species Gain

sgain.trt_fitted <- cbind(s.gain.3$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(s.gain.s, re_formula = NA)) %>% 
  as_tibble() 


sgain.trt_fitted.npk<-sgain.trt_fitted3[sgain.trt_fitted3$trt.y %in% c('NPK'),]
sgain.trt_fitted.ctl<-sgain.trt_fitted3[sgain.trt_fitted3$trt.y %in% c('Control'),]


# fixed effect coefficients 
sgain.trt_fixef <- fixef(s.gain.s)

obs_nest.sgain <- plot %>% 
  mutate(site_group = site_code) %>%
  group_by(site_group, site_code, trt.y) %>% 
  summarise(year.y = seq(min(year.y), max(year.y), length.out = 13 ),
            year.y.m = seq(min(year.y.m), max(year.y.m), length.out = 13) ) %>%
  nest(data = c(site_code,year.y, year.y.m, trt.y)) %>%
  mutate(predicted = map(data, ~predict(s.gain.3, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code) )))

View(obs_nest.sgain)

# coefficients for study-level (random) effects
sgain.trt_coef <- coef(s.gain.s)

sgain.trt_coef2 <-  bind_cols(sgain.trt_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(sgain.trt_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sgain.trt_coef$site_code[,,'year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sgain.trt_coef$site_code[,,'trt.yNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sgain.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(TESlope = Estimate,
                                       TESlope_lower = Q2.5,
                                       TESlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m),
                         by = 'site_code')) 

# note to self change sgain file name to match others later
setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(sgain.trt_fitted.npk,sgain.trt_fitted.ctl,obs_nest.sgain,sgain.trt_coef3,file = 'sgain_dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain_dat.Rdata')


# SL- biomass change associated with species loss
# fixed effects
sl.trt_fitted <- cbind(sl.s$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(sl.s, re_formula = NA)) %>% 
  as_tibble()



sl.trt_fitted.npk<-sl.trt_fitted3[sl.trt_fitted3$trt.y %in% c('NPK'),]
sl.trt_fitted.ctl<-sl.trt_fitted3[sl.trt_fitted3$trt.y %in% c('Control'),]


# fixed effect coefficients 
sl.trt_fixef <- fixef(sl.s)


obs_nest.sl <- plot %>% 
  mutate(site_group = site_code) %>%
  group_by(site_group, site_code, trt.y) %>% 
  summarise(year.y = seq(min(year.y), max(year.y), length.out = 13 ),
            year.y.m = seq(min(year.y.m), max(year.y.m), length.out = 13) ) %>%
  nest(data = c(site_code,year.y, year.y.m, trt.y)) %>%
  mutate(predicted = map(data, ~predict(sl.3, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code) )))

View(obs_nest.sl)

# coefficients for study-level (random) effects
sl.trt_coef <- coef(sl.s)

sl.trt_coef2 <-  bind_cols(sl.trt_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(sl.trt_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sl.trt_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sl.trt_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sl.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(TESlope = Estimate,
                                    TESlope_lower = Q2.5,
                                    TESlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(sl.trt_fitted.npk,sl.trt_fitted.ctl,obs_nest.sl,sl.trt_coef3,file = 'sl.n.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl.n.mod.dat.Rdata')



# SG: biomass change associated with species gains

sg.trt_fitted <- cbind(sg.s$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(sg.s, re_formula = NA)) %>% 
  as_tibble() 


sg.trt_fitted.npk<-sg.trt_fitted3[sg.trt_fitted3$trt.y %in% c('NPK'),]
sg.trt_fitted.ctl<-sg.trt_fitted3[sg.trt_fitted3$trt.y %in% c('Control'),]

# fixed effect coefficients 
sg.trt_fixef <- fixef(sg.s)

# predicted values
obs_nest.sg <- plot %>% 
  mutate(site_group = site_code) %>%
  group_by(site_group, site_code, trt.y) %>% 
  summarise(year.y = seq(min(year.y), max(year.y), length.out = 13 ),
            year.y.m = seq(min(year.y.m), max(year.y.m), length.out = 13) ) %>%
  nest(data = c(site_code,year.y, year.y.m, trt.y)) %>%
  mutate(predicted = map(data, ~predict(sg.3, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code) )))

View(obs_nest.sg)

# coefficients for study-level (random) effects
sg.trt_coef <- coef(sg.s)


sg.trt_coef2 <-  bind_cols(sg.trt_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(sg.trt_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sg.trt_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sg.trt_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sg.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(TESlope = Estimate,
                                    TESlope_lower = Q2.5,
                                    TESlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(sg.trt_fitted.npk,sg.trt_fitted.ctl,obs_nest.sg,sg.trt_coef3,file = 'sg_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/sg_dat.Rdata')




# CDE- this variable is called CDE in previous price equation work
# which stands for context dependent effect
# here because of the temporal comparison approach we call is Persistant species (Ps)
# this is the biomass change associated with persistent species or species that are common between two plots in time


# fixed effects
cde_fitted <- cbind(CDE.s$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(CDE.s, re_formula = NA)) %>% 
  as_tibble() 


cde_fitted.npk<-cde_fitted3[cde_fitted3$trt.y %in% c('NPK'),]
cde_fitted.ctl<-cde_fitted3[cde_fitted3$trt.y %in% c('Control'),]

# fixed effect coefficients
cde_fixef <- fixef(CDE.s)

# predicted values
obs_nest.cde <- plot %>% 
  mutate(site_group = site_code) %>%
  group_by(site_group, site_code, trt.y) %>% 
  summarise(year.y = seq(min(year.y), max(year.y), length.out = 13 ),
            year.y.m = seq(min(year.y.m), max(year.y.m), length.out = 13) ) %>%
  nest(data = c(site_code,year.y, year.y.m, trt.y)) %>%
  mutate(predicted = map(data, ~predict(CDE.3, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code) )))

View(obs_nest.cde)

# coefficients for study-level (random) effects
cde_coef <- coef(CDE.s)

cde_coef2 <-  bind_cols(cde_coef$site_code[,,'Intercept'] %>% 
                          as_tibble() %>% 
                          mutate(Intercept = Estimate,
                                 Intercept_lower = Q2.5,
                                 Intercept_upper = Q97.5,
                                 site_code = rownames(cde_coef$site_code[,,'Intercept'])) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'year.y.m'] %>% 
                          as_tibble() %>% 
                          mutate(ISlope = Estimate,
                                 ISlope_lower = Q2.5,
                                 ISlope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'trt.yNPK'] %>% 
                          as_tibble() %>% 
                          mutate(TE = Estimate,
                                 TE_lower = Q2.5,
                                 TE_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                          as_tibble() %>% 
                          mutate(TESlope = Estimate,
                                 TESlope_lower = Q2.5,
                                 TESlope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat3 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') %>% left_join(start.rich)


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(cde_fitted.npk,cde_fitted.ctl,obs_nest.cde,cde_coef3,file = 'cde.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/cde.mod.dat.Rdata')





