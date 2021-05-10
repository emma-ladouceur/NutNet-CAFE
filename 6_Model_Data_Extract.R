

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
library(bayesplot)
library(patchwork)

# data
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(p.all)
p.all <- p.all %>% group_by(site_code) %>% #filter(max.year >= 3) 
  filter(year_max >= 3) 

p.all$site_code <- as.factor(p.all$site_code)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
p.all$year.y<-as.numeric(p.all$year.y)

# load model objects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/ps.Rdata') # CDE.3

# selected mods
load('~/Desktop/mods/sloss_sigmai.Rdata') # s.loss.3_sigma2
load('~/Desktop/mods/sgain_sigmai.Rdata') # s.gain.3_sigma2
load('~/Desktop/mods/sl.Rdata') # sl.3_sigma2
load('~/Desktop/mods/sg.Rdata') # sg.3_sigma2
load('~/Desktop/mods/cde_sigmai.Rdata') # CDE.3_sigma2


# Species Loss model

#  model summary
summary(sloss.3_sigmai)
# caterpillar plots
plot(sloss.3_sigmai)
# predicted values vs. observed
# color_scheme_set("darkgray")
fig_s3c <- pp_check(sloss.3_sigmai) + theme_classic() + 
  labs(x= "Species loss (s.loss)", y = "Density") + 
  scale_x_continuous(limits = c(-50, 50))

fig_s3c


# residuals (this take a minute)
colnames(p.all)
pairs.sloss <- p.all %>% filter(!is.na(s.loss.n))
pairs.sloss$year.y <- as.factor(pairs.sloss$year.y)
sloss.m <- residuals(sloss.3_sigmai)
sloss.m <- as.data.frame(sloss.m)
head(sloss.m)
sloss.plot <- cbind(pairs.sloss, sloss.m$Estimate)
head(sloss.plot)

par(mfrow=c(2,2))
with(sloss.plot, plot(site_code, sloss.m$Estimate))
with(sloss.plot, plot(block, sloss.m$Estimate))
with(sloss.plot, plot(year.y, sloss.m$Estimate))
with(sloss.plot, plot(plot, sloss.m$Estimate))


# fixed effects
sloss.trt_fitted <- cbind(sloss.3_sigmai$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(sloss.3_sigmai, re_formula = NA)) %>% 
  as_tibble() %>% left_join(p.all)


sloss.trt_fitted.npk <- sloss.trt_fitted %>% filter(trt.y %in% c('NPK'))
sloss.trt_fitted.ctl <- sloss.trt_fitted  %>% filter(trt.y %in% c('Control'))

View(sloss.trt_fitted.npk)
# fixed effect coefficients 
sloss.trt_fixef <- fixef(sloss.3_sigmai)

# predict estimates for each site across a sequence of year.y (comparison plots age)
# this takes ~ 5 minutes

# coefficients for study-level (random) effects
sloss.trt_coef <- coef(sloss.3_sigmai)

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
  inner_join(p.all %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(sloss.trt_fitted,sloss.trt_fitted.npk,sloss.trt_fitted.ctl,sloss.trt_coef2,file = 'sloss.n.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


# Species Gain


#  model summary
summary(sgain.3_sigmai)
# caterpillar plots
plot(sgain.3_sigmai)
# predicted values vs. observed
# color_scheme_set("darkgray")
fig_s3d <- pp_check(sgain.3_sigmai) + theme_classic() + 
  labs(x= "Species gain (s.gain)", y = "Density") + 
  scale_x_continuous(limits = c(-50, 50))

fig_s3d

# residuals (this take a minute)
colnames(p.all)
pairs.sgain <- p.all %>% filter(!is.na(s.gain))
pairs.sgain$year.y <- as.factor(pairs.sgain$year.y)
sgain.m <- residuals(sgain.3_sigmai)
sgain.m <- as.data.frame(sgain.m)
sgain.plot <- cbind(pairs.sgain, sgain.m$Estimate)
head(sgain.plot)

par(mfrow=c(2,2))
with(sgain.plot, plot(site_code, sgain.m$Estimate))
with(sgain.plot, plot(block, sgain.m$Estimate))
with(sgain.plot, plot(year.y, sgain.m$Estimate))
with(sgain.plot, plot(plot, sgain.m$Estimate))


sgain.trt_fitted <- cbind(sgain.3_sigmai$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(sgain.3_sigmai, re_formula = NA)) %>% 
  as_tibble() %>% left_join(p.all)


sgain.trt_fitted.npk<-sgain.trt_fitted[sgain.trt_fitted$trt.y %in% c('NPK'),]
sgain.trt_fitted.ctl<-sgain.trt_fitted[sgain.trt_fitted$trt.y %in% c('Control'),]


# fixed effect coefficients 
sgain.trt_fixef <- fixef(sgain.3_sigmai)

# coefficients for study-level (random) effects
sgain.trt_coef <- coef(sgain.3_sigmai)

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
  inner_join(p.all %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m),
                         by = 'site_code')) 

# note to self change sgain file name to match others later
setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(sgain.trt_fitted.npk,sgain.trt_fitted.ctl,sgain.trt_coef2,file = 'sgain.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain.mod.dat.Rdata')


# persistent species model

#  model summary
summary(ps.3_sigma)
# caterpillar plots
plot(ps.3_sigma)
# predicted values vs. observed
color_scheme_set("darkgray")
fig_s3x <- pp_check(ps.3_sigma) + theme_classic() + 
  labs(x= "Persistent species (p.s)", y = "Density") + 
  scale_x_continuous(limits = c(-50, 50))

fig_s3x

# residuals (this take a minute)
colnames(p.all)
pairs.ps <- p.all %>% filter(!is.na(c.rich))
pairs.ps$year.y <- as.factor(pairs.ps$year.y)
ps.m <- residuals(ps.3_sigma)
ps.m <- as.data.frame(ps.m)
head(ps.m)
ps.plot <- cbind(pairs.ps, ps.m$Estimate)
head(ps.plot)

par(mfrow=c(2,2))
with(ps.plot, plot(site_code, ps.m$Estimate))
with(ps.plot, plot(block, ps.m$Estimate))
with(ps.plot, plot(year.y, ps.m$Estimate))
with(ps.plot, plot(plot, ps.m$Estimate))


# fixed effects
ps.trt_fitted <- cbind(ps.3_sigma$data,
                          # get fitted values; setting re_formula = NA means we are getting 'fixed' effects
                          fitted(ps.3_sigma, re_formula = NA)) %>% 
  as_tibble() %>% left_join(p.all)


ps.trt_fitted.npk <- ps.trt_fitted %>% filter(trt.y %in% c('NPK'))
ps.trt_fitted.ctl <- ps.trt_fitted  %>% filter(trt.y %in% c('Control'))

ps.trt_fitted

# fixed effect coefficients 
ps.trt_fixef <- fixef(ps.3_sigma)

# predict estimates for each site across a sequence of year.y (comparison plots age)
# this takes ~ 5 minutes

head(p.all)


# coefficients for study-level (random) effects
ps.trt_coef <- coef(ps.3_sigma)

head(ps.trt_coef)

ps.trt_coef2 <-  bind_cols(ps.trt_coef$site_code[,,'Intercept'] %>% # intercept
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(ps.trt_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             ps.trt_coef$site_code[,,'year.y.m'] %>% # control slope
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              ps.trt_coef$site_code[,,'trt.yNPK'] %>%  # treatment
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              ps.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>%  # treatment slope
                                as_tibble() %>% 
                                mutate(TESlope = Estimate,
                                       TESlope_lower = Q2.5,
                                       TESlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)
                          ) %>% 
  # join with min and max of the x-values
  inner_join(p.all %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 



setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(ps.trt_fitted,ps.trt_fitted.npk,ps.trt_fitted.ctl,ps.trt_coef2,file = 'ps.mod.dat.Rdata')
#load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/ps.mod.dat.Rdata')

# SL- biomass change associated with species loss

#  model summary
summary(sl.3)
# caterpillar plots
plot(sl.3)
# predicted values vs. observed
# color_scheme_set("darkgray")

summary(p.all)
fig_s3e <- pp_check(sl.3) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with species sloss (SL)')), y = "Density") + 
   scale_x_continuous(limits = c(-1300, 100))

fig_s3e

# residuals (this take a minute)
colnames(p.all)
pairs.sl <- p.all %>% filter(!is.na(SL))
sl.m <- residuals(sl.3)
sl.m <- as.data.frame(sl.m)
head(sl.m)
sl.plot <- cbind(pairs.sl, sl.m$Estimate)
head(sl.plot)

par(mfrow=c(2,2))
with(sl.plot, plot(site_code, sl.m$Estimate))
with(sl.plot, plot(block, sl.m$Estimate))
with(sl.plot, plot(year.y, sl.m$Estimate))
with(sl.plot, plot(plot, sl.m$Estimate))


# fixed effects
sl.trt_fitted <- cbind(sl.3$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(sl.3, re_formula = NA)) %>% 
  as_tibble() %>% left_join(p.all)



sl.trt_fitted.npk<-sl.trt_fitted[sl.trt_fitted$trt.y %in% c('NPK'),]
sl.trt_fitted.ctl<-sl.trt_fitted[sl.trt_fitted$trt.y %in% c('Control'),]


# fixed effect coefficients 
sl.trt_fixef <- fixef(sl.3)



# coefficients for study-level (random) effects
sl.trt_coef <- coef(sl.3)

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
  inner_join(p.all %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(sl.trt_fitted.npk,sl.trt_fitted.ctl,sl.trt_coef2,file = 'sl.n.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl.n.mod.dat.Rdata')



# SG: biomass change associated with species gains

#  model summary
summary(sg.3)
# caterpillar plots
plot(sg.3)
# predicted values vs. observed
# color_scheme_set("darkgray")
fig_s3f <- pp_check(sg.3) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with species gain (SG)')), y = "Density") + 
  scale_x_continuous(limits = c(-100, 1300))

fig_s3f

# residuals (this take a minute)
colnames(p.all)
pairs.sg <- p.all %>% filter(!is.na(SG))
sg.m <- residuals(sg.3)
sg.m <- as.data.frame(sg.m)
head(sg.m)
sg.plot <- cbind(pairs.sg, sg.m$Estimate)
head(sg.plot)

par(mfrow=c(2,2))
with(sg.plot, plot(site_code, sg.m$Estimate))
with(sg.plot, plot(block, sg.m$Estimate))
with(sg.plot, plot(year.y, sg.m$Estimate))
with(sg.plot, plot(plot, sg.m$Estimate))



sg.trt_fitted <- cbind(sg.3$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(sg.3, re_formula = NA)) %>% 
  as_tibble() %>% left_join(p.all)


sg.trt_fitted.npk<-sg.trt_fitted[sg.trt_fitted$trt.y %in% c('NPK'),]
sg.trt_fitted.ctl<-sg.trt_fitted[sg.trt_fitted$trt.y %in% c('Control'),]

# fixed effect coefficients 
sg.trt_fixef <- fixef(sg.3)


# coefficients for study-level (random) effects
sg.trt_coef <- coef(sg.3)


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
  inner_join(p.all %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(sg.trt_fitted.npk,sg.trt_fitted.ctl,sg.trt_coef2,file = 'sg.mod.dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/sg_dat.Rdata')



# CDE- this variable is called CDE in previous price equation work
# which stands for context dependent effect
# here because of the temporal comparison approach we call is Persistant species (Ps)
# this is the biomass change associated with persistent species or species that are common between two plots in time


#  model summary
summary(cde.3_sigmai)
# caterpillar plots
plot(cde.3_sigmai)
# predicted values vs. observed
# color_scheme_set("darkgray")
fig_s3g <- pp_check(cde.3_sigmai) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with persistent species (PS)')), y = "Density") + 
   scale_x_continuous(limits = c(-1000, 1000))


fig_s3g

# residuals (this take a minute)
colnames(p.all)
pairs.cde <- p.all %>% filter(!is.na(CDE))
cde.m <- residuals(cde.3_sigmai)
cde.m <- as.data.frame(cde.m)
head(cde.m)
cde.plot <- cbind(pairs.cde, cde.m$Estimate)
head(cde.plot)

par(mfrow=c(2,2))
with(cde.plot, plot(site_code, cde.m$Estimate))
with(cde.plot, plot(block, cde.m$Estimate))
with(cde.plot, plot(year.y, cde.m$Estimate))
with(cde.plot, plot(plot, cde.m$Estimate))



# fixed effects
cde_fitted <- cbind(cde.3_sigmai$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(cde.3_sigmai, re_formula = NA)) %>% 
  as_tibble() %>% left_join(p.all)


cde_fitted.npk<-cde_fitted[cde_fitted$trt.y %in% c('NPK'),]
cde_fitted.ctl<-cde_fitted[cde_fitted$trt.y %in% c('Control'),]

# fixed effect coefficients
cde_fixef <- fixef(cde.3_sigmai)


# coefficients for study-level (random) effects
cde_coef <- coef(cde.3_sigmai)

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
  inner_join(p.all %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') 


setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(cde_fitted.npk,cde_fitted.ctl,cde_coef2,file = 'cde.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/cde.mod.dat.Rdata')



# Multivariate Models

# multivariate plot richness + biomass
load("~/Desktop/mods/multi/multi_sp.Rdata") # object name: sp.multi

# model summary
summary(sp.multi)
# predicted vs observed values
#color_scheme_set("darkgray")
nnb <- pp_check(sp.multi, resp = 'stripmass')+ theme_classic() + scale_x_continuous(limits = c(-1000, 2000))+ labs(x=expression(paste('Biomass (g/',m^2, ')')),
                                                                                                                 y = '')  
nnr <- pp_check(sp.multi, resp = 'rich')+ theme_classic() + scale_x_continuous(limits = c(-10, 50))+ labs(x='Species Richness',
                                                                                                           y = 'Density') +
  theme(legend.position= "none")
# Figure S3h
fig_s3h <- (nnr  | nnb)

fig_s3h


# multivariate price partitions with all responses possible
load("~/Desktop/mods/multi/multi_price_all.Rdata") # object name: pp.multi_all

# model summary
summary(pp.multi_all)

# predicted vs observed values
color_scheme_set("darkgray")
sloss <- pp_check(pp.multi_all, resp = 'slossn')+ theme_classic()+ scale_x_continuous(limits = c(-50, 50))+ labs(x=expression(paste('s.loss')),
                                                                                                             y = 'Density') +
  theme(legend.position= "none")
sgain <- pp_check(pp.multi_all, resp = 'sgain')+ theme_classic()+ scale_x_continuous(limits = c(-50, 50))+ labs(x='s.gain',
                                                                                                            y = '') +
  theme(legend.position= "none")

sl <- pp_check(pp.multi_all, resp = 'SL')+ theme_classic()+ scale_x_continuous(limits = c(-1000, 200))+ labs(x=expression(paste('SL')),
                                                                                                         y = '') +
  theme(legend.position= "bottom")
sg <- pp_check(pp.multi_all, resp = 'SG')+ theme_classic()+ scale_x_continuous(limits = c(-200, 1000))+ labs(x='SG',
                                                                                                         y = '') +
  theme(legend.position= "none")

cde <- pp_check(pp.multi_all, resp = 'SG')+ theme_classic()+ scale_x_continuous(limits = c(-1000, 1000))+ labs(x='PS', y = '') +
  theme(legend.position= "none")
           
                                                                                                                                                                                                                                                                                                                                                                                                                       
# Figure S3j
fig_s3i <- (sloss | sgain | sl | sg | cde)

fig_s3i

# extract all response correlations for  multivariate model,
#reported in Supplementary Model Information 1.9
cor <- as.mcmc(pp.multi_all, combine_chains = TRUE, 
                            pars = "^cor") %>% 
  as_tibble()


names(cor)
View(cor)

cor_samp <- cor %>% 
  # select correlation 
  select("cor_site_code__SL_trt.yNPK__SG_trt.yNPK:year.y.m",
                           "cor_site_code__SL_trt.yNPK__CDE_trt.yNPK:year.y.m",
                           "cor_site_code__SG_trt.yNPK__CDE_trt.yNPK:year.y.m",
                           "cor_site_code__SL_trt.yNPK__slossn_trt.yNPK:year.y.m",
                           "cor_site_code__SL_trt.yNPK__sgain_trt.yNPK:year.y.m",
                           "cor_site_code__SG_trt.yNPK__slossn_trt.yNPK:year.y.m",
                           "cor_site_code__SG_trt.yNPK__sgain_trt.yNPK:year.y.m",
                           "cor_site_code__CDE_trt.yNPK__slossn_trt.yNPK:year.y.m",
                           "cor_site_code__CDE_trt.yNPK__sgain_trt.yNPK:year.y.m",
  "cor_site_code__slossn_trt.yNPK__sgain_trt.yNPK:year.y.m" ) %>%
  # rename cols
  mutate( corr_SL_SG = `cor_site_code__SL_trt.yNPK__SG_trt.yNPK:year.y.m`,
             corr_SL_CDE = `cor_site_code__SL_trt.yNPK__CDE_trt.yNPK:year.y.m`, 
             corr_SG_CDE = `cor_site_code__SG_trt.yNPK__CDE_trt.yNPK:year.y.m`,
             corr_SL_sloss = `cor_site_code__SL_trt.yNPK__slossn_trt.yNPK:year.y.m`,
             corr_SL_sgain = `cor_site_code__SL_trt.yNPK__sgain_trt.yNPK:year.y.m`,
             corr_SG_sloss = `cor_site_code__SG_trt.yNPK__slossn_trt.yNPK:year.y.m`,
             corr_SG_sgain = `cor_site_code__SG_trt.yNPK__sgain_trt.yNPK:year.y.m`,
             corr_CDE_sloss = `cor_site_code__CDE_trt.yNPK__slossn_trt.yNPK:year.y.m`,
             corr_CDE_sgain = `cor_site_code__CDE_trt.yNPK__sgain_trt.yNPK:year.y.m`,
             corr_sloss_sgain = `cor_site_code__slossn_trt.yNPK__sgain_trt.yNPK:year.y.m`,
             ) %>% 
  # select cols
  select(corr_SL_SG, corr_SL_CDE, corr_SG_CDE, corr_SL_sloss, corr_SL_sgain, corr_SG_sloss,
         corr_SG_sgain, corr_CDE_sloss, corr_CDE_sgain, corr_sloss_sgain) %>%
  gather(metric, correlation, corr_SL_SG:corr_sloss_sgain ) %>%
  group_by(metric) %>%
  # calculate mean and uncertainty
  summarise(corr_coef = mean(correlation),
            lower_ci = quantile(correlation, probs=0.025),
            upper_ci = quantile(correlation, probs=0.975)) 
            

View(cor_samp)

