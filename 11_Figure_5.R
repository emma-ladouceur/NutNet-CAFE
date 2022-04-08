

# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 11_Figure_5.R
# This workflow uses data pulled out of Models below to produce Figure 4


# packages
library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(stringr)

# models
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sloss.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sgain.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/cde.Rdata') # CDE.s


# data
meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(meta)

# 
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/study.price.p.effs.Rdata')


#  mods study level dat
study_levels <- sl.3_p$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)


head(study_levels)

study_sample_posterior <- study_levels %>%
  mutate(sl.ctl.study = purrr::map(data, ~posterior_samples(sl.3,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sg.ctl.study = purrr::map(data, ~posterior_samples(sg.3,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sloss.ctl.study = purrr::map(data, ~posterior_samples(sloss.3,
                                                            pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                            exact = TRUE,
                                                            subset = floor(runif(n = 1000,
                                                                                 min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sgain.ctl.study = purrr::map(data, ~posterior_samples(sgain.3,
                                                            pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                            exact = TRUE,
                                                            subset = floor(runif(n = 1000,
                                                                                 min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          cde.ctl.study = purrr::map(data, ~posterior_samples(cde.3,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.ctl.study = purrr::map(data, ~posterior_samples(rich.3,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.ctl.study = purrr::map(data, ~posterior_samples(bm.3,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
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
         sloss.npk.study = purrr::map(data, ~posterior_samples(sloss.3, 
                                                            pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                            exact = TRUE,
                                                            subset = floor(runif(n = 1000,min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sgain.npk.study = purrr::map(data, ~posterior_samples(sgain.3,
                                                            pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                            exact = TRUE,
                                                            subset = floor(runif(n = 1000,
                                                                                 min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         cde.npk.study = purrr::map(data, ~posterior_samples(cde.3,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.npk.study = purrr::map(data, ~posterior_samples(rich.3,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.npk.study = purrr::map(data, ~posterior_samples(bm.3,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))


# fixed posteriors
sl.fixed.p <- posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
sloss.fixed.p <- posterior_samples(sloss.3_p, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(sgain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p<-posterior_samples(cde.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
rich.fixed.p<-posterior_samples(rich.3, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(bm.3, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 

head(sloss.fixed.p)


rich_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(rich.ctl.study,rich.npk.study) %>%
  mutate( rich.trt.study = (rich.ctl.study + rich.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'rich',
         rich.ctl.global = rich.fixed.p[,'b_year_trt'],
         rich.npk.global = rich.fixed.p[,'b_trtNPK:year_trt'],) %>%
  mutate(rich.trt.global=(rich.ctl.global+ rich.npk.global))


rich.p <- rich_posterior %>% inner_join(meta, by = 'site_code') 


write.csv(rich.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/rich_posteriors.csv")


bm_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(bm.ctl.study,bm.npk.study) %>%
  mutate( bm.trt.study = (bm.ctl.study + bm.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'bm',
         bm.ctl.global = bm.fixed.p[,'b_year_trt'],
         bm.npk.global = bm.fixed.p[,'b_trtNPK:year_trt'],) %>%
  mutate(bm.trt.global=(bm.ctl.global+ bm.npk.global))


bm.p <- bm_posterior %>% inner_join(meta, by = 'site_code') 


write.csv(bm.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/bm_posteriors.csv")


sl_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sl.ctl.study,sl.npk.study) %>%
  mutate( sl.trt.study = (sl.ctl.study + sl.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'sl',
         sl.ctl.global = sl.fixed.p[,'b_year.y.m'],
         sl.npk.global = sl.fixed.p[,'b_trt.yNPK:year.y.m'],) %>%
    mutate(sl.trt.global=(sl.ctl.global+sl.npk.global))


sl.p<-sl_posterior %>% inner_join(meta, by = 'site_code') 


write.csv(sl.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sl_posteriors.csv")

# SLOSS
sloss_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sloss.ctl.study,sloss.npk.study) %>%
  mutate( sloss.trt.study = (sloss.ctl.study + sloss.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'sloss',
         sloss.ctl.global = sloss.fixed.p[,'b_year.y.m'],
         sloss.npk.global = sloss.fixed.p[,'b_trt.yNPK:year.y.m'],) %>%
  mutate(sloss.trt.global=(sloss.ctl.global+sloss.npk.global))


sloss.p<-sloss_posterior %>% inner_join(meta, by = 'site_code') 

write.csv(sloss.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sloss_posteriors.csv")



# SG

sg_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sg.ctl.study,sg.npk.study) %>%
  mutate( sg.trt.study = (sg.ctl.study + sg.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'sg',
         sg.ctl.global = sg.fixed.p[,'b_year.y.m'],
         sg.npk.global = sg.fixed.p[,'b_trt.yNPK:year.y.m'],) %>%
  mutate(sg.trt.global=(sg.ctl.global+sg.npk.global))


sg.p<-sg_posterior %>% inner_join(meta, by = 'site_code') 

write.csv(sg.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sg_posteriors.csv")



#SGAIN

sgain_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sgain.ctl.study,sgain.npk.study) %>%
  mutate( sgain.trt.study = (sgain.ctl.study + sgain.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'sgain',
         sgain.ctl.global = sgain.fixed.p[,'b_year.y.m'],
         sgain.npk.global = sgain.fixed.p[,'b_trt.yNPK:year.y.m'],) %>%
  mutate(sgain.trt.global=(sgain.ctl.global+sgain.npk.global))


sgain.p<-sgain_posterior %>% inner_join(meta, by = 'site_code') 

write.csv(sgain.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sgain_posteriors.csv")


# CDE

cde_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(cde.ctl.study,cde.npk.study) %>%
  mutate( cde.trt.study = (cde.ctl.study + cde.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'cde',
         cde.ctl.global = cde.fixed.p[,'b_year.y.m'],
         cde.npk.global = cde.fixed.p[,'b_trt.yNPK:year.y.m'],) %>%
  mutate(cde.trt.global=(cde.ctl.global+cde.npk.global))

cde.p<-cde_posterior %>% inner_join(meta, by = 'site_code') 

write.csv(cde.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/cde_posteriors.csv")


# data from 
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/study.price.p.effs.Rdata')
npk.effs <- all.effs %>% select(site_code,sg.trt.rate.p, sl.trt.rate.p,cde.trt.rate.p,sloss.trt.rate.p,sgain.trt.rate.p, Quadrant) #%>%
  #filter(!Quadrant == "-biomass +rich")

npk.effs$Quadrant<-factor(npk.effs$Quadrant,  levels=c("-biomass +rich","+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

head(npk.effs)

# for axis titles with expressions
# https://stackoverflow.com/questions/13223846/ggplot2-two-line-label-with-expression


sloss.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sloss_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# sloss.ps <- sloss.ps %>%
#   filter(!Quadrant == "-biomass +rich")

sloss.ps$Quadrant<-factor(sloss.ps$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sloss.ps

fig_5a <- ggplot() +
  geom_density_ridges(data = sloss.ps, #density ridges
                      aes(x = sloss.trt.study + sloss.trt.global, 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = sloss.trt.rate.p), #study-level effects
             colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sloss.ps %>% group_by(Quadrant) %>% # median of study-level effects for each group
               summarise(mean.s.eff = median(sloss.trt.study + sloss.trt.global)),
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Species loss / year in NPK plots')),
        title= 'a) Species loss (s.loss)',
        y= ' Slope'
  )+
  scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_5a


sgain.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sgain_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# sgain.ps <- sgain.ps %>% 
#   filter(!Quadrant == "-biomass +rich")

sgain.ps$Quadrant<-factor(sgain.ps$Quadrant,  levels=c("-biomass +rich","+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

colnames(sgain.ps)
sgain.p


fig_5b <- ggplot() +
  geom_density_ridges(data = sgain.ps,
                      aes(x = sgain.trt.study + sgain.trt.global, 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = sgain.trt.rate.p),
             colour= "#3B9AB2",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sgain.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(sgain.trt.study + sgain.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Species gain / year in NPK plots')),
        title= 'b) Species gain (s.gain)',
        y= ' '
  )+
  scale_x_continuous(breaks=c(-1,0,1), limits=c(-1.5,1.5))+
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none") 

fig_5b

# species loss (s.loss)
sl.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# sl.ps <- sl.ps %>%
#   filter(!Quadrant == "-biomass +rich")

sl.ps$Quadrant<-factor(sl.ps$Quadrant,  levels=c("-biomass +rich","+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

head(sl.ps)

fig_5c <- ggplot() +
  geom_density_ridges(data = sl.ps,
                      aes(x = sl.trt.study + sl.trt.global, 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = sl.trt.rate.p),
             colour= "#B40F20",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sl.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(sl.trt.study + sl.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  scale_x_continuous(breaks=c(-40,-20,-10,-5,0,10), limits=c(-40,15))+
  labs( 
    x = expression(paste(atop( paste('Biomass change (g/' ,m^2, ') / year'), 'in NPK plots'))),
        title= 'c) Biomass change associated \n with species loss (SL)',
        y= ''
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text( hjust=0.5, 
                                   margin = margin(t = 16, r = 0, b = 0, l = 0)), 
        legend.position="none") 


fig_5c


sg.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# sg.ps <- sg.ps %>% 
#   filter(!Quadrant == "-biomass +rich")

sg.ps$Quadrant<-factor(sg.ps$Quadrant,  levels=c("-biomass +rich","+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sg.ps

fig_5d <- ggplot() +
  geom_density_ridges(data = sg.ps,
                      aes(x = sg.trt.study + sg.trt.global, 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = sg.trt.rate.p),
             colour= "#3B9AB2",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sg.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(sg.trt.study + sg.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( #x = '',
    x = expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'in NPK plots'))),
    title= 'd) Biomass change associated \n with species gain (SG)',
    y= ' '
  )+
  scale_x_continuous(breaks=c(-10,0,5,10,20,40,60,80), limits=c(-20,80))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text( hjust=0.5,
                                   margin = margin(t = 16, r = 0, b = 0, l = 0)), 
        legend.position="none")

fig_5d



cde.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# cde.ps <- cde.ps %>% 
#   filter(!Quadrant == "-biomass +rich")

cde.ps$Quadrant<-factor(cde.ps$Quadrant,  levels=c("-biomass +rich","+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

cde.ps


head(npk.effs)


fig_5e <- ggplot() +
  geom_density_ridges(data = cde.ps,
                      aes(x = cde.trt.study + cde.trt.global, 
                          y = Quadrant,
                      ), fill="#F98400",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = cde.trt.rate.p),
             colour= "#F98400",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= cde.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(cde.trt.study + cde.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs(#x='', 
    x = expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'in NPK plots'))),
        title= 'e) Biomass change associated \n with persistent species (PS)',
        y= ''
  )+
  scale_x_continuous(breaks=c(-150,-100,-50,-25,0,25,50,100), limits=c(-175,100))+
  geom_text(data = cde.ps %>%
              group_by(Quadrant) %>%
              mutate(n_sites = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_sites, .keep_all = T),
            aes(x=-100, y=Quadrant,
                label=paste('n[sites] == ', n_sites)),
            size=6,
            nudge_y = 0.5, parse = T) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(  hjust=0.5,
                                   margin = margin(t = 16, r = 0, b = 0, l = 0)), 
        legend.position="none")

fig_5e


# LANDSCAPE 7 X 19
# Figure 5
( fig_5a |  fig_5b | fig_5c | fig_5d |  fig_5e  )




