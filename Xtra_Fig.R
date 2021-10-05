



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
study_levels <- rich.3$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)


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
sloss.fixed.p <- posterior_samples(sloss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(sgain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p<-posterior_samples(cde.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
rich.fixed.p<-posterior_samples(rich.3, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(bm.3, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 

head(rich.fixed.p)


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

npk.effs <- all.effs %>% select(site_code,sg.trt.rate.p, sl.trt.rate.p,cde.trt.rate.p,sloss.trt.rate.p,sgain.trt.rate.p, Quadrant) 


npk.effs$Quadrant <- factor(npk.effs$Quadrant, levels= c("+biomass -rich",  "+biomass +rich", "-biomass -rich", "-biomass +rich"))

head(npk.effs)



fig_stan <- ggplot()+
  facet_wrap(~Quadrant) +
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = npk.effs,  # segments
               aes(x = 0,
                   xend = sloss.trt.rate.p,
                   y = 0,
                   yend = sl.trt.rate.p  ),
               colour= "#B40F20", 
               size = 0.7,  
               arrow= arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = npk.effs, # segment
               aes(x =  sloss.trt.rate.p,
                   xend =  sloss.trt.rate.p + sgain.trt.rate.p, 
                   y =  sl.trt.rate.p,
                   yend = sl.trt.rate.p + sg.trt.rate.p ), 
               colour=  "#3B9AB2",
               size = 0.7,  
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = npk.effs, #segment
               aes(x = sloss.trt.rate.p + sgain.trt.rate.p,
                   xend = sloss.trt.rate.p + sgain.trt.rate.p  ,
                   y = sl.trt.rate.p + sg.trt.rate.p,
                   yend = sl.trt.rate.p + sg.trt.rate.p + cde.trt.rate.p ),
               colour= "#F98400",
               size = 0.7,  
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
   geom_text(data = npk.effs, #segment
              aes( x = sloss.trt.rate.p + sgain.trt.rate.p  ,
                  y = sl.trt.rate.p + sg.trt.rate.p + cde.trt.rate.p, label = site_code ))+
  labs(x = 'Rate of change in species  (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       title = ' ')+ theme_classic(base_size=14) + theme(strip.text = element_text(size=14))

fig_stan







