


library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")


load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.s


meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- meta %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

# STUDY LEVEL POSTERIORS

study_levels <- plot.rich.3$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

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


# FIXED EFFECTS POSTERIORS

rich.fixed.p<-posterior_samples(plot.rich.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))
bm.fixed.p<-posterior_samples(plot.bm.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 


sl.fixed.p<-posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p<-posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )

sloss.fixed.p<-posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 



rich_global_posterior <-  rich.fixed.p %>% dplyr::select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(rich.ctl.global =`b_year_trt`,
         rich.npk.global =`b_trtNPK:year_trt`,
         rich.trt.global =(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  dplyr::select(-c(`b_year_trt`,`b_trtNPK:year_trt`))

head(rich_global_posterior)
nrow(rich_global_posterior)

bm_global_posterior <-  bm.fixed.p %>% dplyr::select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(bm.ctl.global =`b_year_trt`,
         bm.npk.global=`b_trtNPK:year_trt`,
         bm.trt.global=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  dplyr::select(-c(`b_year_trt`,`b_trtNPK:year_trt`)) 

head(bm_global_posterior)


sl_global_posterior <-  sl.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sl.ctl.global =`b_year.y.m`,
         sl.npk.global=`b_trt.yNPK:year.y.m`,
         sl.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sl_global_posterior)

sg_global_posterior <-  sg.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sg.ctl.global =`b_year.y.m`,
         sg.npk.global=`b_trt.yNPK:year.y.m`,
         sg.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sg_global_posterior)


sloss_global_posterior <-  sloss.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sloss.ctl.global =`b_year.y.m`,
         sloss.npk.global=`b_trt.yNPK:year.y.m`,
         sloss.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sloss_global_posterior)



sgain_global_posterior <-  sgain.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sgain.ctl.global =`b_year.y.m`,
         sgain.npk.global=`b_trt.yNPK:year.y.m`,
         sgain.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sgain_global_posterior)


cde_global_posterior <-  cde.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.global =`b_year.y.m`,
         cde.npk.global=`b_trt.yNPK:year.y.m`,
         cde.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(cde_global_posterior)


# richness
rich.site.p <- rich_study_posterior %>% group_by(site_code) %>%
  nest() 

rich.p <- rich.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(rich_global_posterior)))) %>%
  unnest() %>% mutate(rich.study.trt.effect = (rich.trt.study + rich.trt.global)) %>%
  mutate(rich.study.ctl.effect = (rich.ctl.study + rich.ctl.global))

View(rich.p)


# biomass
bm.site.p <- bm_study_posterior %>% group_by(site_code) %>%
  nest() 

bm.p <- bm.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(bm_global_posterior)))) %>%
  unnest() %>% mutate(bm.study.trt.effect = (bm.trt.study + bm.trt.global))  %>%
  mutate(bm.study.ctl.effect = (bm.ctl.study + bm.ctl.global))


head(bm.p)



sl.site.p <- sl_study_posterior %>% group_by(site_code) %>%
  nest() 

sl.p <- sl.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sl_global_posterior)))) %>%
  unnest() %>% mutate(sl.study.trt.effect = (sl.trt.study + sl.trt.global))  %>%
  mutate(sl.study.ctl.effect = (sl.ctl.study + sl.ctl.global))


head(sl.p)


sg.site.p <- sg_study_posterior %>% group_by(site_code) %>%
  nest() 

sg.p <- sg.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sg_global_posterior)))) %>%
  unnest() %>% mutate(sg.study.trt.effect = (sg.trt.study + sg.trt.global))  %>%
  mutate(sg.study.ctl.effect = (sg.ctl.study + sg.ctl.global))


head(sg.p)



sloss.site.p <- sloss_study_posterior %>% group_by(site_code) %>%
  nest() 

sloss.p <- sloss.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sloss_global_posterior)))) %>%
  unnest() %>% mutate(sloss.study.trt.effect = (sloss.trt.study + sloss.trt.global))  %>%
  mutate(sloss.study.ctl.effect = (sloss.ctl.study + sloss.ctl.global))


head(sloss.p)


sgain.site.p <- sgain_study_posterior %>% group_by(site_code) %>%
  nest() 

sgain.p <- sgain.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(sgain_global_posterior)))) %>%
  unnest() %>% mutate(sgain.study.trt.effect = (sgain.trt.study + sgain.trt.global))  %>%
  mutate(sgain.study.ctl.effect = (sgain.ctl.study + sgain.ctl.global))


head(sgain.p)


cde.site.p <- cde_study_posterior %>% group_by(site_code) %>%
  nest() 

cde.p <- cde.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(cde_global_posterior)))) %>%
  unnest() %>% mutate(cde.study.trt.effect = (cde.trt.study + cde.trt.global))  %>%
  mutate(cde.study.ctl.effect = (cde.ctl.study + cde.ctl.global))


head(cde.p)



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

View(study.bm.p)



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

View(study.sl.p)




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

View(study.sg.p)




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

setwd('~/Dropbox/Projects/NutNet/Data/')
save(study.rich.p, study.bm.p,study.sl.p,study.sg.p,study.cde.p,study.sloss.p,study.sgain.p, file = 'study.p.effs.Rdata')



load('~/Dropbox/Projects/NutNet/Data/study.p.effs.Rdata')
load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')



study.rich.p$Model<- "Species Richness"
study.bm.p$Model<- "Biomass"
study.sloss.p$Model<- "Species Loss"
study.sgain.p$Model<- "Species Gain"
study.sl.p$Model<- "Change Biomass Due to Species Loss"
study.sg.p$Model<- "Change in Biomass Due to Species Gain"
study.cde.p$Model<- "Persistent Species Change in Biomass"



p.all <- study.rich.p %>% bind_rows(study.bm.p) %>% bind_rows(study.sloss.p) %>% bind_rows(study.sgain.p) %>%
  bind_rows(study.sl.p) %>% bind_rows(study.sg.p) %>% bind_rows(study.cde.p) %>%
  mutate(Treatment = response,
         Estimate = eff,
         Upper_CI = eff_upper,
         Lower_CI = eff_lower) %>%
  select(-response,-eff,-eff_upper,-eff_lower)


View(p.all)

setwd('~/Dropbox/Projects/NutNet/Data/')
save(p.all, file = 'study.p.all.Rdata')



study.rich.p.s <- study.rich.p %>% filter(site_code == "ahth.is")
study.rich.p.s


rich.eff<-ggplot() + 
  geom_point(data =study.rich.p.s, aes(x = response, y = eff, color=response),size = 2) +
  geom_errorbar(data = study.rich.p.s, aes(x = response, ymin = eff_lower,
                                   ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  # geom_hline(data = rich.p,
  #            aes(yintercept = eff, size = 1,color=response)) +
  # geom_rect(data = rich.p,
  #           aes(xmin = -Inf, xmax = Inf,
  #               ymin = eff_lower, ymax = eff_upper,fill=response),
  #           alpha = 0.3) +
  # facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_hline(yintercept = 0, lty = 2) +
 # scale_y_continuous(breaks=c(0,-0.5)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                               axis.text.y = element_text(size=6),
                               axis.text.x = element_text(size=6),
                               title=element_text(size=8),
                               strip.background = element_blank(),legend.position="none")


rich.eff


View(study.sloss.p)
View(study.sgain.p)




