


library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")


load('~/Dropbox/Projects/NutNet/Model_fits/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/rich.Rdata') # plot.rich.g


meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


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
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))

View(study_sample_posterior)




rich_study_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(rich.ctl.study,rich.npk.study) %>%
  mutate( rich.trt.study = (rich.ctl.study + rich.npk.study))

head(rich_study_posterior)
nrow(rich_study_posterior)

bm_study_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(bm.ctl.study,bm.npk.study) %>%
  mutate( bm.trt.study = (bm.ctl.study + bm.npk.study)) 


head(bm_study_posterior)




# FIXED EFFECTS POSTERIORS

rich.fixed.p<-posterior_samples(plot.rich.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))


bm.fixed.p<-posterior_samples(plot.bm.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 


rich_global_posterior <-  rich.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(rich.ctl.global =`b_year_trt`,
         rich.npk.global =`b_trtNPK:year_trt`,
         rich.trt.global =(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`))

head(rich_global_posterior)
nrow(rich_global_posterior)

bm_global_posterior <-  bm.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(bm.ctl.global =`b_year_trt`,
         bm.npk.global=`b_trtNPK:year_trt`,
         bm.trt.global=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`)) 

head(bm_global_posterior)


# richness
rich.site.p <- rich_study_posterior %>% group_by(site_code) %>%
  nest() 

rich.p <- rich.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(rich_global_posterior)))) %>%
  unnest() %>% mutate(rich.study.effect = (rich.trt.study + rich.trt.global))


# biomass
bm.site.p <- bm_study_posterior %>% group_by(site_code) %>%
  nest() 

bm.p <- bm.site.p %>% 
  mutate(data = purrr::map(data, ~ mutate(.x,cbind(bm_global_posterior)))) %>%
  unnest() %>% mutate(bm.study.effect = (bm.trt.study + bm.trt.global))


View(bm.p)


study.rich.p <-  rich.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(rich.study.effect),
          eff_lower = quantile(rich.study.effect, probs=0.025),
          eff_upper = quantile(rich.study.effect, probs=0.975))  %>%
  select(c(site_code,eff,eff_upper,eff_lower)) %>% distinct()  


head(richp)

study.bm.p <-  bm.p %>% group_by(site_code) %>%
  mutate( response="NPK", eff = mean(bm.study.effect),
          eff_lower = quantile(bm.study.effect, probs=0.025),
          eff_upper = quantile(bm.study.effect, probs=0.975))  %>%
  select(c(site_code,eff,eff_upper,eff_lower)) %>% distinct()  


head(study.bm.p)


setwd('~/Dropbox/Projects/NutNet/Data/')
save(study.rich.p, study.bm.p, file = 'study.p.effs.Rdata')








