
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")

# FIGURE 3 CLOUD VERSION
# EXTRACT 100 POSTERIORS

# models
load('~/Dropbox/Projects/NutNet/Model_fits/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.g

#load('~/Dropbox/Projects/NutNet/Model_fits/sl.Rdata') # sl.s (sl converted to positive values)
load('~/Dropbox/Projects/NutNet/Model_fits/sl.n.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s


# site level meta data for posrteriors
# calculated to site level details found in Climate_Data.R
# latitude and longitude dont match due to decimal rounding
# lat.x long.x is nutnet site, lat.y long.y is world clim
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(meta)
View(meta)


#  mods study level dat
study_levels <- plot.rich.g$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

parnames(plot.rich.g)
study_sample_posterior.s <- study_levels %>%
  mutate(sl.ctl.s = purrr::map(data, ~posterior_samples(sl.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 100,
                                                                           min = 1, max = 200))) %>% unlist() %>% as.numeric()),
         sg.ctl.s = purrr::map(data, ~posterior_samples(sg.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 100,
                                                                           min = 1, max = 200))) %>% unlist() %>% as.numeric()),
         cde.ctl.s = purrr::map(data, ~posterior_samples(CDE.s,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 100, 1, max = 200))) %>%  unlist() %>%  as.numeric()),
         rich.ctl.s = purrr::map(data, ~posterior_samples(plot.rich.g,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 100, 1, max = 200))) %>%  unlist() %>%  as.numeric()),
         bm.ctl.s = purrr::map(data, ~posterior_samples(plot.bm.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 100, 1, max = 200))) %>%  unlist() %>%  as.numeric()),
         sl.trt.s = purrr::map(data, ~posterior_samples(sl.s, 
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 100,min = 1, max = 200))) %>% unlist() %>% as.numeric()),
         sg.trt.s = purrr::map(data, ~posterior_samples(sg.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 100,
                                                                           min = 1, max = 200))) %>% unlist() %>% as.numeric()),
         cde.trt.s = purrr::map(data, ~posterior_samples(CDE.s,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 100, 1, max = 200))) %>%  unlist() %>%  as.numeric()),
         rich.trt.s = purrr::map(data, ~posterior_samples(plot.rich.g,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 100, 1, max = 200))) %>%  unlist() %>%  as.numeric()),
         bm.trt.s = purrr::map(data, ~posterior_samples(plot.bm.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 100, 1, max = 200))) %>%  unlist() %>%  as.numeric()))



sl.trt.i_fixef <- fixef(sl.s)
sg.trt.i_fixef <- fixef(sg.s)
CDE.trt.i_fixef <- fixef(CDE.s)
plot.rich.im_fixef <- fixef(plot.rich.g)
plot.bm.im_fixef <- fixef(plot.bm.s)


sl.fixed.p<-posterior_samples(sl.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
CDE.fixed.p<-posterior_samples(CDE.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
rich.fixed.p<-posterior_samples(plot.rich.g, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(plot.bm.s, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 


sl_posterior.s <- study_sample_posterior.s  %>% 
  select(-data) %>% 
  unnest_legacy(sl.ctl.s,sl.trt.s) %>% 
  mutate(response = 'sl',
         sl.ctl_global_slope = sl.trt.i_fixef['year.y.m','Estimate'],
         sl.ctl_upper_slope = sl.trt.i_fixef['year.y.m','Q97.5'],
         sl.ctl_lower_slope = sl.trt.i_fixef['year.y.m','Q2.5'],
         sl.trt_global_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sl.trt_upper_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sl.trt_lower_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) 


head(sl_posterior.s)
# sl_posterior$sl<-sl_posterior$sl.ctl+sl_posterior$sl.trt
# sl_posterior$sl.global<-sl_posterior$sl.ctl_global_slope + sl_posterior$sl.trt_global_slope
# sl_posterior$sl.upper<-sl_posterior$sl.ctl_upper_slope + sl_posterior$sl.trt_upper_slope
# sl_posterior$sl.lower<-sl_posterior$sl.ctl_lower_slope + sl_posterior$sl.trt_lower_slope
sl.p.s<-sl_posterior.s %>% inner_join(meta, by = 'site_code')

sl.p.s$starting.richness <- ifelse(sl.p.s$r.rich >= 1 & sl.p.s$r.rich <= 5, '1-5 species',
                                 ifelse(sl.p.s$r.rich >=6 & sl.p.s$r.rich <=10, '6-10',
                                        ifelse(sl.p.s$r.rich >=11 & sl.p.s$r.rich <=15, '11-15',    
                                               ifelse(sl.p.s$r.rich >=16 & sl.p.s$r.rich <=20, '16-20',
                                                      ifelse(sl.p.s$r.rich >=21 & sl.p.s$r.rich <=25, '21-25',
                                                             ifelse(sl.p.s$r.rich >=26, '>26', 'other'))))))

sl.p.s$starting.richness <- factor(sl.p.s$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sl.p.s$anthropogenic<-as.factor(sl.p.s$anthropogenic)
sl.p.s$grazed<-as.factor(as.character(sl.p.s$grazed))
sl.p.s$managed<-as.factor(as.character(sl.p.s$managed))
sl.p.s$burned<-as.factor(as.character(sl.p.s$burned))

sl.p.s$site_rich_range <- ifelse(sl.p.s$site_richness >= 2 & sl.p.s$site_richness <= 44, '2-40 species',
                               ifelse(sl.p.s$site_richness >=45 & sl.p.s$site_richness <=69, '45-69',
                                      ifelse(sl.p.s$site_richness >=70 & sl.p.s$site_richness <=90, '70-90',    
                                             ifelse(sl.p.s$site_richness >=91 & sl.p.s$site_richness <=119, '90-119',
                                                    ifelse(sl.p.s$site_richness >=120 & sl.p.s$site_richness <=144, '120-144',
                                                           ifelse(sl.p.s$site_richness >=145, '>145', 'other'))))))

View(sl.p)

levels(sl.p$NDep.cats)
write.csv(sl.p.s,"~/Dropbox/Projects/NutNet/Data/sl_posteriors.small.csv")

# SG
sg_posterior.s <- study_sample_posterior.s  %>% 
  select(-data) %>% 
  unnest_legacy(sg.ctl.s,sg.trt.s) %>% 
  mutate(response = 'sg',
         sg.ctl_global_slope = sg.trt.i_fixef['year.y.m','Estimate'],
         sg.ctl_upper_slope = sg.trt.i_fixef['year.y.m','Q97.5'],
         sg.ctl_lower_slope = sg.trt.i_fixef['year.y.m','Q2.5'],
         sg.trt_global_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sg.trt_upper_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sg.trt_lower_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
  ) 


# sg_posterior$sg<-sg_posterior$sg.ctl+sg_posterior$sg.trt
# sg_posterior$sg.global<-sg_posterior$sg.ctl_global_slope + sg_posterior$sg.trt_global_slope
# sg_posterior$sg.upper<-sg_posterior$sg.ctl_upper_slope + sg_posterior$sg.trt_upper_slope
# sg_posterior$sg.lower<-sg_posterior$sg.ctl_lower_slope + sg_posterior$sg.trt_lower_slope
sg.p.s<-sg_posterior.s %>% inner_join(meta, by = 'site_code')


sg.p.s$starting.richness <- ifelse(sg.p.s$r.rich >= 1 & sg.p.s$r.rich <= 5, '1-5 species',
                                 ifelse(sg.p.s$r.rich >=6 & sg.p.s$r.rich <=10, '6-10',
                                        ifelse(sg.p.s$r.rich >=11 & sg.p.s$r.rich <=15, '11-15',    
                                               ifelse(sg.p.s$r.rich >=16 & sg.p.s$r.rich <=20, '16-20',
                                                      ifelse(sg.p.s$r.rich >=21 & sg.p.s$r.rich <=25, '21-25',
                                                             ifelse(sg.p.s$r.rich >=26, '>26', 'other'))))))


sg.p.s$starting.richness <- factor(sg.p.s$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.p.s$anthropogenic<-as.factor(sg.p.s$anthropogenic)

sg.p.s$grazed<-as.factor(as.character(sg.p.s$grazed))
sg.p.s$managed<-as.factor(as.character(sg.p.s$managed))
sg.p.s$burned<-as.factor(as.character(sg.p.s$burned))

sg.p.s$site_rich_range <- ifelse(sg.p.s$site_richness >= 2 & sg.p.s$site_richness <= 44, '2-40 species',
                               ifelse(sg.p.s$site_richness >=45 & sg.p.s$site_richness <=69, '45-69',
                                      ifelse(sg.p.s$site_richness >=70 & sg.p.s$site_richness <=90, '70-90',    
                                             ifelse(sg.p.s$site_richness >=91 & sg.p.s$site_richness <=119, '90-119',
                                                    ifelse(sg.p.s$site_richness >=120 & sg.p.s$site_richness <=144, '120-144',
                                                           ifelse(sg.p.s$site_richness >=145, '>145', 'other'))))))

write.csv(sg.p.s,"~/Dropbox/Projects/NutNet/Data/sg_posteriors.small.csv")

# CDE
cde_posterior.s <- study_sample_posterior.s  %>% 
  select(-data) %>% 
  unnest_legacy(cde.ctl.s,cde.trt.s) %>% 
  mutate(response = 'cde',
         cde.ctl_global_slope = CDE.trt.i_fixef['year.y.m','Estimate'],
         cde.ctl_upper_slope = CDE.trt.i_fixef['year.y.m','Q97.5'],
         cde.ctl_lower_slope = CDE.trt.i_fixef['year.y.m','Q2.5'],
         cde.trt_global_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         cde.trt_upper_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         cde.trt_lower_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
  ) 

# cde_posterior$cde<-cde_posterior$cde.ctl+cde_posterior$cde.trt
# cde_posterior$cde.global<-cde_posterior$cde.ctl_global_slope + cde_posterior$cde.trt_global_slope
# cde_posterior$cde.upper<-cde_posterior$cde.ctl_upper_slope + cde_posterior$cde.trt_upper_slope
# cde_posterior$cde.lower<-cde_posterior$cde.ctl_lower_slope + cde_posterior$cde.trt_lower_slope
cde.p.s<-cde_posterior.s %>% inner_join(meta, by = 'site_code')


cde.p.s$starting.richness <- ifelse(cde.p.s$r.rich >= 1 & cde.p.s$r.rich <= 5, '1-5 species',
                                  ifelse(cde.p.s$r.rich >=6 & cde.p.s$r.rich <=10, '6-10',
                                         ifelse(cde.p.s$r.rich >=11 & cde.p.s$r.rich <=15, '11-15',    
                                                ifelse(cde.p.s$r.rich >=16 & cde.p.s$r.rich <=20, '16-20',
                                                       ifelse(cde.p.s$r.rich >=21 & cde.p.s$r.rich <=25, '21-25',
                                                              ifelse(cde.p.s$r.rich >=26, '>26', 'other'))))))




cde.p.s$starting.richness <- factor(cde.p.s$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
cde.p.s$anthropogenic<-as.factor(cde.p.s$anthropogenic)
cde.p.s$grazed<-as.factor(as.character(cde.p.s$grazed))
cde.p.s$burned<-as.factor(as.character(cde.p.s$burned))

cde.p.s$site_rich_range <- ifelse(cde.p.s$site_richness >= 2 & cde.p.s$site_richness <= 44, '2-40 species',
                                ifelse(cde.p.s$site_richness >=45 & cde.p.s$site_richness <=69, '45-69',
                                       ifelse(cde.p.s$site_richness >=70 & cde.p.s$site_richness <=90, '70-90',    
                                              ifelse(cde.p.s$site_richness >=91 & cde.p.s$site_richness <=119, '90-119',
                                                     ifelse(cde.p.s$site_richness >=120 & cde.p.s$site_richness <=144, '120-144',
                                                            ifelse(cde.p.s$site_richness >=145, '>145', 'other'))))))


write.csv(cde.p.s,"~/Dropbox/Projects/NutNet/Data/cde_posteriors.small.csv")





# small posteriors for sloss and sgain for fig 3 cloud

#load('~/Dropbox/Projects/NutNet/Model_fits/sloss.Rdata') # s.loss.s (sloss converted to positive values)
load('~/Dropbox/Projects/NutNet/Model_fits/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s


meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(meta)
View(meta)


#  mods study level dat
study_levels <- plot.rich.g$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

parnames(plot.rich.g)
study_sample_posterior.ss <- study_levels %>%
  mutate(sloss.ctl.s = purrr::map(data, ~posterior_samples(s.loss.s,
                                                         pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                         exact = TRUE,
                                                         subset = floor(runif(n = 100,
                                                                              min = 1, max = 200))) %>% unlist() %>% as.numeric()),
         sgain.ctl.s = purrr::map(data, ~posterior_samples(s.gain.s,
                                                         pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                         exact = TRUE,
                                                         subset = floor(runif(n = 100,
                                                                              min = 1, max = 200))) %>% unlist() %>% as.numeric()),
         sloss.trt.s = purrr::map(data, ~posterior_samples(s.loss.s, 
                                                         pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                         exact = TRUE,
                                                         subset = floor(runif(n = 100,min = 1, max = 200))) %>% unlist() %>% as.numeric()),
         sgain.trt.s = purrr::map(data, ~posterior_samples(s.gain.s,
                                                         pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                         exact = TRUE,
                                                         subset = floor(runif(n = 100,
                                                                              min = 1, max = 200))) %>% unlist() %>% as.numeric()))



sloss.trt.i_fixef <- fixef(s.loss.s)
sgain.trt.i_fixef <- fixef(s.gain.s)




sloss_posterior.s <- study_sample_posterior.ss  %>% 
  select(-data) %>% 
  unnest_legacy(sloss.ctl.s,sloss.trt.s) %>% 
  mutate(response = 'sloss',
         sloss.ctl_global_slope = sloss.trt.i_fixef['year.y.m','Estimate'],
         sloss.ctl_upper_slope = sloss.trt.i_fixef['year.y.m','Q97.5'],
         sloss.ctl_lower_slope = sloss.trt.i_fixef['year.y.m','Q2.5'],
         sloss.trt_global_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sloss.trt_upper_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sloss.trt_lower_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) 


head(sl_posterior)
# sl_posterior$sl<-sl_posterior$sl.ctl+sl_posterior$sl.trt
# sl_posterior$sl.global<-sl_posterior$sl.ctl_global_slope + sl_posterior$sl.trt_global_slope
# sl_posterior$sl.upper<-sl_posterior$sl.ctl_upper_slope + sl_posterior$sl.trt_upper_slope
# sl_posterior$sl.lower<-sl_posterior$sl.ctl_lower_slope + sl_posterior$sl.trt_lower_slope
sloss.p.s<-sloss_posterior.s %>% inner_join(meta, by = 'site_code')

sloss.p.s$starting.richness <- ifelse(sloss.p.s$r.rich >= 1 & sloss.p.s$r.rich <= 5, '1-5 species',
                                    ifelse(sloss.p.s$r.rich >=6 & sloss.p.s$r.rich <=10, '6-10',
                                           ifelse(sloss.p.s$r.rich >=11 & sloss.p.s$r.rich <=15, '11-15',    
                                                  ifelse(sloss.p.s$r.rich >=16 & sloss.p.s$r.rich <=20, '16-20',
                                                         ifelse(sloss.p.s$r.rich >=21 & sloss.p.s$r.rich <=25, '21-25',
                                                                ifelse(sloss.p.s$r.rich >=26, '>26', 'other'))))))

sloss.p.s$starting.richness <- factor(sloss.p.s$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sloss.p.s$anthropogenic<-as.factor(sloss.p.s$anthropogenic)
sloss.p.s$grazed<-as.factor(as.character(sloss.p.s$grazed))
sloss.p.s$managed<-as.factor(as.character(sloss.p.s$managed))
sloss.p.s$burned<-as.factor(as.character(sloss.p.s$burned))

sloss.p.s$site_rich_range <- ifelse(sloss.p.s$site_richness >= 2 & sloss.p.s$site_richness <= 44, '2-40 species',
                                  ifelse(sloss.p.s$site_richness >=45 & sloss.p.s$site_richness <=69, '45-69',
                                         ifelse(sloss.p.s$site_richness >=70 & sloss.p.s$site_richness <=90, '70-90',    
                                                ifelse(sloss.p.s$site_richness >=91 & sloss.p.s$site_richness <=119, '90-119',
                                                       ifelse(sloss.p.s$site_richness >=120 & sloss.p.s$site_richness <=144, '120-144',
                                                              ifelse(sloss.p.s$site_richness >=145, '>145', 'other'))))))


write.csv(sloss.p.s,"~/Dropbox/Projects/NutNet/Data/sloss_posteriors.small.csv")




sgain_posterior.s <- study_sample_posterior.ss  %>% 
  select(-data) %>% 
  unnest_legacy(sgain.ctl.s,sgain.trt.s) %>% 
  mutate(response = 'sgain',
         sgain.ctl_global_slope = sgain.trt.i_fixef['year.y.m','Estimate'],
         sgain.ctl_upper_slope = sgain.trt.i_fixef['year.y.m','Q97.5'],
         sgain.ctl_lower_slope = sgain.trt.i_fixef['year.y.m','Q2.5'],
         sgain.trt_global_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sgain.trt_upper_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sgain.trt_lower_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) 


head(sl_posterior)
# sl_posterior$sl<-sl_posterior$sl.ctl+sl_posterior$sl.trt
# sl_posterior$sl.global<-sl_posterior$sl.ctl_global_slope + sl_posterior$sl.trt_global_slope
# sl_posterior$sl.upper<-sl_posterior$sl.ctl_upper_slope + sl_posterior$sl.trt_upper_slope
# sl_posterior$sl.lower<-sl_posterior$sl.ctl_lower_slope + sl_posterior$sl.trt_lower_slope
sgain.p.s<-sgain_posterior.s %>% inner_join(meta, by = 'site_code')

sgain.p.s$starting.richness <- ifelse(sgain.p.s$r.rich >= 1 & sgain.p.s$r.rich <= 5, '1-5 species',
                                    ifelse(sgain.p.s$r.rich >=6 & sgain.p.s$r.rich <=10, '6-10',
                                           ifelse(sgain.p.s$r.rich >=11 & sgain.p.s$r.rich <=15, '11-15',    
                                                  ifelse(sgain.p.s$r.rich >=16 & sgain.p.s$r.rich <=20, '16-20',
                                                         ifelse(sgain.p.s$r.rich >=21 & sgain.p.s$r.rich <=25, '21-25',
                                                                ifelse(sgain.p.s$r.rich >=26, '>26', 'other'))))))

sgain.p.s$starting.richness <- factor(sgain.p.s$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sgain.p.s$anthropogenic<-as.factor(sgain.p.s$anthropogenic)
sgain.p.s$grazed<-as.factor(as.character(sgain.p.s$grazed))
sgain.p.s$managed<-as.factor(as.character(sgain.p.s$managed))
sgain.p.s$burned<-as.factor(as.character(sgain.p.s$burned))

sgain.p.s$site_rich_range <- ifelse(sgain.p.s$site_richness >= 2 & sgain.p.s$site_richness <= 44, '2-40 species',
                                  ifelse(sgain.p.s$site_richness >=45 & sgain.p.s$site_richness <=69, '45-69',
                                         ifelse(sgain.p.s$site_richness >=70 & sgain.p.s$site_richness <=90, '70-90',    
                                                ifelse(sgain.p.s$site_richness >=91 & sgain.p.s$site_richness <=119, '90-119',
                                                       ifelse(sgain.p.s$site_richness >=120 & sgain.p.s$site_richness <=144, '120-144',
                                                              ifelse(sgain.p.s$site_richness >=145, '>145', 'other'))))))


write.csv(sgain.p.s,"~/Dropbox/Projects/NutNet/Data/sgain_posteriors.small.csv")


cde.p.s <- read.csv("~/Dropbox/Projects/NutNet/Data/cde_posteriors.small.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sg.p.s <- read.csv("~/Dropbox/Projects/NutNet/Data/sg_posteriors.small.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sl.p.s <- read.csv("~/Dropbox/Projects/NutNet/Data/sl_posteriors.small.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sgain.p.s <- read.csv("~/Dropbox/Projects/NutNet/Data/sgain_posteriors.small.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sloss.p.s <- read.csv("~/Dropbox/Projects/NutNet/Data/sloss_posteriors.small.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


loss.s<- bind_cols(sl.p.s,sloss.p.s)

gains.s<- bind_cols(sg.p.s,sgain.p.s)

head(gains)
nrow(cde.p.s)

#"#3B9AB2","#B40F20","#35274A"

# only cloud of 100 posteriors for every site = 6500 posteriors for each partition
pcloud<-  ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.p.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt.s + unique(cde.trt_global_slope) ), 
               colour= "#35274A",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.p.s,aes(x=0, #persistent
                              y= cde.trt.s + unique(cde.trt_global_slope), ),
             colour="#35274A",size=0.1,alpha = .1) +
  # geom_errorbar(data = cde.p,aes(x=0,
  #                                ymin = cde.trt.s_lower_slope, ymax = cde.trt.s_upper_slope), width=0,colour = "#35274A", size = 0.55,alpha=0.3) 
  geom_segment(data = loss.s,
               aes(x = 0,
                   xend = (sloss.trt.s + unique(sloss.trt_global_slope)),
                   y = 0,
                   yend = (sl.trt.s + unique(sl.trt_global_slope)) ),
               colour= "#B40F20",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss.s, aes(x= (sloss.trt.s + unique(sloss.trt_global_slope)), #loss
                              y=  (sl.trt.s + unique(sl.trt_global_slope)) ),
             colour="#B40F20",size=0.2,alpha = .1)+
  # geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
  #                                    ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
  # geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
  #                                     xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
  geom_segment(data = gains.s,
               aes(x = 0,
                   xend = sgain.trt.s + unique(sgain.trt_global_slope),
                   y = 0,
                   yend = sg.trt.s + unique(sg.trt_global_slope) ),
               colour= "#3B9AB2",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = gains.s,aes(x=sgain.trt.s + unique(sgain.trt_global_slope), #losses
                              y=sg.trt.s + unique(sg.trt_global_slope) ),
             colour="#3B9AB2",
             size=0.2,alpha = .1)+
    labs(x = 'Effect on Species Richness Over Time',
         y = 'Effect on Biomass Change Over Time',
         title= 'Posterior Cloud')






  # cloud plus fixed effects
  sloss_fixef <- fixef(s.loss.s)
  sgain_fixef <- fixef(s.gain.s)
  sl_fixef <- fixef(sl.s)
  sg_fixef <- fixef(sg.s)
  cde_fixef <- fixef(CDE.s)
  
  sgain_fixef<-as.data.frame(sgain_fixef)
  sloss_fixef<-as.data.frame(sloss_fixef)
  sl_fixef<-as.data.frame(sl_fixef)
  sg_fixef<-as.data.frame(sg_fixef)
  cde_fixef<-as.data.frame(cde_fixef)
  
  sgain_fixef$names <- rownames(sgain_fixef)
  sloss_fixef$names <- rownames(sloss_fixef)
  sl_fixef$names <- rownames(sl_fixef)
  sg_fixef$names <- rownames(sg_fixef)
  cde_fixef$names <- rownames(cde_fixef)
  sgain_fixef
  sloss_fixef
  sl_fixef
  
  # 
  # sloss_fixef$Estimate<- (sloss_fixef$Estimate)
  # sloss_fixef$Q2.5<- (sloss_fixef$Q2.5)
  # sloss_fixef$Q97.5<- (sloss_fixef$Q97.5)
  # 
  # sl_fixef$Estimate<- (sl_fixef$Estimate)
  # sl_fixef$Q2.5<- (sl_fixef$Q2.5)
  # sl_fixef$Q97.5<- (sl_fixef$Q97.5)
  # sl_fixef
  
  
  sgain_fixef
  
  sg_fixef
  
  sgain_fixef$Model<-'Sgain'
  sloss_fixef$Model<-'Sloss'
  sl_fixef$Model<-'SL'
  sg_fixef$Model<-'SG'
  cde_fixef$Model<-'CDE'
  fixedf_pp<-bind_rows(sl_fixef,sg_fixef,cde_fixef,sloss_fixef,sgain_fixef)
  View(fixedf_pp)
  
  #"#3B9AB2","#B40F20","#35274A"
# cloud with fixed effects
 pcloud2<- ggplot()+
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
    theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
    xlim(-0.4,0.3)+ ylim(-20,20)+
    geom_segment(data = cde.p.s,
                 aes(x = 0,
                     xend = 0,
                     y = 0,
                     yend = cde.trt.s + unique(cde.trt_global_slope) ), 
                 colour= "#35274A",
                 size = 0.2,  alpha = .1,
                 arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
    geom_point(data = cde.p.s,aes(x=0, #persistent
                                  y= cde.trt.s + unique(cde.trt_global_slope), ),
               colour="#35274A",size=0.1,alpha = .1) +
    geom_segment(data = loss.s,
                 aes(x = 0,
                     xend = (sloss.trt.s + unique(sloss.trt_global_slope)),
                     y = 0,
                     yend = (sl.trt.s + unique(sl.trt_global_slope)) ),
                 colour= "#B40F20",
                 size = 0.2,  alpha = .1,
                 arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
    geom_point(data = loss.s, aes(x= (sloss.trt.s + unique(sloss.trt_global_slope)), #loss
                                y=  (sl.trt.s + unique(sl.trt_global_slope)) ),
               colour="#B40F20",size=0.2,alpha = .1)+
     geom_segment(data = gains.s,
                 aes(x = 0,
                     xend = sgain.trt.s + unique(sgain.trt_global_slope),
                     y = 0,
                     yend = sg.trt.s + unique(sg.trt_global_slope) ),
                 colour= "#3B9AB2",
                 size = 0.2,  alpha = .1,
                 arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
    geom_point(data = gains.s,aes(x=sgain.trt.s + unique(sgain.trt_global_slope), #losses
                                y=sg.trt.s + unique(sg.trt_global_slope) ),
               colour="#3B9AB2",
               size=0.2,alpha = .1) + # end cloud
    geom_point(data = fixedf_pp,aes(x=Estimate[16], # start fixed effects
                   y=Estimate[4]),
               colour="#B40F20",
               size=0.7)+
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                      ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                       xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                                       ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                                        xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
    geom_point(data = fixedf_pp,aes(x=Estimate[20], #gains
                   y= Estimate[8]),
               colour="#3B9AB2",size=0.7)+
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                      ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                       xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                       ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                                        xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
    geom_point(data = fixedf_pp,aes(x=Estimate[20], #persistent
                   y=Estimate[12]),
               colour="#35274A",size=0.7)+
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                      ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                       ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "#35274A", size = 0.35,alpha=0.3) +
    #geom_errorbarh(aes(y=Estimate[8]+Estimate[12],
    #                xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "#35274A", size = 0.35,alpha=0.3) +
    geom_segment(data = fixedf_pp, # cde
                 aes(x = Estimate[20], #gains
                     xend = Estimate[20], # gains
                     y = Estimate[8],   # effect of  sg on bm
                     yend = Estimate[12]), # effect of cde on biomass
                 colour= "black",
                 size = 1.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp, # cde
                 aes(x = Estimate[20], #gains
                     xend = Estimate[20], # gains
                     y = Estimate[8],   # effect of  sg on bm
                     yend = Estimate[12]), # effect of cde on biomass
                 colour= "#35274A",
                 size = 0.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp, # gains
                 aes(x = Estimate[16], # start at losses
                     xend = Estimate[20], # species gains effect
                     y = Estimate[4],    # effect of sl on biomass
                     yend = Estimate[8]),  # effect of sl + effect of sg on bm
                 colour= "black",
                 size = 1.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp, # gains
                 aes(x = Estimate[16], # start at losses
                     xend = Estimate[20], # species gains effect
                     y = Estimate[4],    # effect of sl on biomass
                     yend = Estimate[8]),  # effect of sl + effect of sg on bm
                 colour= "#3B9AB2",
                 size = 0.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp, # losses
                 aes(x = 0,
                     xend = Estimate[16], # species losses
                     y = 0,
                     yend = Estimate[4]), # effect of sl on biomass
                 colour= "black",
                 size = 1.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp, # losses
                 aes(x = 0,
                     xend = Estimate[16], # species losses
                     y = 0,
                     yend = Estimate[4]), # effect of sl on biomass
                 colour= "#B40F20",
                 size = 0.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    labs(x = 'Effect on Species Richness Over Time',
         y = 'Effect on Biomass Change Over Time',
         title= 'CAFE Vector Plot')
  
  
  
  # cloud with fixed effects that all start from 0
  
  # only cloud of 100 posteriors for every site = 6500 posteriors for each partition
 pcloud3<- ggplot()+
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
   xlim(-0.4,0.3)+ ylim(-20,20)+
    geom_segment(data = cde.p.s,
                 aes(x = 0,
                     xend = 0,
                     y = 0,
                     yend = cde.trt.s + unique(cde.trt_global_slope) ), 
                 colour= "#35274A",
                 size = 0.2,  alpha = .1,
                 arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
    geom_point(data = cde.p.s,aes(x=0, #persistent
                                  y= cde.trt.s + unique(cde.trt_global_slope), ),
               colour="#35274A",size=0.1,alpha = .1) +
  geom_segment(data = loss.s,
                 aes(x = 0,
                     xend = (sloss.trt.s + unique(sloss.trt_global_slope)),
                     y = 0,
                     yend = (sl.trt.s + unique(sl.trt_global_slope)) ),
                 colour= "#B40F20",
                 size = 0.2,  alpha = .1,
                 arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
    geom_point(data = loss.s, aes(x= (sloss.trt.s + unique(sloss.trt_global_slope)), #loss
                                  y=  (sl.trt.s + unique(sl.trt_global_slope)) ),
               colour="#B40F20",size=0.2,alpha = .1)+
  geom_segment(data = gains.s,
                 aes(x = 0,
                     xend = sgain.trt.s + unique(sgain.trt_global_slope),
                     y = 0,
                     yend = sg.trt.s + unique(sg.trt_global_slope) ),
                 colour= "#3B9AB2",
                 size = 0.2,  alpha = .1,
                 arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
    geom_point(data = gains.s,aes(x=sgain.trt.s + unique(sgain.trt_global_slope), #losses
                                  y=sg.trt.s + unique(sg.trt_global_slope) ),
               colour="#3B9AB2",
               size=0.2,alpha = .1) + # end cloud
    geom_point(data = fixedf_pp,aes(x=0, #persistent
                                    y=Estimate[12]),
               colour="#35274A",size=0.7)+
    geom_errorbar(data = fixedf_pp,aes(x=0,
                                       ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbar(data = fixedf_pp,aes(x=0,
                                       ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "#35274A", size = 0.35,alpha=0.3) +
    geom_point(data = fixedf_pp,aes(x=Estimate[20], #gains
                                    y= Estimate[8]),
               colour="#3B9AB2",size=0.7)+
    geom_segment(data = fixedf_pp, # start fixed effects
                 aes(x = 0,
                     xend = 0,
                     y = 0,
                     yend = Estimate[12]),
                 colour= "black",
                 size = 1.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp, # start fixed effects
                 aes(x = 0,
                     xend = 0,
                     y = 0,
                     yend = Estimate[12]),
                 colour= "#35274A",
                 size = 0.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                       ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                                        xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                       ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                                        xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
    geom_segment(data = fixedf_pp,
                 aes(x = 0,
                     xend = Estimate[20],
                     y = 0,
                     yend = Estimate[8]),
                 colour= "black",
                 size = 1.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp,
                 aes(x = 0,
                     xend = Estimate[20],
                     y = 0,
                     yend = Estimate[8]),
                 colour= "#3B9AB2",
                 size = 0.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_point(data = fixedf_pp,aes(x=Estimate[16], #losses
                                    y=Estimate[4]),
               colour="#B40F20",
               size=0.7)+
    geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                                       ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "black", size = 0.55,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                                        xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                                     ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
    geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                                        xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
    geom_segment(data = fixedf_pp,
                 aes(x = 0,
                     xend = Estimate[16],
                     y = 0,
                     yend = Estimate[4]),
                 colour= "black",
                 size = 1.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    geom_segment(data = fixedf_pp,
                 aes(x = 0,
                     xend = Estimate[16],
                     y = 0,
                     yend = Estimate[4]),
                 colour= "#B40F20",
                 size = 0.5,
                 arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
    theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))+
    labs(x = 'Effect on Species Richness Over Time',
         y = 'Effect on Biomass Change Over Time',
         title= 'CAFE Vector Plot') 
  
grid.arrange(pcloud,pcloud3,pcloud2,ncol=3,nrow=1)  

  
  
  