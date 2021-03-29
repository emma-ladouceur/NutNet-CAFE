


library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
p.all <- read.csv(paste0(path, '/nutnet_cumulative_time.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.all <- read.csv('~/Dropbox/NutNet/Data/nutnet_cumulative_time.csv')

p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
# 

p.all <- p.all %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

#prior predictive check
get_prior(bf(mvbind(SL,SG,CDE,s.loss.n, s.gain) ~ trt.y * year.y.m + (trt.y * year.y.m  | p | site_code)),
          data = p.all,
          family=student())

pp.multi_sigma <- brm( bf( mvbind(SL,SG,CDE,s.loss.n,s.gain) ~ trt.y * year.y.m + (trt.y * year.y.m  | p | site_code) + trt.y:year.y.m, 
                    sigma ~ 0 + trt.y + (0 + trt.y | site_code) ), 
                data = p.all,
                family=student(),
                cores = 4, iter=5000, warmup = 1000, chains = 4,
                sample_prior = 'only',
                prior = c(# non-varying intercepts
                  set_prior(prior = 'student_t(3, -20.3, 3)', class='Intercept', resp = 'SL'),
                  set_prior(prior = 'student_t(3, 22.8, 3)', class='Intercept', resp = 'SG'),
                  set_prior(prior = 'student_t(3, 45.5, 3)', class='Intercept', resp = 'CDE'),
                  set_prior(prior = 'student_t(3, -4, 3)', class='Intercept', resp = 'slossn'),
                  set_prior(prior = 'student_t(3, 3, 3)', class='Intercept', resp = 'sgain'),
                  # non-varying slopes
                  set_prior(prior = 'normal(0,1)', class='b', resp = 'SL'),
                  set_prior(prior = 'normal(0,1)', class='b', resp = 'SG'),
                  set_prior(prior = 'normal(0,1)', class='b', resp = 'CDE'),
                  set_prior(prior = 'normal(0,1)', class='b', resp = 'slossn'),
                  set_prior(prior = 'normal(0,1)', class='b', resp = 'sgain'),
                  # varying intercepts and slopes
                  set_prior(prior = 'normal(0,1)', class='sd', resp = 'SL'),
                  set_prior(prior = 'normal(0,1)', class='sd', resp = 'SG'),
                  set_prior(prior = 'normal(0,1)', class='sd', resp = 'CDE'),
                  set_prior(prior = 'normal(0,1)', class='sd', resp = 'slossn'),
                  set_prior(prior = 'normal(0,1)', class='sd', resp = 'sgain')))

# pp_check(pp.multi_all, resp = 'SL')
# pp_check(pp.multi_all, resp = 'SG')
# pp_check(pp.multi_all, resp = 'CDE')
# pp_check(pp.multi_all, resp = 'slossn')
# pp_check(pp.multi_all, resp = 'sgain')

save(pp.multi_sigma,
     file=Sys.getenv('OFILE'))


# pp.multi <- brm(mvbind(SL,SG,CDE) ~ trt.y * year.y.m + (trt.y * year.y.m  | p | site_code),
#                 data = p.all,family=student(),  cores = 4, iter=6000, warmup = 1000,chains = 4)

# prior predictive check
# get_prior(bf(mvbind(SL/1000,SG/1000,CDE/1000,s.loss.n, s.gain) ~ trt.y * year.y.m + (trt.y * year.y.m  | p | site_code)),
#           data = p.all,
#           family=student())
# 
# pp.multi_all <- brm(mvbind(SL/1000,SG/1000,CDE/1000,s.loss.n, s.gain) ~ trt.y * year.y.m + (trt.y * year.y.m  | p | site_code),
#                 data = p.all,
#                 family=student(),  
#                 cores = 4, iter=2000, warmup = 1000, chains = 4, 
#                 sample_prior = 'only',
#                 prior = c(# non-varying intercepts
#                   set_prior(prior = 'student_t(3, -20.3, 3)', class='Intercept', resp = 'SL1000'),
#                   set_prior(prior = 'student_t(3, 22.8, 3)', class='Intercept', resp = 'SG1000'),
#                   set_prior(prior = 'student_t(3, 45.5, 3)', class='Intercept', resp = 'CDE1000'),
#                   set_prior(prior = 'student_t(3, -4, 3)', class='Intercept', resp = 'slossn'),
#                   set_prior(prior = 'student_t(3, 3, 3)', class='Intercept', resp = 'sgain'),
#                   # non-varying slopes
#                   set_prior(prior = 'normal(0,1)', class='b', resp = 'SL1000'),
#                   set_prior(prior = 'normal(0,1)', class='b', resp = 'SG1000'),
#                   set_prior(prior = 'normal(0,1)', class='b', resp = 'CDE1000'),
#                   set_prior(prior = 'normal(0,1)', class='b', resp = 'slossn'),
#                   set_prior(prior = 'normal(0,1)', class='b', resp = 'sgain'),
#                   # varying intercepts and slopes
#                   set_prior(prior = 'normal(0,1)', class='sd', resp = 'SL1000'),
#                   set_prior(prior = 'normal(0,1)', class='sd', resp = 'SG1000'),
#                   set_prior(prior = 'normal(0,1)', class='sd', resp = 'CDE1000'),
#                   set_prior(prior = 'normal(0,1)', class='sd', resp = 'slossn'),
#                   set_prior(prior = 'normal(0,1)', class='sd', resp = 'sgain')))
# 
# pp_check(pp.multi_all, resp = 'SL1000')
# pp_check(pp.multi_all, resp = 'SG1000')
# pp_check(pp.multi_all, resp = 'CDE1000')
# pp_check(pp.multi_all, resp = 'slossn')
# pp_check(pp.multi_all, resp = 'sgain')

