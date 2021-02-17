


library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)

# FIGURE 5


# models
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/bm.Rdata') # plot.bm.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/rich.Rdata') # plot.rich.g

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sloss.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sgain.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/cde.Rdata') # CDE.s


meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_S1.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

#  mods study level dat
study_levels <- plot.rich.3$data %>% 
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
         rich.ctl.study = purrr::map(data, ~posterior_samples(plot.rich.3,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.ctl.study = purrr::map(data, ~posterior_samples(plot.bm.3,
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
         rich.npk.study = purrr::map(data, ~posterior_samples(plot.rich.3,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.npk.study = purrr::map(data, ~posterior_samples(plot.bm.3,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))


# fixed posteriors
sl.fixed.p <- posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
sloss.fixed.p <- posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p<-posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
rich.fixed.p<-posterior_samples(plot.rich.3, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(plot.bm.3, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 

head(rich.fixed.p)

sl_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sl.ctl.study,sl.npk.study) %>%
  mutate( sl.trt.study = (sl.ctl.study + sl.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'sl',
         sl.ctl.global = sl.fixed.p[,'b_year.y.m'],
         sl.npk.global = sl.fixed.p[,'b_trt.yNPK:year.y.m'],) %>%
    mutate(sl.trt.global=(sl.ctl.global+sl.npk.global))


  
View(sl_posterior)

sl.p<-sl_posterior %>% inner_join(meta, by = 'site_code') 

sl.p$anthropogenic<-as.factor(sl.p$anthropogenic)
sl.p$grazed<-as.factor(as.character(sl.p$grazed))
sl.p$managed<-as.factor(as.character(sl.p$managed))
sl.p$burned<-as.factor(as.character(sl.p$burned))

View(sl.p)
write.csv(sl.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl_posteriors.csv")

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



View(sloss_posterior)

sloss.p<-sloss_posterior %>% inner_join(meta, by = 'site_code') 

sloss.p$anthropogenic<-as.factor(sloss.p$anthropogenic)
sloss.p$grazed<-as.factor(as.character(sloss.p$grazed))
sloss.p$managed<-as.factor(as.character(sloss.p$managed))
sloss.p$burned<-as.factor(as.character(sloss.p$burned))

View(sloss.p)
write.csv(sloss.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss_posteriors.csv")



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



View(sg_posterior)

sg.p<-sg_posterior %>% inner_join(meta, by = 'site_code') 

sg.p$anthropogenic<-as.factor(sg.p$anthropogenic)
sg.p$grazed<-as.factor(as.character(sg.p$grazed))
sg.p$managed<-as.factor(as.character(sg.p$managed))
sg.p$burned<-as.factor(as.character(sg.p$burned))

View(sg.p)
write.csv(sg.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sg_posteriors.csv")



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



View(sgain_posterior)

sgain.p<-sgain_posterior %>% inner_join(meta, by = 'site_code') 

sgain.p$anthropogenic<-as.factor(sgain.p$anthropogenic)
sgain.p$grazed<-as.factor(as.character(sgain.p$grazed))
sgain.p$managed<-as.factor(as.character(sgain.p$managed))
sgain.p$burned<-as.factor(as.character(sgain.p$burned))

View(sgain.p)
write.csv(sgain.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain_posteriors.csv")


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

View(cde_posterior)

cde.p<-cde_posterior %>% inner_join(meta, by = 'site_code') 

cde.p$anthropogenic<-as.factor(cde.p$anthropogenic)
cde.p$grazed<-as.factor(as.character(cde.p$grazed))
cde.p$managed<-as.factor(as.character(cde.p$managed))
cde.p$burned<-as.factor(as.character(cde.p$burned))

View(cde.p)
write.csv(cde.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/cde_posteriors.csv")


# RICH

rich_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(rich.ctl.study,rich.npk.study) %>%
  mutate( rich.trt.study = (rich.ctl.study + rich.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'rich',
         rich.ctl.global = rich.fixed.p[,'b_year_trt'],
         rich.npk.global = rich.fixed.p[,'b_trtNPK:year_trt'],) %>%
  mutate(rich.trt.global=(rich.ctl.global+rich.npk.global))

View(rich_posterior)

rich.p<-rich_posterior %>% inner_join(meta, by = 'site_code') 

rich.p$anthropogenic<-as.factor(rich.p$anthropogenic)
rich.p$grazed<-as.factor(as.character(rich.p$grazed))
rich.p$managed<-as.factor(as.character(rich.p$managed))
rich.p$burned<-as.factor(as.character(rich.p$burned))

View(rich.p)
write.csv(rich.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich_posteriors.csv")



# BM

bm_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(bm.ctl.study,bm.npk.study) %>%
  mutate( bm.trt.study = (bm.ctl.study + bm.npk.study)) %>%
  group_by(site_code) %>%
  mutate(response = 'biomass',
         bm.ctl.global = bm.fixed.p[,'b_year_trt'],
         bm.npk.global = bm.fixed.p[,'b_trtNPK:year_trt'],) %>%
  mutate(bm.trt.global=(bm.ctl.global+bm.npk.global))

View(bm_posterior)

bm.p<-bm_posterior %>% inner_join(meta, by = 'site_code') 

bm.p$anthropogenic<-as.factor(bm.p$anthropogenic)
bm.p$grazed<-as.factor(as.character(bm.p$grazed))
bm.p$managed<-as.factor(as.character(bm.p$managed))
bm.p$burned<-as.factor(as.character(bm.p$burned))

View(bm.p)
write.csv(bm.p,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/bm_posteriors.csv")


meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/meta.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- meta %>% select(site_code, Quadrant)

View(meta)

rich.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/p.effs.Rdata')

View(rich.ps)
rich.ps <- rich.ps %>% left_join(meta, by ="site_code")
View(rich.ps)

rich<-ggplot() +
  geom_rect(data = rich.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
            alpha = 0.3)) +
  geom_density_ridges(data = rich.ps,
                      aes(x = rich.trt.study + rich.trt.global, 
                        y = Quadrant, 
                      ), fill="#0B775E",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = rich.ps,
                      aes(x = rich.ctl.study + rich.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_rich_range',
  #'                      #'starting.richness' ,
  #'                     # 'site_rich_range',
  #'                    'anthropogenic',
  #'                    #'NDep.cats',
  #'                    #'biome' ,
  #'                    #'site_dom',
  #'                    #'Realm',
  #'                    #'colimitation',
  #'                    discrete=TRUE) +
  geom_vline(data = rich.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Change in Species Richness/Year',
        title= 'a) Species Richness',
        y= 'Slope'
        #color= ''
  )+
  # geom_text(data = rich.ps %>%
  #             group_by(site_dom) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(site_dom, n_study, .keep_all = T),
  #           aes(x=2.5, y=site_dom,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  scale_x_continuous(breaks=c(-4,-2,0,2), limits=c(-4,2))+
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

rich


bm.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/bm_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



bm.ps <- bm.ps %>% left_join(meta, by ="site_code")
colnames(bm.p)
bm.ps


bm<-ggplot() +
  geom_rect(data = bm.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = bm.ps,
                      aes(x = bm.trt.study + bm.trt.global, 
                          y = Quadrant,
                      ), fill="#0B775E",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = bm.ps,
                      aes(x = bm.ctl.study + bm.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_bm_range',
  #'                      #'starting.bmness' ,
  #'                      'site_bm_range',
  #'                    # 'anthropogenic',
  #'                    #'NDep.cats',
#'                    #'biome' ,
#'                    #'site_dom',
#'                    #'Realm',
#'                    #'colimitation',
#'                    discrete=TRUE) +
geom_vline(data = bm.p %>% filter(response=="NPK"),
           aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/Year')),
        title= 'a) Biomass',
        y= ''
        #color= ''
  )+
  geom_text(data = bm.ps %>%
              group_by(Quadrant) %>%
              mutate(n_sites = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_sites, .keep_all = T),
            aes(x=175, y=Quadrant,
                label=paste('n[sites] == ', n_sites)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

bm


(rich | bm )






sl.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sl.ps <- sl.ps %>% left_join(meta, by ="site_code")
View(sl.ps)
View(sl.p)
sl.p


sl<-ggplot() +
  geom_rect(data = sl.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = sl.ps,
                      aes(x = sl.trt.study + sl.trt.global, 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sl.ps,
                      aes(x = sl.ctl.study + sl.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_sl_range',
  #'                      #'starting.slness' ,
  #'                      'site_sl_range',
  #'                    # 'anthropogenic',
  #'                    #'NDep.cats',
  #'                    #'biome' ,
  #'                    #'site_dom',
  #'                    #'Realm',
  #'                    #'colimitation',
  #'                    discrete=TRUE) +
  geom_vline(data = sl.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( #x='',
    x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')')),
        title= 'e) Change in Biomass Due to Species Loss',
        y= ''
        #color= ''
  )+
  # geom_text(data = sl.ps %>%
  #             group_by(Quadrant) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(Quadrant, n_study, .keep_all = T),
  #           aes(x=20, y=Quadrant,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sl


sg.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sg.ps <- sg.ps %>% left_join(meta, by ="site_code")
colnames(sg.p)
sg.p


sg<-ggplot() +
  geom_rect(data = sg.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = sg.ps,
                      aes(x = sg.trt.study + sg.trt.global, 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sg.ps,
                      aes(x = sg.ctl.study + sg.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_sg_range',
  #'                      #'starting.sgness' ,
  #'                      'site_sg_range',
  #'                    # 'anthropogenic',
  #'                    #'NDep.cats',
  #'                    #'biome' ,
  #'                    #'site_dom',
  #'                    #'Realm',
  #'                    #'colimitation',
  #'                    discrete=TRUE) +
  geom_vline(data = sg.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x = '',
    #x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/Year')),
    title= 'f) Change in Biomass Due to Species Gain',
    y= ' '
    #color= ''
  )+
  geom_text(data = sg.ps %>%
              group_by(Quadrant) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_study, .keep_all = T),
            aes(x=75, y=Quadrant,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sg



cde.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

cde.ps <- cde.ps %>% left_join(meta, by ="site_code")
colnames(cde.p)
cde.p


cde<-ggplot() +
  geom_rect(data = cde.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = cde.ps,
                      aes(x = cde.trt.study + cde.trt.global, 
                          y = Quadrant,
                      ), fill="#F98400",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = cde.ps,
                      aes(x = cde.ctl.study + cde.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_cde_range',
  #'                      #'starting.cdeness' ,
  #'                      'site_cde_range',
  #'                    # 'anthropogenic',
  #'                    #'NDep.cats',
  #'                    #'biome' ,
  #'                    #'site_dom',
  #'                    #'Realm',
  #'                    #'colimitation',
  #'                    discrete=TRUE) +
  geom_vline(data = cde.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(x='', 
    #x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')')),
        title= 'd) Persistent Species Change in Biomass',
        y= 'Slope'
        #color= ''
  )+
  xlim(-250,100)+
  # geom_text(data = cde.ps %>%
  #             group_by(Quadrant) %>%
  #             mutate(n_sites = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(Quadrant, n_sites, .keep_all = T),
  #           aes(x=80, y=Quadrant,
  #               label=paste('n[sites] == ', n_sites)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

cde


(sl | sg | cde)




(rich | bm )/(sl | sg | cde)





sloss.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sloss.ps <- sloss.ps %>% left_join(meta, by ="site_code")
View(sloss.ps)
View(sloss.p)
sloss.p


sloss<-ggplot() +
  geom_rect(data = sloss.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = sloss.ps,
                      aes(x = sloss.trt.study + sloss.trt.global, 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sloss.ps,
                      aes(x = sloss.ctl.study + sloss.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_sl_range',
  #'                      #'starting.slness' ,
  #'                      'site_sl_range',
  #'                    # 'anthropogenic',
  #'                    #'NDep.cats',
  #'                    #'biome' ,
  #'                    #'site_dom',
  #'                    #'Realm',
  #'                    #'colimitation',
  #'                    discrete=TRUE) +
  geom_vline(data = sloss.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x = expression(paste('Effect of NPK on Species Loss / Year')),
        title= 'a) Species Loss',
        y= ' Slope'
        #color= ''
  )+
  # geom_text(data = sl.ps %>%
  #             group_by(multilimited) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(multilimited, n_study, .keep_all = T),
  #           aes(x=20, y=multilimited,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  scale_x_continuous(breaks=c(-4,-2,0,2), limits=c(-4,2))+
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sloss


sgain.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sgain.ps <- sgain.ps %>% left_join(meta, by ="site_code")
colnames(sgain.ps)
sgain.p


sgain<-ggplot() +
  geom_rect(data = sgain.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = sgain.ps,
                      aes(x = sgain.trt.study + sgain.trt.global, 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sgain.ps,
                      aes(x = sgain.ctl.study + sgain.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_sg_range',
  #'                      #'starting.sgness' ,
  #'                      'site_sg_range',
  #'                    # 'anthropogenic',
  #'                    #'NDep.cats',
  #'                    #'biome' ,
  #'                    #'site_dom',
  #'                    #'Realm',
  #'                    #'colimitation',
  #'                    discrete=TRUE) +
  geom_vline(data = sgain.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x = expression(paste('Effect of NPK on Species Gain / Year')),
        title= 'b) Species Gain',
        y= ' '
        #color= ''
  )+
  geom_text(data = sg.ps %>%
              group_by(Quadrant) %>%
              mutate(n_sites = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_sites, .keep_all = T),
            aes(x=2.75, y=Quadrant,
                label=paste('n[sites] == ', n_sites)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sgain


( rich | bm )/( sloss | sgain )/( sl | sg | cde )




( sloss | sgain )/( cde  | sl | sg )




