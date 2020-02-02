
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)

# FIGURE 4
# POSTERIORS ACROSS GROUPS

load('~/Dropbox/Projects/NutNet/Model_fits/biomass2.Rdata') # plot.bm.logt
load('~/Dropbox/Projects/NutNet/Model_fits/rich3.Rdata') # plot.rich.g

load('~/Dropbox/Projects/NutNet/Model_fits/sl3.Rdata') # sl.trt.h.t
load('~/Dropbox/Projects/NutNet/Model_fits/sg2.Rdata') # sg.trt.d
load('~/Dropbox/Projects/NutNet/Model_fits/cde4.Rdata') # CDE.s.t

# meta data
p <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# ANTHROPOGENIC
p2<-distinct(p,site_code,anthropogenic)

dat <- read.csv('~/Dropbox/Projects/NutNet/Data/cumulative_time_only2.csv', sep=',') %>% 
  as_tibble() 

dat.ctl<-dat[dat$trt.y%in% c('Control'),]
dat.trt<-dat[dat$trt.y %in% c('NPK'),]

dat.trt2<-dat.trt %>% group_by(site_code,continent, habitat) %>%
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))

dat.ctl2<-dat.ctl %>% group_by(site_code,continent, habitat) %>%
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))


meta.ctl<-inner_join(dat.ctl2,p2)
meta.trt<-inner_join(dat.trt2,p2)
# View(meta.trt)
meta.c<-distinct(meta.ctl,site_code, continent, habitat,anthropogenic,r.rich)
meta.t<-distinct(meta.trt,site_code, continent, habitat,anthropogenic,r.rich)


study_levels <- plot.rich.g$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

parnames(plot.rich.g)
study_sample_posterior <- study_levels %>%
  mutate(sl.ctl = purrr::map(data, ~posterior_samples(sl.trt.h.t,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sg.ctl = purrr::map(data, ~posterior_samples(sg.trt.d,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         cde.ctl = purrr::map(data, ~posterior_samples(CDE.s.t,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.ctl = purrr::map(data, ~posterior_samples(plot.rich.g,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.ctl = purrr::map(data, ~posterior_samples(plot.bm.logt,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         sl.trt = purrr::map(data, ~posterior_samples(sl.trt.h.t, 
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sg.trt = purrr::map(data, ~posterior_samples(sg.trt.d, 
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         cde.trt = purrr::map(data, ~posterior_samples(CDE.s.t,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.trt = purrr::map(data, ~posterior_samples(plot.rich.g,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.trt = purrr::map(data, ~posterior_samples(plot.bm.logt,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))



sl.trt.i_fixef <- fixef(sl.trt.h.t)
sg.trt.i_fixef <- fixef(sg.trt.d)
CDE.trt.i_fixef <- fixef(CDE.s.t)
plot.rich.im_fixef <- fixef(plot.rich.g)
plot.bm.im_fixef <- fixef(plot.bm.logt)



sl_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sl.ctl,sl.trt) %>% 
  mutate(response = 'sl',
         sl.ctl_global_slope = sl.trt.i_fixef['year.y.m','Estimate'],
         sl.ctl_upper_slope = sl.trt.i_fixef['year.y.m','Q97.5'],
         sl.ctl_lower_slope = sl.trt.i_fixef['year.y.m','Q2.5'],
         sl.trt_global_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sl.trt_upper_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sl.trt_lower_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) 


colnames(sl_posterior)
sl.ctl <- c("site_code","sl.ctl" , "response","sl.ctl_global_slope", "sl.ctl_upper_slope" , "sl.ctl_lower_slope")
sl_posterior.ctl <- sl_posterior[sl.ctl]
sl.trt <- c("site_code","sl.trt" , "response","sl.trt_global_slope", "sl.trt_upper_slope" , "sl.trt_lower_slope")
sl_posterior.trt <- sl_posterior[sl.trt]
# View(sl_posterior.trt)
# View(meta)
sl.p.c<-sl_posterior.ctl %>% inner_join(meta.c, by = 'site_code')
sl.p.t<-sl_posterior.trt %>% inner_join(meta.t, by = 'site_code')

# View(sl.p.c)


sl.p.c$starting.richness <- ifelse(sl.p.c$r.rich >= 1 & sl.p.c$r.rich <= 5, '1-5 species',
                                   ifelse(sl.p.c$r.rich >=6 & sl.p.c$r.rich <=10, '6-10',
                                          ifelse(sl.p.c$r.rich >=11 & sl.p.c$r.rich <=15, '11-15',    
                                                 ifelse(sl.p.c$r.rich >=16 & sl.p.c$r.rich <=20, '16-20',
                                                        ifelse(sl.p.c$r.rich >=21 & sl.p.c$r.rich <=25, '21-25',
                                                               ifelse(sl.p.c$r.rich >=26, '>26', 'other'))))))

sl.p.t$starting.richness <- ifelse(sl.p.t$r.rich >= 1 & sl.p.t$r.rich <= 5, '1-5 species',
                                   ifelse(sl.p.t$r.rich >=6 & sl.p.t$r.rich <=10, '6-10',
                                          ifelse(sl.p.t$r.rich >=11 & sl.p.t$r.rich <=15, '11-15',    
                                                 ifelse(sl.p.t$r.rich >=16 & sl.p.t$r.rich <=20, '16-20',
                                                        ifelse(sl.p.t$r.rich >=21 & sl.p.t$r.rich <=25, '21-25',
                                                               ifelse(sl.p.t$r.rich >=26, '>26', 'other'))))))



# View(sg.trt.i_fixef)
sg_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sg.ctl,sg.trt) %>% 
  mutate(response = 'sg',
         sg.ctl_global_slope = sg.trt.i_fixef['year.y.m','Estimate'],
         sg.ctl_upper_slope = sg.trt.i_fixef['year.y.m','Q97.5'],
         sg.ctl_lower_slope = sg.trt.i_fixef['year.y.m','Q2.5'],
         sg.trt_global_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sg.trt_upper_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sg.trt_lower_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
  ) 

sg.ctl <- c("site_code","sg.ctl" , "response","sg.ctl_global_slope", "sg.ctl_upper_slope" , "sg.ctl_lower_slope")
sg_posterior.ctl <- sg_posterior[sg.ctl]
sg.trt <- c("site_code","sg.trt" , "response","sg.trt_global_slope", "sg.trt_upper_slope" , "sg.trt_lower_slope")
sg_posterior.trt <- sg_posterior[sg.trt]
# View(sg_posterior.trt)
# View(meta)
sg.p.c<-sg_posterior.ctl %>% inner_join(meta.c, by = 'site_code')
sg.p.t<-sg_posterior.trt %>% inner_join(meta.t, by = 'site_code')

# View(sl.p.c)


sg.p.c$starting.richness <- ifelse(sg.p.c$r.rich >= 1 & sg.p.c$r.rich <= 5, '1-5 species',
                                   ifelse(sg.p.c$r.rich >=6 & sg.p.c$r.rich <=10, '6-10',
                                          ifelse(sg.p.c$r.rich >=11 & sg.p.c$r.rich <=15, '11-15',    
                                                 ifelse(sg.p.c$r.rich >=16 & sg.p.c$r.rich <=20, '16-20',
                                                        ifelse(sg.p.c$r.rich >=21 & sg.p.c$r.rich <=25, '21-25',
                                                               ifelse(sg.p.c$r.rich >=26, '>26', 'other'))))))

sg.p.t$starting.richness <- ifelse(sg.p.t$r.rich >= 1 & sg.p.t$r.rich <= 5, '1-5 species',
                                   ifelse(sg.p.t$r.rich >=6 & sg.p.t$r.rich <=10, '6-10',
                                          ifelse(sg.p.t$r.rich >=11 & sg.p.t$r.rich <=15, '11-15',    
                                                 ifelse(sg.p.t$r.rich >=16 & sg.p.t$r.rich <=20, '16-20',
                                                        ifelse(sg.p.t$r.rich >=21 & sg.p.t$r.rich <=25, '21-25',
                                                               ifelse(sg.p.t$r.rich >=26, '>26', 'other'))))))

cde_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(cde.ctl,cde.trt) %>% 
  mutate(response = 'cde',
         cde.ctl_global_slope = CDE.trt.i_fixef['year.y.m','Estimate'],
         cde.ctl_upper_slope = CDE.trt.i_fixef['year.y.m','Q97.5'],
         cde.ctl_lower_slope = CDE.trt.i_fixef['year.y.m','Q2.5'],
         cde.trt_global_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         cde.trt_upper_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         cde.trt_lower_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
  ) 

# View(cde_posterior)
cde.ctl <- c("site_code","cde.ctl" , "response","cde.ctl_global_slope", "cde.ctl_upper_slope" , "cde.ctl_lower_slope")
cde_posterior.ctl <- cde_posterior[cde.ctl]
cde.trt <- c("site_code","cde.trt" , "response","cde.trt_global_slope", "cde.trt_upper_slope" , "cde.trt_lower_slope")
cde_posterior.trt <- cde_posterior[cde.trt]

cde.p.c<-cde_posterior.ctl %>% inner_join(meta.c, by = 'site_code')
cde.p.t<-cde_posterior.trt %>% inner_join(meta.t, by = 'site_code')

# View(cde.p.c)


cde.p.c$starting.richness <- ifelse(cde.p.c$r.rich >= 1 & cde.p.c$r.rich <= 5, '1-5 species',
                                    ifelse(cde.p.c$r.rich >=6 & cde.p.c$r.rich <=10, '6-10',
                                           ifelse(cde.p.c$r.rich >=11 & cde.p.c$r.rich <=15, '11-15',    
                                                  ifelse(cde.p.c$r.rich >=16 & cde.p.c$r.rich <=20, '16-20',
                                                         ifelse(cde.p.c$r.rich >=21 & cde.p.c$r.rich <=25, '21-25',
                                                                ifelse(cde.p.c$r.rich >=26, '>26', 'other'))))))

cde.p.t$starting.richness <- ifelse(cde.p.t$r.rich >= 1 & cde.p.t$r.rich <= 5, '1-5 species',
                                    ifelse(cde.p.t$r.rich >=6 & cde.p.t$r.rich <=10, '6-10',
                                           ifelse(cde.p.t$r.rich >=11 & cde.p.t$r.rich <=15, '11-15',    
                                                  ifelse(cde.p.t$r.rich >=16 & cde.p.t$r.rich <=20, '16-20',
                                                         ifelse(cde.p.t$r.rich >=21 & cde.p.t$r.rich <=25, '21-25',
                                                                ifelse(cde.p.t$r.rich >=26, '>26', 'other'))))))

library("scales")

# View(sl_posterior)
sl.p.c$starting.richness <- factor(sl.p.c$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sl.p.c$anthropogenic<-as.factor(sl.p.c$anthropogenic)
sl.p.t$starting.richness <- factor(sl.p.t$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sl.p.t$anthropogenic<-as.factor(sl.p.t$anthropogenic)

slf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = sl.p.t %>% distinct(sl.trt_lower_slope, sl.trt_upper_slope),
            aes(xmin = sl.trt_lower_slope, xmax =  sl.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sl.p.c,
                      aes(x = sl.ctl + unique(sl.ctl_global_slope),
                          y = anthropogenic,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sl.p.t,
                      aes(x = sl.trt + unique(sl.trt_global_slope), 
                          y = anthropogenic,
                          fill = starting.richness
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #scale_fill_viridis_d(name = 'habitat') +
  scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
                               "6-10" = "#75B41EFF",
                               "11-15" ="#5AC2F1FF",
                               "16-20"= "#0C5BB0FF",
                               "21-25" = "#972C8DFF",
                               ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = sl_posterior,
             aes(xintercept = sl.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Anthropogenic',
       x = 'Species Loss',
       title= 'Biomass Change') +
  xlim(-0.50,0.50) +
  scale_x_continuous(trans = reverse_trans()) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

slf


sg.p.c$starting.richness <- factor(sg.p.c$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.p.c$anthropogenic<-as.factor(sg.p.c$anthropogenic)
sg.p.t$starting.richness <- factor(sg.p.t$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.p.t$anthropogenic<-as.factor(sg.p.t$anthropogenic)


sgf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = sg.p.t %>% distinct(sg.trt_lower_slope, sg.trt_upper_slope),
            aes(xmin = sg.trt_lower_slope, xmax =  sg.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sg.p.c,
                      aes(x = sg.ctl + unique(sg.ctl_global_slope),
                          y = anthropogenic,
                          color = "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sg.p.t,
                      aes(x = sg.trt + unique(sg.trt_global_slope), 
                          y = anthropogenic,
                          fill= starting.richness
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #scale_fill_viridis_d(name = 'habitat') +
  scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
                               "6-10" = "#75B41EFF",
                               "11-15" ="#5AC2F1FF",
                               "16-20"= "#0C5BB0FF",
                               "21-25" = "#972C8DFF",
                               ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = sg_posterior,
             aes(xintercept = sg.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Anthropogenic',
       x = 'Species Gains',
       title= 'Biomass Change') +
  xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

sgf

cde.p.c$starting.richness <- factor(cde.p.c$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
cde.p.c$anthropogenic<-as.factor(cde.p.c$anthropogenic)
cde.p.t$starting.richness <- factor(cde.p.t$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
cde.p.t$anthropogenic<-as.factor(cde.p.t$anthropogenic)

cdef<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = cde.p.t %>% distinct(cde.trt_lower_slope, cde.trt_upper_slope),
            aes(xmin = cde.trt_lower_slope, xmax =  cde.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = cde.p.c,
                      aes(x = cde.ctl + unique(cde.ctl_global_slope),
                          y = anthropogenic,
                          color = "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = cde.p.t,
                      aes(x = cde.trt + unique(cde.trt_global_slope), 
                          y = anthropogenic,
                          fill= starting.richness
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #scale_fill_viridis_d(name = 'habitat') +
  scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
                               "6-10" = "#75B41EFF",
                               "11-15" ="#5AC2F1FF",
                               "16-20"= "#0C5BB0FF",
                               "21-25" = "#972C8DFF",
                               ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = cde_posterior,
             aes(xintercept = cde.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Anthropogenic',
       x = 'Persistent Species',
       title= 'Biomass Change') +
  xlim(-150,150) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

cdef

grid_arrange_shared_legend(slf,sgf,cdef,nrow=1)


#########################
# BIOMASS RICHNESS MODELS
#########################


p <- read.csv('~/Dropbox/Projects/NutNet/Data/plot_calc.csv', sep=',') %>% 
  as_tibble() 


plot<-p[p$year_trt %in% c('0'),]
colnames(plot)

plot.ctl<-plot[plot$trt%in% c('Control'),]
plot.trt<-plot[plot$trt %in% c('NPK'),]

plot.trt2<-plot.trt %>% group_by(site_code,continent, habitat) %>%
  summarise(s.rich = mean(rich),
            r.rich = round(s.rich))

plot.ctl2<-plot.ctl %>% group_by(site_code,continent, habitat) %>%
  summarise(s.rich = mean(rich),
            r.rich = round(s.rich))

# View(dat.ctl2)
meta.ctl.p<-inner_join(plot.ctl2,plot)
meta.trt.p<-inner_join(plot.trt2,plot)
# View(meta.trt)
meta.c.p<-distinct(meta.ctl.p,site_code, continent, habitat,anthropogenic,r.rich)
meta.t.p<-distinct(meta.trt.p,site_code, continent, habitat,anthropogenic,r.rich)


#plotmeta<-distinct(plot,site_code, continent, habitat)



#plot.rich.im_fixef
rich_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(rich.ctl,rich.trt) %>% 
  mutate(response = 'rich',
         rich.ctl_global_slope = plot.rich.im_fixef['year_trt','Estimate'],
         rich.ctl_upper_slope = plot.rich.im_fixef['year_trt','Q97.5'],
         rich.ctl_lower_slope = plot.rich.im_fixef['year_trt','Q2.5'],
         rich.trt_global_slope = plot.rich.im_fixef['trtNPK:year_trt','Estimate'],
         rich.trt_upper_slope = plot.rich.im_fixef['trtNPK:year_trt','Q97.5'],
         rich.trt_lower_slope = plot.rich.im_fixef['trtNPK:year_trt','Q2.5'],
  ) 

# View(cde_posterior)
rich.ctl <- c("site_code","rich.ctl" , "response","rich.ctl_global_slope", "rich.ctl_upper_slope" , "rich.ctl_lower_slope")
rich_posterior.ctl <- rich_posterior[rich.ctl]
rich.trt <- c("site_code","rich.trt" , "response","rich.trt_global_slope", "rich.trt_upper_slope" , "rich.trt_lower_slope")
rich_posterior.trt <- rich_posterior[rich.trt]

rich.p.c<-rich_posterior.ctl %>% inner_join(meta.c.p, by = 'site_code')
rich.p.t<-rich_posterior.trt %>% inner_join(meta.t.p, by = 'site_code')

# View(rich.p.c)


rich.p.c$starting.richness <- ifelse(rich.p.c$r.rich >= 1 & rich.p.c$r.rich <= 5, '1-5 species',
                                     ifelse(rich.p.c$r.rich >=6 & rich.p.c$r.rich <=10, '6-10',
                                            ifelse(rich.p.c$r.rich >=11 & rich.p.c$r.rich <=15, '11-15',    
                                                   ifelse(rich.p.c$r.rich >=16 & rich.p.c$r.rich <=20, '16-20',
                                                          ifelse(rich.p.c$r.rich >=21 & rich.p.c$r.rich <=25, '21-25',
                                                                 ifelse(rich.p.c$r.rich >=26, '>26', 'other'))))))

rich.p.t$starting.richness <- ifelse(rich.p.t$r.rich >= 1 & rich.p.t$r.rich <= 5, '1-5 species',
                                     ifelse(rich.p.t$r.rich >=6 & rich.p.t$r.rich <=10, '6-10',
                                            ifelse(rich.p.t$r.rich >=11 & rich.p.t$r.rich <=15, '11-15',    
                                                   ifelse(rich.p.t$r.rich >=16 & rich.p.t$r.rich <=20, '16-20',
                                                          ifelse(rich.p.t$r.rich >=21 & rich.p.t$r.rich <=25, '21-25',
                                                                 ifelse(rich.p.t$r.rich >=26, '>26', 'other'))))))


bm_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(bm.ctl,bm.trt) %>% 
  mutate(response = 'bm',
         bm.ctl_global_slope = plot.bm.im_fixef['year_trt','Estimate'],
         bm.ctl_upper_slope = plot.bm.im_fixef['year_trt','Q97.5'],
         bm.ctl_lower_slope = plot.bm.im_fixef['year_trt','Q2.5'],
         bm.trt_global_slope = plot.bm.im_fixef['trtNPK:year_trt','Estimate'],
         bm.trt_upper_slope = plot.bm.im_fixef['trtNPK:year_trt','Q97.5'],
         bm.trt_lower_slope = plot.bm.im_fixef['trtNPK:year_trt','Q2.5'],
  ) 

bm.ctl <- c("site_code","bm.ctl" , "response","bm.ctl_global_slope", "bm.ctl_upper_slope" , "bm.ctl_lower_slope")
bm_posterior.ctl <- bm_posterior[bm.ctl]
bm.trt <- c("site_code","bm.trt" , "response","bm.trt_global_slope", "bm.trt_upper_slope" , "bm.trt_lower_slope")
bm_posterior.trt <- bm_posterior[bm.trt]

bm.p.c<-bm_posterior.ctl %>% inner_join(meta.c.p, by = 'site_code')
bm.p.t<-bm_posterior.trt %>% inner_join(meta.t.p, by = 'site_code')

# View(bm.p.c)


bm.p.c$starting.richness <- ifelse(bm.p.c$r.rich >= 1 & bm.p.c$r.rich <= 5, '1-5 species',
                                   ifelse(bm.p.c$r.rich >=6 & bm.p.c$r.rich <=10, '6-10',
                                          ifelse(bm.p.c$r.rich >=11 & bm.p.c$r.rich <=15, '11-15',    
                                                 ifelse(bm.p.c$r.rich >=16 & bm.p.c$r.rich <=20, '16-20',
                                                        ifelse(bm.p.c$r.rich >=21 & bm.p.c$r.rich <=25, '21-25',
                                                               ifelse(bm.p.c$r.rich >=26, '>26', 'other'))))))

bm.p.t$starting.richness <- ifelse(bm.p.t$r.rich >= 1 & bm.p.t$r.rich <= 5, '1-5 species',
                                   ifelse(bm.p.t$r.rich >=6 & bm.p.t$r.rich <=10, '6-10',
                                          ifelse(bm.p.t$r.rich >=11 & bm.p.t$r.rich <=15, '11-15',    
                                                 ifelse(bm.p.t$r.rich >=16 & bm.p.t$r.rich <=20, '16-20',
                                                        ifelse(bm.p.t$r.rich >=21 & bm.p.t$r.rich <=25, '21-25',
                                                               ifelse(bm.p.t$r.rich >=26, '>26', 'other'))))))

nrow(bm_posterior)
nrow(rich_posterior)

rich.p.c$starting.richness <- factor(rich.p.c$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
rich.p.c$anthropogenic<-as.factor(rich.p.c$anthropogenic)
rich.p.t$starting.richness <- factor(rich.p.t$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
rich.p.t$anthropogenic<-as.factor(rich.p.t$anthropogenic)


rf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = rich.p.t %>% distinct(rich.trt_lower_slope, rich.trt_upper_slope),
            aes(xmin = rich.trt_lower_slope, xmax =  rich.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = rich.p.c,
                      aes(x = rich.ctl + unique(rich.ctl_global_slope),
                          y = anthropogenic,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = rich.p.t,
                      aes(x = rich.trt + unique(rich.trt_global_slope), 
                          y = anthropogenic,
                          fill = starting.richness
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #scale_fill_viridis_d(name = 'habitat') +
  scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
                               "6-10" = "#75B41EFF",
                               "11-15" ="#5AC2F1FF",
                               "16-20"= "#0C5BB0FF",
                               "21-25" = "#972C8DFF",
                               ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = rich_posterior,
             aes(xintercept = rich.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Anthropogenic',
       x = 'Richness') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

rf


bm.p.c$starting.richness <- factor(bm.p.c$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
bm.p.c$anthropogenic<-as.factor(bm.p.c$anthropogenic)
bm.p.t$starting.richness <- factor(bm.p.t$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
bm.p.t$anthropogenic<-as.factor(bm.p.t$anthropogenic)

bf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = bm.p.t %>% distinct(bm.trt_lower_slope, bm.trt_upper_slope),
            aes(xmin = bm.trt_lower_slope, xmax =  bm.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = bm.p.c,
                      aes(x = bm.ctl + unique(bm.ctl_global_slope),
                          y = anthropogenic,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = bm.p.t,
                      aes(x = bm.trt + unique(bm.trt_global_slope), 
                          y = anthropogenic,
                          fill = starting.richness
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #scale_fill_viridis_d(name = 'habitat') +
  scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
                               "6-10" = "#75B41EFF",
                               "11-15" ="#5AC2F1FF",
                               "16-20"= "#0C5BB0FF",
                               "21-25" = "#972C8DFF",
                               ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = bm_posterior,
             aes(xintercept = bm.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Anthropogenic',
       x = 'Biomass') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")
bf

grid_arrange_shared_legend(rf,bf,nrow=1)






#THESE WERE OLD PLOTS, PLOTTED BY CONTINENT


slf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = sl_posterior %>% distinct(sl.trt_lower_slope, sl.trt_upper_slope),
            aes(xmin = sl.trt_lower_slope, xmax =  sl.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sl_posterior,
                      aes(x = sl.trt + unique(sl.trt_global_slope), 
                          y = continent,
                          fill = continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sl_posterior,
                      aes(x = sl.ctl + unique(sl.ctl_global_slope), 
                          y = continent,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = sl_posterior,
             aes(xintercept = sl.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'Biomass change due to Species Loss') +
  xlim(-0.50,0.50) +
  scale_x_continuous(trans = reverse_trans()) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")




sgf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = sg_posterior %>% distinct(sg.trt_lower_slope, sg.trt_upper_slope),
            aes(xmin = sg.trt_lower_slope, xmax =  sg.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sg_posterior,
                      aes(x = sg.trt + unique(sg.trt_global_slope), 
                          y = continent,
                          fill= continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  # geom_density_ridges(data = sg_posterior,
  #                     aes(x = sg.ctl + unique(sg.ctl_global_slope), 
  #                         y = continent,
  #                         color = "grey"
  #                     ),
  #                     scale = 1, alpha = 0.6,
  #                     linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = sg_posterior,
             aes(xintercept = sg.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'Biomass change due to Species Gains') +
  xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")


cdef2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = cde_posterior %>% distinct(cde.trt_lower_slope, cde.trt_upper_slope),
            aes(xmin = cde.trt_lower_slope, xmax =  cde.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = cde_posterior,
                      aes(x = cde.trt + unique(cde.trt_global_slope), 
                          y = continent,
                          fill= continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = cde_posterior,
                      aes(x = cde.ctl + unique(cde.ctl_global_slope), 
                          y = continent,
                          color = "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = cde_posterior,
             aes(xintercept = cde.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'Biomass change in Persistent Species') +
  xlim(-150,150) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

grid_arrange_shared_legend(slf2,sgf2,cdef2,nrow=1)




rf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = rich_posterior %>% distinct(rich.trt_lower_slope, rich.trt_upper_slope),
            aes(xmin = rich.trt_lower_slope, xmax =  rich.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = rich_posterior,
                      aes(x = rich.trt + unique(rich.trt_global_slope), 
                          y = continent,
                          fill = continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = rich_posterior,
                      aes(x = rich.ctl + unique(rich.ctl_global_slope), 
                          y = continent,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = rich_posterior,
             aes(xintercept = rich.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'continent-level Richness') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")



bf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = bm_posterior %>% distinct(bm.trt_lower_slope, bm.trt_upper_slope),
            aes(xmin = bm.trt_lower_slope, xmax =  bm.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = bm_posterior,
                      aes(x = bm.ctl + unique(bm.ctl_global_slope), 
                          y = continent,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = bm_posterior,
                      aes(x = bm.trt + unique(bm.trt_global_slope), 
                          y = continent,
                          fill = continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = bm_posterior,
             aes(xintercept = bm.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'continent-level Biomass') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

grid_arrange_shared_legend(rf2,bf2,nrow=1)

# SITE DIVERSITY
# CO-LIMITED (STAN'S PAPER--show him coef effects and ask how he would determine) PLOTS WITH EFFECT VS. PLOTS WITH NO EFFECT
# EXOTIC VS. NATIVE DOMINATED

# HERBIVORY
# BIOGEO / CLIMATE
# N. DEPOSITION

# FACETED AS LOSSES, GAINS, CDE? WITH POSTERIORS GROUPED AS ABOVE IN DIFF COLOURS
# CONTROLS GREY IN THE BACKGROUND

