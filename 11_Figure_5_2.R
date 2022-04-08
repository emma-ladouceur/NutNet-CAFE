


# packages
library(patchwork)
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(stringr)



# data
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


colnames(p.all)

p.all <- p.all %>% group_by(site_code) %>% #filter(max.year >= 3) 
  filter(year_max >= 3) %>% mutate(year.y == max(year.y))

head(p.all)

p.all %>% ungroup() %>% select(year_max) %>% distinct() %>% mutate(mean(year_max))

meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(meta)

# models
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sloss.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sgain.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/cde.Rdata') # CDE.s


# population fitted values for year = 13 (code in fitted.r)
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.gain.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sl.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sg.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.cde.Rdata')

View(s.loss.fitted.df)

colnames(p.all)

#sloss
s.loss.site.pred <- p.all %>% 
  mutate(site_code_group = site_code) %>%
  group_by(site_code_group, site_code, trt.y) %>% 
  summarise(year.y.m =  8.21,
            year.y =  13 ) %>%
  nest(data = c(site_code, trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~epred_draws(sloss.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m), ndraws = 1000 ))) 


head(s.loss.site.pred)

s.loss.predicted.df  <- s.loss.site.pred %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration)) %>%
  group_by(site_code) %>%
  spread(trt.y, .epred) %>%
  mutate(Control.site = Control,
         NPK.site = NPK) %>%
  select(-c(Control, NPK, .draw))

head(s.loss.predicted.df)

head(s.loss.fitted.df)
nrow(s.loss.fitted.df)

s.loss.fitted <- s.loss.fitted.df %>%
  ungroup() %>%
  select(-Trt_group) %>%
  group_by( year.y, year.y.m) %>%
  spread(trt.y, .epred) %>%
  sample_n(1000, replace = F) %>%
  mutate(Control.global = Control,
         NPK.global = NPK) %>%
  select(-c(Control, NPK, .draw))


head(s.loss.fitted)
nrow(s.loss.fitted)


s.loss <- s.loss.predicted.df %>%
  group_by(site_code) %>%
  mutate(response = 'sloss',
         s.loss.fitted[,'Control.global'],
         s.loss.fitted[,'NPK.global'],) %>%
  mutate(sloss.ctl.global = (Control.global + Control.site),
         sloss.trt.global = (NPK.global + NPK.site)) %>%
  left_join(meta)

View(s.loss)


s.loss$Quadrant<-factor(s.loss$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sloss.ps

fig_5a <- ggplot() +
  geom_density_ridges(data = s.loss, #density ridges
                      aes(x = NPK.global , 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data = s.loss %>% group_by(site_code, Quadrant) %>% # study level means
               summarise(mean.s.eff = mean(NPK.site)),
             aes(y = Quadrant, x = mean.s.eff), 
             colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= s.loss %>% group_by(Quadrant) %>% # median per quad
               summarise(mean.q.eff = median(NPK.global )),
             aes(x= mean.q.eff, y= Quadrant),  size = 4, shape = 5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Average total species loss in NPK plots')),
        title= 'a) Species loss (s.loss)',
        y= ''
  )+
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_5a





#cde
cde.site.pred <- p.all %>% 
  mutate(site_code_group = site_code) %>%
  group_by(site_code_group, site_code, trt.y) %>% 
  summarise(year.y.m =  8.21,
            year.y =  13 ) %>%
  # summarise(year.y.m =  max(year.y.m),
  #           year.y =  max.year.y ) %>%
  nest(data = c(site_code, trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~epred_draws(cde.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m), ndraws = 1000 ))) 


head(cde.site.pred)

cde.predicted.df  <- cde.site.pred %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration)) %>%
  group_by(site_code) %>%
  spread(trt.y, .epred) %>%
  mutate(Control.site = Control,
         NPK.site = NPK) %>%
  select(-c(Control, NPK, .draw))

head(cde.predicted.df)

head(cde.fitted.df)
nrow(cde.fitted.df)

cde.fitted <- s.cde.fitted.df %>%
  ungroup() %>%
  select(-Trt_group) %>%
  group_by( year.y, year.y.m) %>%
  spread(trt.y, .epred) %>%
  sample_n(1000, replace = F) %>%
  mutate(Control.global = Control,
         NPK.global = NPK) %>%
  select(-c(Control, NPK, .draw))


head(cde.fitted)
nrow(cde.fitted)


cde <- cde.predicted.df %>%
  group_by(site_code) %>%
  mutate(response = 'sloss',
         cde.fitted[,'Control.global'],
         cde.fitted[,'NPK.global'],) %>%
  mutate(sloss.ctl.global = (Control.global + Control.site),
         sloss.trt.global = (NPK.global + NPK.site)) %>%
  left_join(meta)

View(cde)


cde$Quadrant<-factor(cde$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))



fig_5e <- ggplot() +
  geom_density_ridges(data = cde, #density ridges
                      aes(x = NPK.global , 
                          y = Quadrant,
                      ), fill="#F98400",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data = cde %>% group_by(site_code, Quadrant) %>% # study level means
               summarise(mean.s.eff = mean(NPK.site)),
             aes(y = Quadrant, x = mean.s.eff), 
             colour= "#F98400", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= cde %>% group_by(Quadrant) %>% # median per quad
               summarise(mean.q.eff = median(NPK.global )),
             aes(x= mean.q.eff, y= Quadrant),  size = 4, shape = 5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs(     x = expression(paste(atop(paste('Average total biomass change (g/' ,m^2, ')'), 'in NPK plots'))),
            title= 'e) Biomass change associated \n with persistent species (PS)',
            y= ''
  )+
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_5e






