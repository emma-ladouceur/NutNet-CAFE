

# Authors: Emma Ladouceur & Shane A. Blowes

# Last Updated May 2022

# Produces Figure S10 a-j

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

# produced in 7_Model_Data_Posteriors.R
meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(meta)

# model objects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sloss.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sgain.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/cde.Rdata') # CDE.s


# population fitted values for year = 13 8_Figure_2
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
  mutate(predicted = map(data, ~epred_draws(sloss.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code), ndraws = 1000 ))) 


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
  #mutate(sloss.ctl.global = (Control.global + Control.site),
   #      sloss.trt.global = (NPK.global + NPK.site)) %>%
  left_join(meta)

head(s.loss)


s.loss$Quadrant<-factor(s.loss$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sloss.ps

fig_s10a <- ggplot() +
  geom_density_ridges(data = s.loss, #density ridges
                      aes(x = NPK.site , 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data = s.loss %>% group_by(site_code, Quadrant) %>% # study level means
               summarise(mean.s.eff = mean(NPK.site)),
             aes(y = Quadrant, x = mean.s.eff), 
             colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= s.loss %>% group_by(Quadrant) %>% # mean per quad
               summarise(mean.q.eff = mean(NPK.global )),
             aes(x= mean.q.eff, y= Quadrant),  size = 4, shape = 5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Average total species loss in NPK plots')),
        title= 'Species loss (s.loss)',
        subtitle = "a)",
        y= ''
  )+
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_s10a



# sgain
s.gain.site.pred <- p.all %>% 
  mutate(site_code_group = site_code) %>%
  group_by(site_code_group, site_code, trt.y) %>% 
  summarise(year.y.m =  8.21,
            year.y =  13 ) %>%
  nest(data = c(site_code, trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~epred_draws(sgain.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code), ndraws = 1000 ))) 


head(s.gain.site.pred)

s.gain.predicted.df  <- s.gain.site.pred %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration)) %>%
  group_by(site_code) %>%
  spread(trt.y, .epred) %>%
  mutate(Control.site = Control,
         NPK.site = NPK) %>%
  select(-c(Control, NPK, .draw))

head(s.gain.predicted.df)

head(s.gain.fitted.df)
nrow(s.gain.fitted.df)

s.gain.fitted <- s.gain.fitted.df %>%
  ungroup() %>%
  select(-Trt_group) %>%
  group_by( year.y, year.y.m) %>%
  spread(trt.y, .epred) %>%
  sample_n(1000, replace = F) %>%
  mutate(Control.global = Control,
         NPK.global = NPK) %>%
  select(-c(Control, NPK, .draw))


head(s.gain.fitted)
nrow(s.gain.fitted)


s.gain <- s.gain.predicted.df %>%
  group_by(site_code) %>%
  mutate(response = 'sgain',
         s.gain.fitted[,'Control.global'],
         s.gain.fitted[,'NPK.global'],) %>%
  #mutate(sgain.ctl.global = (Control.global + Control.site),
  #      sgain.trt.global = (NPK.global + NPK.site)) %>%
  left_join(meta)

head(s.gain)


s.gain$Quadrant<-factor(s.gain$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sgain.ps

fig_s10b <- ggplot() +
  geom_density_ridges(data = s.gain, #density ridges
                      aes(x = NPK.site , 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data = s.gain %>% group_by(site_code, Quadrant) %>% # study level means
               summarise(mean.s.eff = mean(NPK.site)),
             aes(y = Quadrant, x = mean.s.eff), 
             colour= "#3B9AB2", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= s.gain %>% group_by(Quadrant) %>% # mean per quad
               summarise(mean.q.eff = mean(NPK.global )),
             aes(x= mean.q.eff, y= Quadrant),  size = 4, shape = 5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Average total species gain in NPK plots')),
        title= 'Species gain (s.gain)',
        subtitle = "b)",
        y= ''
  )+
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_s10b


sl.site.pred <- p.all %>% 
  mutate(site_code_group = site_code) %>%
  group_by(site_code_group, site_code, trt.y) %>% 
  summarise(year.y.m =  8.21,
            year.y =  13 ) %>%
  nest(data = c(site_code, trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~epred_draws(sl.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code), ndraws = 1000 ))) 


head(sl.site.pred)

sl.predicted.df  <- sl.site.pred %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration)) %>%
  group_by(site_code) %>%
  spread(trt.y, .epred) %>%
  mutate(Control.site = Control,
         NPK.site = NPK) %>%
  select(-c(Control, NPK, .draw))

head(sl.predicted.df)

head(s.sl.fitted.df)
nrow(s.sl.fitted.df)

sl.fitted <- s.sl.fitted.df %>%
  ungroup() %>%
  select(-Trt_group) %>%
  group_by( year.y, year.y.m) %>%
  spread(trt.y, .epred) %>%
  sample_n(1000, replace = F) %>%
  mutate(Control.global = Control,
         NPK.global = NPK) %>%
  select(-c(Control, NPK, .draw))


head(sl.fitted)
nrow(sl.fitted)


sl <- sl.predicted.df %>%
  group_by(site_code) %>%
  mutate(response = 'sloss',
         sl.fitted[,'Control.global'],
         sl.fitted[,'NPK.global'],) %>%
  #mutate(sloss.ctl.global = (Control.global + Control.site),
  #      sloss.trt.global = (NPK.global + NPK.site)) %>%
  left_join(meta)

head(sl)


sl$Quadrant<-factor(sl$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sl

fig_s10c <- ggplot() +
  geom_density_ridges(data = sl, #density ridges
                      aes(x = NPK.site , 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data = sl %>% group_by(site_code, Quadrant) %>% # study level means
               summarise(mean.s.eff = mean(NPK.site)),
             aes(y = Quadrant, x = mean.s.eff), 
             colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sl %>% group_by(Quadrant) %>% # mean per quad
               summarise(mean.q.eff = mean(NPK.global )),
             aes(x= mean.q.eff, y= Quadrant),  size = 4, shape = 5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x =  expression(paste(atop( paste('Average total biomass change (g/' ,m^2, ')'), 'in NPK plots'))),
        title= 'Biomass change associated \n with species loss (SL)',
        subtitle = "c)",
        y= ''
  )+
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_s10c



sg.site.pred <- p.all %>% 
  mutate(site_code_group = site_code) %>%
  group_by(site_code_group, site_code, trt.y) %>% 
  summarise(year.y.m =  8.21,
            year.y =  13 ) %>%
  nest(data = c(site_code, trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~epred_draws(sg.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m | site_code), ndraws = 1000 ))) 


head(sg.site.pred)

sg.predicted.df  <- sg.site.pred %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration)) %>%
  group_by(site_code) %>%
  spread(trt.y, .epred) %>%
  mutate(Control.site = Control,
         NPK.site = NPK) %>%
  select(-c(Control, NPK, .draw))

head(sg.predicted.df)

head(s.sg.fitted.df)
nrow(s.sg.fitted.df)

sg.fitted <- s.sg.fitted.df %>%
  ungroup() %>%
  select(-Trt_group) %>%
  group_by( year.y, year.y.m) %>%
  spread(trt.y, .epred) %>%
  sample_n(1000, replace = F) %>%
  mutate(Control.global = Control,
         NPK.global = NPK) %>%
  select(-c(Control, NPK, .draw))


head(sg.fitted)
nrow(sg.fitted)


sg <- sg.predicted.df %>%
  group_by(site_code) %>%
  mutate(response = 'sloss',
         sg.fitted[,'Control.global'],
         sg.fitted[,'NPK.global'],) %>%
  #mutate(sloss.ctl.global = (Control.global + Control.site),
  #      sloss.trt.global = (NPK.global + NPK.site)) %>%
  left_join(meta)

head(sg)


sg$Quadrant<-factor(sl$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sg

fig_s10d <- ggplot() +
  geom_density_ridges(data = sg, #density ridges
                      aes(x = NPK.site , 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data = sg %>% group_by(site_code, Quadrant) %>% # study level means
               summarise(mean.s.eff = mean(NPK.site)),
             aes(y = Quadrant, x = mean.s.eff), 
             colour= "#3B9AB2", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sg %>% group_by(Quadrant) %>% # mean per quad
               summarise(mean.q.eff = mean(NPK.global )),
             aes(x= mean.q.eff, y= Quadrant),  size = 4, shape = 5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x =  expression(paste(atop( paste('Average total biomass change (g/' ,m^2, ')'), 'in NPK plots'))),
        title= 'Biomass change associated \n with species gain (SG)',
        subtitle = "d)",
        y= ''
  )+
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_s10d

#cde
cde.site.pred <- p.all %>% 
  mutate(site_code_group = site_code) %>%
  group_by(site_code_group, site_code, trt.y) %>% 
  summarise(year.y.m =  8.21,
            year.y =  13 ) %>%
  # summarise(year.y.m =  max(year.y.m),
  #           year.y =  max.year.y ) %>%
  nest(data = c(site_code, trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~epred_draws(cde.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m  | site_code), ndraws = 1000 ))) 


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

head(s.cde.fitted.df)
nrow(s.cde.fitted.df)

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
  # mutate(sloss.ctl.global = (Control.global + Control.site),
  #        sloss.trt.global = (NPK.global + NPK.site)) %>%
  left_join(meta)

View(cde)


cde$Quadrant<-factor(cde$Quadrant,  levels=c("-biomass +rich", "+biomass +rich",  "-biomass -rich" , "+biomass -rich"))



fig_s10e <- ggplot() +
  geom_density_ridges(data = cde, #density ridges
                      aes(x = NPK.site , 
                          y = Quadrant,
                      ), fill="#F98400",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data = cde %>% group_by(site_code, Quadrant) %>% # study level means
               summarise(mean.s.eff = mean(NPK.site)),
             aes(y = Quadrant, x = mean.s.eff), 
             colour= "#F98400", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= cde %>% group_by(Quadrant) %>% # mean per quad
               summarise(mean.q.eff = mean(NPK.global )),
             aes(x= mean.q.eff, y= Quadrant),  size = 4, shape = 5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs(     x = expression(paste(atop(paste('Average total biomass change (g/' ,m^2, ')'), 'in NPK plots'))),
            title= 'Biomass change associated \n with persistent species (PS)',
            subtitle = "e)",
            y= ''
  )+
  geom_text(data = cde %>%
              group_by(Quadrant) %>%
              mutate(n_sites = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_sites, .keep_all = T),
            aes(x=-1100, y=Quadrant,
                label=paste('n[sites] == ', n_sites)),
            size=6,
            nudge_y = 0.5, parse = T) +
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

fig_s10e



# LANDSCAPE 10 X 19
# Figure S10
fig_s10 <- (fig_s10a | fig_s10b | fig_s10c | fig_s10d | fig_s10e) /
            (fig_s10f | fig_s10g | fig_s10h | fig_s10i | fig_s10j )
fig_s10
