

# Supplementary ggridges with precip

# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated May 22, 2021

# 11_Figure_S11.R


# packages
library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(stringr)


# selected mods
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.3_sigma2
#selected best mods
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # bm.3 
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # rich.3



# data
# not provided
#enviro_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/site-worldclim-9-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(meta)

head(meta)

# 
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/study.price.p.effs.Rdata')

# run code from Figure 5 first to produce datasets loaded below

# data for the site level means and overall medians of each cat
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/study.price.p.effs.Rdata')
npk.effs <- all.effs %>% select(site_code,sg.trt.rate.p, sl.trt.rate.p,cde.trt.rate.p,sloss.trt.rate.p,sgain.trt.rate.p) %>%
  left_join(enviro_dat)

head(npk.effs)

# for axis titles with expressions
# https://stackoverflow.com/questions/13223846/ggplot2-two-line-label-with-expression


rich.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/rich_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rich.ps <- rich.ps %>% left_join(enviro_dat)
head(rich.ps)
summary(rich.ps)

fig_s11a <- ggplot() +
  geom_density_ridges(data = rich.ps, 
                      aes(x = rich.trt.study + rich.trt.global, 
                          y =  MAP_v2, group = MAP_v2
                      ), fill= "#0B775E",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Species richness change / year in NPK plots')),
        title= 'a) Species richness',
        y= 'Annual precipitation (mm)'
  )+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="none")

fig_s11a


bm.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/bm_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

bm.ps <- bm.ps %>% left_join(enviro_dat)
head(bm.ps)
summary(bm.ps)

fig_s11b <- ggplot() +
  geom_density_ridges(data = bm.ps, 
                      aes(x = bm.trt.study + bm.trt.global, 
                          y =  MAP_v2, group = MAP_v2
                      ), fill= "#0B775E",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs(   x = expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'in NPK plots'))),
        title= 'b) Biomass',
        y= ''
  )+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none")

fig_s11b



sloss.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sloss_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sloss.ps <- sloss.ps %>% left_join(enviro_dat)
head(sloss.ps)
summary(sloss.ps)

fig_s11c <- ggplot() +
   geom_density_ridges(data = sloss.ps, 
                      aes(x = sloss.trt.study + sloss.trt.global, 
                          y =  MAP_v2, group = MAP_v2
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Species loss / year in NPK plots')),
        title= 'c) Species loss (s.loss)',
        y= 'Annual precipitation (mm)'
  )+
  scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  scale_y_continuous(
     breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
     expand = c(0, 0)
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="none")

fig_s11c


sgain.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sgain_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sgain.ps <- sgain.ps %>% left_join(enviro_dat)

fig_s11d <- ggplot() +
  geom_density_ridges(data = sgain.ps,
                      aes(x = sgain.trt.study + sgain.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Species gain / year in NPK plots')),
        title= 'd) Species gain (s.gain)',
        y= ' '
  )+
  scale_x_continuous(breaks=c(-1,0,1), limits=c(-1.5,1.5))+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none") 

fig_s11d

# species loss (s.loss)
sl.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sl.ps <- sl.ps %>% left_join(enviro_dat)

fig_s11e <- ggplot() +
  geom_density_ridges(data = sl.ps,
                      aes(x = sl.trt.study + sl.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  scale_x_continuous(breaks=c(-40,-20,-10,-5,0,10), limits=c(-40,15))+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  labs( 
    x = expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'in NPK plots'))),
    title= 'e) Biomass change associated \n with species loss (SL)',
    y= 'Annual precipitation (mm)'
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.title.x=element_text( hjust=0.5, 
                                   margin = margin(t = 16, r = 0, b = 0, l = 0)), 
        legend.position="none") 


fig_s11e


sg.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sg.ps <- sg.ps %>% left_join(enviro_dat)

fig_s11f <- ggplot() +
  geom_density_ridges(data = sg.ps,
                      aes(x = sg.trt.study + sg.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( #x = '',
    x = expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'in NPK plots'))),
    title= 'f) Biomass change associated \n with species gain (SG)',
    y= ' '
  )+
  scale_x_continuous(breaks=c(-10,0,5,10,20,40,60,80), limits=c(-20,80))+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text( hjust=0.5,
                                   margin = margin(t = 16, r = 0, b = 0, l = 0)), 
        legend.position="none")

fig_s11f



cde.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

cde.ps <- cde.ps %>% left_join(enviro_dat)

fig_s11g <- ggplot() +
  geom_density_ridges(data = cde.ps,
                      aes(x = cde.trt.study + cde.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#F98400",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs(#x='', 
    x = expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'in NPK plots'))),
    title= 'g) Biomass change associated \n with persistent species (PS)',
    y= ''
  )+
  scale_x_continuous(breaks=c(-150,-100,-50,-25,0,25,50,100), limits=c(-175,100))+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  # geom_text(data = cde.ps %>%
  #             group_by(Quadrant) %>%
  #             mutate(n_sites = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(Quadrant, n_sites, .keep_all = T),
  #           aes(x=-100, y=Quadrant,
  #               label=paste('n[sites] == ', n_sites)),
  #           size=6,
  #           nudge_y = 0.5, parse = T) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(  hjust=0.5,
                                    margin = margin(t = 16, r = 0, b = 0, l = 0)), 
        legend.position="none")

fig_s11g


# PORTRAIT 12 x 15
( fig_s11a |  fig_s11b)/( fig_s11c | fig_s11d)/(  fig_s11e | fig_s11f | fig_s11g)




