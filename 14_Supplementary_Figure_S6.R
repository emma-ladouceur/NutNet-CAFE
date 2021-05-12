



# Supplementary ggridges with precip



# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 11_Figure_5.R
# This workflow uses data pulled out of Modelsbelow to produce Figure 4


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
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss_sigmai.Rdata') # s.loss.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain_sigmai.Rdata') # s.gain.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde_sigmai.Rdata') # CDE.3_sigma2
#selected best mods
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm_sigmai.Rdata') # bm.3_sigmai 
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/rich_sigmai.Rdata') # rich.3_sigmai



# data
enviro_dat <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/site-worldclim-9-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

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

fig_s6a <- ggplot() +
  geom_density_ridges(data = rich.ps, #%>% arrange(desc(MAP_v2)) %>%
                      #                       mutate(site_code= fct_reorder(site_code, MAP_v2)),
                      aes(x = rich.trt.study + rich.trt.global, 
                          y =  MAP_v2, group = MAP_v2
                      ), fill= "#0B775E",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  # geom_point(data =npk.effs, aes(y = Quadrant, x = rich.trt.rate.p),
  #            colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  # geom_point(data= rich.ps %>% group_by(Quadrant) %>%
  #              summarise(mean.s.eff = median(rich.trt.study + rich.trt.global)), 
  #            aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Effect of NPK on species loss / year')),
        title= 'A) Species Richness',
        y= 'Annual precipitation (mm)'
  )+
 # scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="none")

fig_s6a


bm.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/bm_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

bm.ps <- bm.ps %>% left_join(enviro_dat)
head(bm.ps)
summary(bm.ps)

fig_s6b <- ggplot() +
  geom_density_ridges(data = bm.ps, #%>% arrange(desc(MAP_v2)) %>%
                      #                       mutate(site_code= fct_reorder(site_code, MAP_v2)),
                      aes(x = bm.trt.study + bm.trt.global, 
                          y =  MAP_v2, group = MAP_v2
                      ), fill= "#0B775E",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  # geom_point(data =npk.effs, aes(y = Quadrant, x = bm.trt.rate.p),
  #            colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  # geom_point(data= bm.ps %>% group_by(Quadrant) %>%
  #              summarise(mean.s.eff = median(bm.trt.study + bm.trt.global)), 
  #            aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Effect of NPK on species loss / year')),
        title= 'B) Biomass',
        y= ''
  )+
  #scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none")

fig_s6b



sloss.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sloss_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sloss.ps <- sloss.ps %>% left_join(enviro_dat)
head(sloss.ps)
summary(sloss.ps)

fig_s6c <- ggplot() +
   geom_density_ridges(data = sloss.ps, #%>% arrange(desc(MAP_v2)) %>%
  #                       mutate(site_code= fct_reorder(site_code, MAP_v2)),
                      aes(x = sloss.trt.study + sloss.trt.global, 
                          y =  MAP_v2, group = MAP_v2
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  # geom_point(data =npk.effs, aes(y = Quadrant, x = sloss.trt.rate.p),
  #            colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  # geom_point(data= sloss.ps %>% group_by(Quadrant) %>%
  #              summarise(mean.s.eff = median(sloss.trt.study + sloss.trt.global)), 
  #            aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Effect of NPK on species loss / year')),
        title= 'C) Species loss (s.loss)',
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

fig_s6c


sgain.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sgain_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sgain.ps <- sgain.ps %>% left_join(enviro_dat)

fig_s6d <- ggplot() +
  geom_density_ridges(data = sgain.ps,
                      aes(x = sgain.trt.study + sgain.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  # geom_point(data =npk.effs, aes(y = Quadrant, x = sgain.trt.rate.p),
  #            colour= "#3B9AB2",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  # geom_point(data= sgain.ps %>% group_by(Quadrant) %>%
  #              summarise(mean.s.eff = median(sgain.trt.study + sgain.trt.global)), 
  #            aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( x = expression(paste('Effect of NPK on species gain / year')),
        title= 'D) Species gain (s.gain)',
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

fig_s6d

# species loss (s.loss)
sl.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sl.ps <- sl.ps %>% left_join(enviro_dat)

fig_s6e <- ggplot() +
  geom_density_ridges(data = sl.ps,
                      aes(x = sl.trt.study + sl.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  # geom_point(data =npk.effs, aes(y = Quadrant, x = sl.trt.rate.p),
  #            colour= "#B40F20",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  # geom_point(data= sl.ps %>% group_by(Quadrant) %>%
  #              summarise(mean.s.eff = median(sl.trt.study + sl.trt.global)), 
  #            aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  scale_x_continuous(breaks=c(-40,-20,-10,-5,0,10), limits=c(-40,15))+
  scale_y_continuous(
    breaks = c(200, 500, 800, 900, 1000, 1500, 1800),
    expand = c(0, 0)
  ) +
  labs( 
    x = expression(paste(atop('Effect of NPK on', paste('biomass change (g/' ,m^2, ') / year')))),
    title= 'E) Biomass change associated \n with species loss (SL)',
    y= 'Annual precipitation (mm)'
  ) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        axis.title.x=element_text( hjust=0.5, 
                                   margin = margin(t = 16, r = 0, b = 0, l = 0)), 
        legend.position="none") 


fig_s6e


sg.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sg.ps <- sg.ps %>% left_join(enviro_dat)

fig_s6f <- ggplot() +
  geom_density_ridges(data = sg.ps,
                      aes(x = sg.trt.study + sg.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  # geom_point(data =npk.effs, aes(y = Quadrant, x = sg.trt.rate.p),
  #            colour= "#3B9AB2",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  # geom_point(data= sg.ps %>% group_by(Quadrant) %>%
  #              summarise(mean.s.eff = median(sg.trt.study + sg.trt.global)), 
  #            aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs( #x = '',
    x = expression(paste(atop('Effect of NPK on', paste('biomass change (g/' ,m^2, ') / year')))),
    title= 'F) Biomass change associated \n with species gain (SG)',
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

fig_s6f



cde.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

cde.ps <- cde.ps %>% left_join(enviro_dat)

fig_s6g <- ggplot() +
  geom_density_ridges(data = cde.ps,
                      aes(x = cde.trt.study + cde.trt.global, 
                          y = MAP_v2, group = MAP_v2
                      ), fill="#F98400",
                      scale = 1, alpha = 0.3,
                      linetype = 0,
                      scale = 10, size = 0.25, rel_min_height = 0.03) +
  # geom_point(data =npk.effs, aes(y = Quadrant, x = cde.trt.rate.p),
  #            colour= "#F98400",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  # geom_point(data= cde.ps %>% group_by(Quadrant) %>%
  #              summarise(mean.s.eff = median(cde.trt.study + cde.trt.global)), 
  #            aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw(base_size=14) +
  labs(#x='', 
    x = expression(paste(atop('Effect of NPK on', paste('biomass change (g/' ,m^2, ') / year')))),
    title= 'G) Biomass change associated \n with persistent species (PS)',
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

fig_s6g


# PORTRAIT 12 x 15
( fig_s6a |  fig_s6b)/( fig_s6c | fig_s6d)/(  fig_s6e | fig_s6f | fig_s6g)




