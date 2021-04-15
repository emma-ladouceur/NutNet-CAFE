

# Fig 2

library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)



load('~/Dropbox/Projects/NutNet/Data/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Data/3/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')



yr<-plot.rich_coef3 %>% select(site_code,xmax)

plot.rich_fitted.npk <- plot.rich_fitted.npk %>% left_join(yr)



View(plot.rich_fitted.npk)
View(plot.rich_coef3)



rmatrix<-ggplot() +
  facet_wrap(~site_code) +
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = all.div, color=xmax), alpha=0.3,
             size = 1.3, position = position_jitter(width = 0.45 )) +
  geom_segment(data = plot.rich_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code,colour =xmax),
               size = .7) +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(
    x = 'Year',
    y = ' Species richness', title= 'Species richness  ') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))

rmatrix


# BIOMASS



yr<-plot.bm_coef3 %>% select(site_code,xmax)

plot.bm_fitted.npk <- plot.bm_fitted.npk %>% left_join(yr)


bmatrix<-ggplot() +
  facet_wrap(~site_code)+
  geom_point(data = plot.bm_fitted.npk,
             aes(x = year_trt, y = plot.mass,colour = xmax), alpha=0.3,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = plot.bm_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code,colour = xmax), 
               size = 0.7) +
  # uncertainy in fixed effect
  labs(x='Year',
       #x = 'Years',
       y = expression(paste('Biomass (g/',m^2, ')')), title= 'Biomass') +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
 # ylim(0,1900)+
  # xlim(0,11) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))


bmatrix




#--------------------------------------------------------------------------------


# PARTITIONS

sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



# GAINS N LOSSES
load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


yr<-sgain.trt_coef3 %>% select(site_code,xmax)

sgain.trt_fitted.npk <- sgain.trt_fitted.npk %>% left_join(yr)

sgain.trt_coef3$xs<-1

sgain.matrix <- ggplot()  +
  facet_wrap(~site_code) +
  geom_point(data = sgain.trt_fitted.npk,
             aes(x = year.y, y = s.gain,colour = xmax),  alpha=0.3,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sgain.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   colour = xmax),
                size = .7) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(0,20) +
  labs(x = 'Year',
       y = expression(paste('Species Gain')), title= 'Species Gain') +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                    # plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


sgain.matrix




# SLOSS


yr<-sloss.trt_coef3 %>% select(site_code,xmax)

sloss.trt_fitted.npk <- sloss.trt_fitted.npk %>% left_join(yr)

sloss.trt_coef3$xs<-1

sloss.matrix <- ggplot()  +
  facet_wrap(~site_code) +
  geom_point(data = sloss.trt_fitted.npk,
             aes(x = year.y, y = s.loss.n,colour = xmax),  alpha=0.3,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sloss.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   colour = xmax),
               size = .7) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
 # ylim(0,20) +
  labs(x = 'Year',
       y = expression(paste('Species Loss')), title= 'Species Loss') +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


sloss.matrix




#--------------------------------------------------------------------------------




load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/sg_dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')




yr<-sg.trt_coef3 %>% select(site_code,xmax)

sg.trt_fitted.npk <- sg.trt_fitted.npk %>% left_join(yr)

sg.trt_coef3$xs<-1

sg.matrix <- ggplot()  +
  facet_wrap(~site_code) +
  geom_point(data = sg.trt_fitted.npk,
             aes(x = year.y, y = SG,colour = xmax),  alpha=0.3,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sg.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   colour = xmax),
               size = .7) +
 # scale_x_continuous(breaks=c(1,3,6,9,12)) +
 # ylim(0,20) +
  labs(x = 'Year',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')),  title= 'Biomass Change Due To Species Gain') +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


sg.matrix




#SLOSS


yr<-sl.trt_coef3 %>% select(site_code,xmax)

sl.trt_fitted.npk <- sl.trt_fitted.npk %>% left_join(yr)

sl.trt_coef3$xs<-1

sl.matrix <- ggplot()  +
  facet_wrap(~site_code) +
  geom_point(data = sl.trt_fitted.npk,
             aes(x = year.y, y = SL,colour = xmax),  alpha=0.3,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sl.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   colour = xmax),
               size = .7) +
 # scale_x_continuous(breaks=c(1,3,6,9,12)) +
  # ylim(0,20) +
  labs(x = 'Year',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'Biomass Change Due To Species Loss') +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                    # plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


sl.matrix





# CDE


yr<-cde_coef3 %>% select(site_code,xmax)

cde_fitted.npk <- cde_fitted.npk %>% left_join(yr)

cde_coef3$xs<-1


View(cde_coef3)

cde.matrix <- ggplot()  +
  facet_wrap(~site_code) +
  geom_point(data = cde_fitted.npk,
             aes(x = year.y, y = CDE,colour = xmax),  alpha=0.3,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = cde_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   colour = xmax),
               size = .7) +
  # scale_x_continuous(breaks=c(1,3,6,9,12)) +
  # ylim(0,20) +
  labs(x = 'Year',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'Persistent Species Change in Biomass') +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


cde.matrix





