
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(wesanderson)

# models
load('~/Dropbox/Projects/NutNet/Model_fits/sl.n.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Model_fits/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s


# coeffiecients for experiment level (random) effects, fitted values, fixed effects
load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sg_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')



sloss.trt_coef4<-sloss.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
sl.trt_coef4<-sl.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]

colnames(sloss.trt_coef4)
colnames(sl.trt_coef4)

names(sloss.trt_coef4) <- c("site_code","IX.Slope","IX.Slope_lower","IX.Slope_upper","X.Slope","X.Slope_lower","X.Slope_upper")
names(sl.trt_coef4) <- c("site_code","IY.Slope","IY.Slope_lower","IY.Slope_upper","Y.Slope","Y.Slope_lower","Y.Slope_upper")

loss.coefs<-left_join(sloss.trt_coef4,sl.trt_coef4)
loss.coefs$group<-"Losses"

head(loss.coefs)

#gains
sgain.trt_coef4<-sgain.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
sg.trt_coef4<-sg.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]

colnames(sgain.trt_coef4)
colnames(sg.trt_coef4)

names(sgain.trt_coef4) <- c("site_code","IX.Slope","IX.Slope_lower","IX.Slope_upper","X.Slope","X.Slope_lower","X.Slope_upper")
names(sg.trt_coef4) <- c("site_code","IY.Slope","IY.Slope_lower","IY.Slope_upper","Y.Slope","Y.Slope_lower","Y.Slope_upper")

gain.coefs<-left_join(sgain.trt_coef4,sg.trt_coef4)
gain.coefs$group<-"Gains"

# persistent sp.
colnames(cde_coef3)
cde_coef4<-cde_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
colnames(cde_coef4)
names(cde_coef4) <- c("site_code","IY.Slope","IY.Slope_lower","IY.Slope_upper","Y.Slope","Y.Slope_lower","Y.Slope_upper")
cde_coef4$group<-"Persistent Sp."
cde_coef4$IX.Slope<-0
cde_coef4$IX.Slope_lower<-0
  cde_coef4$IX.Slope_upper<-0
  cde_coef4$X.Slope<-0
  cde_coef4$X.Slope_lower<-0
  cde_coef4$X.Slope_upper<-0
  head(cde_coef4)
  
m.coefs<-bind_rows(loss.coefs,gain.coefs,cde_coef4)
head(m.coefs)



sloss.trt_fitted.npk
sl.trt_fitted.npk


# Simplest Version
# all going from 0
# vec.slope.fig<-ggplot(data= m.coefs, aes(x= IX.Slope+X.Slope, y= IY.Slope+Y.Slope,color=group)) +
#   geom_point(alpha=0.3,size=2) +
#   geom_errorbar(data= m.coefs, aes(ymin = IY.Slope_lower+Y.Slope_lower, ymax = IY.Slope_upper+Y.Slope_upper, color=group), width = 0, size = 0.45,alpha=0.3) +
#   geom_errorbarh(data= m.coefs, aes(xmin = IX.Slope_lower+X.Slope_lower, xmax = IX.Slope_upper+X.Slope_upper,color=group), width = 0, size = 0.45,alpha=0.3) +
#   geom_segment(data=. %>% filter(group == "Losses"),aes(x=0,
#                                xend=IX.Slope+X.Slope,
#                                y=0,
#                                yend=IY.Slope+Y.Slope,
#                                colour=group), alpha=0.3,
#                arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
#   geom_segment(data=. %>% filter(group == "Gains"),aes(x= 0,
#                                                         xend=IX.Slope+X.Slope,
#                                                         y=0,
#                                                         yend=IY.Slope+Y.Slope,
#                                                         colour=group), alpha=0.3,
#                arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
#   geom_segment(data=. %>% filter(group == "Persistent Sp."),aes(x= 0,
#                                                        xend=IX.Slope+X.Slope,
#                                                        y=0,
#                                                        yend=IY.Slope+Y.Slope,
#                                                        colour=group), alpha=0.3,
#                arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
#   scale_color_manual(values=c("#3B9AB2","#B40F20","#35274A"))+
#   labs(x = 'Species Change Slope',
#        y = 'Biomass Change Slope',
#        title= 'Vector 4.0') +
#     ylim(-50,50)+
#   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")
# 
# vec.slope.fig



# Simplest Version
# all going from 0
vec.slope.fig<-ggplot(data= m.coefs, aes(x= X.Slope, y= Y.Slope,color=group)) +
  geom_point(alpha=0.3,size=2) +
  geom_errorbar(data= m.coefs, aes(ymin = Y.Slope_lower, ymax = Y.Slope_upper, color=group), width = 0, size = 0.45,alpha=0.3) +
  geom_errorbarh(data= m.coefs, aes(xmin = X.Slope_lower, xmax = X.Slope_upper,color=group), width = 0, size = 0.45,alpha=0.3) +
  geom_segment(data=. %>% filter(group == "Losses"),aes(x=0,
                                                        xend=X.Slope,
                                                        y=0,
                                                        yend=Y.Slope,
                                                        colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data=. %>% filter(group == "Gains"),aes(x= 0,
                                                       xend=X.Slope,
                                                       y=0,
                                                       yend=Y.Slope,
                                                       colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data=. %>% filter(group == "Persistent Sp."),aes(x= 0,
                                                                xend=X.Slope,
                                                                y=0,
                                                                yend=Y.Slope,
                                                                colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  scale_color_manual(values=c("#3B9AB2","#B40F20","#35274A"))+
  labs(x = 'Species Change Slope',
       y = 'Biomass Change Slope',
       title= 'Vector 4.0') +
  ylim(-50,50)+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

vec.slope.fig




# all going from 0
# plus fixed effect
vec.slope.fig<-ggplot(data= m.coefs, aes(x= X.Slope, y= Y.Slope,color=group)) +
  geom_point(alpha=0.3,size=2) +
  geom_errorbar(data= m.coefs, aes(ymin = Y.Slope_lower, ymax = Y.Slope_upper, color=group), width = 0, size = 0.45,alpha=0.3) +
  geom_errorbarh(data= m.coefs, aes(xmin = X.Slope_lower, xmax = X.Slope_upper,color=group), width = 0, size = 0.45,alpha=0.3) +
  geom_segment(data=. %>% filter(group == "Losses"),aes(x=0,
                                                        xend=X.Slope,
                                                        y=0,
                                                        yend=Y.Slope,
                                                        colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data=. %>% filter(group == "Gains"),aes(x= 0,
                                                       xend=X.Slope,
                                                       y=0,
                                                       yend=Y.Slope,
                                                       colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data=. %>% filter(group == "Persistent Sp."),aes(x= 0,
                                                                xend=X.Slope,
                                                                y=0,
                                                                yend=Y.Slope,
                                                                colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_line(data = sloss.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  
  scale_color_manual(values=c("#3B9AB2","#B40F20","#35274A"))+
  labs(x = 'Species Change Slope',
       y = 'Biomass Change Slope',
       title= 'Vector 4.0') +
  ylim(-50,50)+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

vec.slope.fig




