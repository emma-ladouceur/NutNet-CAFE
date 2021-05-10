
# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 9_Figure 3.R
# This workflow uses data pulled out of models from previous steps and plots Figure 3

# packages
library(tidyverse)
library(brms)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)

# data
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.all <- p.all %>% group_by(site_code) %>% #filter(max.year >= 3) 
  filter(year_max >= 3) 

p.all$site_code <- as.factor(p.all$site_code)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
p.all$year.y<-as.numeric(p.all$year.y)


# model object data
# note to self change sgain model object data to match others
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/sgain.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/sloss.n.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/sl.n.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/sg.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/cde.mod.dat.Rdata')
#load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/ps.mod.dat.Rdata') # ps.3_sigma


# saved posterior data from 7_ Model_Data_Posteriors
# Global/ Overall/ Population Effects for inset plots
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/global.p.effs.Rdata')

# colors used
# loss "#B40F20"
# gains "#046C9A"
# cde "#816687"

# I thik this will now be removed
#  Figure 3 a)
# Model <- c('A) Conceptual figure')
# conceptdat<- data.frame(Model)
# conceptdat  
# 
# fig_3a <- ggplot(data=conceptdat)+
#   facet_grid(~Model)+
#   geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
#   theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                         strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
#   geom_segment(
#                aes(x = -0.5+0.05,
#                    xend = -0.5+0.05,
#                    y = -8+10,
#                    yend = -8+10+4 ), 
#                size = 1.5, color= "#F98400",
#                arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
#   geom_segment(
#                aes(x = 0,
#                    xend = -0.5,
#                    y = 0,
#                    yend = -8,),
#                size = 1.5, color="#B40F20" ,
#                arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
#   geom_segment(
#                aes(x = -0.5,
#                    xend =  -0.5+0.05,
#                    y = -8,
#                    yend =  -8+10,),
#                size = 1.5, color= "#046C9A",
#                arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
#   geom_segment(
#                aes(x = 0,
#                    xend = -0.5+0.07,
#                    y = 0,
#                    yend = 0 ), 
#                size = 1.5, color= "#0B775E",
#                arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
#   geom_segment(
#     aes(x = -0.5+0.07,
#         xend = -0.5+0.07,
#         y = 0,
#         yend = -8+10+4  ), 
#     size = 1.5, color= "#0B775E",
#     arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
#   scale_y_continuous(breaks=c(0)) +
#   scale_x_continuous(breaks=c(0)) +
#   labs(x = 'Change in Species',
#        y = 'Change in  Biomass') + theme_bw(base_size=14) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
#                                                      plot.margin= margin(t = 0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
#                                                      strip.text = element_text(size=17))
# 
# fig_3a


#  Fig 3b) species loss regression (s.loss)

sloss.trt_coef2$xs<-1

sloss.trt_fitted.npk$Model<-"A) Species loss (s.loss)"
sloss.trt_fitted.ctl$Model<-"A) Species loss (s.loss)"
sloss.trt_fitted.npk <- sloss.trt_fitted.npk  %>% left_join(p.all) %>% rename(Treatment = trt.y) 
sloss.trt_fitted.ctl <- sloss.trt_fitted.ctl %>% left_join(p.all) %>% rename(Treatment = trt.y) 
fitted.sloss <- bind_rows(sloss.trt_fitted.npk,sloss.trt_fitted.ctl)


fitted.sloss$Treatment <- factor(fitted.sloss$Treatment , levels=c("NPK","Control"))



fig_3a_r <- ggplot() +
  facet_grid(~Model)+
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sloss.trt_fitted.npk,
             aes(x = year.y, y = s.loss.n),color="black", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sloss.trt_coef2,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax)
               ),
               color="black",alpha=0.2,size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sloss.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#B40F20",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.sloss,
            aes(x = year.y, y = Estimate,linetype=Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = sloss.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  labs(x = 'Year',
       y = expression(paste('Species Loss')), 
       #title= 'B) Species loss (s.loss)'
       title='') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#B40F20", drop =FALSE))+
  theme_bw(base_size=16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                  strip.background = element_blank(),legend.position="none",
                                  plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                                  strip.text = element_text(size=19),
  )


fig_3a_r


# fig 3c species gain regression (s.gain)

sgain.trt_coef2$xs<-1

sgain.trt_fitted.npk$Model<-"B) Species gain (s.gain)"
sgain.trt_fitted.ctl$Model<-"B) Species gain (s.gain)"
sgain.trt_fitted.npk <- sgain.trt_fitted.npk %>% left_join(p.all) %>% rename(Treatment = trt.y) 
sgain.trt_fitted.ctl <- sgain.trt_fitted.ctl %>% left_join(p.all) %>% rename(Treatment = trt.y) 
fitted.sgain<-bind_rows(sgain.trt_fitted.npk,sgain.trt_fitted.ctl)
fitted.sgain$Treatment <- factor(fitted.sgain$Treatment , levels=c("NPK","Control"))

fig_3b_r <- ggplot()  +
  geom_hline(yintercept = 0,linetype="longdash") +
  facet_grid(~Model)+
  geom_point(data = sgain.trt_fitted.npk,
             aes(x = year.y, y = s.gain), colour = "black", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sgain.trt_coef2,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax)),
               colour = "black", alpha=0.2,size = .7) +
  geom_ribbon(data = sgain.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#046C9A",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.sgain,
            aes(x = year.y, y = Estimate, linetype=Treatment, color=Treatment),
            size = 1.5) +
  geom_ribbon(data = sgain.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(0,20) +
  labs(x = 'Year',
       y = expression(paste('Species Gain')), 
  title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#046C9A",
                                 drop =FALSE))+
  theme_bw(base_size=16 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     strip.text = element_text(size=19)
                     )


fig_3b_r


# persistent species regression
#  ps.trt_coef2$xs<-1
# 
# ps.trt_fitted.npk$Model<-"C) Persistent species (p.s)"
# ps.trt_fitted.ctl$Model<-"C) Persistent species (p.s)"
# ps.trt_fitted.npk <- ps.trt_fitted.npk %>% rename(Treatment = trt.y) 
# ps.trt_fitted.ctl <- ps.trt_fitted.ctl %>% rename(Treatment = trt.y) 
# fitted.ps <- bind_rows(ps.trt_fitted.npk,ps.trt_fitted.ctl)
# fitted.ps$Treatment <- factor(fitted.ps$Treatment , levels=c("NPK","Control"))
# 
# 
# fig_3c_r <- ggplot() +
#   facet_grid(~Model)+
#   geom_hline(yintercept = 0,linetype="longdash") +
#   geom_point(data = ps.trt_fitted.npk,
#              aes(x = year.y, y = c.rich),color="black", alpha=0.2,
#              size = .7, position = position_jitter(width = 0.45)) +
#   geom_segment(data = ps.trt_coef2,
#                aes(x = xs, 
#                    xend = xmax,
#                    y = ( (Intercept +  TE)   + (ISlope + TESlope) * cxmin),
#                    yend = ( (Intercept +  TE)  + (ISlope + TESlope) * cxmax)
#                ),
#                color="black",alpha=0.2,size = .7) +
#   # uncertainy in fixed effect
#   geom_ribbon(data = ps.trt_fitted.npk,
#               aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
#               fill="#F98400",alpha = 0.5) +
#   # fixed effect
#   geom_line(data = fitted.ps,
#             aes(x = year.y, y = Estimate, linetype=Treatment,color=Treatment),
#             size = 1.5) +
#   geom_ribbon(data = ps.trt_fitted.ctl,
#               aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
#               fill="black",alpha = 0.5) +
#   scale_x_continuous(breaks=c(1,3,6,9,12)) +
#   labs(x = 'Year',
#        y = expression(paste('Persistent species')), 
#        title= '') +
#   scale_colour_manual(values = c("Control" = "black",
#                                  "NPK" = "#F98400", drop =FALSE))+
#   theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                                  strip.background = element_blank(),legend.position="none",
#                                  plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
#                                  strip.text = element_text(size=17),
#   )
# 
# 
# fig_3c_r

# BIOMASS PARTITIONS

# Figure 3 d) -CDE or biomass change associated with persistant species (CDE)

cde_coef2$xs<-1

cde_fitted.npk <- cde_fitted.npk  %>% left_join(p.all) %>% rename(Treatment = trt.y) 
cde_fitted.ctl <- cde_fitted.ctl %>% left_join(p.all) %>% rename(Treatment = trt.y) 
fitted.cde<-bind_rows(cde_fitted.npk,cde_fitted.ctl)
fitted.cde$Treatment <- factor(fitted.cde$Treatment , levels=c("NPK","Control"))

fig_3e_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = cde_fitted.npk,
             aes(x = year.y, y = CDE),color="black", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = cde_coef2,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope)  * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax)),
               color="black",alpha=0.2,size = .7) +
  geom_ribbon(data = cde_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#F98400",alpha = 0.5) +
  geom_line(data = fitted.cde,
            aes(x = year.y, y = Estimate,linetype=Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = cde_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(-500,1000)+
  labs(x='Year',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'E) Biomass change associated \n with persistent species (PS)') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#F98400",drop =FALSE))+
  theme_bw(base_size=16 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                                  legend.position="none", plot.margin= margin(t = 0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                                  strip.text = element_text(size=14)
  )

fig_3e_r

# Figure 3e Biomass changes associated with species loss (SL)

sl.trt_coef2$xs<-1

sl.trt_fitted.npk <- sl.trt_fitted.npk %>% left_join(p.all) %>% rename(Treatment = trt.y) 
sl.trt_fitted.ctl <- sl.trt_fitted.ctl %>% left_join(p.all) %>% rename(Treatment = trt.y) 
fitted.sl<-bind_rows(sl.trt_fitted.npk,sl.trt_fitted.ctl)
fitted.sl$Treatment <- factor(fitted.sl$Treatment , levels=c("NPK","Control"))

fig_3c_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sl.trt_fitted.npk,
             aes(x = year.y, y = SL),color="black",alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sl.trt_coef2,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax)
               ),
               color="black", alpha=0.2,size = .7) +
  # uncertainty in fixed effect
  geom_ribbon(data = sl.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#B40F20",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.sl,
            aes(x = year.y, y = Estimate,linetype=Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = sl.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(-400,5) +
  labs(x='Year',
       #x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')),
       title= 'C) Biomass change associated \n with species loss (SL)') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#B40F20", drop =FALSE))+
  theme_bw(base_size=16 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                  strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = 0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     strip.text = element_text(size=14)
                     )


fig_3c_r


# Figure 3f Biomass changes associated with species gain (SG)

sg.trt_coef2$xs<-1

sg.trt_fitted.npk <- sg.trt_fitted.npk %>% left_join(p.all) %>% rename(Treatment = trt.y) 
sg.trt_fitted.ctl <- sg.trt_fitted.ctl %>% left_join(p.all) %>% rename(Treatment = trt.y) 
fitted.sg<-bind_rows(sg.trt_fitted.npk,sg.trt_fitted.ctl)

fitted.sg$Treatment <- factor(fitted.sg$Treatment , levels=c("NPK","Control"))

fig_3d_r <- ggplot()  +
  geom_hline(yintercept = 0,linetype="longdash") +
  # data
  geom_point(data = sg.trt_fitted.npk,
             aes(x = year.y, y = SG), color="black",alpha =0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sg.trt_coef2,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax)),
               color="black",alpha=0.2,size = .7) +
  geom_ribbon(data = sg.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#046C9A",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.sg,
            aes(x = year.y, y = Estimate,linetype=Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = sg.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(0,400) +
  labs(x = 'Year',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'D) Biomass change associated \n with species gain (SG)',
       color='Treatment') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#046C9A", drop =FALSE))+
  theme_bw(base_size=16 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none", plot.margin= margin(t = 0.5, r = 0, b = 0.5, l = 0, unit = "cm"),
                     strip.text = element_text(size=14)
                     )

fig_3d_r


# Figure 3 Legend

sg.trt_fitted.npk$Plot <- "Temporal comparison cetween plot t0 and plot tn"
sg.trt_coef2$Site <- "Site"

fig_3_legend <- ggplot()  +
  geom_point(data = sg.trt_fitted.npk,
             aes(x = year.y, y = SG, fill="Temporal comparison between \n plot t0 and plot tn"), alpha =0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sg.trt_coef2,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax), color=Site),
               alpha=0.2,size = .7) +
  geom_ribbon(data = sg.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#046C9A",alpha = 0.5) +
  geom_line(data = fitted.sg,
            aes(x = year.y, y = Estimate, linetype=Treatment),
            size = 1.5) +
  geom_ribbon(data = sg.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(0,400) +
  labs(x = 'Years',
       y='',
       title= '', color='',fill='',linetype='') +
  scale_fill_manual(values = c("black", drop =FALSE))+
   scale_color_manual(values = c("black",drop =FALSE))+
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="left", plot.margin= margin(t = -0.5, r = 0, b = 0.5, l = 0, unit = "cm"),
                     legend.spacing.y = unit(0.02, 'cm')
                     )


fig_3_legend

 # extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

fig_3_legend <- g_legend(fig_3_legend)


# produce inset effect plots in upper corners of Fig 3 b) - f)

# again using posterior data from '7_Model_Data_Posteriors.R' (loaded at beginning)
# Global/ Overall/ Population Effects
 load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/global.p.effs.Rdata')

fig_3a_e <- ggplot() + 
  geom_point(data = global.sloss.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.sloss.p, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(-0.5,-0.2,0)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                              strip.background = element_blank(),legend.position="none")

fig_3a_e


fig_3b_e <- ggplot() + 
  geom_point(data = global.sgain.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.sgain.p, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,0.1, 0.2)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                              strip.background = element_blank(),legend.position="none")

fig_3b_e

# fig_3c_e <- ggplot() + 
#   geom_point(data = global.ps.p, aes(x = response, y = eff,color=response),size = 2) +
#   geom_errorbar(data = global.ps.p, aes(x = response,ymin = eff_lower,
#                                            ymax = eff_upper,color=response),
#                 width = 0, size = 0.7) +
#   labs(x = '',
#        y='Slope') +
#   geom_hline(yintercept = 0, lty = 2) +
#   scale_y_continuous(breaks=c(0,-0.2,-0.5)) +
#   scale_color_manual(values = c("#000000","#F98400")) +
#   theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                                plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
#                                strip.background = element_blank(),legend.position="none")
# 
# fig_3c_e




fig_3c_e <-ggplot() + 
  geom_point(data = global.sl.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.sl.p, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                              strip.background = element_blank(),legend.position="none")

fig_3c_e

fig_3d_e <- ggplot() + 
  geom_point(data = global.sg.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.sg.p, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y= 'Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,5,10)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                              strip.background = element_blank(),legend.position="none")

fig_3d_e

fig_3e_e <- ggplot() + 
  geom_point(data = global.cde.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.cde.p, aes(x = response,ymin = eff_lower,
                                         ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(-8,0,4,14)) +
  scale_color_manual(values = c("#000000","#F98400")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_3e_e


# Inset effect plots within regression plots

fig_3a <- fig_3a_r +  annotation_custom(ggplotGrob(fig_3a_e), xmin = 6.5, xmax = 12.75, 
                                         ymin = -20, ymax = -11)

fig_3b <- fig_3b_r +  annotation_custom(ggplotGrob(fig_3b_e), xmin = 6.5, xmax = 12.75, 
                                         ymin = 11, ymax = 20)


# fig_3c <- fig_3c_r +  annotation_custom(ggplotGrob(fig_3c_e), xmin = 6.5, xmax = 12.75, 
#                                         ymin = 21, ymax = 30)

fig_3c <- fig_3c_r +  annotation_custom(ggplotGrob(fig_3c_e), xmin = 6.5, xmax = 12.75, 
                                   ymin = -400, ymax = -255)

fig_3d <- fig_3d_r +  annotation_custom(ggplotGrob(fig_3d_e), xmin = 6.5, xmax = 12.75, 
                                   ymin = 255, ymax = 400)

fig_3e <- fig_3e_r +  annotation_custom(ggplotGrob(fig_3e_e), xmin = 6.5, xmax = 12.75, 
                                        ymin = 400, ymax = 1050)


# put everything together with grid arrange and grob
# Save As LANDSCAPE 11 X 14

fig_3 <- grid.arrange(arrangeGrob(fig_3a, fig_3b, fig_3_legend, 
                                  fig_3c , fig_3d , fig_3e, ncol = 3, nrow=2))


fig_3


