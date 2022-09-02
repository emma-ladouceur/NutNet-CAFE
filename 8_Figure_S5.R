

# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 8_ Figure 2
# This workflow uses data pulled out of models from previous steps and plots Figure 2

# packages
library(tidyverse)
library(brms)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)

# data
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot$site_code <- as.factor(plot$site_code)
plot$block <- as.factor(plot$block)
plot$plot <- as.factor(plot$plot)

colnames(plot)
plot <- plot %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()


View(plot %>% distinct(site_code, year_max))

# saved model data objects  from '5_Model_Extract.R'
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/rich.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/bm.mod.dat.Rdata')

# saved posterior data from 7_ Model_Data_Posteriors
# Global/ Overall/ Population Effects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/global.p.effs.Rdata')
# Study-level effects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/study.p.effs.Rdata')


rich.p2 <- global.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) %>%
  filter(response=="NPK")

bm.p2 <- global.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) %>%
  filter(response=="NPK")

global.effs.p <- rich.p2 %>% left_join(bm.p2)

global.effs.p

rich.p2.ctl <- global.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) %>%
  filter(response=="Control")

bm.p2.ctl <- global.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) %>%
  filter(response=="Control")

global.effs.p.ctl <- rich.p2.ctl %>% left_join(bm.p2.ctl)

global.effs.p.ctl


study.rich.p2 <- study.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) 

study.bm.p2 <- study.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) 

study.effs.p <- study.rich.p2 %>% left_join(study.bm.p2) %>% filter(response == "NPK")

study.effs.p

# Quadrant Plot Figure 2 c)
fig_2c <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # study-level effects
  geom_point(data = study.effs.p, aes(x = r.eff , y = b.eff),colour="#0B775E", alpha=0.2,size=2) +
  geom_errorbar(data = study.effs.p,aes(x = r.eff, y = b.eff,ymin = b.eff_lower, ymax = b.eff_upper), colour="#0B775E", alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(data = study.effs.p,aes(x = r.eff, y = b.eff,xmin =  r.eff_lower, xmax = r.eff_upper), colour="#0B775E", alpha=0.2, width = 0, size = 0.75) +
  # overall effects
  geom_point(data = global.effs.p, aes(x= r.eff,
                                y=  b.eff ),
             fill="#0B775E",color="#0B775E",size=8, alpha=0.5)+
  geom_errorbar(data = global.effs.p,aes(x=r.eff,
                                  ymin = b.eff_lower, ymax = b.eff_upper),width=0,colour = "#0B775E", size = 2,alpha=0.9) +
  geom_errorbarh(data = global.effs.p,aes(y=b.eff,
                                   xmin = r.eff_lower, xmax = r.eff_upper),height=0,colour = "#0B775E", size = 2, alpha=0.9) +
  geom_point(data = global.effs.p.ctl, aes(x= r.eff,
                                       y=  b.eff ),
             fill="black",color="black",size=8, alpha=0.5)+
  geom_errorbar(data = global.effs.p.ctl, aes(x=r.eff,
                                         ymin = b.eff_lower, ymax = b.eff_upper),width=0,colour = "black", size = 2,alpha=0.7) +
  geom_errorbarh(data = global.effs.p.ctl, aes(y=b.eff,
                                          xmin = r.eff_lower, xmax = r.eff_upper),height=0,colour = "black", size = 2, alpha=0.7) +
  scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  annotate("text", x = -2.5, y = 200, label = "+biomass -rich", size=5) +
  annotate("text", x = 1.5, y = 200, label = "+biomass +rich", size=5) +
  annotate("text", x = -2.5, y = -200, label = "-biomass -rich", size=5) +
  annotate("text", x = 1.5, y = -200, label = "-biomass +rich", size=5) +
  labs(x = 'Rate of change in species richness (species/year)',
       y = expression(paste('Rate of change in plot biomass (g/' ,m^2, '/year)')),
       title = ' c)') + theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

fig_2c


# Quadrant plot Figure 2 c) Legend

head(study.effs.p)
study.effs.p$Site <- "Site-Level Effects: NPK"
global.effs.p$Overall<-"Overall Effects: NPK"
global.effs.p.ctl$Overall<-"Overall Effects: Control"

study_2c_legend <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  geom_point(data=study.effs.p, aes(x= r.eff , y= b.eff, color=Site), alpha=0.2,size=2) +
  geom_errorbar(data=study.effs.p,aes(x= r.eff, y= b.eff,ymin = b.eff_lower, ymax = b.eff_upper, color=Site),  alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(data=study.effs.p,aes(x= r.eff, y= b.eff,xmin =  r.eff_lower, xmax =r.eff_upper, color=Site), alpha=0.2, width = 0, size = 0.75) +
  scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  labs(x = 'Rate of change in species richness (species/year)',
       y = expression(paste('Rate of change in plot biomass (g/' ,m^2, '/year)')),
       title = ' C)',  color='',fill='',linetype='') + 
  scale_color_manual(values = c("#0B775E"))+
  theme_classic(base_size=18 ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

study_2c_legend

global.effs.p.all <- global.effs.p %>% bind_rows(global.effs.p.ctl)
head(global.effs.p.all)

overall_2c_legend <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  geom_point(data = global.effs.p.all, aes(x= r.eff, 
                                y=  b.eff ,color=Overall),
             size=8, alpha=0.6)+
  geom_errorbar(data = global.effs.p,aes(x=r.eff,
                                  ymin = b.eff_lower, ymax = b.eff_upper,color=Overall),width=0, size = 2,alpha=0.9) +
  geom_errorbarh(data = global.effs.p.all,aes(y=b.eff,
                                   xmin = r.eff_lower, xmax = r.eff_upper,color=Overall),height=0, size = 2, alpha=0.9) +
  scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  labs(x = 'Rate of change in species richness (species/year)',
       y = expression(paste('Rate of change in plot biomass (g/' ,m^2, '/year)')),
       title = ' C)',  color='',fill='',linetype='') + 
  scale_color_manual(values = c("black", "#0B775E"))+
  theme_classic(base_size=18 ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

overall_2c_legend

# extract legend
# Sourced from: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# extract legends
sc.leg <- g_legend(study_2c_legend)
oc.leg <- g_legend(overall_2c_legend)
# arrange together using grid arrange (patchwork does not work for this)
fig_2c_legend <- grid.arrange(oc.leg, sc.leg, ncol=2,nrow=1)



# Richness & Biomass Regressions
plot.rich_fitted.npk$Model <- "a) Species richness"
plot.rich_fitted.ctl$Model <- "a) Species richness"
plot.rich_fitted.npk <- plot.rich_fitted.npk %>% rename(Treatment = trt) 
plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% rename(Treatment = trt) 
fitted.rich <- bind_rows(plot.rich_fitted.npk,plot.rich_fitted.ctl)


fitted.rich$Treatment <- factor(fitted.rich$Treatment , levels=c("NPK","Control"))
plot.rich_coef2 <- plot.rich_coef2 %>% filter(!is.na(TESlope))


fig_2a_r <- ggplot() +
  facet_wrap(~Model) +
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = rich), colour ="#0B775E", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45) ) +
  geom_segment(data = plot.rich_coef2 ,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code),
               color="#0B775E", alpha=0.2,size = .7) +
 # uncertainty in fixed effect
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              fill="#0B775E",alpha = 0.5) +
  # fixed effects
  geom_line(data = fitted.rich,
            aes(x = year_trt, y = Estimate,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12,13)) +
  labs(x='Year',
       y = ' Species richness', title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#0B775E", drop =FALSE))+
  theme_bw(base_size=16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 strip.background = element_blank(),legend.position="none",
                                 strip.text = element_text(size=17),
                     plot.margin= margin(t = 0.1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                    )

fig_2a_r

# legend for richness & biomass regressions
plot.rich_fitted.npk$Plot <- "Plot: NPK"
plot.rich_coef2$Site <- "Site: NPK"

fig_2ab_legend_o <- ggplot() +
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              fill= "#0B775E",alpha = 0.5) +
  geom_line(data = fitted.rich,
            aes(x = year_trt, y = Estimate, color= Treatment),
            size = 1.5) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(x='',
       y = ' Species richness', title= '', color='',fill='') +
  scale_fill_manual(values = c(  "#0B775E" ))+
  scale_color_manual(values = c( "#0B775E" , "black"))+
  scale_linetype_manual("",values=c("Site" = 1))+
  theme_bw(base_size=16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 strip.background = element_blank(),
                                 legend.position="bottom",
                     plot.margin= margin(t = 0.1, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     legend.spacing.x = unit(0.25, 'cm'))

fig_2ab_legend_o

fig_2ab_legend_s <- ggplot() +
  #facet_wrap(~Model) +
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = rich, fill=Plot), alpha=0.2, col =  "#0B775E",
             size = .7, position = position_jitter(width = 0.45 )) +
  geom_segment(data = plot.rich_coef2 ,
               aes(x = xmin,
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code, linetype= Site ,color = Site ), col =  "#0B775E",
                alpha = 0.2, size = .7) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(x='',
       y = ' Species richness', title= '', color='',fill='') +
  scale_fill_manual(values = c(  "#0B775E" ))+
  scale_color_manual(values = c( "#0B775E" ))+
  scale_linetype_manual("",values=c("Site: NPK" = 1))+
  theme_bw(base_size=16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 strip.background = element_blank(),
                                 legend.position="bottom",
                                 plot.margin= margin(t = 0.1, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                                 legend.spacing.x = unit(0.25, 'cm'))

fig_2ab_legend_s



# Biomass Regression
plot.bm_fitted.npk$Model<-"b) Biomass"
plot.bm_fitted.ctl$Model<-"b) Biomass"
plot.bm_fitted.npk <- plot.bm_fitted.npk %>% rename(Treatment = trt) 
plot.bm_fitted.ctl <- plot.bm_fitted.ctl %>% rename(Treatment = trt) 
fitted.bm<-bind_rows(plot.bm_fitted.npk,plot.bm_fitted.ctl)

fitted.bm$Treatment <- factor(fitted.bm$Treatment , levels=c("NPK","Control"))

plot.bm_coef2 <- plot.bm_coef2 %>% filter(!is.na(TESlope))

# note to self:  predicted values instead of coefficients?
fig_2b_r <- ggplot() +
 # facet_wrap(~site_code) +
  geom_hline(yintercept = 0,linetype="longdash") +
  facet_grid(~Model)+
  geom_point(data = plot.bm_fitted.npk,
             aes(x = year_trt, y = strip.mass), color="#0B775E",alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = plot.bm_coef2,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code),
               color="#0B775E",alpha=0.2,size = 0.7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.bm_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              fill="#0B775E",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.bm,
            aes(x = year_trt, y = Estimate, color=Treatment),
            size = 1.5) +
  geom_ribbon(data = plot.bm_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  labs(x='Year',
       y = expression(paste('Biomass (g/',m^2, ')')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#0B775E", drop =FALSE))+
  ylim(0,2000)+
  scale_x_continuous(breaks=c(0,1,3,6,9,12,13)) +
  theme_bw(base_size=16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                 strip.background = element_blank(),legend.position="none",
                                 strip.text = element_text(size=17),
                     plot.margin= margin(t = 0.1, r = 0.2, b =0.2, l = 0.2, unit = "cm"),
                    )


fig_2b_r

(fig_2a_r + fig_2b_r)

# not needed?
#load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/effs.Rdata')

# produce inset effect plots in upper corners of Fig 2 a) & b)

# again using posterior data from '7_Model_Data_Posteriors.R' (loaded at beginning)
# load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/global.p.effs.Rdata')

fig_2a_e <- ggplot() + 
  geom_point(data = global.rich.p, aes(x = response, y = eff, color=response),size = 2) +
  geom_errorbar(data = global.rich.p, aes(x = response, ymin = eff_lower,
                                   ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope')+
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-0.5)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_2a_e


fig_2b_e <- ggplot() + 
  geom_point(data = global.bm.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.bm.p, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                              strip.background = element_blank(),legend.position="none")
fig_2b_e


# Put the Figures together to make up Figure 2 a), b) , c) + legends

# extract legend
# Sourced from: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

fig_2ab_leg_o <- g_legend(fig_2ab_legend_o)

fig_2ab_leg_s <- g_legend(fig_2ab_legend_s)

o_2c_leg <- g_legend(overall_2c_legend)

s_2c_leg <- g_legend(study_2c_legend)

# use grid extra to add inset effect plots to regression
fig_2a <- fig_2a_r +  annotation_custom(ggplotGrob(fig_2a_e), xmin = 7, xmax = 13.5, 
                                ymin = 20, ymax = 42)

fig_2b <- fig_2b_r +  annotation_custom(ggplotGrob(fig_2b_e), xmin = 7, xmax = 13.5, 
                             ymin = 1100 ,ymax = 2075)


# SAVE AS PORTRAIT 10 X 12
fig_2ab <- ( fig_2a | fig_2b ) / ( fig_2ab_leg_o) / (fig_2ab_leg_s) + plot_layout(heights = c(10, 0.75, 0.75))

fig_2ab

fig_2cc <- ( fig_2c ) / (s_2c_leg) / (o_2c_leg) + plot_layout(heights = c(13,0.75,0.75))

fig_2cc

fig_2 <- (fig_2ab) / (fig_2cc) + plot_layout(heights = c(10,0.75, 0.75,13, 0.75,0.75)) 

fig_2


