

# Fig 2

library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)



load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')



load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')

load('~/Dropbox/Projects/NutNet/Data/study.p.effs.Rdata')

rich.p2 <-rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) %>%
  filter(response=="NPK")

bm.p2<-bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) %>%
  filter(response=="NPK")

effs.p <- rich.p2 %>% left_join(bm.p2)

effs.p

study.rich.p2 <-study.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) 

study.bm.p2<-study.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) 

study.effs.p <- study.rich.p2 %>% left_join(study.bm.p2)

View(study.effs.p)

#"#0B775E"


bef.cloud <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_point(data=study.effs.p, aes(x= r.eff , y= b.eff),colour="black", alpha=0.2,size=2) +
  geom_errorbar(data=study.effs.p,aes(x= r.eff, y= b.eff,ymin = b.eff_lower, ymax = b.eff_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(data=study.effs.p,aes(x= r.eff, y= b.eff,xmin =  r.eff_lower, xmax =r.eff_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_point(data = effs.p, aes(x= r.eff, #loss
                                y=  b.eff ),
             fill="#0B775E",color="#0B775E",size=8, alpha=0.6)+
  geom_errorbar(data = effs.p,aes(x=r.eff,
                                  ymin = b.eff_lower, ymax = b.eff_upper),width=0,colour = "#0B775E", size = 2,alpha=0.9) +
  geom_errorbarh(data = effs.p,aes(y=b.eff,
                                   xmin = r.eff_lower, xmax = r.eff_upper),height=0,colour = "#0B775E", size = 2, alpha=0.9) +
scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  labs(x = 'Rate of change in species richness (species/year)',
       y = expression(paste('Rate of change in plot biomass (g/' ,m^2, '/year)')),
       # title= 'Control Slope + NPK Slope'
       title = ' C)')

bef.cloud




# RICHNESS AND BIOMASS SUPPLEMENTARY FIGURE S4


plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()



plot.rich_fitted.npk

plot.rich_fitted.npk$Model<-"Species Richness"
plot.rich_fitted.ctl$Model<-"Species Richness"
plot.rich_fitted.npk <- plot.rich_fitted.npk %>% rename(Treatment = trt) 
plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% rename(Treatment = trt) 
fitted.rich<-bind_rows(plot.rich_fitted.npk,plot.rich_fitted.ctl)

fitted.rich$Treatment <- factor(fitted.rich$Treatment , levels=c("NPK","Control"))

plot.rich_coef3 <- plot.rich_coef3 %>% filter(!is.na(TESlope))


View(plot.rich_fitted.npk)


View(plot.rich_coef3)

# orange code "#F98400"
r1<-ggplot() +
  facet_wrap(~Model) +
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = all.div), colour ="black", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45 )) +
  geom_segment(data = plot.rich_coef3 ,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code),
               color="black", alpha=0.2,size = .7) +
 # uncertainy in fixed effect
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              fill="#0B775E",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.rich,
            aes(x = year_trt, y = Estimate, linetype= Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(x='Year',
       #x = 'Years',
       y = ' Species richness', title= 'A)') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#0B775E", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.title = element_text(size=12),
                     plot.margin= margin(t = 0.1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))

r1


plot.rich_fitted.npk$Plot<-"Plot"
plot.rich_coef3$Site<-"Site"

r.leg<-ggplot() +
  facet_wrap(~Model) +
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = all.div, fill=Plot), alpha=0.2,
             size = .7, position = position_jitter(width = 0.45 )) +
  geom_segment(data = plot.rich_coef3 ,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code, color=Site),
                alpha=0.2,size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              fill="#F98400",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.rich,
            aes(x = year_trt, y = Estimate, linetype= Treatment),
            size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(x='',
       #x = 'Years',
       y = ' Species richness', title= '', color='',fill='',linetype='') +
  scale_fill_manual(values = c("black", drop =FALSE))+
  scale_color_manual(values = c("black",drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12),
                     plot.margin= margin(t = 0.1, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),legend.spacing.x = unit(0.25, 'cm'))

r.leg



yr<-plot.rich_coef3 %>% select(site_code,xmax)

plot.rich_fitted.npk <- plot.rich_fitted.npk %>% left_join(yr)


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


plot.bm_fitted.npk$Model<-"Biomass"
plot.bm_fitted.ctl$Model<-"Biomass"
plot.bm_fitted.npk <- plot.bm_fitted.npk %>% rename(Treatment = trt) 
plot.bm_fitted.ctl <- plot.bm_fitted.ctl %>% rename(Treatment = trt) 
fitted.bm<-bind_rows(plot.bm_fitted.npk,plot.bm_fitted.ctl)

fitted.bm


fitted.bm$Treatment <- factor(fitted.bm$Treatment , levels=c("NPK","Control"))

plot.bm_coef3 <- plot.bm_coef3 %>% filter(!is.na(TESlope))


b1<-ggplot() +
  facet_grid(~Model)+
  geom_point(data = plot.bm_fitted.npk,
             aes(x = year_trt, y = plot.mass), color="black",alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = plot.bm_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code),
               color="black",alpha=0.2,size = 0.7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.bm_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              fill="#0B775E",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.bm,
            aes(x = year_trt, y = Estimate, linetype=Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = plot.bm_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  labs(x='Year',
       #x = 'Years',
       y = expression(paste('Biomass (g/',m^2, ')')), title= 'B)') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#0B775E", drop =FALSE))+
  ylim(0,2000)+
  #scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500)) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.title = element_text(size=12),
                     plot.margin= margin(t = 0.1, r = 0.2, b =0.2, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


b1

(r1 + b1)


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
  ylim(0,1900)+
  # xlim(0,11) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))


bmatrix

load('~/Dropbox/Projects/NutNet/Data/effs.Rdata')

load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')



View(rich.p)

rich.eff<-ggplot() + 
  geom_point(data =rich.p, aes(x = response, y = eff, color=response),size = 2) +
  geom_errorbar(data = rich.p, aes(x = response, ymin = eff_lower,
                                   ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-0.5)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.text.x = element_text(size=6),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")


rich.eff


bm.eff<-ggplot() + 
  geom_point(data =bm.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = bm.p, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  #facet_wrap(~Model)+
  labs(x = '',
       #y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') ')),
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.text.x = element_text(size=6),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")
bm.eff



#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


rlegend<-g_legend(r.leg)


rich <- r1 +  annotation_custom(ggplotGrob(rich.eff), xmin = 7, xmax = 12, 
                                ymin = 28, ymax = 40)

bm <- b1+  annotation_custom(ggplotGrob(bm.eff), xmin = 7, xmax = 12, 
                             ymin = 1400 ,ymax = 2000)



# PORTRAIT 11 X 10
( rich | bm ) / ( rlegend ) / ( bef.cloud ) + plot_layout(heights = c(10,0.75,10))



