


library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")




# CONCEPTUAL

Model<-c('A) Conceptual Figure')
conceptdat<- data.frame(Model)
conceptdat  

conceptual<-ggplot(data=conceptdat)+
  facet_grid(~Model)+
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(
               aes(x = -0.5+0.05,
                   xend = -0.5+0.05,
                   y = -8+10,
                   yend = -8+10+4 ), 
               size = 1.5, color= "#F98400",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(
               aes(x = 0,
                   xend = -0.5,
                   y = 0,
                   yend = -8,),
               size = 1.5, color="#B40F20" ,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(
               aes(x = -0.5,
                   xend =  -0.5+0.05,
                   y = -8,
                   yend =  -8+10,),
               size = 1.5, color= "#3B9AB2",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(
               aes(x = 0,
                   xend = -0.5+0.07,
                   y = 0,
                   yend = 0 ), 
               size = 1.5, color= "#0B775E",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(
    aes(x = -0.5+0.07,
        xend = -0.5+0.07,
        y = 0,
        yend = -8+10+4  ), 
    size = 1.5, color= "#0B775E",
    arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_y_continuous(breaks=c(0)) +
  scale_x_continuous(breaks=c(0)) +
  labs(x = 'Change in Species',
       y = 'Change in  Biomass') + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                                                     plot.margin= margin(t = 0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                                                     axis.title.x = element_text(size=9),
                                                     axis.title.y = element_text(size=9),
                                                     axis.text=element_text(size=9))

conceptual



# PARTITIONS

sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



# GAINS N LOSSES


load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


is.factor(sgain.trt_fitted.npk$starting.richness)
is.factor(sgain.trt_coef3$starting.richness)

sgain.trt_coef3$xs<-1

sgain.trt_fitted.npk$Model<-"C) Species Gain"
sgain.trt_fitted.ctl$Model<-"C) Species Gain"

View(sgain.trt_fitted.npk)
sgain.trt_fitted.npk <- sgain.trt_fitted.npk %>% rename(Treatment = trt.y) 
sgain.trt_fitted.ctl <- sgain.trt_fitted.ctl %>% rename(Treatment = trt.y) 



fitted.sgain<-bind_rows(sgain.trt_fitted.npk,sgain.trt_fitted.ctl)

fitted.sgain

fitted.sgain$Treatment <- factor(fitted.sgain$Treatment , levels=c("NPK","Control"))



View(sgain.trt_coef3)
# loss "#B40F20"
# gains "#046C9A"
# cde "#816687"

head(sgain.trt_fitted.ctl)
View(sgain.trt_coef3)
sgain.trtm <- ggplot()  +
  # data
  facet_grid(~Model)+
  geom_point(data = sgain.trt_fitted.npk,
             aes(x = year.y, y = s.gain), colour = "black", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sgain.trt_coef3,
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
       y = expression(paste('Species Gain')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#046C9A",
                                 drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


sgain.trtm



# LOSS
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')

fitted.sloss


summary(sloss.s)


as.factor(as.character(sloss.trt_fitted.npk$starting.richness))
as.factor(as.character(sloss.trt_coef3$starting.richness))

sloss.trt_fitted.npk<-sloss.trt_fitted.npk[complete.cases(sloss.trt_fitted.npk$starting.richness), ]
sloss.trt_coef3<-sloss.trt_coef3[complete.cases(sloss.trt_coef3$starting.richness), ]

sloss.trt_fitted.npk$starting.richness <- factor(sloss.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sloss.trt_coef3$starting.richness <- factor(sloss.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


sloss.trt_coef3$xs<-1

sloss.trt_fitted.npk$Model<-"B) Species Loss"
sloss.trt_fitted.ctl$Model<-"B) Species Loss"
sloss.trt_fitted.npk <- sloss.trt_fitted.npk %>% rename(Treatment = trt.y) 
sloss.trt_fitted.ctl <- sloss.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sloss<-bind_rows(sloss.trt_fitted.npk,sloss.trt_fitted.ctl)

fitted.sloss

fitted.sloss$Treatment <- factor(fitted.sloss$Treatment , levels=c("NPK","Control"))


View(sloss.trt_coef3)

sloss.trtm<-ggplot() +
  facet_grid(~Model)+
  geom_point(data = sloss.trt_fitted.npk,
             aes(x = year.y, y = s.loss.n),color="black", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sloss.trt_coef3,
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
  #scale_y_reverse( lim=c(20,0))+
  #ylim(-20,0) +
  labs(x = 'Year',
       y = expression(paste('Species Loss')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#B40F20", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))



sloss.trtm




# BIOMASS PARTITIONS


load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/sg_dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')


as.factor(as.character(sl.trt_fitted.npk$starting.richness))
as.factor(as.character(sl.trt_coef3$starting.richness))

sl.trt_fitted.npk<-sl.trt_fitted.npk[complete.cases(sl.trt_fitted.npk$starting.richness), ]
sl.trt_coef3<-sl.trt_coef3[complete.cases(sl.trt_coef3$starting.richness), ]

sl.trt_fitted.npk$starting.richness <- factor(sl.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sl.trt_coef3$starting.richness <- factor(sl.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


sl.trt_coef3$xs<-1

sl.trt_fitted.npk$Model<-"D) Biomass Change Due To Species Loss"
sl.trt_fitted.ctl$Model<-"D) Biomass Change Due To Species Loss"
sl.trt_fitted.npk <- sl.trt_fitted.npk %>% rename(Treatment = trt.y) 
sl.trt_fitted.ctl <- sl.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sl<-bind_rows(sl.trt_fitted.npk,sl.trt_fitted.ctl)

fitted.sl

fitted.sl$Treatment <- factor(fitted.sl$Treatment , levels=c("NPK","Control"))



View(sl.trt_coef3)

sl.trtm<-ggplot() +
  facet_grid(~Model)+
  geom_point(data = sl.trt_fitted.npk,
             aes(x = year.y, y = SL),color="black",alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sl.trt_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax)
               ),
               color="black", alpha=0.2,size = .7) +
  # uncertainy in fixed effect
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
  ylim(-400,0) +
  labs(x='Year',
       #x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#B40F20", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


sl.trtm



# SG
is.factor(sg.trt_fitted.npk$starting.richness)
is.factor(sg.trt_coef3$starting.richness)

sg.trt_fitted.npk<-sg.trt_fitted.npk[complete.cases(sg.trt_fitted.npk$starting.richness), ]
sg.trt_coef3<-sg.trt_coef3[complete.cases(sg.trt_coef3$starting.richness), ]

sg.trt_fitted.npk$starting.richness <- factor(sg.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.trt_coef3$starting.richness <- factor(sg.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sg.trt_coef3$xs<-1

sg.trt_fitted.npk$Model<-"E) Biomass Change Due To Species Gain"
sg.trt_fitted.ctl$Model<-"E) Biomass Change Due To Species Gain"
sg.trt_fitted.npk <- sg.trt_fitted.npk %>% rename(Treatment = trt.y) 
sg.trt_fitted.ctl <- sg.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sg<-bind_rows(sg.trt_fitted.npk,sg.trt_fitted.ctl)

fitted.sg

fitted.sg$Treatment <- factor(fitted.sg$Treatment , levels=c("NPK","Control"))


View(sg.trt_coef3)
#gai
sg.trtm<-ggplot()  +
  # data
  facet_grid(~Model) +
  geom_point(data = sg.trt_fitted.npk,
             aes(x = year.y, y = SG), color="black",alpha =0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sg.trt_coef3,
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
  #scale_y_continuous(trans = 'log', breaks=c(1,2,4,6,8,16,24,64,512,1024,2048,4096)) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(0,400) +
  labs(x = 'Year',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= '', color='Treatment') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#046C9A", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none", plot.margin= margin(t = -0.5, r = 0, b = 0.5, l = 0, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))


sg.trtm

sg.trt_fitted.npk$Plot<-"Pairwise Plot"
sg.trt_coef3$Site<-"Site"

r.leg<-ggplot()  +
  # data
  facet_grid(~Model) +
  geom_point(data = sg.trt_fitted.npk,
             aes(x = year.y, y = SG, fill="Pairwise Plot"), alpha =0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = sg.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),color=Site),
               alpha=0.2,size = .7) +
  geom_ribbon(data = sg.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#046C9A",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.sg,
            aes(x = year.y, y = Estimate,linetype=Treatment),
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
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="bottom", plot.margin= margin(t = -0.5, r = 0, b = 0.5, l = 0, unit = "cm"),
                     legend.spacing.x = unit(0.25, 'cm'))


r.leg



# CDE

cde_fitted.npk<-cde_fitted.npk[complete.cases(cde_fitted.npk$starting.richness), ]
cde_coef3<-cde_coef3[complete.cases(cde_coef3$starting.richness), ]

cde_fitted.npk$starting.richness <- factor(cde_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
cde_coef3$starting.richness <- factor(cde_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))



cde_fitted.npk
cde_coef3
cde_fitted.npk
cde_fitted.ctl

cde_coef3$xs<-1

cde_fitted.npk$Model<-"F) Persistent Species Change in Biomass"
cde_fitted.ctl$Model<-"F) Persistent Species Change in Biomass"
cde_fitted.npk <- cde_fitted.npk %>% rename(Treatment = trt.y) 
cde_fitted.ctl <- cde_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.cde<-bind_rows(cde_fitted.npk,cde_fitted.ctl)

fitted.cde

fitted.cde$Treatment <- factor(fitted.cde$Treatment , levels=c("NPK","Control"))

View(cde_coef3)

#cde
cdem<-ggplot() +
  # data
  facet_grid(~Model) +
  geom_point(data = cde_fitted.npk,
             aes(x = year.y, y = CDE),color="black", alpha=0.2,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = cde_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope)  * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax)),
               color="black",alpha=0.2,size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = cde_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#F98400",alpha = 0.5) +
  # fixed effect
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
       title= '',  color='Starting Richness') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#F98400",drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none", plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))

cdem



#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

rlegend<-g_legend(r.leg)

# patchwork solution

(conceptual | sloss.trtm | sgain.trtm) / (sl.trtm | sg.trtm  | cdem ) / (rlegend) +
  plot_layout( heights = c(10,10,2)) 


# EFFECT PLOTS
load('~/Dropbox/Projects/NutNet/Data/effs.Rdata')

load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')

rich.p
bm.p
sloss.p
sl.p
sgain.p
sg.p
cde.p
# sloss.f<-sloss.f %>% 
#   summarise(eff = sum(eff), eff_upper = sum(eff_upper), eff_lower = sum(eff_lower) ) %>%
#   mutate(response= 'NPK Slope Add', Model= 'Species Loss') %>% bind_rows(rich.f) %>%
#   filter(!(response == 'NPK Slope')) %>%
#   mutate( response = recode(response,  'NPK Slope Add' = "NPK Slope" ) )
# 
 sloss.p
sloss.eff<-ggplot() + 
  geom_point(data =sloss.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sloss.p, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  #facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Loss'))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(-0.5,-0.2,0)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.text.x = element_text(size=6),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

sloss.eff


# sgain.f<-sgain.p %>% 
#   summarise(eff = sum(eff), eff_upper = sum(eff_upper), eff_lower = sum(eff_lower) ) %>%
#   mutate(response= 'NPK Slope Add', Model= 'Species Gain') %>% bind_rows(rich.f) %>%
#   filter(!(response == 'NPK Slope')) %>%
#   mutate( response = recode(response,  'NPK Slope Add' = "NPK Slope" ) )
sgain.p
sgain.eff<-ggplot() + 
  geom_point(data =sgain.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sgain.p, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Gain'))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,0.05,0.3)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.text.x = element_text(size=6),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

sgain.eff


# sl.f<-sl.f %>% 
#   summarise(eff = sum(eff), eff_upper = sum(eff_upper), eff_lower = sum(eff_lower) ) %>%
#   mutate(response= 'NPK Slope Add', Model= 'Species Loss Effect on Biomass') %>% bind_rows(rich.f) %>%
#   filter(!(response == 'NPK Slope')) %>%
#   mutate( response = recode(response,  'NPK Slope Add' = "NPK Slope" ) )
sl.p
sl.eff<-ggplot() + 
  geom_point(data =sl.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sl.p, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') '))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.text.x = element_text(size=6),
                              title=element_text(size=6),
                              strip.background = element_blank(),legend.position="none")

sl.eff


# sg.f<-sg.f %>% 
#   summarise(eff = sum(eff), eff_upper = sum(eff_upper), eff_lower = sum(eff_lower) ) %>%
#   mutate(response= 'NPK Slope Add', Model= 'Species Gain Effect on Biomass') %>% bind_rows(rich.f) %>%
#   filter(!(response == 'NPK Slope')) %>%
#   mutate( response = recode(response,  'NPK Slope Add' = "NPK Slope" ) )
sg.p
sg.eff<-ggplot() + 
  geom_point(data =sg.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sg.p, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       #y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') '))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,5,10)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.text.x = element_text(size=6),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

sg.eff




# cde.f<-cde.f %>% 
#   summarise(eff = sum(eff), eff_upper = sum(eff_upper), eff_lower = sum(eff_lower) ) %>%
#   mutate(response= 'NPK Slope Add', Model= 'Persistent Species Change in Biomass') %>% bind_rows(rich.f) %>%
#   filter(!(response == 'NPK Slope')) %>%
#   mutate( response = recode(response,  'NPK Slope Add' = "NPK Slope" ) )
# 
cde.p

cde.eff<-ggplot() + 
  geom_point(data =cde.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = cde.p, aes(x = response,ymin = eff_lower,
                                  ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       #y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') '))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(-8,0,4,14)) +
  scale_color_manual(values = c("#000000","#F98400")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.text.x = element_text(size=6),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

cde.eff




(rich.eff | bm.eff)/(sloss.eff | sgain.eff)/(sl.eff | sg.eff | cde.eff)





# INSETS

(sloss.eff | sgain.eff)/(sl.eff | sg.eff | cde.eff)

(sloss.trtm|sgain.trtm)/(sl.trtm|sg.trtm+ theme(legend.position="none")|cdem )/(rlegend) +
  plot_layout(heights = c(10,10,3.5))



sloss <- sloss.trtm +  annotation_custom(ggplotGrob(sloss.eff), xmin = 7, xmax = 12, 
                                         ymin = -20, ymax = -13)



sgain <- sgain.trtm +  annotation_custom(ggplotGrob(sgain.eff), xmin = 7, xmax = 12, 
                                         ymin = 13, ymax = 20)


sl <- sl.trtm +  annotation_custom(ggplotGrob(sl.eff), xmin = 7, xmax = 12, 
                                   ymin = -400, ymax = -275)



sg <- sg.trtm +  annotation_custom(ggplotGrob(sg.eff), xmin = 7, xmax = 12, 
                                   ymin = 275, ymax = 400)

cde <- cdem +  annotation_custom(ggplotGrob(cde.eff), xmin = 7, xmax = 12, 
                                 ymin = 500, ymax = 1000)




(plot_spacer() | sloss | sgain) / (sl | sg  | cde ) / (rlegend) +
  plot_layout( heights = c(10,10,2)) 


# LANDSCAPE 11X14
(conceptual | sloss | sgain) / (sl | sg  | cde ) / (rlegend) +
  plot_layout( heights = c(10,10,0.5)) 






