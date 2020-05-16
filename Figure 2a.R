

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)
library(bayesplot)


plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(plot)
plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$log.rich<-log(plot$rich)
#bm
plot$log.live.mass<-log(plot$plot_mass)



load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')


plot.rich_fitted.npk$starting.richness <- factor(plot.rich_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.rich_coef3$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


summary(plot.rich_fitted.npk)
colnames(plot.rich_coef3)
colnames(plot.rich_fitted.npk)
colnames(plot.rich_fitted.ctl)

plot.rich_fitted.npk$Response<- "Species Richness"
plot.rich_fitted.npk <- plot.rich_fitted.npk %>% rename(Treatment = trt) 
plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% rename(Treatment = trt) 
fitted.rich<-bind_rows(plot.rich_fitted.npk,plot.rich_fitted.ctl)
plot.rich_coef3$Response<- "Species Richness"

summary(plot.bm_fitted.npk)
colnames(plot.bm_coef3)
colnames(plot.bm_fitted.npk)
colnames(plot.bm_fitted.ctl)

plot.bm_fitted.npk$Response<- "Biomass"
plot.bm_fitted.npk <- plot.bm_fitted.npk %>% rename(Treatment = trt) 
plot.bm_fitted.ctl <- plot.bm_fitted.ctl %>% rename(Treatment = trt) 
fitted.bm<-bind_rows(plot.bm_fitted.npk,plot.bm_fitted.ctl)
plot.bm_coef3$Response<- "Biomass"
fitted.bm$block<- as.factor(as.character(fitted.bm$block))

summary(plot.rich_coef3)

View(fitted.bm)
fitted.all<-bind_rows(fitted.rich,fitted.bm)
coef.all<-bind_rows(plot.rich_coef3,plot.bm_coef3)

summary(fitted.all)
summary(coef.all)


fitted.all$starting.richness <- factor(fitted.all$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
coef.all$starting.richness <- factor(coef.all$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


rich.bm.r<-ggplot() +
  facet_wrap(Response~., scales="free_y")+
  # geom_point(data = fitted.all,
  #            aes(x = year_trt, y = live_mass,
  #                colour = starting.richness), alpha=0.6,
  #            size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = coef.all,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code,
                   colour = starting.richness),
               size = 0.7) +
  # uncertainy in fixed effect
  # geom_ribbon(data = plot.bm_fitted.npk,
  #             aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
  #             alpha = 0.5) +
  # # fixed effect
  # geom_line(data = plot.bm_fitted.npk,
  #           aes(x = year_trt, y = Estimate),
  #           size = 1.5) +
  # geom_ribbon(data = plot.bm_fitted.ctl,
  #             aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
  #             alpha = 0.5) +
  # # fixed effect
  # geom_line(data = plot.bm_fitted.ctl,
  #           aes(x = year_trt, y = Estimate),
  #           size = 1.5,linetype= "dashed") +
 # labs(x='',
       #x = 'Years',
  #     y = expression(paste('Biomass (g/',m^2, ')')), title= 'b) Biomass ', color='Starting Richness') +
  # xlim(0,11) +
ylab(NULL)+
  scale_x_continuous(breaks=c(0,1,3,6,9,11)) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="none")

rich.bm.r


grid_arrange_shared_legend(r1,b1,nrow=1)


# PARTITIONS

sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only4.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )


# GAINS N LOSSES


load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')

is.factor(sgain.trt_fitted.npk$starting.richness)
is.factor(sgain.trt_coef3$starting.richness)

sgain.trt_fitted.npk<-sgain.trt_fitted.npk[complete.cases(sgain.trt_fitted.npk$starting.richness), ]
sgain.trt_coef3<-sgain.trt_coef3[complete.cases(sgain.trt_coef3$starting.richness), ]

sgain.trt_fitted.npk$starting.richness <- factor(sgain.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sgain.trt_coef3$starting.richness <- factor(sgain.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sgain.trt_coef3$xs<-1

sgain.trtm<-ggplot()  +
  # data
  geom_point(data = sgain.trt_fitted.npk,
             aes(x = year.y, y = s.gain,
                 colour = starting.richness), alpha=0.6,
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = sgain.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   # group = starting.richness,
                   colour = starting.richness),
               size = .7) +
  geom_ribbon(data = sgain.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sgain.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sgain.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sgain.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(0,20) +
  labs(x='',
       #x = 'Years',
       y = expression(paste('Species Gain')), title= 'd) Species Gain', color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="none")


sgain.trtm


# LOSS
as.factor(as.character(sloss.trt_fitted.npk$starting.richness))
as.factor(as.character(sloss.trt_coef3$starting.richness))

sloss.trt_fitted.npk<-sloss.trt_fitted.npk[complete.cases(sloss.trt_fitted.npk$starting.richness), ]
sloss.trt_coef3<-sloss.trt_coef3[complete.cases(sloss.trt_coef3$starting.richness), ]

sloss.trt_fitted.npk$starting.richness <- factor(sloss.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sloss.trt_coef3$starting.richness <- factor(sloss.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


sloss.trt_coef3$xs<-1


sloss.trtm<-ggplot() +
  geom_point(data = sloss.trt_fitted.npk,
             aes(x = year.y, y = s.loss,
                 colour = starting.richness), alpha=0.6,
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = sloss.trt_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax),
                   # group = site_code,
                   colour = starting.richness
               ),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sloss.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sloss.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sloss.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sloss.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(-20,0) +
  labs(x='',
       #x = 'Years',
       y = expression(paste('Species Loss')), title= 'c) Species Loss', color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="none")



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


sl.trtm<-ggplot() +
  geom_point(data = sl.trt_fitted.npk,
             aes(x = year.y, y = SL,
                 colour = starting.richness),alpha=0.6,
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = sl.trt_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax),
                   # group = site_code,
                   colour = starting.richness
               ),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sl.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sl.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sl.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sl.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(-400,0) +
  labs(x='',
       #x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'e) Species Loss', color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="none")


sl.trtm

# SG
is.factor(sg.trt_fitted.npk$starting.richness)
is.factor(sg.trt_coef3$starting.richness)

sg.trt_fitted.npk<-sg.trt_fitted.npk[complete.cases(sg.trt_fitted.npk$starting.richness), ]
sg.trt_coef3<-sg.trt_coef3[complete.cases(sg.trt_coef3$starting.richness), ]

sg.trt_fitted.npk$starting.richness <- factor(sg.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.trt_coef3$starting.richness <- factor(sg.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sg.trt_coef3$xs<-1
#gai
sg.trtm<-ggplot()  +
  # data
  geom_point(data = sg.trt_fitted.npk,
             aes(x = year.y, y = SG,
                 colour = starting.richness), alpha =0.6,
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=sg.trt_fitted.npk,
  #             aes(x = year.y, y = SG,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  # uncertainy in fixed effect
  geom_segment(data = sg.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   # group = starting.richness,
                   colour = starting.richness),
               size = .7) +
  geom_ribbon(data = sg.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sg.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sg.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sg.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  #scale_y_continuous(trans = 'log', breaks=c(1,2,4,6,8,16,24,64,512,1024,2048,4096)) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(0,400) +
  labs(x = 'Years',
       y='',
       #y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'f) Species Gain', color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="none")


sg.trtm


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
#cde
cdem<-ggplot() +
  # data
  geom_point(data = cde_fitted.npk,
             aes(x = year.y, y = CDE,
                 colour = starting.richness), alpha=0.6,
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = cde_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope)  * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = cde_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = cde_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = cde_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = cde_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(-500,1000)+
  labs(x='',
       #x = 'Years',
       y= '',
       # y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'g) Persistent Species',  color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                     legend.position="bottom")

cdem

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

rlegend<-g_legend(cdem)

r.combine <- grid.arrange(arrangeGrob(r1 ,
                                      b1 ,
                                      sloss.trtm,
                                      sgain.trtm,
                                      sl.trtm,
                                      sg.trtm,
                                      cdem + theme(legend.position="none"),
                                      rlegend,
                                      nrow=4,ncol=3,
                                      layout_matrix=rbind(c(1,2,NA),c(3,4,NA),c(5,6,7),c(NA,8,NA)),
                                      heights = c(10,10,10, 2)))

r.combine



t.combine <- grid.arrange(arrangeGrob(r1 ,
                                      b1 ,
                                      sp.trt,
                                      sloss.trtm,
                                      sgain.trtm,
                                      sp.slope,
                                      sl.trtm,
                                      sg.trtm,
                                      cdem + theme(legend.position="none"),
                                      bm.trt, bm.slope,
                                      rlegend,
                                      nrow=5,ncol=3,
                                      layout_matrix=rbind(c(1,2,3),c(4,5,6),c(7,8,9),c(NA,10,11),c(NA,12,NA)),
                                      heights = c(10,10,10,10, 2)))

t.combine


grid.arrange(sp.trt,sp.slope,bm.trt,bm.slope,nrow=2,ncol=2)


