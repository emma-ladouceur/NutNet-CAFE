
library(brms)
library(tidyverse)
library(ggplot2)

# Figs 3-4


# FIGURE 3
# DERIVATIVES DELTA OVER TIME
# CAFE BAYES VECTORS

# SP LOSS VECTOR, SP GAIN VECTOR, CDE VECTOR... but what about....transformations?
# data
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )


#price partition data
pp <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only3.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(pp)

# models
load('~/Dropbox/Projects/NutNet/Model_fits/sl.Rdata') # sl..s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Model_fits/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s
#load('~/Dropbox/Projects/NutNet/Model_fits/rich3.Rdata') # plot.rich.g


summary(sl.trt.h.t)
s.loss.p.d
CDE.s.t
sloss_fixef

sloss_fixef <- fixef(s.loss.s)
sgain_fixef <- fixef(s.gain.s)
sl_fixef <- fixef(sl.s)
sg_fixef <- fixef(sg.s)
cde_fixef <- fixef(CDE.s)

sgain_fixef<-as.data.frame(sgain_fixef)
sloss_fixef<-as.data.frame(sloss_fixef)
sl_fixef<-as.data.frame(sl_fixef)
sg_fixef<-as.data.frame(sg_fixef)
cde_fixef<-as.data.frame(cde_fixef)

sgain_fixef$names <- rownames(sgain_fixef)
sloss_fixef$names <- rownames(sloss_fixef)
sl_fixef$names <- rownames(sl_fixef)
sg_fixef$names <- rownames(sg_fixef)
cde_fixef$names <- rownames(cde_fixef)
sloss_fixef

# 
sloss_fixef$Estimate<- -abs(sloss_fixef$Estimate)
sloss_fixef$Q2.5<- -abs(sloss_fixef$Q2.5)
sloss_fixef$Q97.5<- -abs(sloss_fixef$Q97.5)

sl_fixef$Estimate<- -abs(sl_fixef$Estimate)
sl_fixef$Q2.5<- -abs(sl_fixef$Q2.5)
sl_fixef$Q97.5<- -abs(sl_fixef$Q97.5)
sl_fixef

sgain_fixef$Estimate<- abs(sgain_fixef$Estimate)
sgain_fixef$Q2.5<- abs(sgain_fixef$Q2.5)
sgain_fixef$Q97.5<- abs(sgain_fixef$Q97.5)
sgain_fixef

# sg_fixef$Estimate<- -abs(sg_fixef$Estimate)
# sg_fixef$Q2.5<- -abs(sg_fixef$Q2.5)
# sg_fixef$Q97.5<- -abs(sg_fixef$Q97.5)
sg_fixef

sgain_fixef$Model<-'Sgain'
sloss_fixef$Model<-'Sloss'
sl_fixef$Model<-'SL'
sg_fixef$Model<-'SG'
cde_fixef$Model<-'CDE'
fixedf_pp<-bind_rows(sl_fixef,sg_fixef,cde_fixef,sloss_fixef,sgain_fixef)
View(fixedf_pp)


ggplot(data = fixedf_pp)+
  geom_point(aes(x=Estimate[16], #losses
                 y=Estimate[4]),
                 colour="red",
                 size=0.7)+
  geom_errorbar(aes(x=Estimate[16],
                    ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_errorbarh(aes(y=Estimate[4],
                    xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_point(aes(x=Estimate[16]+Estimate[20], #gains
             y= Estimate[8]),
                 colour="blue",size=0.7)+
  geom_errorbar(aes(x=Estimate[16]+Estimate[20],
                ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "blue", size = 0.35,alpha=0.3) +
  # geom_errorbarh(aes(y=Estimate[8],
  #                   xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "blue", size = 0.35,alpha=0.3) +
   geom_point(aes(x=Estimate[16]+Estimate[20], #persistent
                 y=Estimate[12]),
             colour="purple",size=0.7)+
  geom_errorbar(aes(x=Estimate[16]+Estimate[20],
                    ymin = Q2.5[8]+Q2.5[12], ymax = Q97.5[4]+Q97.5[8]+Q97.5[12]),width=0,colour = "purple", size = 0.35,alpha=0.3) +
  # geom_errorbarh(aes(y=Estimate[8]+Estimate[12],
  #                    xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "purple", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp, # cde
               aes(x = Estimate[16]+Estimate[20], #losses + gains
                   xend = Estimate[16]+Estimate[20], # losses + gains
                   y = Estimate[8],   # effect of sl + effect of sg on bm
                   yend = Estimate[12]), # effect of sl + sg + cde on biomass
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # gains
               aes(x = Estimate[16], # start at losses
                   xend = Estimate[16]+Estimate[20], #species losses + species gains
                   y = Estimate[4],    # effect of sl on biomass
                   yend = Estimate[8]),  # effect of sl + effect of sg on bm
               colour= "blue",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
   geom_segment(data = fixedf_pp, # losses
                            aes(x = 0,
                                xend = Estimate[16], # species losses
                                y = 0,
                                yend = Estimate[4]), # effect of sl on biomass
                            colour= "red",
                            size = 1.5,
                            arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
   labs(x = 'Effect on Species Richness Over Time',
       y = 'Effect on Biomass Change Over Time',
       title= 'CAFE Bayes Vector Plot') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")



# Try 2
ggplot(data = fixedf_pp)+
  geom_point(aes(x=Estimate[16], #losses
                 y=Estimate[4]),
             colour="red",
             size=0.7)+
  geom_errorbar(aes(x=Estimate[16],
                    ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_errorbarh(aes(y=Estimate[4],
                     xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_point(aes(x=Estimate[20], #gains
                 y= Estimate[8]),
             colour="blue",size=0.7)+
  geom_errorbar(aes(x=Estimate[20],
                    ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_errorbarh(aes(y=Estimate[8],
                     xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_point(aes(x=Estimate[20], #persistent
                 y=Estimate[12]),
             colour="purple",size=0.7)+
   geom_errorbar(aes(x=Estimate[20],
                     ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "purple", size = 0.35,alpha=0.3) +
   #geom_errorbarh(aes(y=Estimate[8]+Estimate[12],
     #                xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "purple", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp, # cde
               aes(x = Estimate[20], #gains
                   xend = Estimate[20], # gains
                   y = Estimate[8],   # effect of  sg on bm
                   yend = Estimate[12]), # effect of cde on biomass
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # gains
               aes(x = Estimate[16], # start at losses
                   xend = Estimate[20], # species gains effect
                   y = Estimate[4],    # effect of sl on biomass
                   yend = Estimate[8]),  # effect of sl + effect of sg on bm
               colour= "blue",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # losses
               aes(x = 0,
                   xend = Estimate[16], # species losses
                   y = 0,
                   yend = Estimate[4]), # effect of sl on biomass
               colour= "red",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  labs(x = 'Effect on Species Richness Over Time',
       y = 'Effect on Biomass Change Over Time',
       title= 'CAFE Bayes Vector Plot') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


#you could also start them all from 0?
ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate[12]),
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data = fixedf_pp,aes(x=0, #persistent
                 y=Estimate[12]),
             colour="purple",size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=0,
                    ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "purple", size = 0.55,alpha=0.3) +
   geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[20],
                   y = 0,
                   yend = Estimate[8]),
               colour= "blue",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data = fixedf_pp,aes(x=Estimate[20], #gains
                 y= Estimate[8]),
             colour="blue",size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                    ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                     xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[16],
                   y = 0,
                   yend = Estimate[4]),
               colour= "red",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data = fixedf_pp,aes(x=Estimate[16], #losses
                 y=Estimate[4]),
             colour="red",
             size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                    ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                     xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "red", size = 0.35,alpha=0.3) 

  


##########################################
######SCRATCH THAT , GOING DOWN WRONG PATH

#fitted values
sl_fitted <- cbind(sl.trt.i$data,fitted(sl.trt.i, re_formula = NA)) %>%  as_tibble() 
sg_fitted <- cbind(sg.trt.i$data,fitted(sg.trt.i, re_formula = NA)) %>% as_tibble() 
cde_fitted <- cbind(p.CDE.trt.i$data,fitted(p.CDE.trt.i, re_formula = NA)) %>% as_tibble() 
View(sl_fitted)
head(pp)
dat<-distinct(pp,habitat,continent,site_code, trt.y,year.y.m,year.y,block, plot,x.rich,y.rich,x.func,y.func,SL.p,SL,SG,CDE,s.loss,s.gain,c.rich)
sl_fitted2<-full_join(sl_fitted,dat)
sg_fitted2<-full_join(sg_fitted,dat)
cde_fitted2<-full_join(cde_fitted,dat)

sl_fitted.npk<-sl_fitted2[sl_fitted2$trt.y %in% c('NPK'),]
sl_fitted_fitted.ctl<-sl_fitted2[sl_fitted2$trt.y %in% c('Control'),]

sg_fitted.npk<-sg_fitted2[sg_fitted2$trt.y %in% c('NPK'),]
View(sg_fitted.npk)

cde_fitted.npk<-cde_fitted2[cde_fitted2$trt.y %in% c('NPK'),]
cde_fitted_fitted.ctl<-cde_fitted2[cde_fitted2$trt.y %in% c('Control'),]



# CAFE component    richness    function
# base = c(x.rich,x.func)
# SL = c(c.rich,SL)
# SG = c(y.rich,SL+SG)
# CDE = c(y.rich,y.func)

ggplot() +
  # geom_point(data = sl_fitted.npk,
  #            aes(x = x.rich, y = x.func),
  #            colour = "black", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_point(data = sl_fitted.npk,
  #            aes(x = c.rich, y = x.func+SL),
  #            colour = "blue", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_point(data = sg_fitted.npk,
  #            aes(x = c.rich, y = x.func+SL+SG),
  #            colour = "red", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_point(data = cde_fitted.npk,
  #            aes(x = y.rich, y = y.func),
  #            colour = "purple", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_segment(data = sl_fitted.npk,
  #              aes(x = x.rich,
  #                  xend = c.rich,
  #                  y = x.func,
  #                  yend =  x.func+SL),
  #              colour= "blue",
  #              size = 1.5,
  #              arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  # geom_segment(data = sg_fitted.npk,
  #              aes(x = c.rich,
  #                  xend = y.rich,
  #                  y = SL,
  #                  yend = x.func+SL+SG),
  #              colour= "red",
  #              size = 1.5,
  #              arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  # geom_segment(data = cde_fitted.npk,
  #              aes(x = y.rich,
  #                  xend = y.rich,
  #                  y = x.func+SL+SG,
  #                  yend = y.func),
  #              colour= "purple",
  #              size = 1.5,
  #              arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  labs(x = 'Species Richness',
       y = 'Function', title= 'Manual CAFE Plot') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"))






