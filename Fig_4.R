

rm(list=ls())

library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")

# FIGURE 3 CLOUD VERSION
# EXTRACT 100 POSTERIORS

# models


load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.s



# site level meta data for posrteriors
# calculated to site level details found in Climate_Data.R
# latitude and longitude dont match due to decimal rounding
# lat.x long.x is nutnet site, lat.y long.y is world clim
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


meta <- meta %>% group_by(site_code) %>% filter(max.year >= 3) %>%
ungroup()


colnames(meta)
View(meta)


sl.trt.i_fixef <- fixef(sl.3)
sg.trt.i_fixef <- fixef(sg.3)
cde.trt.i_fixef <- fixef(CDE.3)
sloss.trt.i_fixef <- fixef(s.loss.3)
sgain.trt.i_fixef <- fixef(s.gain.3)


sl.fixed.p<-posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p<-posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )

sloss.fixed.p<-posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 


head(sl.fixed.p)
head(sl.trt.i_fixef)


sl.fixed.p2 <-sl.fixed.p %>% 
  mutate(sl.trt.rate.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sl.ctl.rate.p=`b_year.y.m`) %>%
  mutate( sl.ctl.rate_global_slope = mean(sl.ctl.rate.p),
           sl.ctl.rate_lower_slope = quantile(sl.ctl.rate.p, probs=0.025),
           sl.ctl.rate_upper_slope = quantile(sl.ctl.rate.p, probs=0.975) )  %>%
  filter(sl.ctl.rate.p > quantile(sl.ctl.rate.p, probs=0.025),
         sl.ctl.rate.p < quantile(sl.ctl.rate.p, probs=0.975)) %>%
mutate( sl.trt.rate_global_slope = mean(sl.trt.rate.p),
        sl.trt.rate_lower_slope = quantile(sl.trt.rate.p, probs=0.025),
        sl.trt.rate_upper_slope = quantile(sl.trt.rate.p, probs=0.975) )  %>%
filter(sl.trt.rate.p > quantile(sl.trt.rate.p, probs=0.025),
       sl.trt.rate.p < quantile(sl.trt.rate.p, probs=0.975)) %>%
  select(sl.ctl.rate.p,sl.ctl.rate_global_slope, sl.ctl.rate_upper_slope,sl.ctl.rate_lower_slope,
         sl.trt.rate.p,sl.trt.rate_global_slope, sl.trt.rate_upper_slope,sl.trt.rate_lower_slope) %>%
  sample_n(50) 


sg.fixed.p2 <-sg.fixed.p %>% 
  mutate(sg.trt.rate.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sg.ctl.rate.p=`b_year.y.m`) %>%
  mutate( sg.ctl.rate_global_slope = mean(sg.ctl.rate.p),
          sg.ctl.rate_lower_slope = quantile(sg.ctl.rate.p, probs=0.025),
          sg.ctl.rate_upper_slope = quantile(sg.ctl.rate.p, probs=0.975) )  %>%
  filter(sg.ctl.rate.p > quantile(sg.ctl.rate.p, probs=0.025),
         sg.ctl.rate.p < quantile(sg.ctl.rate.p, probs=0.975)) %>%
  mutate( sg.trt.rate_global_slope = mean(sg.trt.rate.p),
          sg.trt.rate_lower_slope = quantile(sg.trt.rate.p, probs=0.025),
          sg.trt.rate_upper_slope = quantile(sg.trt.rate.p, probs=0.975) )  %>%
  filter(sg.trt.rate.p > quantile(sg.trt.rate.p, probs=0.025),
         sg.trt.rate.p < quantile(sg.trt.rate.p, probs=0.975)) %>%
  select(sg.ctl.rate.p,sg.ctl.rate_global_slope, sg.ctl.rate_upper_slope,sg.ctl.rate_lower_slope,
         sg.trt.rate.p,sg.trt.rate_global_slope, sg.trt.rate_upper_slope,sg.trt.rate_lower_slope) %>% sample_n(50) 


cde.fixed.p2 <-cde.fixed.p %>% 
  mutate(cde.trt.rate.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.rate.p=`b_year.y.m`) %>%
  mutate( cde.ctl.rate_global_slope = mean(cde.ctl.rate.p),
          cde.ctl.rate_lower_slope = quantile(cde.ctl.rate.p, probs=0.025),
          cde.ctl.rate_upper_slope = quantile(cde.ctl.rate.p, probs=0.975) )  %>%
  filter(cde.ctl.rate.p > quantile(cde.ctl.rate.p, probs=0.025),
         cde.ctl.rate.p < quantile(cde.ctl.rate.p, probs=0.975)) %>%
  mutate( cde.trt.rate_global_slope = mean(cde.trt.rate.p),
          cde.trt.rate_lower_slope = quantile(cde.trt.rate.p, probs=0.025),
          cde.trt.rate_upper_slope = quantile(cde.trt.rate.p, probs=0.975) )  %>%
  filter(cde.trt.rate.p > quantile(cde.trt.rate.p, probs=0.025),
         cde.trt.rate.p < quantile(cde.trt.rate.p, probs=0.975)) %>%
  select(cde.ctl.rate.p,cde.ctl.rate_global_slope, cde.ctl.rate_upper_slope,cde.ctl.rate_lower_slope,
         cde.trt.rate.p,cde.trt.rate_global_slope, cde.trt.rate_upper_slope,cde.trt.rate_lower_slope) %>% sample_n(50) 


sloss.fixed.p2 <-sloss.fixed.p %>% 
  mutate(sloss.trt.rate.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sloss.ctl.rate.p=`b_year.y.m`) %>%
  mutate( sloss.ctl.rate_global_slope = mean(sloss.ctl.rate.p),
          sloss.ctl.rate_lower_slope = quantile(sloss.ctl.rate.p, probs=0.025),
          sloss.ctl.rate_upper_slope = quantile(sloss.ctl.rate.p, probs=0.975) )  %>%
  filter(sloss.ctl.rate.p > quantile(sloss.ctl.rate.p, probs=0.025),
         sloss.ctl.rate.p < quantile(sloss.ctl.rate.p, probs=0.975)) %>%
  mutate( sloss.trt.rate_global_slope = mean(sloss.trt.rate.p),
          sloss.trt.rate_lower_slope = quantile(sloss.trt.rate.p, probs=0.025),
          sloss.trt.rate_upper_slope = quantile(sloss.trt.rate.p, probs=0.975) )  %>%
  filter(sloss.trt.rate.p > quantile(sloss.trt.rate.p, probs=0.025),
         sloss.trt.rate.p < quantile(sloss.trt.rate.p, probs=0.975)) %>%
  select(sloss.ctl.rate.p,sloss.ctl.rate_global_slope, sloss.ctl.rate_upper_slope,sloss.ctl.rate_lower_slope,
         sloss.trt.rate.p,sloss.trt.rate_global_slope, sloss.trt.rate_upper_slope,sloss.trt.rate_lower_slope) %>% sample_n(50) 


sgain.fixed.p2 <-sgain.fixed.p %>% 
  mutate(sgain.trt.rate.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sgain.ctl.rate.p=`b_year.y.m`) %>%
  mutate( sgain.ctl.rate_global_slope = mean(sgain.ctl.rate.p),
          sgain.ctl.rate_lower_slope = quantile(sgain.ctl.rate.p, probs=0.025),
          sgain.ctl.rate_upper_slope = quantile(sgain.ctl.rate.p, probs=0.975) )  %>%
  filter(sgain.ctl.rate.p > quantile(sgain.ctl.rate.p, probs=0.025),
         sgain.ctl.rate.p < quantile(sgain.ctl.rate.p, probs=0.975)) %>%
  mutate( sgain.trt.rate_global_slope = mean(sgain.trt.rate.p),
          sgain.trt.rate_lower_slope = quantile(sgain.trt.rate.p, probs=0.025),
          sgain.trt.rate_upper_slope = quantile(sgain.trt.rate.p, probs=0.975) )  %>%
  filter(sgain.trt.rate.p > quantile(sgain.trt.rate.p, probs=0.025),
         sgain.trt.rate.p < quantile(sgain.trt.rate.p, probs=0.975)) %>%
  select(sgain.ctl.rate.p,sgain.ctl.rate_global_slope, sgain.ctl.rate_upper_slope,sgain.ctl.rate_lower_slope,
         sgain.trt.rate.p,sgain.trt.rate_global_slope, sgain.trt.rate_upper_slope,sgain.trt.rate_lower_slope) %>% sample_n(50) 


cde.s<-cde.fixed.p2
loss.s<- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2)
gains.s<- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2)
loss.s$Vector="Losses"
gains.s$Vector="Gains"
cde.s$Vector="Persistent Sp."
loss.gain <- loss.s %>% bind_cols(gains.s)

all.effs <- loss.gain %>% bind_cols(cde.fixed.p2)

head(all.effs)
colnames(all.effs)





# GET LEGENDS


sgain.effs$response <- factor(sgain.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


fixed.leg<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt.rate_global_slope,colour= Vector ), 
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                              y=  cde.trt.rate_global_slope),
             colour="#816687",size=0.1,alpha = 0.4) +
  geom_segment(data = loss.s,
               aes(x = 0,
                   xend = sloss.trt.rate_global_slope,
                   y = 0,
                   yend = sl.trt.rate_global_slope,colour= Vector,),
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss.s, aes(x= sloss.trt.rate_global_slope, #loss
                                y=  sl.trt.rate_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = gains.s,
               aes(x = 0,
                   xend =  sgain.trt.rate_global_slope,
                   y = 0,
                   yend =  sg.trt.rate_global_slope, colour= Vector),
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = gains.s, aes(x= sgain.trt.rate_global_slope, #losses
                                 y=  sg.trt.rate_global_slope ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  scale_color_manual(name='Overall Effects',breaks=c("Losses","Gains","Persistent Sp."),
                     values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))


fixed.leg


post.leg<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt.rate.p ,colour= Vector),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = loss.s,
               aes(x = 0,
                   xend = sloss.trt.rate.p ,
                   y = 0,
                   yend = sl.trt.rate.p ,colour= Vector ),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = gains.s,
               aes(x = 0,
                   xend = sgain.trt.rate.p ,
                   y = 0,
                   yend = sg.trt.rate.p ,
                   colour= Vector), size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_color_manual(name='Uncertainty',breaks=c("Losses","Gains","Persistent Sp."),
                     values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')


post.leg




View(all.effs)

price.cloud<-ggplot()+
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
# controls
geom_segment(data = all.effs,
             aes(x = 0,
                 xend = sloss.ctl.rate.p ,
                 y = 0,
                 yend = sl.ctl.rate.p  ),
             colour= "#B40F20", linetype=2,
             size = 0.2,  alpha = 0.2,
             arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate.p, #loss
                                  y=  sl.ctl.rate.p  ),
             colour="black",size=0.2,alpha = 0.2)+
  geom_segment(data = all.effs,
               aes(x = sloss.ctl.rate.p,
                   xend =  sloss.ctl.rate.p+sgain.ctl.rate.p ,
                   y = sl.ctl.rate.p,
                   yend = sl.ctl.rate.p+ sg.ctl.rate.p ),
               colour= "#046C9A",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate.p+ sgain.ctl.rate.p , #losses
                                  y=  sl.ctl.rate.p+sg.ctl.rate.p ) ,
             colour="black",
             size=0.2,alpha = 0.2)+
  
  geom_segment(data = all.effs,
               aes(x =  sloss.ctl.rate.p+sgain.ctl.rate.p,
                   xend =  sloss.ctl.rate.p+sgain.ctl.rate.p,
                   y =  sl.ctl.rate.p+sg.ctl.rate.p,
                   yend = sl.ctl.rate.p+sg.ctl.rate.p+cde.ctl.rate.p ), 
               colour=  "#F98400",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y= sl.ctl.rate.p+sg.ctl.rate.p+cde.ctl.rate.p ),
             colour="#F98400",size=0.1,alpha = 0.2) +
  
  # Fixed effects section
  # CONTROLS
  geom_segment(data = all.effs %>% distinct(sloss.ctl.rate_global_slope,sl.ctl.rate_global_slope),
               aes(x = 0,
                   xend = sloss.ctl.rate_global_slope,
                   y = 0,
                   yend = sl.ctl.rate_global_slope  ),
               colour= "#B40F20", linetype=2,
               size = 1.5, alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate_global_slope, #loss
                                  y=  sl.ctl.rate_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  # geom_errorbar(data = all.effs,aes(x=sloss.ctl.rate_global_slope,
  #                                   ymin = sl.ctl.rate_lower_slope, ymax = sl.ctl.rate_upper_slope),width=0,colour = "black", size = 0.55,alpha=0.3) +
  # geom_errorbarh(data = all.effs,aes(y=sl.ctl.rate_global_slope,
  #                                    xmin = sloss.ctl.rate_lower_slope, xmax = sloss.ctl.rate_upper_slope),height=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs %>% distinct(sloss.ctl.rate_global_slope,sgain.ctl.rate_global_slope,sl.ctl.rate_global_slope,sg.ctl.rate_global_slope),
               aes(x = sloss.ctl.rate_global_slope,
                   xend = sloss.ctl.rate_global_slope +  sgain.ctl.rate_global_slope,
                   y = sl.ctl.rate_global_slope,
                   yend =  sl.ctl.rate_global_slope+sg.ctl.rate_global_slope),
               colour= "#046C9A",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope, #losses
                                  y= sl.ctl.rate_global_slope+ sg.ctl.rate_global_slope ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  # geom_errorbar(data = all.effs,aes(x=sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope,
  #                                   ymin = sl.ctl.rate_lower_slope+sg.ctl.rate_lower_slope, ymax = sl.ctl.rate_upper_slope+sg.ctl.rate_upper_slope),width=0,colour = "black", size = 0.55,alpha=0.3) +
  # geom_errorbarh(data = all.effs,aes(y=sl.ctl.rate_global_slope+sg.ctl.rate_global_slope,
  #                                    xmin = sloss.ctl.rate_lower_slope+sgain.ctl.rate_lower_slope, xmax = sloss.ctl.rate_upper_slope+sgain.ctl.rate_upper_slope),height=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs %>% distinct(sloss.ctl.rate_global_slope,sgain.ctl.rate_global_slope,sl.ctl.rate_global_slope,sg.ctl.rate_global_slope,cde.ctl.rate_global_slope ),
               aes(x = sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope,
                   xend = sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope,
                   y = sl.ctl.rate_global_slope+sg.ctl.rate_global_slope,
                   yend = sl.ctl.rate_global_slope+sg.ctl.rate_global_slope+cde.ctl.rate_global_slope ), 
               colour=  "#F98400",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope+cde.ctl.rate_global_slope),
             colour="#F98400",size=0.1,alpha = 0.4) +
  # geom_errorbar(data = all.effs,aes(x=sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope,
  #                                   ymin = sl.ctl.rate_lower_slope+sg.ctl.rate_lower_slope+cde.ctl.rate_lower_slope, ymax = sl.ctl.rate_upper_slope+sg.ctl.rate_upper_slope+cde.ctl.rate_upper_slope),width=0,colour = "black", size = 0.55,alpha=0.3) +
  #treatment effects
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt.rate.p  ,
                   y = 0,
                   yend = sl.trt.rate.p   ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate.p , #loss
                                  y=  sl.trt.rate.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate.p ,
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p ) ,
                   y = sl.trt.rate.p ,
                   yend = (sl.trt.rate.p)+(sg.trt.rate.p  ) ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= (sloss.trt.rate.p)+(sgain.trt.rate.p ) , #losses
                                  y= (sl.trt.rate.p)+(sg.trt.rate.p ) ),
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   y = (sl.trt.rate.p)+(sg.trt.rate.p ),
                   yend =(sl.trt.rate.p)+(sg.trt.rate.p)+ (cde.trt.rate.p  )),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y= (sl.trt.rate.p)+(sg.trt.rate.p)+(cde.trt.rate.p) ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = all.effs %>% distinct(sloss.trt.rate_global_slope , sloss.ctl.rate_global_slope,sl.trt.rate_global_slope, sl.ctl.rate_global_slope),
               aes(x = 0,
                   xend = sloss.trt.rate_global_slope ,
                   y = 0,
                   yend = sl.trt.rate_global_slope ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate_global_slope , #loss
                                  y=  sl.trt.rate_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  # geom_errorbar(data = all.effs,aes(x=sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope,
  #                                   ymin = sl.trt.rate_lower_slope + sl.ctl.rate_lower_slope, ymax = sl.trt.rate_upper_slope + sl.ctl.rate_upper_slope),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  # geom_errorbarh(data = all.effs,aes(y=sl.trt.rate_global_slope + sl.ctl.rate_global_slope,
  #                                    xmin = sloss.trt.rate_lower_slope + sloss.ctl.rate_lower_slope, xmax = sloss.trt.rate_upper_slope + sloss.ctl.rate_upper_slope),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs %>% distinct(sloss.trt.rate_global_slope , sloss.ctl.rate_global_slope,sgain.trt.rate_global_slope , sgain.ctl.rate_global_slope,sl.trt.rate_global_slope, sl.ctl.rate_global_slope, sg.trt.rate_global_slope , sg.ctl.rate_global_slope),
               aes(x = sloss.trt.rate_global_slope,
                   xend = (sloss.trt.rate_global_slope ) + (sgain.trt.rate_global_slope ),
                   y = sl.trt.rate_global_slope ,
                   yend = (sl.trt.rate_global_slope) + (sg.trt.rate_global_slope ) ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x=  (sloss.trt.rate_global_slope ) + (sgain.trt.rate_global_slope )  , #losses
                                  y=  (sl.trt.rate_global_slope) + (sg.trt.rate_global_slope ) ,
  ) ,
  colour="#046C9A",
  size=0.2,alpha = 0.4)+
  # geom_errorbar(data = all.effs, aes(x= (sloss.trt.rate_global_slope+sloss.ctl.rate_global_slope) + (sgain.trt.rate_global_slope + sgain.ctl.rate_global_slope),
  #                                    ymin =  (sl.trt.rate_lower_slope + sl.ctl.rate_lower_slope)+(sg.trt.rate_lower_slope + sg.ctl.rate_lower_slope)  , 
  #                                    ymax = (sl.trt.rate_upper_slope+sl.ctl.rate_upper_slope)+(sg.trt.rate_upper_slope + sg.ctl.rate_upper_slope)),
  #               width=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  # geom_errorbarh(data = all.effs,aes(y=(sl.trt.rate_global_slope+sl.ctl.rate_global_slope)+ (sg.trt.rate_global_slope + sg.trt.rate_global_slope),
  #                                    xmin =  (sloss.trt.rate_lower_slope+sloss.ctl.rate_lower_slope)+(sgain.trt.rate_lower_slope + sgain.ctl.rate_lower_slope)  ,
  #                                    xmax =  (sloss.trt.rate_upper_slope+sloss.ctl.rate_upper_slope)+(sgain.trt.rate_upper_slope + sgain.ctl.rate_upper_slope) ) ,
  #                height=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs %>% distinct(sloss.trt.rate_global_slope , sloss.ctl.rate_global_slope,sgain.trt.rate_global_slope , sgain.ctl.rate_global_slope,sl.trt.rate_global_slope, sl.ctl.rate_global_slope, sg.trt.rate_global_slope , sg.ctl.rate_global_slope,cde.trt.rate_global_slope, cde.ctl.rate_global_slope),
               aes(x = (sloss.trt.rate_global_slope ) + (sgain.trt.rate_global_slope ),
                   xend = (sloss.trt.rate_global_slope)+(sgain.trt.rate_global_slope ),
                   y = (sl.trt.rate_global_slope) + (sg.trt.rate_global_slope ),
                   yend = (sl.trt.rate_global_slope) + (sg.trt.rate_global_slope)+(cde.trt.rate_global_slope) ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  (sl.trt.rate_global_slope) + (sg.trt.rate_global_slope)+(cde.trt.rate_global_slope ) ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  # geom_errorbar(data = all.effs,aes(x=(sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope) + (sgain.trt.rate_global_slope + sgain.ctl.rate_global_slope),
  #                                   ymin = (sl.trt.rate_lower_slope+sl.ctl.rate_lower_slope)+(sg.trt.rate_lower_slope+ sg.ctl.rate_lower_slope)+(cde.trt.rate_lower_slope + cde.ctl.rate_lower_slope), ymax = (sl.trt.rate_upper_slope+ sl.ctl.rate_upper_slope)+(sg.trt.rate_upper_slope+sg.ctl.rate_upper_slope)+(cde.trt.rate_upper_slope+ cde.ctl.rate_upper_slope) ),width=0,colour = "#816687", size = 0.55,alpha=0.3) +
  
  # ylim(-11,35) +
  #  xlim(-0.65,0)+
  scale_y_continuous(breaks=c(-10,-5,0,5,10,15)) +
  scale_x_continuous(breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.05,0.1)) +
   annotate("text", x = -0.015, y = 0.75, label = "t0") +
   annotate("text", x = -0.415, y = 7.25, label = "tn") +
  annotate("text", x = 0.03, y = -1.5, label = "tn") +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
      # title= 'Rate of change / year '
      title = '')

  
price.cloud  
  
#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

f.legend<-g_legend(fixed.leg)
p.legend<-g_legend(post.leg)


library(patchwork)


(price.cloud ) / (f.legend) / (p.legend) +
  plot_layout(heights = c(10,0.5,0.5))




