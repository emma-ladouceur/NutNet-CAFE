







library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")

# FIGURE 3 CLOUD VERSION
# EXTRACT 100 POSTERIORS

# models
load('~/Dropbox/Projects/NutNet/Model_fits/full/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Model_fits/full/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Model_fits/full/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/sgain.Rdata') # s.gain.s


summary(plot.rich.g)
summary(plot.bm.s)

summary(s.loss.n.s)
summary(s.gain.s)

summary(sl.s)
summary(sg.s)
summary(CDE.s)
# site level meta data for posrteriors
# calculated to site level details found in Climate_Data.R
# latitude and longitude dont match due to decimal rounding
# lat.x long.x is nutnet site, lat.y long.y is world clim
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(meta)
View(meta)

plot.rich.im_fixef <- fixef(plot.rich.g)
plot.bm.im_fixef <- fixef(plot.bm.s)
sl.trt.i_fixef <- fixef(sl.s)
sg.trt.i_fixef <- fixef(sg.s)
CDE.trt.i_fixef <- fixef(CDE.s)
sloss.trt.i_fixef <- fixef(s.loss.n.s)
sgain.trt.i_fixef <- fixef(s.gain.s)


sl.fixed.p<-posterior_samples(sl.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
CDE.fixed.p<-posterior_samples(CDE.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
rich.fixed.p<-posterior_samples(plot.rich.g, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(plot.bm.s, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
sloss.fixed.p<-posterior_samples(s.loss.n.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 


sl.fixed.p2 <-sl.fixed.p %>% 
  filter(`b_Intercept` > quantile(sl.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sl.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sl.ctl.p=`b_Intercept`) %>%
  mutate(response = 'sl',
         sl.ctl_global_slope = sl.trt.i_fixef['Intercept','Estimate'],
         sl.ctl_upper_slope = sl.trt.i_fixef['Intercept','Q97.5'],
         sl.ctl_lower_slope = sl.trt.i_fixef['Intercept','Q2.5'],) %>%
  filter(`b_trt.yNPK` > quantile(sl.fixed.p$`b_trt.yNPK`, probs=0.025),
         `b_trt.yNPK` < quantile(sl.fixed.p$`b_trt.yNPK`, probs=0.975)) %>%
  mutate(sl.trt.p=`b_trt.yNPK`) %>%
  mutate(response = 'sl',
         sl.trt_global_slope = sl.trt.i_fixef['trt.yNPK','Estimate'],
         sl.trt_upper_slope = sl.trt.i_fixef['trt.yNPK','Q97.5'],
         sl.trt_lower_slope = sl.trt.i_fixef['trt.yNPK','Q2.5'],) %>%
  filter(`b_trt.yNPK:year.y.m` > quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(sl.trt.rate.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sl',
         sl.trt.rate_global_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sl.trt.rate_upper_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sl.trt.rate_lower_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(sl.ctl.p,sl.ctl_global_slope, sl.ctl_upper_slope,sl.ctl_lower_slope,
         sl.trt.p,sl.trt_global_slope, sl.trt_upper_slope,sl.trt_lower_slope,
         sl.trt.rate.p,sl.trt.rate_global_slope, sl.trt.rate_upper_slope,sl.trt.rate_lower_slope)

nrow(sl.fixed.p2)



sg.fixed.p2 <-sg.fixed.p %>% 
  filter(`b_Intercept` > quantile(sg.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sg.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sg.ctl.p=`b_Intercept`) %>%
  mutate(response = 'sg',
         sg.ctl_global_slope = sg.trt.i_fixef['Intercept','Estimate'],
         sg.ctl_upper_slope = sg.trt.i_fixef['Intercept','Q97.5'],
         sg.ctl_lower_slope = sg.trt.i_fixef['Intercept','Q2.5'],) %>%
  filter(`b_trt.yNPK` > quantile(sg.fixed.p$`b_trt.yNPK`, probs=0.025),
         `b_trt.yNPK` < quantile(sg.fixed.p$`b_trt.yNPK`, probs=0.975)) %>%
  mutate(sg.trt.p=`b_trt.yNPK`) %>%
  mutate(response = 'sg',
         sg.trt_global_slope = sg.trt.i_fixef['trt.yNPK','Estimate'],
         sg.trt_upper_slope = sg.trt.i_fixef['trt.yNPK','Q97.5'],
         sg.trt_lower_slope = sg.trt.i_fixef['trt.yNPK','Q2.5'],) %>%
  filter(`b_trt.yNPK:year.y.m` > quantile(sg.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sg.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(sg.trt.rate.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sg',
         sg.trt.rate_global_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sg.trt.rate_upper_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sg.trt.rate_lower_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(sg.ctl.p,sg.ctl_global_slope, sg.ctl_upper_slope,sg.ctl_lower_slope,
         sg.trt.p,sg.trt_global_slope, sg.trt_upper_slope,sg.trt_lower_slope,
         sg.trt.rate.p,sg.trt.rate_global_slope, sg.trt.rate_upper_slope,sg.trt.rate_lower_slope)
nrow(sg.fixed.p2)

cde.fixed.p2 <-CDE.fixed.p %>% 
  filter(`b_Intercept` > quantile(CDE.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(CDE.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(cde.ctl.p=`b_Intercept`) %>%
  mutate(response = 'cde',
         cde.ctl_global_slope = CDE.trt.i_fixef['Intercept','Estimate'],
         cde.ctl_upper_slope = CDE.trt.i_fixef['Intercept','Q97.5'],
         cde.ctl_lower_slope = CDE.trt.i_fixef['Intercept','Q2.5'],) %>%
  filter(`b_trt.yNPK` > quantile(CDE.fixed.p$`b_trt.yNPK`, probs=0.025),
         `b_trt.yNPK` < quantile(CDE.fixed.p$`b_trt.yNPK`, probs=0.975)) %>%
  mutate(cde.trt.p=`b_trt.yNPK`) %>%
  mutate(response = 'cde',
         cde.trt_global_slope = CDE.trt.i_fixef['trt.yNPK','Estimate'],
         cde.trt_upper_slope = CDE.trt.i_fixef['trt.yNPK','Q97.5'],
         cde.trt_lower_slope = CDE.trt.i_fixef['trt.yNPK','Q2.5'],) %>%
  filter(`b_trt.yNPK:year.y.m` > quantile(CDE.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(CDE.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(cde.trt.rate.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'cde',
         cde.trt.rate_global_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         cde.trt.rate_upper_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         cde.trt.rate_lower_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(cde.ctl.p,cde.ctl_global_slope, cde.ctl_upper_slope,cde.ctl_lower_slope,
         cde.trt.p,cde.trt_global_slope, cde.trt_upper_slope,cde.trt_lower_slope,
         cde.trt.rate.p,cde.trt.rate_global_slope, cde.trt.rate_upper_slope,cde.trt.rate_lower_slope)
nrow(cde.fixed.p2)

sloss.fixed.p2 <-sloss.fixed.p %>% 
  filter(`b_Intercept` > quantile(sloss.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sloss.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sloss.ctl.p=`b_Intercept`) %>%
  mutate(response = 'sloss',
         sloss.ctl_global_slope = sloss.trt.i_fixef['Intercept','Estimate'],
         sloss.ctl_upper_slope = sloss.trt.i_fixef['Intercept','Q97.5'],
         sloss.ctl_lower_slope = sloss.trt.i_fixef['Intercept','Q2.5'],) %>%
  filter(`b_trt.yNPK` > quantile(sloss.fixed.p$`b_trt.yNPK`, probs=0.025),
         `b_trt.yNPK` < quantile(sloss.fixed.p$`b_trt.yNPK`, probs=0.975)) %>%
  mutate(sloss.trt.p=`b_trt.yNPK`) %>%
  mutate(response = 'sloss',
         sloss.trt_global_slope = sloss.trt.i_fixef['trt.yNPK','Estimate'],
         sloss.trt_upper_slope = sloss.trt.i_fixef['trt.yNPK','Q97.5'],
         sloss.trt_lower_slope = sloss.trt.i_fixef['trt.yNPK','Q2.5'],) %>%
  filter(`b_trt.yNPK:year.y.m` > quantile(sloss.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.027),
         `b_trt.yNPK:year.y.m` < quantile(sloss.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.977)) %>%
  mutate(sloss.trt.rate.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sloss',
         sloss.trt.rate_global_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sloss.trt.rate_upper_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sloss.trt.rate_lower_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],)  %>%
  select(sloss.ctl.p,sloss.ctl_global_slope, sloss.ctl_upper_slope,sloss.ctl_lower_slope,
         sloss.trt.p,sloss.trt_global_slope, sloss.trt_upper_slope,sloss.trt_lower_slope,
         sloss.trt.rate.p,sloss.trt.rate_global_slope, sloss.trt.rate_upper_slope,sloss.trt.rate_lower_slope)

nrow(sloss.fixed.p2)
View(sloss.fixed.p2)

sgain.fixed.p2 <-sgain.fixed.p %>% 
  filter(`b_Intercept` > quantile(sgain.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sgain.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sgain.ctl.p=`b_Intercept`) %>%
  mutate(response = 'sgain',
         sgain.ctl_global_slope = sgain.trt.i_fixef['Intercept','Estimate'],
         sgain.ctl_upper_slope = sgain.trt.i_fixef['Intercept','Q97.5'],
         sgain.ctl_lower_slope = sgain.trt.i_fixef['Intercept','Q2.5'],) %>%
  filter(`b_trt.yNPK` > quantile(sgain.fixed.p$`b_trt.yNPK`, probs=0.025),
         `b_trt.yNPK` < quantile(sgain.fixed.p$`b_trt.yNPK`, probs=0.975)) %>%
  mutate(sgain.trt.p=`b_trt.yNPK`) %>%
  mutate(response = 'sgain',
         sgain.trt_global_slope = sgain.trt.i_fixef['trt.yNPK','Estimate'],
         sgain.trt_upper_slope = sgain.trt.i_fixef['trt.yNPK','Q97.5'],
         sgain.trt_lower_slope = sgain.trt.i_fixef['trt.yNPK','Q2.5'],) %>%
  filter(`b_trt.yNPK:year.y.m` > quantile(sgain.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sgain.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(sgain.trt.rate.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sgain',
         sgain.trt.rate_global_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sgain.trt.rate_upper_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sgain.trt.rate_lower_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(sgain.ctl.p,sgain.ctl_global_slope, sgain.ctl_upper_slope,sgain.ctl_lower_slope,
         sgain.trt.p,sgain.trt_global_slope, sgain.trt_upper_slope,sgain.trt_lower_slope,
         sgain.trt.rate.p, sgain.trt.rate_global_slope, sgain.trt.rate_upper_slope,sgain.trt.rate_lower_slope )
nrow(sgain.fixed.p2)
View(sgain.fixed.p2)

sl.ss <- sl.fixed.p2 %>% sample_n(50) 
View(sl.ss)
sloss.ss <- sloss.fixed.p2 %>% sample_n(50) 
sloss.ss
sg.ss <- sg.fixed.p2 %>% sample_n(50) 
sgain.ss <- sgain.fixed.p2 %>% sample_n(50) 
cde.s <- cde.fixed.p2 %>% sample_n(50) 

loss.s<- sl.ss %>% bind_cols(sloss.ss)
gains.s<- sg.ss %>% bind_cols(sgain.ss)
loss.s$Vector="Losses"
gains.s$Vector="Gains"
cde.s$Vector="Persistent Sp."
loss.gain <- loss.s %>% bind_cols(gains.s)

all.effs <- loss.gain %>% bind_cols(cde.s)

head(all.effs)
colnames(all.effs)



# GET LEGENDS

fixed.leg<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt_global_slope,colour= Vector ), 
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                              y=  cde.trt_global_slope),
             colour="#816687",size=0.1,alpha = 0.4) +
  geom_segment(data = loss.s,
               aes(x = 0,
                   xend = sloss.trt_global_slope,
                   y = 0,
                   yend = sl.trt_global_slope,colour= Vector,),
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss.s, aes(x= sloss.trt_global_slope, #loss
                                y=  sl.trt_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = gains.s,
               aes(x = 0,
                   xend =  sgain.trt_global_slope,
                   y = 0,
                   yend =  sg.trt_global_slope, colour= Vector),
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = gains.s, aes(x= sgain.trt_global_slope, #losses
                                 y=  sg.trt_global_slope ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  scale_color_manual(name='Overall Effects',values=c("#3B9AB2","#B40F20","#816687"))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')


fixed.leg


post.leg<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt.p ,colour= Vector),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = loss.s,
               aes(x = 0,
                   xend = sloss.trt.p ,
                   y = 0,
                   yend = sl.trt.p ,colour= Vector ),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = gains.s,
               aes(x = 0,
                   xend = sgain.trt.p ,
                   y = 0,
                   yend = sg.trt.p ,
                   colour= Vector), size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_color_manual(name='Uncertainty',values=c("#3B9AB2","#B40F20","#816687"))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')


post.leg



# rates add



price.cloud.add<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt.rate.p ,
                   y = 0,
                   yend = sl.trt.rate.p  ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate.p, #loss
                                  y=  sl.trt.rate.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate.p,
                   xend = sloss.trt.rate.p+sgain.trt.rate.p ,
                   y = sl.trt.rate.p,
                   yend = sl.trt.rate.p+sg.trt.rate.p ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate.p+sgain.trt.rate.p , #losses
                                  y= sl.trt.rate.p+sg.trt.rate.p ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate.p+sgain.trt.rate.p,
                   xend = sloss.trt.rate.p+sgain.trt.rate.p,
                   y = sl.trt.rate.p+sg.trt.rate.p,
                   yend =sl.trt.rate.p+sg.trt.rate.p+ cde.trt.rate.p ),
               colour= "#816687",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y= sl.trt.rate.p+sg.trt.rate.p+cde.trt.rate.p ),
             colour="#816687",size=0.1,alpha = 0.4) +
  
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt.rate_global_slope,
                   y = 0,
                   yend = sl.trt.rate_global_slope),
               colour= "#B40F20",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate_global_slope, #loss
                                  y=  sl.trt.rate_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs,aes(x=sloss.trt.rate_global_slope,
                                    ymin = sl.trt.rate_lower_slope, ymax = sl.trt.rate_upper_slope),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sl.trt.rate_global_slope,
                                     xmin = sloss.trt.rate_lower_slope, xmax = sloss.trt.rate_upper_slope),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate_global_slope,
                   xend = sloss.trt.rate_global_slope + sgain.trt.rate_global_slope,
                   y = sl.trt.rate_global_slope,
                   yend = sl.trt.rate_global_slope+ sg.trt.rate_global_slope),
               colour= "#046C9A",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x=  (sloss.trt.rate_global_slope + sgain.trt.rate_global_slope)  , #losses
                                  y=  (sl.trt.rate_global_slope+sg.trt.rate_global_slope) ,
  ) ,
  colour="#046C9A",
  size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs, aes(x= sloss.trt.rate_global_slope+sgain.trt.rate_global_slope,
                                     ymin =  (sl.trt.rate_lower_slope+sg.trt.rate_lower_slope) , 
                                     ymax = (sl.trt.rate_upper_slope+sg.trt.rate_upper_slope)),
                width=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sl.trt.rate_global_slope+sg.trt.rate_global_slope,
                                     xmin =  (sloss.trt.rate_lower_slope+sgain.trt.rate_lower_slope)  ,
                                     xmax =  (sloss.trt.rate_upper_slope+sgain.trt.rate_upper_slope) ) ,
                 height=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate_global_slope+sgain.trt.rate_global_slope,
                   xend = sloss.trt.rate_global_slope+sgain.trt.rate_global_slope,
                   y = sl.trt.rate_global_slope+sg.trt.rate_global_slope,
                   yend = sl.trt.rate_global_slope+sg.trt.rate_global_slope+cde.trt.rate_global_slope ), 
               colour= "#816687",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  sl.trt.rate_global_slope+sg.trt.rate_global_slope+cde.trt.rate_global_slope),
             colour="#816687",size=0.1,alpha = 0.4) +
  geom_errorbar(data = all.effs,aes(x=sloss.trt.rate_global_slope+sgain.trt.rate_global_slope,
                                    ymin = sl.trt.rate_lower_slope+sg.trt.rate_lower_slope+cde.trt.rate_lower_slope, ymax = sl.trt.rate_upper_slope+sg.trt.rate_upper_slope+cde.trt.rate_upper_slope),width=0,colour = "#816687", size = 0.55,alpha=0.3) +
  
  ylim(-11,35) +
  xlim(-0.65,0)+
  labs(x = 'Effect of NPK on Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= ' ')

price.cloud.add





#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

f.legend<-g_legend(fixed.leg)
p.legend<-g_legend(post.leg)



cloud<-(price.cloud.add )/(f.legend)/(p.legend) +
  plot_layout(heights = c(10,0.5,0.5))

cloud





# PARTITIONS

sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



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

sgain.trt_fitted.npk$Model<-"Species Gain"
sgain.trt_fitted.ctl$Model<-"Species Gain"
sgain.trt_fitted.npk <- sgain.trt_fitted.npk %>% rename(Treatment = trt.y) 
sgain.trt_fitted.ctl <- sgain.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sgain<-bind_rows(sgain.trt_fitted.npk,sgain.trt_fitted.ctl)

fitted.sgain

fitted.sgain$Treatment <- factor(fitted.sgain$Treatment , levels=c("NPK","Control"))

# loss "#B40F20"
# gains "#046C9A"
# cde "#816687"

sgain.trtm<-ggplot()  +
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
  labs(x = 'Years',
       y = expression(paste('Species Gain')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#046C9A",
                                  drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = -0.5, r = 0.2, b = -0.5, l = 0.2, unit = "cm"))


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

sloss.trt_fitted.npk$Model<-"Species Loss"
sloss.trt_fitted.ctl$Model<-"Species Loss"
sloss.trt_fitted.npk <- sloss.trt_fitted.npk %>% rename(Treatment = trt.y) 
sloss.trt_fitted.ctl <- sloss.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sloss<-bind_rows(sloss.trt_fitted.npk,sloss.trt_fitted.ctl)

fitted.sloss

fitted.sloss$Treatment <- factor(fitted.sloss$Treatment , levels=c("NPK","Control"))




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
  labs(x = 'Years',
       y = expression(paste('Species Loss')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#B40F20", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = -0.5, r = 0.2, b = -0.5, l = 0.2, unit = "cm"))



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

sl.trt_fitted.npk$Model<-"Effect of Species Loss on Biomass"
sl.trt_fitted.ctl$Model<-"Effect of Species Loss on Biomass"
sl.trt_fitted.npk <- sl.trt_fitted.npk %>% rename(Treatment = trt.y) 
sl.trt_fitted.ctl <- sl.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sl<-bind_rows(sl.trt_fitted.npk,sl.trt_fitted.ctl)

fitted.sl

fitted.sl$Treatment <- factor(fitted.sl$Treatment , levels=c("NPK","Control"))

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
  labs(x='',
       #x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#B40F20", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.margin= margin(t = -0.5, r = 0.1, b = -0.5, l = 0, unit = "cm"))


sl.trtm



# SG
is.factor(sg.trt_fitted.npk$starting.richness)
is.factor(sg.trt_coef3$starting.richness)

sg.trt_fitted.npk<-sg.trt_fitted.npk[complete.cases(sg.trt_fitted.npk$starting.richness), ]
sg.trt_coef3<-sg.trt_coef3[complete.cases(sg.trt_coef3$starting.richness), ]

sg.trt_fitted.npk$starting.richness <- factor(sg.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.trt_coef3$starting.richness <- factor(sg.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sg.trt_coef3$xs<-1

sg.trt_fitted.npk$Model<-"Effect of Species Gain on Biomass"
sg.trt_fitted.ctl$Model<-"Effect of Species Gain on Biomass"
sg.trt_fitted.npk <- sg.trt_fitted.npk %>% rename(Treatment = trt.y) 
sg.trt_fitted.ctl <- sg.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sg<-bind_rows(sg.trt_fitted.npk,sg.trt_fitted.ctl)

fitted.sg

fitted.sg$Treatment <- factor(fitted.sg$Treatment , levels=c("NPK","Control"))

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
  labs(x = '',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= '', color='Treatment') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#046C9A", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none", plot.margin= margin(t = -0.5, r = 0, b = -0.5, l = 0, unit = "cm"))


sg.trtm

r.leg<-ggplot()  +
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
            aes(x = year.y, y = Estimate,linetype=Treatment),
            size = 1.5) +
  geom_ribbon(data = sg.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  #scale_y_continuous(trans = 'log', breaks=c(1,2,4,6,8,16,24,64,512,1024,2048,4096)) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(0,400) +
  labs(x = 'Years',
       y='',
       #y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= '', color='Treatment') +
  # scale_colour_manual(values = c("Control" = "black",
  #                                "NPK" = "#046C9A", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="bottom", plot.margin= margin(t = -0.5, r = 0, b = -0.5, l = 0, unit = "cm"))


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

cde_fitted.npk$Model<-"Persistent Species Change in Biomass"
cde_fitted.ctl$Model<-"Persistent Species Change in Biomass"
cde_fitted.npk <- cde_fitted.npk %>% rename(Treatment = trt.y) 
cde_fitted.ctl <- cde_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.cde<-bind_rows(cde_fitted.npk,cde_fitted.ctl)

fitted.cde

fitted.cde$Treatment <- factor(fitted.cde$Treatment , levels=c("NPK","Control"))

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
              fill="#816687",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.cde,
            aes(x = year.y, y = Estimate,linetype=Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = cde_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(1,3,6,9,12)) +
  ylim(-500,1000)+
  labs(x='',
        y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= '',  color='Starting Richness') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#816687",drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none", plot.margin= margin(t = -0.5, r = 0.2, b = -0.5, l = 0, unit = "cm"))

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

(sloss.trtm|sgain.trtm)/(sl.trtm|sg.trtm+ theme(legend.position="none")|cdem ) +
  plot_layout(heights = c(10,10))





# EFFECT PLOTS
load('~/Dropbox/Projects/NutNet/Data/effs.Rdata')



sloss.eff<-ggplot() + 
  geom_point(data =sloss.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sloss.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  #facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Loss'))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(-0.3,0)) +
  scale_color_manual(values = c("#B40F20")) +
  theme_bw(base_size=9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.title.x = element_text(size=8),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

sloss.eff

sgain.eff<-ggplot() + 
  geom_point(data =sgain.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sgain.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Gain'))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(-0.2,0)) +
  scale_color_manual(values = c("#046C9A")) +
  theme_bw(base_size=9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.title.x = element_text(size=8),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

sgain.eff



sl.eff<-ggplot() + 
  geom_point(data =sl.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sl.f, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') '))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-10)) +
  scale_color_manual(values = c("#B40F20")) +
  theme_bw(base_size=9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.title.x = element_text(size=8),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

sl.eff

sg.eff<-ggplot() + 
  geom_point(data =sg.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sg.f, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       #y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') '))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,8)) +
  scale_color_manual(values = c("#046C9A")) +
  theme_bw(base_size=9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.title.x = element_text(size=8),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

sg.eff


cde.eff<-ggplot() + 
  geom_point(data =cde.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = cde.f, aes(x = response,ymin = eff_lower,
                                  ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       #y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') '))
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,20)) +
  scale_color_manual(values = c("#816687")) +
  theme_bw(base_size=9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.title.x = element_text(size=8),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")

cde.eff




(rich.eff | bm.eff)/(sloss.eff | sgain.eff)/(sl.eff | sg.eff | cde.eff)





# INSETS

(sloss.eff | sgain.eff)/(sl.eff | sg.eff | cde.eff)

(sloss.trtm|sgain.trtm)/(sl.trtm|sg.trtm+ theme(legend.position="none")|cdem )/(rlegend) +
  plot_layout(heights = c(10,10,3.5))



sloss <- sloss.trtm +  annotation_custom(ggplotGrob(sloss.eff), xmin = 8, xmax = 12, 
                                         ymin = -20, ymax = -15)



sgain <- sgain.trtm +  annotation_custom(ggplotGrob(sgain.eff), xmin = 8, xmax = 12, 
                                         ymin = 15, ymax = 20)


sl <- sl.trtm +  annotation_custom(ggplotGrob(sl.eff), xmin = 8, xmax = 12, 
                                   ymin = -400, ymax = -300)



sg <- sg.trtm +  annotation_custom(ggplotGrob(sg.eff), xmin = 8, xmax = 12, 
                                   ymin = 300, ymax = 400)

cde <- cdem +  annotation_custom(ggplotGrob(cde.eff), xmin = 8, xmax = 12, 
                                 ymin = 600, ymax = 1000)



( sg | cde | sl )/(sgain | cloud | sloss + theme(legend.position="none") ) +
  plot_layout(heights = c(10,10))







# RICHNESS AND BIOMASS SUPPLEMENTARY FIGURE S4


plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

# plot <- plot %>% group_by(site_code) %>% filter(max.year >= 5) %>%
#   ungroup()

load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')

plot.rich_fitted.npk

plot.rich_fitted.npk$starting.richness <- factor(plot.rich_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.rich_coef3$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

plot.rich_fitted.npk$Model<-"Species Richness"
plot.rich_fitted.ctl$Model<-"Species Richness"
plot.rich_fitted.npk <- plot.rich_fitted.npk %>% rename(Treatment = trt) 
plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% rename(Treatment = trt) 
fitted.rich<-bind_rows(plot.rich_fitted.npk,plot.rich_fitted.ctl)

fitted.rich

fitted.rich$Treatment <- factor(fitted.rich$Treatment , levels=c("NPK","Control"))

View(plot.rich_fitted.npk)

View(plot.rich_coef3)


r1<-ggplot() +
  facet_wrap(~Model) +
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = rich), colour ="black", alpha=0.2,
             size = 1.3, position = position_jitter(width = 0.45 )) +
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
              fill="#F98400",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.rich,
            aes(x = year_trt, y = Estimate, linetype= Treatment,color=Treatment),
            size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(x='',
       #x = 'Years',
       y = ' Species richness', title= ' ') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#F98400", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.title = element_text(size=12),
                     plot.margin= margin(t = 0.1, r = 0.2, b = -0.5, l = 0.2, unit = "cm"))

r1

rmatrix<-ggplot() +
  facet_wrap(~site_code) +
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = rich,
                 colour = starting.richness), alpha=0.6,
             size = 1.3, position = position_jitter(width = 0.45 )) +
  geom_segment(data = plot.rich_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(x='',
       #x = 'Years',
       y = ' Species richness', title= 'Species richness  ', color= ' Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))

rmatrix


# BIOMASS

plot.bm_fitted.npk$starting.richness <- factor(plot.bm_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.bm_coef3$starting.richness <- factor(plot.bm_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

View(plot.bm_fitted.npk)
plot.bm_fitted.npk$Model<-"Biomass"
plot.bm_fitted.ctl$Model<-"Biomass"
plot.bm_fitted.npk <- plot.bm_fitted.npk %>% rename(Treatment = trt) 
plot.bm_fitted.ctl <- plot.bm_fitted.ctl %>% rename(Treatment = trt) 
fitted.bm<-bind_rows(plot.bm_fitted.npk,plot.bm_fitted.ctl)

fitted.bm

fitted.bm$Treatment <- factor(fitted.bm$Treatment , levels=c("NPK","Control"))

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
  labs(x='',
       #x = 'Years',
       y = expression(paste('Biomass (g/',m^2, ')')), title= '') +
  scale_colour_manual(values = c("Control" = "black",
                                 "NPK" = "#0B775E", drop =FALSE))+
  ylim(0,2000)+
  #scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500)) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.title = element_text(size=12),
                     plot.margin= margin(t = 0.1, r = 0.2, b =-0.5, l = 0.2, unit = "cm"))


b1

(r1 + b1)


bmatrix<-ggplot() +
  facet_wrap(~site_code)+
  geom_point(data = plot.bm_fitted.npk,
             aes(x = year_trt, y = plot.mass,
                 colour = starting.richness), alpha=0.6,
             size = .7, position = position_jitter(width = 0.45)) +
  geom_segment(data = plot.bm_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code,
                   colour = starting.richness),
               size = 0.7) +
  # uncertainy in fixed effect
  labs(x='',
       #x = 'Years',
       y = expression(paste('Biomass (g/',m^2, ')')), title= 'Biomass', color='Starting Richness') +
  ylim(0,1900)+
  # xlim(0,11) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))


bmatrix



rich.eff<-ggplot() + 
  geom_point(data =rich.f, aes(x = response, y = eff, color=response),size = 2) +
  geom_errorbar(data = rich.f, aes(x = response,ymin = eff_lower,
                                   ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  # facet_wrap(~Model)+
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-0.5)) +
  scale_color_manual(values = c("#F98400")) +
  theme_bw(base_size=9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.title.x = element_text(size=8),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")


rich.eff


bm.eff<-ggplot() + 
  geom_point(data =bm.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = bm.f, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  #facet_wrap(~Model)+
  labs(x = '',
       #y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') ')),
       y='') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#0B775E")) +
  theme_bw(base_size=9)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                              axis.text.y = element_text(size=6),
                              axis.title.x = element_text(size=8),
                              title=element_text(size=8),
                              strip.background = element_blank(),legend.position="none")
bm.eff



rich <- r1 +  annotation_custom(ggplotGrob(rich.eff), xmin = 8, xmax = 12, 
                                ymin = 30, ymax = 40)

bm <- b1+  annotation_custom(ggplotGrob(bm.eff), xmin = 8, xmax = 12, 
                             ymin = 1500 ,ymax = 2000)



(rich + bm ) /(rlegend) + plot_layout(heights = c(10,2))




