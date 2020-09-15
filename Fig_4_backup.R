
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


load('~/Dropbox/Projects/NutNet/Model_fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Model_fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sgain.Rdata') # s.gain.s



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
CDE.trt.i_fixef <- fixef(CDE.3)
sloss.trt.i_fixef <- fixef(s.loss.3)
sgain.trt.i_fixef <- fixef(s.gain.3)


sl.fixed.p<-posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
CDE.fixed.p<-posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )

sloss.fixed.p<-posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 


head(sl.fixed.p)
head(sl.trt.i_fixef)

sl.fixed.p2 <-sl.fixed.p %>% 
  filter(`b_Intercept` > quantile(sl.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sl.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sl.ctl.p=`b_Intercept`) %>%
  mutate(response = 'sl',
         sl.ctl_global_slope = sl.trt.i_fixef['Intercept','Estimate'],
         sl.ctl_upper_slope = sl.trt.i_fixef['Intercept','Q97.5'],
         sl.ctl_lower_slope = sl.trt.i_fixef['Intercept','Q2.5'],) %>%
  filter(`b_year.y.m` > quantile(sl.fixed.p$`b_year.y.m`, probs=0.025),
         `b_year.y.m` < quantile(sl.fixed.p$`b_year.y.m`, probs=0.975)) %>%
  mutate(sl.ctl.rate.p=`b_year.y.m`) %>%
  mutate(response = 'sl',
         sl.ctl.rate_global_slope = sl.trt.i_fixef['year.y.m','Estimate'],
         sl.ctl.rate_upper_slope = sl.trt.i_fixef['year.y.m','Q97.5'],
         sl.ctl.rate_lower_slope = sl.trt.i_fixef['year.y.m','Q2.5'],) %>%
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
         sl.ctl.rate.p,sl.ctl.rate_global_slope, sl.ctl.rate_upper_slope,sl.ctl.rate_lower_slope,
         sl.trt.p,sl.trt_global_slope, sl.trt_upper_slope,sl.trt_lower_slope,
         sl.trt.rate.p,sl.trt.rate_global_slope, sl.trt.rate_upper_slope,sl.trt.rate_lower_slope)

nrow(sl.fixed.p2)
sl.fixed.p2


sg.fixed.p2 <-sg.fixed.p %>% 
  filter(`b_Intercept` > quantile(sg.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sg.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sg.ctl.p=`b_Intercept`) %>%
  mutate(response = 'sg',
         sg.ctl_global_slope = sg.trt.i_fixef['Intercept','Estimate'],
         sg.ctl_upper_slope = sg.trt.i_fixef['Intercept','Q97.5'],
         sg.ctl_lower_slope = sg.trt.i_fixef['Intercept','Q2.5'],) %>%
  filter(`b_year.y.m` > quantile(sg.fixed.p$`b_year.y.m`, probs=0.025),
         `b_year.y.m` < quantile(sg.fixed.p$`b_year.y.m`, probs=0.975)) %>%
  mutate(sg.ctl.rate.p=`b_year.y.m`) %>%
  mutate(response = 'sl',
         sg.ctl.rate_global_slope = sg.trt.i_fixef['year.y.m','Estimate'],
         sg.ctl.rate_upper_slope = sg.trt.i_fixef['year.y.m','Q97.5'],
         sg.ctl.rate_lower_slope = sg.trt.i_fixef['year.y.m','Q2.5'],) %>%
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
         sg.ctl.rate.p,sg.ctl.rate_global_slope, sg.ctl.rate_upper_slope,sg.ctl.rate_lower_slope,
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
  filter(`b_year.y.m` > quantile(CDE.fixed.p$`b_year.y.m`, probs=0.025),
         `b_year.y.m` < quantile(CDE.fixed.p$`b_year.y.m`, probs=0.975)) %>%
  mutate(cde.ctl.rate.p=`b_year.y.m`) %>%
  mutate(response = 'sl',
         cde.ctl.rate_global_slope = CDE.trt.i_fixef['year.y.m','Estimate'],
         cde.ctl.rate_upper_slope = CDE.trt.i_fixef['year.y.m','Q97.5'],
         cde.ctl.rate_lower_slope = CDE.trt.i_fixef['year.y.m','Q2.5'],) %>%
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
         cde.ctl.rate.p,cde.ctl.rate_global_slope, cde.ctl.rate_upper_slope,cde.ctl.rate_lower_slope,
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
  filter(`b_year.y.m` > quantile(sloss.fixed.p$`b_year.y.m`, probs=0.025),
         `b_year.y.m` < quantile(sloss.fixed.p$`b_year.y.m`, probs=0.975)) %>%
  mutate(sloss.ctl.rate.p=`b_year.y.m`) %>%
  mutate(response = 'sl',
         sloss.ctl.rate_global_slope = sloss.trt.i_fixef['year.y.m','Estimate'],
         sloss.ctl.rate_upper_slope = sloss.trt.i_fixef['year.y.m','Q97.5'],
         sloss.ctl.rate_lower_slope = sloss.trt.i_fixef['year.y.m','Q2.5'],) %>%
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
         sloss.ctl.rate.p,sloss.ctl.rate_global_slope, sloss.ctl.rate_upper_slope,sloss.ctl.rate_lower_slope,
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
  filter(`b_year.y.m` > quantile(sgain.fixed.p$`b_year.y.m`, probs=0.025),
         `b_year.y.m` < quantile(sgain.fixed.p$`b_year.y.m`, probs=0.975)) %>%
  mutate(sgain.ctl.rate.p=`b_year.y.m`) %>%
  mutate(response = 'sl',
         sgain.ctl.rate_global_slope = sgain.trt.i_fixef['year.y.m','Estimate'],
         sgain.ctl.rate_upper_slope = sgain.trt.i_fixef['year.y.m','Q97.5'],
         sgain.ctl.rate_lower_slope = sgain.trt.i_fixef['year.y.m','Q2.5'],) %>%
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
         sgain.ctl.rate.p,sgain.ctl.rate_global_slope, sgain.ctl.rate_upper_slope,sgain.ctl.rate_lower_slope,
         sgain.trt.p,sgain.trt_global_slope, sgain.trt_upper_slope,sgain.trt_lower_slope,
         sgain.trt.rate.p, sgain.trt.rate_global_slope, sgain.trt.rate_upper_slope,sgain.trt.rate_lower_slope )
nrow(sgain.fixed.p2)
head(sgain.fixed.p2)

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
  scale_color_manual(name='Overall Effects',values=c("#3B9AB2","#B40F20","#F98400"))+
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
  scale_color_manual(name='Uncertainty',values=c("#3B9AB2","#B40F20","#F98400"))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')


post.leg



View(all.effs)



price.cloud.add<-ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       title= 'a) Rate of change in NPK treatments relative to control ')

price.cloud.add



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
                   xend = sloss.trt.rate.p + sloss.ctl.rate.p ,
                   y = 0,
                   yend = sl.trt.rate.p + sl.ctl.rate.p   ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate.p + sloss.ctl.rate.p, #loss
                                  y=  sl.trt.rate.p + sl.ctl.rate.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate.p + sloss.ctl.rate.p,
                   xend = (sloss.trt.rate.p+sloss.ctl.rate.p)+(sgain.trt.rate.p + sgain.ctl.rate.p) ,
                   y = sl.trt.rate.p + sl.ctl.rate.p,
                   yend = (sl.trt.rate.p+sl.ctl.rate.p)+(sg.trt.rate.p + sg.ctl.rate.p ) ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= (sloss.trt.rate.p+sloss.ctl.rate.p)+(sgain.trt.rate.p + sgain.ctl.rate.p) , #losses
                                  y= (sl.trt.rate.p+sl.ctl.rate.p)+(sg.trt.rate.p + sg.ctl.rate.p ) ),
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = (sloss.trt.rate.p+sloss.ctl.rate.p)+(sgain.trt.rate.p + sgain.ctl.rate.p),
                   xend = (sloss.trt.rate.p+sloss.ctl.rate.p)+(sgain.trt.rate.p+ sgain.ctl.rate.p),
                   y = (sl.trt.rate.p+sl.ctl.rate.p)+(sg.trt.rate.p + sg.ctl.rate.p),
                   yend =(sl.trt.rate.p+sl.ctl.rate.p)+(sg.trt.rate.p+sg.ctl.rate.p)+ (cde.trt.rate.p + cde.ctl.rate.p )),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y= (sl.trt.rate.p+sl.ctl.rate.p)+(sg.trt.rate.p+sg.ctl.rate.p)+(cde.trt.rate.p + cde.ctl.rate.p) ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = all.effs %>% distinct(sloss.trt.rate_global_slope , sloss.ctl.rate_global_slope,sl.trt.rate_global_slope, sl.ctl.rate_global_slope),
               aes(x = 0,
                   xend = sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope,
                   y = 0,
                   yend = sl.trt.rate_global_slope + sl.ctl.rate_global_slope),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope, #loss
                                  y=  sl.trt.rate_global_slope + sl.ctl.rate_global_slope  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  # geom_errorbar(data = all.effs,aes(x=sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope,
  #                                   ymin = sl.trt.rate_lower_slope + sl.ctl.rate_lower_slope, ymax = sl.trt.rate_upper_slope + sl.ctl.rate_upper_slope),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  # geom_errorbarh(data = all.effs,aes(y=sl.trt.rate_global_slope + sl.ctl.rate_global_slope,
  #                                    xmin = sloss.trt.rate_lower_slope + sloss.ctl.rate_lower_slope, xmax = sloss.trt.rate_upper_slope + sloss.ctl.rate_upper_slope),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs %>% distinct(sloss.trt.rate_global_slope , sloss.ctl.rate_global_slope,sgain.trt.rate_global_slope , sgain.ctl.rate_global_slope,sl.trt.rate_global_slope, sl.ctl.rate_global_slope, sg.trt.rate_global_slope , sg.ctl.rate_global_slope),
               aes(x = sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope,
                   xend = (sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope) + (sgain.trt.rate_global_slope + sgain.ctl.rate_global_slope),
                   y = sl.trt.rate_global_slope + sl.ctl.rate_global_slope,
                   yend = (sl.trt.rate_global_slope+ sl.ctl.rate_global_slope) + (sg.trt.rate_global_slope + sg.ctl.rate_global_slope) ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x=  (sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope) + (sgain.trt.rate_global_slope + sgain.ctl.rate_global_slope)  , #losses
                                  y=  (sl.trt.rate_global_slope+ sl.ctl.rate_global_slope) + (sg.trt.rate_global_slope + sg.ctl.rate_global_slope) ,
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
               aes(x = (sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope) + (sgain.trt.rate_global_slope + sgain.ctl.rate_global_slope),
                   xend = (sloss.trt.rate_global_slope+sloss.ctl.rate_global_slope)+(sgain.trt.rate_global_slope + sgain.ctl.rate_global_slope),
                   y = (sl.trt.rate_global_slope+ sl.ctl.rate_global_slope) + (sg.trt.rate_global_slope + sg.ctl.rate_global_slope),
                   yend = (sl.trt.rate_global_slope+ sl.ctl.rate_global_slope) + (sg.trt.rate_global_slope+ sg.ctl.rate_global_slope)+(cde.trt.rate_global_slope+ cde.ctl.rate_global_slope ) ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  (sl.trt.rate_global_slope+ sl.ctl.rate_global_slope) + (sg.trt.rate_global_slope+ sg.ctl.rate_global_slope)+(cde.trt.rate_global_slope+ cde.ctl.rate_global_slope ) ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  # geom_errorbar(data = all.effs,aes(x=(sloss.trt.rate_global_slope + sloss.ctl.rate_global_slope) + (sgain.trt.rate_global_slope + sgain.ctl.rate_global_slope),
  #                                   ymin = (sl.trt.rate_lower_slope+sl.ctl.rate_lower_slope)+(sg.trt.rate_lower_slope+ sg.ctl.rate_lower_slope)+(cde.trt.rate_lower_slope + cde.ctl.rate_lower_slope), ymax = (sl.trt.rate_upper_slope+ sl.ctl.rate_upper_slope)+(sg.trt.rate_upper_slope+sg.ctl.rate_upper_slope)+(cde.trt.rate_upper_slope+ cde.ctl.rate_upper_slope) ),width=0,colour = "#816687", size = 0.55,alpha=0.3) +
  
  # ylim(-11,35) +
  #  xlim(-0.65,0)+
  annotate("text", x = -0.015, y = 0.75, label = "Y0") +
  annotate("text", x = -0.415, y = 7.25, label = "Yn") +
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




(price.cloud.add  | price.cloud ) / (f.legend) / (p.legend) +
  plot_layout(heights = c(10,0.5,0.5))




