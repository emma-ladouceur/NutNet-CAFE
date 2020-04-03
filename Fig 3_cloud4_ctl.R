
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")

# FIGURE 3 CLOUD VERSION
# EXTRACT 100 POSTERIORS

# models
load('~/Dropbox/Projects/NutNet/Model_fits/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Model_fits/sl.n.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Model_fits/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s

# site level meta data for posrteriors
# calculated to site level details found in Climate_Data.R
# latitude and longitude dont match due to decimal rounding
# lat.x long.x is nutnet site, lat.y long.y is world clim
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(meta)
View(meta)

plot.rich.im_fixef <- fixef(plot.rich.g)
plot.bm.im_fixef <- fixef(plot.bm.s)
sl.trt.i_fixef <- fixef(sl.s)
sg.trt.i_fixef <- fixef(sg.s)
CDE.trt.i_fixef <- fixef(CDE.s)
sloss.trt.i_fixef <- fixef(s.loss.s)
sgain.trt.i_fixef <- fixef(s.gain.s)


sl.fixed.p<-posterior_samples(sl.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
CDE.fixed.p<-posterior_samples(CDE.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
rich.fixed.p<-posterior_samples(plot.rich.g, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(plot.bm.s, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
sloss.fixed.p<-posterior_samples(s.loss.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 

colnames(sl.fixed.p)
colnames(sl.trt.i_fixef)
sl.trt.i_fixef
sl.fixed.p2 <-sl.fixed.p %>% 
  filter(`b_Intercept` > quantile(sl.fixed.p$`b_Intercept`, probs=0.025),
                                    `b_Intercept` < quantile(sl.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sl.trt.p=`b_Intercept`) %>%
  mutate(response = 'sl',
         sl.trt_global_slope = sl.trt.i_fixef['Intercept','Estimate'],
         sl.trt_upper_slope = sl.trt.i_fixef['Intercept','Q97.5'],
         sl.trt_lower_slope = sl.trt.i_fixef['Intercept','Q2.5'],) %>%
  select(sl.trt.p,sl.trt_global_slope, sl.trt_upper_slope,sl.trt_lower_slope)

nrow(sl.fixed.p2)

sg.fixed.p2 <-sg.fixed.p %>% 
  filter(`b_Intercept` > quantile(sg.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sg.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sg.trt.p=`b_Intercept`) %>%
  mutate(response = 'sl',
         sg.trt_global_slope = sg.trt.i_fixef['Intercept','Estimate'],
         sg.trt_upper_slope = sg.trt.i_fixef['Intercept','Q97.5'],
         sg.trt_lower_slope = sg.trt.i_fixef['Intercept','Q2.5'],) %>%
  select(sg.trt.p,sg.trt_global_slope, sg.trt_upper_slope,sg.trt_lower_slope)

nrow(sg.fixed.p2)

cde.fixed.p2 <-CDE.fixed.p %>% 
  filter(`b_Intercept` > quantile(CDE.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(CDE.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(cde.trt.p=`b_Intercept`) %>%
  mutate(response = 'sl',
         cde.trt_global_slope = CDE.trt.i_fixef['Intercept','Estimate'],
         cde.trt_upper_slope = CDE.trt.i_fixef['Intercept','Q97.5'],
         cde.trt_lower_slope = CDE.trt.i_fixef['Intercept','Q2.5'],) %>%
  select(cde.trt.p,cde.trt_global_slope, cde.trt_upper_slope,cde.trt_lower_slope)

nrow(cde.fixed.p2)

sloss.fixed.p2 <-sloss.fixed.p %>% 
  filter(`b_Intercept` > quantile(sloss.fixed.p$`b_Intercept`, probs=0.025),
         `b_Intercept` < quantile(sloss.fixed.p$`b_Intercept`, probs=0.975)) %>%
  mutate(sloss.trt.p=`b_Intercept`) %>%
  mutate(response = 'sl',
         sloss.trt_global_slope = sloss.trt.i_fixef['Intercept','Estimate'],
         sloss.trt_upper_slope = sloss.trt.i_fixef['Intercept','Q97.5'],
         sloss.trt_lower_slope = sloss.trt.i_fixef['Intercept','Q2.5'],)  %>%
  select(sloss.trt.p,sloss.trt_global_slope, sloss.trt_upper_slope,sloss.trt_lower_slope)

nrow(sloss.fixed.p2)

sgain.fixed.p2 <-sgain.fixed.p %>% 
  filter(`b_Intercept` > quantile(sgain.fixed.p$`b_Intercept`, probs=0.026),
         `b_Intercept` < quantile(sgain.fixed.p$`b_Intercept`, probs=0.976)) %>%
  mutate(sgain.trt.p=`b_Intercept`) %>%
  mutate(response = 'sl',
         sgain.trt_global_slope = sgain.trt.i_fixef['Intercept','Estimate'],
         sgain.trt_upper_slope = sgain.trt.i_fixef['Intercept','Q97.5'],
         sgain.trt_lower_slope = sgain.trt.i_fixef['Intercept','Q2.5'],) %>%
  select(sgain.trt.p,sgain.trt_global_slope, sgain.trt_upper_slope,sgain.trt_lower_slope)

nrow(sgain.fixed.p2)


loss.s<- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2)
gains.s<- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2)



loss.ss <- loss.s %>% sample_n(50) 
loss.ss$Vector="Losses"
gains.ss <- gains.s  %>% sample_n(50)
gains.ss$Vector="Gains"
cde.s <- cde.fixed.p2  %>% sample_n(50)
cde.s$Vector="Persistent Sp."
loss.gain <- loss.ss %>% bind_cols(gains.ss)

all.effs <- loss.gain %>% bind_cols(cde.s)



#VERSION 2



cloud2<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt.p ,colour= Vector),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                              y= cde.trt.p,colour= Vector),size=0.1,alpha = 0.4) +
  geom_segment(data = loss.ss,
               aes(x = 0,
                   xend = sloss.trt.p ,
                   y = 0,
                   yend = sl.trt.p ,colour= Vector ),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss.ss, aes(x= sloss.trt.p, #loss
                                 y=  sl.trt.p ,colour= Vector ),size=0.2,alpha = 0.4)+
  geom_segment(data = gains.ss,
               aes(x = 0,
                   xend = sgain.trt.p ,
                   y = 0,
                   yend = sg.trt.p ,
                   colour= Vector), size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = gains.ss, aes(x= sgain.trt.p , #losses
                                  y= sg.trt.p ,
                                  colour= Vector) ,
             size=0.2,alpha = 0.4)+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt_global_slope ), 
               colour= "#35274A",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                              y=  cde.trt_global_slope),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_errorbar(data = cde.s,aes(x=0,
                                 ymin = cde.trt_lower_slope, ymax = cde.trt_upper_slope),width=0,colour = "#35274A", size = 0.55,alpha=0.3) +
  geom_segment(data = loss.ss,
               aes(x = 0,
                   xend = sloss.trt_global_slope,
                   y = 0,
                   yend = sl.trt_global_slope),
               colour= "#B40F20",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss.ss, aes(x= sloss.trt_global_slope, #loss
                                 y=  sl.trt_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_errorbar(data = loss.ss,aes(x=sloss.trt_global_slope,
                                   ymin = sl.trt_lower_slope, ymax = sl.trt_upper_slope),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = loss.ss,aes(y=sl.trt_global_slope,
                                    xmin = sloss.trt_lower_slope, xmax = sloss.trt_upper_slope),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_segment(data = gains.ss,
               aes(x = 0,
                   xend =  sgain.trt_global_slope,
                   y = 0,
                   yend =  sg.trt_global_slope),
               colour= "#3B9AB2",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = gains.ss, aes(x= sgain.trt_global_slope, #losses
                                  y=  sg.trt_global_slope ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  geom_errorbar(data = gains.ss,aes(x=sgain.trt_global_slope,
                                    ymin = sg.trt_lower_slope, ymax = sg.trt_upper_slope),width=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = gains.ss,aes(y=sg.trt_global_slope,
                                     xmin = sgain.trt_lower_slope, xmax = sgain.trt_upper_slope),height=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  scale_color_manual(values=c("#3B9AB2","#B40F20","#35274A"))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')


cloud2

# cloud with fixed effects, vector style
cloud4<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),  plot.margin = margin(0.3, 0.3, 1.5, 0.3, "cm"),)+
  geom_segment(data = all.effs,
               aes(x = sgain.trt.p,
                   xend = sgain.trt.p,
                   y = sg.trt.p,
                   yend = cde.trt.p ), 
               colour= "#35274A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                              y= cde.trt.p ),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt.p ,
                   y = 0,
                   yend = sl.trt.p  ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.p, #loss
                                  y=  sl.trt.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = sloss.trt.p,
                   xend = sgain.trt.p ,
                   y = sl.trt.p,
                   yend = sg.trt.p ),
               colour= "#3B9AB2",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sgain.trt.p , #losses
                                  y= sg.trt.p ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = all.effs,
               aes(x = sgain.trt_global_slope,
                   xend = sgain.trt_global_slope,
                   y = sg.trt_global_slope,
                   yend = cde.trt_global_slope ), 
               colour= "#35274A",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  cde.trt_global_slope),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_errorbar(data = all.effs,aes(x=sgain.trt_global_slope,
                                    ymin = cde.trt_lower_slope, ymax = cde.trt_upper_slope),width=0,colour = "#35274A", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt_global_slope,
                   y = 0,
                   yend = sl.trt_global_slope),
               colour= "#B40F20",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt_global_slope, #loss
                                  y=  sl.trt_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs,aes(x=sloss.trt_global_slope,
                                    ymin = sl.trt_lower_slope, ymax = sl.trt_upper_slope),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sl.trt_global_slope,
                                     xmin = sloss.trt_lower_slope, xmax = sloss.trt_upper_slope),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = sloss.trt_global_slope,
                   xend =  sgain.trt_global_slope,
                   y = sl.trt_global_slope,
                   yend =  sg.trt_global_slope),
               colour= "#3B9AB2",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sgain.trt_global_slope, #losses
                                  y=  sg.trt_global_slope ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs,aes(x=sgain.trt_global_slope,
                                    ymin = sg.trt_lower_slope, ymax = sg.trt_upper_slope),width=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sg.trt_global_slope,
                                     xmin = sgain.trt_lower_slope, xmax = sgain.trt_upper_slope),height=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')


cloud4

grid_arrange_shared_legend(cloud2,cloud4,nrow=1,ncol=2)

sl.trt.i_fixef
colnames(sl.fixed.p)
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
  select(sl.ctl.p,sl.ctl_global_slope, sl.ctl_upper_slope,sl.ctl_lower_slope,
         sl.trt.p,sl.trt_global_slope, sl.trt_upper_slope,sl.trt_lower_slope)

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
  select(sg.ctl.p,sg.ctl_global_slope, sg.ctl_upper_slope,sg.ctl_lower_slope,
         sg.trt.p,sg.trt_global_slope, sg.trt_upper_slope,sg.trt_lower_slope)
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
  select(cde.ctl.p,cde.ctl_global_slope, cde.ctl_upper_slope,cde.ctl_lower_slope,
         cde.trt.p,cde.trt_global_slope, cde.trt_upper_slope,cde.trt_lower_slope)
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
  select(sloss.ctl.p,sloss.ctl_global_slope, sloss.ctl_upper_slope,sloss.ctl_lower_slope,
         sloss.trt.p,sloss.trt_global_slope, sloss.trt_upper_slope,sloss.trt_lower_slope)
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
  select(sgain.ctl.p,sgain.ctl_global_slope, sgain.ctl_upper_slope,sgain.ctl_lower_slope,
         sgain.trt.p,sgain.trt_global_slope, sgain.trt_upper_slope,sgain.trt_lower_slope)
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
View(loss.s)
gains.s<- sg.ss %>% bind_cols(sgain.ss)
loss.ss$Vector="Losses"
gains.ss$Vector="Gains"
cde.s$Vector="Persistent Sp."
loss.gain <- loss.s %>% bind_cols(gains.s)

all.effs <- loss.gain %>% bind_cols(cde.s)

head(all.effs)
colnames(all.effs)
#CTL

# cloud with fixed effects, vector style
ctl.vec.cloud<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),  plot.margin = margin(0.3, 0.3, 1.5, 0.3, "cm"),)+
  geom_segment(data = all.effs,
               aes(x = sgain.ctl.p,
                   xend = sgain.ctl.p,
                   y = sg.ctl.p,
                   yend = cde.ctl.p ), 
               colour= "#35274A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                              y= cde.ctl.p ),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.ctl.p ,
                   y = 0,
                   yend = sl.ctl.p  ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.p, #loss
                                  y=  sl.ctl.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = sloss.ctl.p,
                   xend = sgain.ctl.p ,
                   y = sl.ctl.p,
                   yend = sg.ctl.p ),
               colour= "#3B9AB2",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sgain.ctl.p , #losses
                                  y= sg.ctl.p ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = all.effs,
               aes(x = sgain.ctl_global_slope,
                   xend = sgain.ctl_global_slope,
                   y = sg.ctl_global_slope,
                   yend = cde.ctl_global_slope ), 
               colour= "#35274A",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  cde.ctl_global_slope),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_errorbar(data = all.effs,aes(x=sgain.ctl_global_slope,
                                    ymin = cde.ctl_lower_slope, ymax = cde.ctl_upper_slope),width=0,colour = "#35274A", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.ctl_global_slope,
                   y = 0,
                   yend = sl.ctl_global_slope),
               colour= "#B40F20",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl_global_slope, #loss
                                  y=  sl.ctl_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs,aes(x=sloss.ctl_global_slope,
                                    ymin = sl.ctl_lower_slope, ymax = sl.ctl_upper_slope),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sl.ctl_global_slope,
                                     xmin = sloss.ctl_lower_slope, xmax = sloss.ctl_upper_slope),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = sloss.ctl_global_slope,
                   xend =  sgain.ctl_global_slope,
                   y = sl.ctl_global_slope,
                   yend =  sg.ctl_global_slope),
               colour= "#3B9AB2",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sgain.ctl_global_slope, #losses
                                  y=  sg.ctl_global_slope ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs,aes(x=sgain.ctl_global_slope,
                                    ymin = sg.ctl_lower_slope, ymax = sg.ctl_upper_slope),width=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sg.ctl_global_slope,
                                     xmin = sgain.ctl_lower_slope, xmax = sgain.ctl_upper_slope),height=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  labs(x = 'Change in Species in Control Plots',
       y = expression(paste('Change in Biomass (g/' ,m^2, ') in Control Plots')),
       title= 'a)')


ctl.vec.cloud



#TRT

# cloud with fixed effects, vector style
trt.vec.cloud<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),  plot.margin = margin(0.3, 0.3, 1.5, 0.3, "cm"),)+
  geom_segment(data = all.effs,
               aes(x = sgain.trt.p,
                   xend = sgain.trt.p,
                   y = sg.trt.p,
                   yend = cde.trt.p ), 
               colour= "#35274A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                              y= cde.trt.p ),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt.p ,
                   y = 0,
                   yend = sl.trt.p  ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.p, #loss
                                  y=  sl.trt.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = sloss.trt.p,
                   xend = sgain.trt.p ,
                   y = sl.trt.p,
                   yend = sg.trt.p ),
               colour= "#3B9AB2",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sgain.trt.p , #losses
                                  y= sg.trt.p ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = all.effs,
               aes(x = sgain.trt_global_slope,
                   xend = sgain.trt_global_slope,
                   y = sg.trt_global_slope,
                   yend = cde.trt_global_slope ), 
               colour= "#35274A",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  cde.trt_global_slope),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_errorbar(data = all.effs,aes(x=sgain.trt_global_slope,
                                    ymin = cde.trt_lower_slope, ymax = cde.trt_upper_slope),width=0,colour = "#35274A", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt_global_slope,
                   y = 0,
                   yend = sl.trt_global_slope),
               colour= "#B40F20",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt_global_slope, #loss
                                  y=  sl.trt_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs,aes(x=sloss.trt_global_slope,
                                    ymin = sl.trt_lower_slope, ymax = sl.trt_upper_slope),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sl.trt_global_slope,
                                     xmin = sloss.trt_lower_slope, xmax = sloss.trt_upper_slope),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_segment(data = all.effs,
               aes(x = sloss.trt_global_slope,
                   xend =  sgain.trt_global_slope,
                   y = sl.trt_global_slope,
                   yend =  sg.trt_global_slope),
               colour= "#3B9AB2",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sgain.trt_global_slope, #losses
                                  y=  sg.trt_global_slope ) ,
             colour="#3B9AB2",
             size=0.2,alpha = 0.4)+
  geom_errorbar(data = all.effs,aes(x=sgain.trt_global_slope,
                                    ymin = sg.trt_lower_slope, ymax = sg.trt_upper_slope),width=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=sg.trt_global_slope,
                                     xmin = sgain.trt_lower_slope, xmax = sgain.trt_upper_slope),height=0,colour = "#3B9AB2", size = 0.55,alpha=0.3) +
  labs(x = 'Effect of NPK on Change in Species',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')')),
       title= 'b)')


trt.vec.cloud

grid.arrange(ctl.vec.cloud,trt.vec.cloud,nrow=1,ncol=2)

