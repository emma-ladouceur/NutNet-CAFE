
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
sl.fixed.p2 <-sl.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
                                    `b_trt.yNPK:year.y.m` < quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.973)) %>%
  mutate(sl.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sl',
         sl.trt_global_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sl.trt_upper_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sl.trt_lower_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(sl.trt.p,sl.trt_global_slope, sl.trt_upper_slope,sl.trt_lower_slope)

nrow(sl.fixed.p2)

sg.fixed.p2 <-sg.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sg.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sg.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(sg.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sl',
         sg.trt_global_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sg.trt_upper_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sg.trt_lower_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(sg.trt.p,sg.trt_global_slope, sg.trt_upper_slope,sg.trt_lower_slope)

nrow(sg.fixed.p2)

cde.fixed.p2 <-CDE.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(CDE.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(CDE.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(cde.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sl',
         cde.trt_global_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         cde.trt_upper_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         cde.trt_lower_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(cde.trt.p,cde.trt_global_slope, cde.trt_upper_slope,cde.trt_lower_slope)

nrow(cde.fixed.p2)

sloss.fixed.p2 <-sloss.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sloss.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sloss.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(sloss.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sl',
         sloss.trt_global_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sloss.trt_upper_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sloss.trt_lower_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],)  %>%
  select(sloss.trt.p,sloss.trt_global_slope, sloss.trt_upper_slope,sloss.trt_lower_slope)

nrow(sloss.fixed.p2)

sgain.fixed.p2 <-sgain.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sgain.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sgain.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(sgain.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sl',
         sgain.trt_global_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sgain.trt_upper_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sgain.trt_lower_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(sgain.trt.p,sgain.trt_global_slope, sgain.trt_upper_slope,sgain.trt_lower_slope)

nrow(sgain.fixed.p2)


loss.s<- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2)
gains.s<- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2)

colnames(cde.fixed.p2)
colnames(loss.s)
colnames(gains.s)

# cloud of 50 samples from posteriors from overall effects
cloud1<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.fixed.p2,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt.p  ), 
               colour= "#35274A",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.fixed.p2,aes(x=0, #persistent
                              y= cde.trt.p),
             colour="#35274A",size=0.1,alpha = .1) +
  geom_segment(data = loss.s,
               aes(x = 0,
                   xend = sloss.trt.p ,
                   y = 0,
                   yend = sl.trt.p ),
               colour= "#B40F20",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss.s, aes(x= sloss.trt.p, #loss
                              y=  sl.trt.p  ),
             colour="#B40F20",size=0.2,alpha = .1)+
  geom_segment(data = gains.s,
               aes(x = 0,
                   xend = sgain.trt.p ,
                   y = 0,
                   yend = sg.trt.p ),
               colour= "#3B9AB2",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = gains.s, aes(x= sgain.trt.p , #losses
                              y= sg.trt.p ) ,
             colour="#3B9AB2",
             size=0.2,alpha = .1)+
    labs(x = '',
         y = 'Rate of Change on Biomass Over Time',
         title= 'Varitaion in Overall Effect (95% CI of 1000 samples)')

View(loss.ss)
loss.ss <- loss.s %>% sample_n(50)
gains.ss <- gains.s  %>% sample_n(50)
cde.s <- cde.fixed.p2  %>% sample_n(50)

colnames(cde.s)
# cloud with fixed effects, all starting from 0
cloud2<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt.p ), 
               colour= "#35274A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.s,aes(x=0, #persistent
                                     y= cde.trt.p ),
             colour="#35274A",size=0.1,alpha = 0.4) +
  geom_segment(data = loss.ss,
               aes(x = 0,
                   xend = sloss.trt.p ,
                   y = 0,
                   yend = sl.trt.p  ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss.ss, aes(x= sloss.trt.p, #loss
                                y=  sl.trt.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = gains.ss,
               aes(x = 0,
                   xend = sgain.trt.p ,
                   y = 0,
                   yend = sg.trt.p ),
               colour= "#3B9AB2",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = gains.ss, aes(x= sgain.trt.p , #losses
                                 y= sg.trt.p ) ,
             colour="#3B9AB2",
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
  labs(x = '',
       y = '',
       title= 'Overall Effect (+ Variation in 50 samples-95% CI)')


# bind data sets togeher for this one-

loss.ss <- loss.s %>% sample_n(50) 
loss.ss$Vector="Losses"
gains.ss <- gains.s  %>% sample_n(50)
gains.ss$Vector="Gains"
cde.s <- cde.fixed.p2  %>% sample_n(50)
cde.s$Vector="Persistent Sp."
loss.gain <- loss.ss %>% bind_cols(gains.ss)

all.effs <- loss.gain %>% bind_cols(cde.s)

colnames(all.effs)


cloud3<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
  labs(x = 'Rate of Change on Species Over Time',
       y = 'Rate of Change on Biomass Over Time',
       title= 'Vector Style - (+ Varitaion in 50 samples - 95% CI)')


# cloud with fixed effects, vector style
cloud4<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
  labs(x = 'Rate of Change on Species Over Time',
       y = '',
       title= 'Vector Style - Overall Effect (+ Variation in 50 samples-95% CI)')

grid.arrange(cloud1,cloud2,cloud3,cloud4,nrow=2,ncol=2)

grid_arrange_shared_legend(cloud1,cloud2,cloud3,cloud4,nrow=2,ncol=2)




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
       title= 'From 0- Overall Effect (+ Variation in 50 samples-95% CI)')


cloud2

# cloud with fixed effects, vector style
cloud4<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
       title= 'Vector Style - Overall Effect (+ Variation in 50 samples-95% CI)')


grid_arrange_shared_legend(cloud2,cloud4,nrow=1,ncol=2)


# VERSION 3

sl.fixed.p2 <-sl.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(Y.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response='sl',group = 'Losses', axis= 'Y',
         Y.trt_global_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         Y.trt_upper_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         Y.trt_lower_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(response,axis,group,Y.trt.p, Y.trt_global_slope, Y.trt_upper_slope, Y.trt_lower_slope) %>%
  sample_n(50)

nrow(sl.fixed.p2)

sg.fixed.p2 <-sg.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sg.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sg.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(Y.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response= 'sg', group = 'Gains', axis= 'Y',
         Y.trt_global_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         Y.trt_upper_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         Y.trt_lower_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(response, axis,group, Y.trt.p, Y.trt_global_slope, Y.trt_upper_slope, Y.trt_lower_slope)%>%
  sample_n(50)

nrow(sg.fixed.p2)

cde.fixed.p2 <-CDE.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(CDE.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(CDE.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'cde',group='Persistent Sp.', axis= 'Y',
         trt_global_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         trt_upper_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
        trt_lower_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(response,axis,group,trt.p,trt_global_slope, trt_upper_slope,trt_lower_slope)%>%
  sample_n(50)

nrow(cde.fixed.p2)

sloss.fixed.p2 <-sloss.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sloss.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sloss.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(X.trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response ='sloss', group= 'Losses', axis= 'X',
         X.trt_global_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         X.trt_upper_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         X.trt_lower_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],)  %>%
  select(response,axis,group,X.trt.p, X.trt_global_slope, X.trt_upper_slope, X.trt_lower_slope)%>%
  sample_n(50)

nrow(sloss.fixed.p2)

sgain.fixed.p2 <-sgain.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sgain.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sgain.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
  mutate(trt.p=`b_trt.yNPK:year.y.m`) %>%
  mutate(response = 'sgain',group='Gains', axis='X',
         trt_global_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         trt_upper_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         trt_lower_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],) %>%
  select(response,axis,trt.p,trt_global_slope, trt_upper_slope,trt_lower_slope)%>%
  sample_n(50)

head(sgain.fixed.p2)
nrow(sgain.fixed.p2)

loss <-sl.fixed.p2 %>% bind_cols(sloss.fixed.p2 )
head(loss)
gains <-sg.fixed.p2 %>% bind_rows(sgain.fixed.p2 )
loss.gain <- loss %>% bind_rows(gains)
all.effs <- loss.gain %>% bind_rows(cde.fixed.p2)


colnames(all.effs)
head(all.effs)


ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data =  all.effs %>% filter(response == "Losses"),
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Y.trt.p, colour= group ), 
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs %>% filter(response == "sl"), 
             aes(x=0, #persistent
                              y= Y.trt.p,
                              colour=response ),size=0.1,alpha = 0.4) 
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  # geom_segment(data = all.effs,
  #              aes(x = 0,
  #                  xend = 0,
  #                  y = 0,
  #                  yend = trt_global_slope ,
  #                  colour=response), 
  #              size = 1.5,
  #              arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  # geom_point(data = all.effs,aes(x=0, #persistent
  #                             y=  trt_global_slope), size=0.1,alpha = 0.4) +
  # geom_errorbar(data = all.effs,aes(x=trt_global_slope,
  #                                  ymin = trt_lower_slope, ymax = trt_upper_slope),width=0, size = 0.55,alpha=0.3) +
  # geom_errorbarh(data = all.effs,aes(y=trt_global_slope,
  #                                   xmin = trt_lower_slope, xmax = trt_upper_slope),height=0,colour = response, size = 0.55,alpha=0.3) +
  #scale_color_manual(values=c("#3B9AB2","#B40F20","#35274A"))+
  # labs(x = 'Effect of NPK on Change in Species / Year',
  #      y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
  #      title= 'From 0- Overall Effect (+ Variation in 50 samples-95% CI)')




# cloud with fixed effects, vector style
cloud4<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
       title= 'Vector Style - Overall Effect (+ Variation in 50 samples-95% CI)')


grid.arrange(cloud2,cloud4,nrow=1,ncol=2)

