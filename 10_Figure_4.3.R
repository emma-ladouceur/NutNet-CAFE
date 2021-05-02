

# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 10_Figure 4.R
# This workflow uses data pulled out of Models below to produce Figure 4.3

# packages
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(patchwork)


# load modelobjects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.s

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.s
load('~/Desktop/test_mods/ps.Rdata') # ps.3_sigma


# site level meta data for posteriors
meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- meta %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

colnames(meta)

# Similarly  to '7_Model_Data_Posteriors.R' we 
# Extract 1000 posterior samples from Fixed Effects (Overall/Population/Global Effects) 
# for the price partitions
sloss.fixed.p <- posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sgain.fixed.p <- posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 
ps.fixed.p <- posterior_samples(ps.3_sigma, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 


cde.fixed.p <- posterior_samples(CDE.3, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) )
sl.fixed.p <- posterior_samples(sl.3, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sg.fixed.p <- posterior_samples(sg.3, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 

# except here, we take 50 samples of the posterior distribution from overall effects
# within the range of 95 % probability to represent uncertainty around these effects


sl.fixed.p2 <- sl.fixed.p %>% 
  mutate(sl.trt.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sl.ctl.p=`b_year.y.m`) %>%
  select(sl.ctl.p,
         sl.trt.p) 

sg.fixed.p2 <-sg.fixed.p %>% 
  mutate(sg.trt.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sg.ctl.p=`b_year.y.m`) %>%
  select(sg.ctl.p,
         sg.trt.p,)

cde.fixed.p2 <-cde.fixed.p %>% 
  mutate(cde.trt.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.p=`b_year.y.m`) %>%
  select(cde.ctl.p,
         cde.trt.p,) 


sloss.fixed.p2 <-sloss.fixed.p %>% 
  mutate(sloss.trt.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sloss.ctl.p=`b_year.y.m`) %>%
  select(sloss.ctl.p,
         sloss.trt.p,) 


sgain.fixed.p2 <-sgain.fixed.p %>% 
  mutate(sgain.trt.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sgain.ctl.p=`b_year.y.m`) %>%
  select(sgain.ctl.p,
         sgain.trt.p)

ps.fixed.p2 <-ps.fixed.p %>% 
  mutate(ps.trt.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(ps.ctl.p=`b_year.y.m`) %>%
  select(ps.ctl.p,
         ps.trt.p)


cde.s <- cde.fixed.p2  

loss.s <- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2) 

gains.s <- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2) 

loss.gain <- loss.s %>% bind_cols(gains.s) %>% bind_cols(ps.fixed.p2)

all.effs <- loss.gain %>% bind_cols(cde.fixed.p2)

head(all.effs)
nrow(all.effs)

effs_calc <- all.effs %>%
  # add posterior samples together
  mutate( # controls
    sloss.sgain.ctl.p = (sloss.ctl.p + sgain.ctl.p + ps.ctl.p),
    sloss.sgain.ps.ctl.p = (sloss.ctl.p + sgain.ctl.p),
    sl.sg.ctl.p = (sl.ctl.p + sg.ctl.p),
    sl.sg.cde.ctl.p = (sl.ctl.p + sg.ctl.p + cde.ctl.p),
    #  npk treatments
    sloss.sgain.trt.p = (sloss.trt.p + sgain.trt.p),
    sloss.sgain.ps.trt.p = (sloss.trt.p + sgain.trt.p + ps.trt.p),
    sl.sg.trt.p = (sl.trt.p + sg.trt.p),
    sl.sg.cde.trt.p = (sl.trt.p + sg.trt.p + cde.trt.p)) %>%
  # select columns we need
  select(sloss.ctl.p, sloss.sgain.ctl.p, sloss.sgain.ps.ctl.p, sl.ctl.p, sl.sg.ctl.p, sl.sg.cde.ctl.p,
         sloss.trt.p, sloss.sgain.trt.p, sloss.sgain.ps.trt.p, sl.trt.p, sl.sg.trt.p, sl.sg.cde.trt.p)

# take mean and quantiles within probs
#controls
sloss.ctl.effs <- effs_calc %>%
  select(sloss.ctl.p) %>%
  # sloss control
  mutate( sloss.ctl_global_slope = mean(sloss.ctl.p),
          sloss.ctl_lower_slope = quantile(sloss.ctl.p, probs=0.025),
          sloss.ctl_upper_slope = quantile(sloss.ctl.p, probs=0.975) )  %>%
  filter(sloss.ctl.p > quantile(sloss.ctl.p, probs=0.025),
         sloss.ctl.p < quantile(sloss.ctl.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.ctl.effs <- effs_calc %>% 
  select(sloss.sgain.ctl.p) %>%
  # sloss + sgain control
  mutate( sloss.sgain.ctl_global_slope = mean(sloss.sgain.ctl.p),
          sloss.sgain.ctl_lower_slope = quantile(sloss.sgain.ctl.p, probs=0.025),
          sloss.sgain.ctl_upper_slope = quantile(sloss.sgain.ctl.p, probs=0.975) )  %>%
  filter(sloss.sgain.ctl.p > quantile(sloss.sgain.ctl.p, probs=0.025),
         sloss.sgain.ctl.p < quantile(sloss.sgain.ctl.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.ps.ctl.effs <- effs_calc %>% 
  select(sloss.sgain.ps.ctl.p) %>%
  # sloss + sgain control
  mutate( sloss.sgain.ps.ctl_global_slope = mean(sloss.sgain.ps.ctl.p),
          sloss.sgain.ps.ctl_lower_slope = quantile(sloss.sgain.ps.ctl.p, probs=0.025),
          sloss.sgain.ps.ctl_upper_slope = quantile(sloss.sgain.ps.ctl.p, probs=0.975) )  %>%
  filter(sloss.sgain.ps.ctl.p > quantile(sloss.sgain.ps.ctl.p, probs=0.025),
         sloss.sgain.ps.ctl.p < quantile(sloss.sgain.ps.ctl.p, probs=0.975)) %>% sample_n(50) 

sl.ctl.effs <- effs_calc %>% 
  select(sl.ctl.p) %>%
  # sl control
  mutate( sl.ctl_global_slope = mean(sl.ctl.p),
          sl.ctl_lower_slope = quantile(sl.ctl.p, probs=0.025),
          sl.ctl_upper_slope = quantile(sl.ctl.p, probs=0.975) )  %>%
  filter(sl.ctl.p > quantile(sl.ctl.p, probs=0.025),
         sl.ctl.p < quantile(sl.ctl.p, probs=0.975)) %>% sample_n(50) 

sl.sg.ctl.effs <- effs_calc %>% 
  select(sl.sg.ctl.p) %>%
  # sl + sg control
  mutate( sl.sg.ctl_global_slope = mean(sl.sg.ctl.p),
          sl.sg.ctl_lower_slope = quantile(sl.sg.ctl.p, probs=0.025),
          sl.sg.ctl_upper_slope = quantile(sl.sg.ctl.p, probs=0.975) )  %>%
  filter(sl.sg.ctl.p > quantile(sl.sg.ctl.p, probs=0.025),
         sl.sg.ctl.p < quantile(sl.sg.ctl.p, probs=0.975)) %>% sample_n(50)

sl.sg.cde.ctl.effs <- effs_calc %>% 
  select(sl.sg.cde.ctl.p) %>%
  # sl + sg + cde control
  mutate( sl.sg.cde.ctl_global_slope = mean(sl.sg.cde.ctl.p),
          sl.sg.cde.ctl_lower_slope = quantile(sl.sg.cde.ctl.p, probs=0.025),
          sl.sg.cde.ctl_upper_slope = quantile(sl.sg.cde.ctl.p, probs=0.975) )  %>%
  filter(sl.sg.cde.ctl.p > quantile(sl.sg.cde.ctl.p, probs=0.025),
         sl.sg.cde.ctl.p < quantile(sl.sg.cde.ctl.p, probs=0.975)) %>% sample_n(50) 

# treatments
sloss.trt.effs <- effs_calc %>% 
  select(sloss.trt.p) %>%
  # sloss treatment
  mutate( sloss.trt_global_slope = mean(sloss.trt.p),
          sloss.trt_lower_slope = quantile(sloss.trt.p, probs=0.025),
          sloss.trt_upper_slope = quantile(sloss.trt.p, probs=0.975) )  %>%
  filter(sloss.trt.p > quantile(sloss.trt.p, probs=0.025),
         sloss.trt.p < quantile(sloss.trt.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.trt.effs <- effs_calc %>% 
  select(sloss.sgain.trt.p) %>%
  # sloss + sgain treatment
  mutate( sloss.sgain.trt_global_slope = mean(sloss.sgain.trt.p),
          sloss.sgain.trt_lower_slope = quantile(sloss.sgain.trt.p, probs=0.025),
          sloss.sgain.trt_upper_slope = quantile(sloss.sgain.trt.p, probs=0.975) )  %>%
  filter(sloss.sgain.trt.p > quantile(sloss.sgain.trt.p, probs=0.025),
         sloss.sgain.trt.p < quantile(sloss.sgain.trt.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.ps.trt.effs <- effs_calc %>% 
  select(sloss.sgain.ps.trt.p) %>%
  # sloss + sgain treatment
  mutate( sloss.sgain.ps.trt_global_slope = mean(sloss.sgain.ps.trt.p),
          sloss.sgain.ps.trt_lower_slope = quantile(sloss.sgain.ps.trt.p, probs=0.025),
          sloss.sgain.ps.trt_upper_slope = quantile(sloss.sgain.ps.trt.p, probs=0.975) )  %>%
  filter(sloss.sgain.ps.trt.p > quantile(sloss.sgain.ps.trt.p, probs=0.025),
         sloss.sgain.ps.trt.p < quantile(sloss.sgain.ps.trt.p, probs=0.975)) %>% sample_n(50) 


sl.trt.effs <- effs_calc %>% 
  select(sl.trt.p) %>%
  # sl treatment
  mutate( sl.trt_global_slope = mean(sl.trt.p),
          sl.trt_lower_slope = quantile(sl.trt.p, probs=0.025),
          sl.trt_upper_slope = quantile(sl.trt.p, probs=0.975) )  %>%
  filter(sl.trt.p > quantile(sl.trt.p, probs=0.025),
         sl.trt.p < quantile(sl.trt.p, probs=0.975)) %>% sample_n(50)

sl.sg.trt.effs <- effs_calc %>% 
  select(sl.sg.trt.p) %>%
  # sl + sg treatment
  mutate( sl.sg.trt_global_slope = mean(sl.sg.trt.p),
          sl.sg.trt_lower_slope = quantile(sl.sg.trt.p, probs=0.025),
          sl.sg.trt_upper_slope = quantile(sl.sg.trt.p, probs=0.975) )  %>%
  filter(sl.sg.trt.p > quantile(sl.sg.trt.p, probs=0.025),
         sl.sg.trt.p < quantile(sl.sg.trt.p, probs=0.975)) %>% sample_n(50) 

sl.sg.cde.trt.effs <- effs_calc %>% 
  select(sl.sg.cde.trt.p) %>%
  # sl + sg + cde treatment
  mutate( sl.sg.cde.trt_global_slope = mean(sl.sg.cde.trt.p),
          sl.sg.cde.trt_lower_slope = quantile(sl.sg.cde.trt.p, probs=0.025),
          sl.sg.cde.trt_upper_slope = quantile(sl.sg.cde.trt.p, probs=0.975) )  %>%
  filter(sl.sg.cde.trt.p > quantile(sl.sg.cde.trt.p, probs=0.025),
         sl.sg.cde.trt.p < quantile(sl.sg.cde.trt.p, probs=0.975)) %>% sample_n(50) 


added.p.effs <- sloss.ctl.effs %>% cbind(sloss.sgain.ctl.effs,sloss.sgain.ps.ctl.effs, sl.ctl.effs, sl.sg.ctl.effs, sl.sg.cde.ctl.effs,
                                         sloss.trt.effs, sloss.sgain.trt.effs,sloss.sgain.ps.trt.effs, sl.trt.effs, sl.sg.trt.effs, sl.sg.cde.trt.effs) %>% 
  select(sloss.ctl_global_slope, sloss.sgain.ctl.p, 
         sloss.sgain.ctl_global_slope, sloss.ctl.p,
         sloss.sgain.ps.ctl_global_slope, sloss.sgain.ps.ctl.p,
         sl.ctl_global_slope,sl.ctl.p,
         sl.sg.ctl_global_slope, sl.sg.ctl.p,
         sl.sg.cde.ctl_global_slope,sl.sg.cde.ctl.p,
         # treatments
         sloss.trt_global_slope, sloss.trt.p,
         sloss.sgain.trt_global_slope,sloss.sgain.trt.p,
         sloss.sgain.ps.trt_global_slope, sloss.sgain.ps.trt.p,
         sl.trt_global_slope,sl.trt.p,
         sl.sg.trt_global_slope, sl.sg.trt.p,
         sl.sg.cde.trt_global_slope,sl.sg.cde.trt.p) 


nrow(added.p.effs)
colnames(added.p.effs)


fig_4 <- ggplot() +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  # posterior uncertainty samples for Controls (small dashed lines)
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs,  # segments
               aes(x = 0,
                   xend = sloss.ctl.p,
                   y = 0,
                   yend = sl.ctl.p  ),
               colour= "#B40F20", linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow= arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.ctl.p, # points
                                      y=  sl.ctl.p  ),
             colour="black",size=0.2,alpha = 0.2) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs, # segment
               aes(x = sloss.ctl.p,
                   xend =  sloss.sgain.ctl.p ,
                   y = sl.ctl.p,
                   yend = sl.sg.ctl.p ),
               colour= "#046C9A",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.sgain.ctl.p, # points
                                      y=  sl.sg.ctl.p ) ,
             colour="black",
             size=0.2,alpha = 0.2) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs, # segment
               aes(x =  sloss.sgain.ctl.p,
                   xend =  sloss.sgain.ps.ctl.p,
                   y =  sl.sg.ctl.p,
                   yend = sl.sg.cde.ctl.p ), 
               colour=  "#F98400",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs,aes(x=sloss.sgain.ps.ctl.p, # points
                                     y= sl.sg.cde.ctl.p),
             colour="#F98400",size=0.1,alpha = 0.2) +
  # Overall effects in Controls (thick dashed lines) 
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.ctl_global_slope,
                                                sl.ctl_global_slope), # segments
               aes(x = 0,
                   xend = sloss.ctl_global_slope,
                   y = 0,
                   yend = sl.ctl_global_slope  ),
               colour= "#B40F20", linetype=2,
               size = 1.5, alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.ctl_global_slope, # points
                                      y=  sl.ctl_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% 
                 distinct(sloss.ctl_global_slope,sloss.sgain.ctl_global_slope,
                          sl.ctl_global_slope,sl.sg.ctl_global_slope),
               aes(x = sloss.ctl_global_slope,
                   xend = sloss.sgain.ctl_global_slope,
                   y = sl.ctl_global_slope,
                   yend =  sl.sg.ctl_global_slope),
               colour= "#046C9A",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.sgain.ctl_global_slope, # point
                                      y= sl.sg.ctl_global_slope ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% #segment
                 distinct( sloss.sgain.ctl_global_slope,
                           sloss.sgain.ps.ctl_global_slope,
                           sl.sg.ctl_global_slope,
                           sl.sg.cde.ctl_global_slope),
               aes(x = sloss.sgain.ctl_global_slope,
                   xend = sloss.sgain.ps.ctl_global_slope,
                   y = sl.sg.ctl_global_slope,
                   yend = sl.sg.cde.ctl_global_slope), 
               colour=  "#F98400",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs,aes(x=sloss.sgain.ps.ctl_global_slope, # points
                                     y=  sl.sg.cde.ctl_global_slope),
             colour="#F98400",size=0.1,alpha = 0.4) +
  # posterior uncertainty samples for treatments (NPK) (thin solid lines)
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs, # segment
               aes(x = 0,
                   xend = sloss.trt.p  ,
                   y = 0,
                   yend = sl.trt.p   ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.trt.p , # points
                                      y=  sl.trt.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs,  #segment
               aes(x = sloss.trt.p ,
                   xend = sloss.sgain.trt.p ,
                   y = sl.trt.p ,
                   yend = sl.sg.trt.p ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.sgain.trt.p , #points
                                      y= sl.sg.trt.p),
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs, #segment
               aes(x = sloss.sgain.trt.p,
                   xend = sloss.sgain.ps.trt.p,
                   y = sl.sg.trt.p,
                   yend = sl.sg.cde.trt.p ),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs,aes(x= sloss.sgain.ps.trt.p, # point
                                     y= sl.sg.cde.trt.p ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  #   # Overall effects in Treatments (NPK) (thick solid lines) 
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global_slope , sloss.ctl_global_slope,
                                                sl.trt_global_slope, sl.ctl_global_slope),
               aes(x = 0,
                   xend = sloss.trt_global_slope ,
                   y = 0,
                   yend = sl.trt_global_slope ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.trt_global_slope , #loss
                                      y=  sl.trt_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global_slope ,sloss.sgain.trt_global_slope, 
                                                sl.trt_global_slope, sl.sg.trt_global_slope ),
               aes(x = sloss.trt_global_slope,
                   xend = sloss.sgain.trt_global_slope,
                   y = sl.trt_global_slope ,
                   yend = sl.sg.trt_global_slope ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x=  sloss.sgain.trt_global_slope, #losses
                                      y=  sl.sg.trt_global_slope,
  ), colour="#046C9A", size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global_slope ,
                                                sloss.sgain.trt_global_slope, sloss.sgain.ps.trt_global_slope,
                                                sl.trt_global_slope, sl.sg.trt_global_slope,sl.sg.cde.trt_global_slope ),
               aes(x = sloss.sgain.trt_global_slope,
                   xend = sloss.sgain.ps.trt_global_slope,
                   y = sl.sg.trt_global_slope,
                   yend = sl.sg.cde.trt_global_slope ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs,aes(x=sloss.sgain.ps.trt_global_slope, #persistent
                                     y=  sl.sg.cde.trt_global_slope ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  scale_y_continuous(breaks=c(-10,-5,0,5,10,15)) +
  #scale_x_continuous(breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.05,0.1)) +
  annotate("text", x = -0.015, y = 0.75, label = "t0") +
  annotate("text", x = -0.415, y = 7.25, label = "tn") +
  annotate("text", x = 0.03, y = -1.5, label = "tn") +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       title = '')


fig_4
#WTF


# GET LEGENDS
cde.s <- cde.fixed.p2  %>%
  mutate( cde.ctl_global_slope = mean(cde.ctl.p),
          cde.trt_global_slope = mean(cde.trt.p)) %>%
  mutate( Vector = "Persistent Sp.")

loss.s <- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2) %>%
  mutate( sl.ctl_global_slope = mean(sl.ctl.p),
          sl.trt_global_slope = mean(sl.trt.p),
          sloss.ctl_global_slope = mean(sloss.ctl.p),
          sloss.trt_global_slope = mean(sloss.trt.p)) %>%
  mutate( Vector = "Losses")

gains.s <- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2) %>%
  mutate( sg.ctl_global_slope = mean(sg.ctl.p),
          sg.trt_global_slope = mean(sg.trt.p),
          sgain.ctl_global_slope = mean(sgain.ctl.p),
          sgain.trt_global_slope = mean(sgain.trt.p)) %>%
  mutate( Vector = "Gains")

# legend for overall effects (thick lines)
fixed.leg <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
                   yend = sl.trt_global_slope, colour= Vector,),
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
  scale_color_manual(name='Overall Effects',breaks=c("Losses","Gains","Persistent Sp."),
                     values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))


fixed.leg

# legend for posterior samples (thin lines)
post.leg <- ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
  scale_color_manual(name='Uncertainty',breaks=c("Losses","Gains","Persistent Sp."),
                     values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')


post.leg



# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

f.legend<-g_legend(fixed.leg)
p.legend<-g_legend(post.leg)



(fig_4 ) / (f.legend) / (p.legend) +
  plot_layout(heights = c(10,0.5,0.5))



