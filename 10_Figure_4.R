# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 10_Figure 4.R
# This workflow uses data pulled out of Models below to produce Figure 4

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

# site level meta data for posteriors
meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- meta %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()

colnames(meta)

# Similarly  to '7_Model_Data_Posteriors.R' we 
# Extract 1000 posterior samples from Fixed Effects (Overall/Population/Global Effects) 
# for the price partitions
sloss.fixed.p <- posterior_samples(sloss.3_p, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sgain.fixed.p <- posterior_samples(sgain.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 

cde.fixed.p <- posterior_samples(cde.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) )
sl.fixed.p <- posterior_samples(sl.3_p, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sg.fixed.p <- posterior_samples(sg.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 

# except here, we take 50 samples of the posterior distribution from overall effects
# within the range of 95 % probability to represent uncertainty around these effects

head(sl.fixed.p)

# here we take 1000 samps, then add all together, then take the mean
sl.fixed.p2 <- sl.fixed.p %>% 
  # slope
  mutate(sl.trt.slope.p =`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sl.ctl.slope.p =`b_year.y.m`) %>%
  #  trt
  mutate(sl.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sl.ctl.p =`b_Intercept`) %>%
  select(sl.trt.slope.p, sl.ctl.slope.p,
         sl.ctl.p, sl.trt.p)

sg.fixed.p2 <- sg.fixed.p %>% 
  mutate(sg.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sg.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(sg.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sg.ctl.p =`b_Intercept`) %>%
  select(sg.trt.slope.p, sg.ctl.slope.p,
         sg.ctl.p, sg.trt.p)

cde.fixed.p2 <- cde.fixed.p %>% 
  mutate(cde.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(cde.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(cde.ctl.p =`b_Intercept`) %>%
  select(cde.trt.slope.p, cde.ctl.slope.p,
         cde.ctl.p,  cde.trt.p) 


sloss.fixed.p2 <- sloss.fixed.p %>% 
  mutate(sloss.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sloss.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(sloss.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sloss.ctl.p =`b_Intercept`) %>%
  select(sloss.trt.slope.p, sloss.ctl.slope.p,
         sloss.ctl.p, sloss.trt.p) 


sgain.fixed.p2 <-sgain.fixed.p %>% 
  mutate(sgain.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sgain.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(sgain.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sgain.ctl.p =`b_Intercept`) %>%
  select(sgain.trt.slope.p, sgain.ctl.slope.p,
         sgain.ctl.p, sgain.trt.p)


cde.s <- cde.fixed.p2  

loss.s <- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2) 

gains.s <- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2) 

loss.gain <- loss.s %>% bind_cols(gains.s)

all.effs <- loss.gain %>% bind_cols(cde.fixed.p2)

head(all.effs)
nrow(all.effs)

effs_calc <- all.effs %>%
  # add posterior samples together
  mutate( # controls
          sloss.sgain.ctl.p = (sloss.ctl.p + sgain.ctl.p),
          sl.sg.ctl.p = (sl.ctl.p + sg.ctl.p),
          sl.sg.cde.ctl.p = (sl.ctl.p + sg.ctl.p + cde.ctl.p),
          #  npk treatments
          sloss.sgain.trt.p = (sloss.trt.p + sgain.trt.p),
          sl.sg.trt.p = (sl.trt.p + sg.trt.p),
          sl.sg.cde.trt.p = (sl.trt.p + sg.trt.p + cde.trt.p)) %>%
  # select columns we need
  select(sloss.ctl.p, sloss.sgain.ctl.p, sl.ctl.p, sl.sg.ctl.p, sl.sg.cde.ctl.p,
         sloss.trt.p, sloss.sgain.trt.p, sl.trt.p, sl.sg.trt.p, sl.sg.cde.trt.p)

# take mean and quantiles within probs
#controls
sloss.ctl.effs <- effs_calc %>%
  select(sloss.ctl.p) %>%
  # sloss control
  mutate( sloss.ctl_global = mean(sloss.ctl.p),
          sloss.ctl_lower = quantile(sloss.ctl.p, probs=0.025),
          sloss.ctl_upper = quantile(sloss.ctl.p, probs=0.975) )  %>%
  filter(sloss.ctl.p > quantile(sloss.ctl.p, probs=0.025),
         sloss.ctl.p < quantile(sloss.ctl.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.ctl.effs <- effs_calc %>% 
  select(sloss.sgain.ctl.p) %>%
  # sloss + sgain control
  mutate( sloss.sgain.ctl_global = mean(sloss.sgain.ctl.p),
          sloss.sgain.ctl_lower = quantile(sloss.sgain.ctl.p, probs=0.025),
          sloss.sgain.ctl_upper = quantile(sloss.sgain.ctl.p, probs=0.975) )  %>%
  filter(sloss.sgain.ctl.p > quantile(sloss.sgain.ctl.p, probs=0.025),
         sloss.sgain.ctl.p < quantile(sloss.sgain.ctl.p, probs=0.975)) %>% sample_n(50) 

sl.ctl.effs <- effs_calc %>% 
  select(sl.ctl.p) %>%
  # sl control
  mutate( sl.ctl_global = mean(sl.ctl.p),
          sl.ctl_lower = quantile(sl.ctl.p, probs=0.025),
          sl.ctl_upper = quantile(sl.ctl.p, probs=0.975) )  %>%
  filter(sl.ctl.p > quantile(sl.ctl.p, probs=0.025),
         sl.ctl.p < quantile(sl.ctl.p, probs=0.975)) %>% sample_n(50) 

sl.sg.ctl.effs <- effs_calc %>% 
  select(sl.sg.ctl.p) %>%
  # sl + sg control
  mutate( sl.sg.ctl_global = mean(sl.sg.ctl.p),
          sl.sg.ctl_lower = quantile(sl.sg.ctl.p, probs=0.025),
          sl.sg.ctl_upper = quantile(sl.sg.ctl.p, probs=0.975) )  %>%
  filter(sl.sg.ctl.p > quantile(sl.sg.ctl.p, probs=0.025),
         sl.sg.ctl.p < quantile(sl.sg.ctl.p, probs=0.975)) %>% sample_n(50)

sl.sg.cde.ctl.effs <- effs_calc %>% 
  select(sl.sg.cde.ctl.p) %>%
  # sl + sg + cde control
  mutate( sl.sg.cde.ctl_global = mean(sl.sg.cde.ctl.p),
          sl.sg.cde.ctl_lower = quantile(sl.sg.cde.ctl.p, probs=0.025),
          sl.sg.cde.ctl_upper = quantile(sl.sg.cde.ctl.p, probs=0.975) )  %>%
  filter(sl.sg.cde.ctl.p > quantile(sl.sg.cde.ctl.p, probs=0.025),
         sl.sg.cde.ctl.p < quantile(sl.sg.cde.ctl.p, probs=0.975)) %>% sample_n(50) 

# treatments
sloss.trt.effs <- effs_calc %>% 
  select(sloss.trt.p) %>%
  # sloss treatment
  mutate( sloss.trt_global = mean(sloss.trt.p),
          sloss.trt_lower = quantile(sloss.trt.p, probs=0.025),
          sloss.trt_upper = quantile(sloss.trt.p, probs=0.975) )  %>%
  filter(sloss.trt.p > quantile(sloss.trt.p, probs=0.025),
         sloss.trt.p < quantile(sloss.trt.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.trt.effs <- effs_calc %>% 
  select(sloss.sgain.trt.p) %>%
  # sloss + sgain treatment
  mutate( sloss.sgain.trt_global = mean(sloss.sgain.trt.p),
          sloss.sgain.trt_lower = quantile(sloss.sgain.trt.p, probs=0.025),
          sloss.sgain.trt_upper = quantile(sloss.sgain.trt.p, probs=0.975) )  %>%
  filter(sloss.sgain.trt.p > quantile(sloss.sgain.trt.p, probs=0.025),
         sloss.sgain.trt.p < quantile(sloss.sgain.trt.p, probs=0.975)) %>% sample_n(50) 

sl.trt.effs <- effs_calc %>% 
  select(sl.trt.p) %>%
  # sl treatment
  mutate( sl.trt_global = mean(sl.trt.p),
          sl.trt_lower = quantile(sl.trt.p, probs=0.025),
          sl.trt_upper = quantile(sl.trt.p, probs=0.975) )  %>%
  filter(sl.trt.p > quantile(sl.trt.p, probs=0.025),
         sl.trt.p < quantile(sl.trt.p, probs=0.975)) %>% sample_n(50)

sl.sg.trt.effs <- effs_calc %>% 
  select(sl.sg.trt.p) %>%
  # sl + sg treatment
  mutate( sl.sg.trt_global = mean(sl.sg.trt.p),
          sl.sg.trt_lower = quantile(sl.sg.trt.p, probs=0.025),
          sl.sg.trt_upper = quantile(sl.sg.trt.p, probs=0.975) )  %>%
  filter(sl.sg.trt.p > quantile(sl.sg.trt.p, probs=0.025),
         sl.sg.trt.p < quantile(sl.sg.trt.p, probs=0.975)) %>% sample_n(50) 

sl.sg.cde.trt.effs <- effs_calc %>% 
  select(sl.sg.cde.trt.p) %>%
  # sl + sg + cde treatment
  mutate( sl.sg.cde.trt_global = mean(sl.sg.cde.trt.p),
          sl.sg.cde.trt_lower = quantile(sl.sg.cde.trt.p, probs=0.025),
          sl.sg.cde.trt_upper = quantile(sl.sg.cde.trt.p, probs=0.975) )  %>%
  filter(sl.sg.cde.trt.p > quantile(sl.sg.cde.trt.p, probs=0.025),
         sl.sg.cde.trt.p < quantile(sl.sg.cde.trt.p, probs=0.975)) %>% sample_n(50) 


added.p.effs <- sloss.ctl.effs %>% cbind(sloss.sgain.ctl.effs, sl.ctl.effs, sl.sg.ctl.effs, sl.sg.cde.ctl.effs,
                                         sloss.trt.effs, sloss.sgain.trt.effs, sl.trt.effs, sl.sg.trt.effs, sl.sg.cde.trt.effs) %>% 
  select(sloss.ctl_global, sloss.sgain.ctl.p, 
         sloss.sgain.ctl_global, sloss.ctl.p,
         sl.ctl_global,sl.ctl.p,
         sl.sg.ctl_global, sl.sg.ctl.p,
         sl.sg.cde.ctl_global,sl.sg.cde.ctl.p,
         # treatments
         sloss.trt_global, sloss.trt.p,
         sloss.sgain.trt_global,sloss.sgain.trt.p,
         sl.trt_global,sl.trt.p,
         sl.sg.trt_global, sl.sg.trt.p,
         sl.sg.cde.trt_global,sl.sg.cde.trt.p) 
  
  
nrow(added.p.effs)
colnames(added.p.effs)

# Fig 4a: Effects
#____________________________________

fig_4a_trt <- ggplot() +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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
                   xend = sloss.sgain.trt.p,
                   y = sl.sg.trt.p,
                   yend = sl.sg.cde.trt.p ),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs,aes(x=0, # point
                                 y= sl.sg.cde.trt.p ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  #   # Overall effects in Treatments (NPK) (thick solid lines)
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global , sloss.ctl_global,
                                                sl.trt_global, sl.ctl_global),
               aes(x = 0,
                   xend = sloss.trt_global ,
                   y = 0,
                   yend = sl.trt_global ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.trt_global , #loss
                                  y=  sl.trt_global ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global ,sloss.sgain.trt_global,
                                                sl.trt_global, sl.sg.trt_global ),
               aes(x = sloss.trt_global,
                   xend = sloss.sgain.trt_global,
                   y = sl.trt_global ,
                   yend = sl.sg.trt_global ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x=  sloss.sgain.trt_global, #losses
                                  y=  sl.sg.trt_global,
  ), colour="#046C9A", size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global ,sloss.sgain.trt_global,
                                                sl.trt_global, sl.sg.trt_global,sl.sg.cde.trt_global ),
               aes(x = sloss.sgain.trt_global,
                   xend = sloss.sgain.trt_global,
                   y = sl.sg.trt_global,
                   yend = sl.sg.cde.trt_global ),
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs,aes(x=0, #persistent
                                 y=  sl.sg.cde.trt_global ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  ##############################################
  #posterior uncertainty samples for Controls (small dashed lines)
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
                   xend =  sloss.sgain.ctl.p,
                   y =  sl.sg.ctl.p,
                   yend = sl.sg.cde.ctl.p ),
               colour=  "#F98400",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs,aes(x=0, # points
                                     y= sl.sg.cde.ctl.p),
             colour="#F98400",size=0.1,alpha = 0.2) +
  # Overall effects in Controls (thick dashed lines)
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.ctl_global,
                                                sl.ctl_global), # segments
               aes(x = 0,
                   xend = sloss.ctl_global,
                   y = 0,
                   yend = sl.ctl_global  ),
               colour= "#B40F20", linetype=2,
               size = 1.5, alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.ctl_global, # points
                                      y=  sl.ctl_global ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>%
                 distinct(sloss.ctl_global,sloss.sgain.ctl_global,
                          sl.ctl_global,sl.sg.ctl_global),
               aes(x = sloss.ctl_global,
                   xend = sloss.sgain.ctl_global,
                   y = sl.ctl_global,
                   yend =  sl.sg.ctl_global),
               colour= "#046C9A",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs, aes(x= sloss.sgain.ctl_global, # point
                                      y= sl.sg.ctl_global ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% #segment
                 distinct( sloss.sgain.ctl_global,sl.sg.ctl_global,
                           sl.sg.cde.ctl_global),
               aes(x = sloss.sgain.ctl_global,
                   xend = sloss.sgain.ctl_global,
                   y = sl.sg.ctl_global,
                   yend = sl.sg.cde.ctl_global),
               colour=  "#F98400",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs,aes(x=0, # points
                                     y=  sl.sg.cde.ctl_global),
             colour="#F98400",size=0.1,alpha = 0.4) +
  #scale_y_continuous(breaks=c(-10,-5,0,5,10,15)) +
  #scale_x_continuous(breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.05,0.1)) +
  # annotate("text", x = -0.015, y = 0.75, label = "t0") +
  # annotate("text", x = -0.415, y = 7.25, label = "tn") +
  # annotate("text", x = 0.03, y = -1.5, label = "tn") +
  labs(x = 'Average change in species',
       y = expression(paste('Average change in biomass (g/' ,m^2, ')')),
       subtitle = 'a) Average change in species and biomass')


fig_4a_trt


# fig_4a_ctl <- ggplot() +
#   geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
#  # posterior uncertainty samples for Controls (small dashed lines)
#   # species loss (x-axis) & SL (y-axis)
#   geom_segment(data = added.p.effs,  # segments
#                aes(x = 0,
#                    xend = sloss.ctl.p,
#                    y = 0,
#                    yend = sl.ctl.p  ),
#                colour= "#B40F20", linetype=2,
#                size = 0.2,  alpha = 0.2,
#                arrow= arrow(type="closed",length=unit(0.1,"cm"))) +
#   geom_point(data = added.p.effs, aes(x= sloss.ctl.p, # points
#                                       y=  sl.ctl.p  ),
#              colour="black",size=0.2,alpha = 0.2) +
#   # species gain (x-axis) & SG (y-axis)
#   geom_segment(data = added.p.effs, # segment
#                aes(x = sloss.ctl.p,
#                    xend =  sloss.sgain.ctl.p ,
#                    y = sl.ctl.p,
#                    yend = sl.sg.ctl.p ),
#                colour= "#046C9A",linetype=2,
#                size = 0.2,  alpha = 0.2,
#                arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
#   geom_point(data = added.p.effs, aes(x= sloss.sgain.ctl.p, # points
#                                       y=  sl.sg.ctl.p ) ,
#              colour="black",
#              size=0.2,alpha = 0.2) +
#   # persistent species (cde/ps) (y axis only)
#   geom_segment(data = added.p.effs, # segment
#                aes(x =  sloss.sgain.ctl.p,
#                    xend =  sloss.sgain.ctl.p,
#                    y =  sl.sg.ctl.p,
#                    yend = sl.sg.cde.ctl.p ), 
#                colour=  "#F98400",linetype=2,
#                size = 0.2,  alpha = 0.2,
#                arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
#   geom_point(data = added.p.effs,aes(x=0, # points
#                                      y= sl.sg.cde.ctl.p),
#              colour="#F98400",size=0.1,alpha = 0.2) +
#   # Overall effects in Controls (thick dashed lines) 
#   # species loss (x-axis) & SL (y-axis)
#   geom_segment(data = added.p.effs %>% distinct(sloss.ctl_global,
#                                                 sl.ctl_global), # segments
#                aes(x = 0,
#                    xend = sloss.ctl_global,
#                    y = 0,
#                    yend = sl.ctl_global  ),
#                colour= "#B40F20", linetype=2,
#                size = 1.5, alpha=0.7,
#                arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
#   geom_point(data = added.p.effs, aes(x= sloss.ctl_global, # points
#                                       y=  sl.ctl_global ),
#              colour="#B40F20",size=0.2,alpha = 0.4) +
#   # species gain (x-axis) & SG (y-axis)
#   geom_segment(data = added.p.effs %>% 
#                  distinct(sloss.ctl_global,sloss.sgain.ctl_global,
#                           sl.ctl_global,sl.sg.ctl_global),
#                aes(x = sloss.ctl_global,
#                    xend = sloss.sgain.ctl_global,
#                    y = sl.ctl_global,
#                    yend =  sl.sg.ctl_global),
#                colour= "#046C9A",linetype=2,
#                size = 1.5,alpha=0.7,
#                arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
#   geom_point(data = added.p.effs, aes(x= sloss.sgain.ctl_global, # point
#                                       y= sl.sg.ctl_global ) ,
#              colour="#046C9A",
#              size=0.2,alpha = 0.4) +
#   # persistent species (cde/ps) (y axis only)
#   geom_segment(data = added.p.effs %>% #segment
#                  distinct( sloss.sgain.ctl_global,sl.sg.ctl_global,
#                            sl.sg.cde.ctl_global),
#                aes(x = sloss.sgain.ctl_global,
#                    xend = sloss.sgain.ctl_global,
#                    y = sl.sg.ctl_global,
#                    yend = sl.sg.cde.ctl_global), 
#                colour=  "#F98400",linetype=2,
#                size = 1.5,alpha=0.7,
#                arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
#   geom_point(data = added.p.effs,aes(x=0, # points
#                                      y=  sl.sg.cde.ctl_global),
#              colour="#F98400",size=0.1,alpha = 0.4) +
#   labs(#x = 'Change in species',
#     x='', y= ''
#        #y = expression(paste('Change in biomass (g/' ,m^2, ')')),
#        #subtitle = ''
#        ) +   theme_classic(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                                           plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
#                                           strip.background = element_blank(),legend.position="none")
# 
# 
# fig_4a_ctl
# 
# 
# fig_4a <- fig_4a_trt +  annotation_custom(ggplotGrob(fig_4a_ctl), xmin = -6.5, xmax = -3.5, 
#                                         ymin = 40, ymax = 160)
# 
# fig_4a

# GET LEGENDS
cde.s <- cde.fixed.p2  %>%
  mutate( Control = mean(cde.ctl.p),
         NPK = mean(cde.trt.p)) %>%
  select(-cde.ctl.p,-cde.trt.p) %>% distinct() %>%
  gather(Treatment, Estimate, Control:NPK) %>% 
  mutate( Vector = "Persistent Sp.")


loss.s <- sl.fixed.p2 %>% 
  mutate( Control = mean(sl.ctl.p),
          NPK = mean(sl.trt.p)) %>%
  select(-sl.ctl.p,-sl.trt.p) %>% distinct() %>%
  gather(Treatment, Estimate, Control:NPK ) %>% 
  mutate( Vector = "Losses")

gains.s <- sg.fixed.p2 %>% 
  mutate( Control = mean(sg.ctl.p),
          NPK = mean(sg.trt.p)) %>%
  select(-sg.ctl.p,-sg.trt.p) %>% distinct() %>%
  gather(Treatment, Estimate, Control:NPK) %>% 
  mutate( Vector = "Gains")

leg.dat <- bind_rows(cde.s, loss.s, gains.s) %>%
  unite(vec_trt,  Vector,  Treatment, sep=" " , remove= FALSE) %>%
  mutate(vec_trt = factor(vec_trt, levels = c("Losses Control", "Losses NPK", "Gains Control", "Gains NPK", "Persistent Sp. Control", "Persistent Sp. NPK")))



# legend for overall effects (thick lines)
head(leg.dat)

fixed.leg.npk <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = leg.dat %>% filter(Treatment == "NPK"),
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate, colour= Vector ), 
               size = 1.5, #linetype=2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_color_manual(name='NPK',
                     breaks=c("Losses","Gains","Persistent Sp."),
                     values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))+
  theme(legend.key.width = unit(2,"cm"))


fixed.leg.npk



fixed.leg.ctl <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=14 )+
  geom_segment(data = leg.dat %>% filter(Treatment == "Control"),
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate,colour= Vector ), 
               size = 1.5, linetype=2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_color_manual(name='Control',
                     breaks=c("Losses","Gains","Persistent Sp."),
                     values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))+
  theme(legend.key.width = unit(2,"cm"))+ theme(panel.grid.major = element_blank(), 
                                               panel.grid.minor = element_blank(), 
                                               strip.background = element_rect(colour="black", fill="white"),legend.position="bottom"
                                               #legend.title = element_text(vjust = 6) 
                                               )
  

fixed.leg.ctl

 head(cde.s)
# legend for posterior samples (thin lines)
post.leg <- ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.s,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate ,colour= Vector),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = loss.s,
               aes(x = 0,
                   xend = Estimate ,
                   y = 0,
                   yend = Estimate ,colour= Vector ),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = gains.s,
               aes(x = 0,
                   xend = Estimate ,
                   y = 0,
                   yend = Estimate ,
                   colour= Vector), size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_color_manual(name='Uncertainty',breaks=c("Losses","Gains","Persistent Sp."),
                     values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400"))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')+
  theme(legend.key.width = unit(2,"cm"))


post.leg



# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



f.legend.c<-g_legend(fixed.leg.ctl)
f.legend.n<-g_legend(fixed.leg.npk)

p.legend<-g_legend(post.leg)



(fig_4a ) / (f.legend.c)/(f.legend.n) / (p.legend) + plot_layout(heights = c(10,0.5,0.5,0.5))



# Fig 4b: The Slopes
#______________________________________________________
head(all.effs)


effs_calc_slope <- all.effs %>%
  # add posterior samples together
  mutate( # controls
    sloss.sgain.ctl.slope.p = (sloss.ctl.slope.p + sgain.ctl.slope.p),
    sl.sg.ctl.slope.p = (sl.ctl.slope.p + sg.ctl.slope.p),
    sl.sg.cde.ctl.slope.p = (sl.ctl.slope.p + sg.ctl.slope.p + cde.ctl.slope.p),
    #  npk treatments
    sloss.sgain.trt.slope.p = (sloss.trt.slope.p + sgain.trt.slope.p),
    sl.sg.trt.slope.p = (sl.trt.slope.p + sg.trt.slope.p),
    sl.sg.cde.trt.slope.p = (sl.trt.slope.p + sg.trt.slope.p + cde.trt.slope.p)) %>%
  # select columns we need
  select(sloss.ctl.slope.p, sloss.sgain.ctl.slope.p, sl.ctl.slope.p, sl.sg.ctl.slope.p, sl.sg.cde.ctl.slope.p,
         sloss.trt.slope.p, sloss.sgain.trt.slope.p, sl.trt.slope.p, sl.sg.trt.slope.p, sl.sg.cde.trt.slope.p)

head(effs_calc_slope)

# take mean and quantiles within probs
#controls
sloss.ctl.slope.effs <- effs_calc_slope %>%
  select(sloss.ctl.slope.p) %>%
  # sloss control
  mutate( sloss.ctl_global_slope = mean(sloss.ctl.slope.p),
          sloss.ctl_lower_slope = quantile(sloss.ctl.slope.p, probs=0.025),
          sloss.ctl_upper_slope = quantile(sloss.ctl.slope.p, probs=0.975) )  %>%
  filter(sloss.ctl.slope.p > quantile(sloss.ctl.slope.p, probs=0.025),
         sloss.ctl.slope.p < quantile(sloss.ctl.slope.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.ctl.slope.effs <- effs_calc_slope %>% 
  select(sloss.sgain.ctl.slope.p) %>%
  # sloss + sgain control
  mutate( sloss.sgain.ctl_global_slope = mean(sloss.sgain.ctl.slope.p),
          sloss.sgain.ctl_lower_slope = quantile(sloss.sgain.ctl.slope.p, probs=0.025),
          sloss.sgain.ctl_upper_slope = quantile(sloss.sgain.ctl.slope.p, probs=0.975) )  %>%
  filter(sloss.sgain.ctl.slope.p > quantile(sloss.sgain.ctl.slope.p, probs=0.025),
         sloss.sgain.ctl.slope.p < quantile(sloss.sgain.ctl.slope.p, probs=0.975)) %>% sample_n(50) 

sl.ctl.slope.effs <- effs_calc_slope %>% 
  select(sl.ctl.slope.p) %>%
  # sl control
  mutate( sl.ctl_global_slope = mean(sl.ctl.slope.p),
          sl.ctl_lower_slope = quantile(sl.ctl.slope.p, probs=0.025),
          sl.ctl_upper_slope = quantile(sl.ctl.slope.p, probs=0.975) )  %>%
  filter(sl.ctl.slope.p > quantile(sl.ctl.slope.p, probs=0.025),
         sl.ctl.slope.p < quantile(sl.ctl.slope.p, probs=0.975)) %>% sample_n(50) 

sl.sg.ctl.slope.effs <- effs_calc_slope %>% 
  select(sl.sg.ctl.slope.p) %>%
  # sl + sg control
  mutate( sl.sg.ctl_global_slope = mean(sl.sg.ctl.slope.p),
          sl.sg.ctl_lower_slope = quantile(sl.sg.ctl.slope.p, probs=0.025),
          sl.sg.ctl_upper_slope = quantile(sl.sg.ctl.slope.p, probs=0.975) )  %>%
  filter(sl.sg.ctl.slope.p > quantile(sl.sg.ctl.slope.p, probs=0.025),
         sl.sg.ctl.slope.p < quantile(sl.sg.ctl.slope.p, probs=0.975)) %>% sample_n(50)

sl.sg.cde.ctl.slope.effs <- effs_calc_slope %>% 
  select(sl.sg.cde.ctl.slope.p) %>%
  # sl + sg + cde control
  mutate( sl.sg.cde.ctl_global_slope = mean(sl.sg.cde.ctl.slope.p),
          sl.sg.cde.ctl_lower_slope = quantile(sl.sg.cde.ctl.slope.p, probs=0.025),
          sl.sg.cde.ctl_upper_slope = quantile(sl.sg.cde.ctl.slope.p, probs=0.975) )  %>%
  filter(sl.sg.cde.ctl.slope.p > quantile(sl.sg.cde.ctl.slope.p, probs=0.025),
         sl.sg.cde.ctl.slope.p < quantile(sl.sg.cde.ctl.slope.p, probs=0.975)) %>% sample_n(50) 

# treatments
sloss.trt.slope.effs <- effs_calc_slope %>% 
  select(sloss.trt.slope.p) %>%
  # sloss treatment
  mutate( sloss.trt_global_slope = mean(sloss.trt.slope.p),
          sloss.trt_lower_slope = quantile(sloss.trt.slope.p, probs=0.025),
          sloss.trt_upper_slope = quantile(sloss.trt.slope.p, probs=0.975) )  %>%
  filter(sloss.trt.slope.p > quantile(sloss.trt.slope.p, probs=0.025),
         sloss.trt.slope.p < quantile(sloss.trt.slope.p, probs=0.975)) %>% sample_n(50) 

sloss.sgain.trt.slope.effs <- effs_calc_slope %>% 
  select(sloss.sgain.trt.slope.p) %>%
  # sloss + sgain treatment
  mutate( sloss.sgain.trt_global_slope = mean(sloss.sgain.trt.slope.p),
          sloss.sgain.trt_lower_slope = quantile(sloss.sgain.trt.slope.p, probs=0.025),
          sloss.sgain.trt_upper_slope = quantile(sloss.sgain.trt.slope.p, probs=0.975) )  %>%
  filter(sloss.sgain.trt.slope.p > quantile(sloss.sgain.trt.slope.p, probs=0.025),
         sloss.sgain.trt.slope.p < quantile(sloss.sgain.trt.slope.p, probs=0.975)) %>% sample_n(50) 

sl.trt.slope.effs <- effs_calc_slope %>% 
  select(sl.trt.slope.p) %>%
  # sl treatment
  mutate( sl.trt_global_slope = mean(sl.trt.slope.p),
          sl.trt_lower_slope = quantile(sl.trt.slope.p, probs=0.025),
          sl.trt_upper_slope = quantile(sl.trt.slope.p, probs=0.975) )  %>%
  filter(sl.trt.slope.p > quantile(sl.trt.slope.p, probs=0.025),
         sl.trt.slope.p < quantile(sl.trt.slope.p, probs=0.975)) %>% sample_n(50)

sl.sg.trt.slope.effs <- effs_calc_slope %>% 
  select(sl.sg.trt.slope.p) %>%
  # sl + sg treatment
  mutate( sl.sg.trt_global_slope = mean(sl.sg.trt.slope.p),
          sl.sg.trt_lower_slope = quantile(sl.sg.trt.slope.p, probs=0.025),
          sl.sg.trt_upper_slope = quantile(sl.sg.trt.slope.p, probs=0.975) )  %>%
  filter(sl.sg.trt.slope.p > quantile(sl.sg.trt.slope.p, probs=0.025),
         sl.sg.trt.slope.p < quantile(sl.sg.trt.slope.p, probs=0.975)) %>% sample_n(50) 

sl.sg.cde.trt.slope.effs <- effs_calc_slope %>% 
  select(sl.sg.cde.trt.slope.p) %>%
  # sl + sg + cde treatment
  mutate( sl.sg.cde.trt_global_slope = mean(sl.sg.cde.trt.slope.p),
          sl.sg.cde.trt_lower_slope = quantile(sl.sg.cde.trt.slope.p, probs=0.025),
          sl.sg.cde.trt_upper_slope = quantile(sl.sg.cde.trt.slope.p, probs=0.975) )  %>%
  filter(sl.sg.cde.trt.slope.p > quantile(sl.sg.cde.trt.slope.p, probs=0.025),
         sl.sg.cde.trt.slope.p < quantile(sl.sg.cde.trt.slope.p, probs=0.975)) %>% sample_n(50) 


added.p.effs.slope <- sloss.ctl.slope.effs %>% cbind(sloss.sgain.ctl.slope.effs, sl.ctl.slope.effs, sl.sg.ctl.slope.effs, sl.sg.cde.ctl.slope.effs,
                                         sloss.trt.slope.effs, sloss.sgain.trt.slope.effs, sl.trt.slope.effs, sl.sg.trt.slope.effs, sl.sg.cde.trt.slope.effs) %>% 
  select(sloss.ctl_global_slope, sloss.sgain.ctl.slope.p, 
         sloss.sgain.ctl_global_slope, sloss.ctl.slope.p,
         sl.ctl_global_slope,sl.ctl.slope.p,
         sl.sg.ctl_global_slope, sl.sg.ctl.slope.p,
         sl.sg.cde.ctl_global_slope,sl.sg.cde.ctl.slope.p,
         # treatments
         sloss.trt_global_slope, sloss.trt.slope.p,
         sloss.sgain.trt_global_slope,sloss.sgain.trt.slope.p,
         sl.trt_global_slope,sl.trt.slope.p,
         sl.sg.trt_global_slope, sl.sg.trt.slope.p,
         sl.sg.cde.trt_global_slope,sl.sg.cde.trt.slope.p) 


nrow(added.p.effs.slope)
colnames(added.p.effs.slope)


fig_4b <- ggplot() +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  # posterior uncertainty samples for Controls (small dashed lines)
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs.slope,  # segments
               aes(x = 0,
                   xend = sloss.ctl.slope.p,
                   y = 0,
                   yend = sl.ctl.slope.p  ),
               colour= "#B40F20", linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow= arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.ctl.slope.p, # points
                                      y=  sl.ctl.slope.p  ),
             colour="black",size=0.2,alpha = 0.2) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope, # segment
               aes(x = sloss.ctl.slope.p,
                   xend =  sloss.sgain.ctl.slope.p ,
                   y = sl.ctl.slope.p,
                   yend = sl.sg.ctl.slope.p ),
               colour= "#046C9A",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.sgain.ctl.slope.p, # points
                                      y=  sl.sg.ctl.slope.p ) ,
             colour="black",
             size=0.2,alpha = 0.2) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope, # segment
               aes(x =  sloss.sgain.ctl.slope.p,
                   xend =  sloss.sgain.ctl.slope.p,
                   y =  sl.sg.ctl.slope.p,
                   yend = sl.sg.cde.ctl.slope.p ), 
               colour=  "#F98400",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope,aes(x=0, # points
                                     y= sl.sg.cde.ctl.slope.p),
             colour="#F98400",size=0.1,alpha = 0.2) +
  # Overall effects in Controls (thick dashed lines) 
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.ctl_global_slope,
                                                sl.ctl_global_slope), # segments
               aes(x = 0,
                   xend = sloss.ctl_global_slope,
                   y = 0,
                   yend = sl.ctl_global_slope  ),
               colour= "#B40F20", linetype=2,
               size = 1.5, alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.ctl_global_slope, # points
                                      y=  sl.ctl_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% 
                 distinct(sloss.ctl_global_slope,sloss.sgain.ctl_global_slope,
                          sl.ctl_global_slope,sl.sg.ctl_global_slope),
               aes(x = sloss.ctl_global_slope,
                   xend = sloss.sgain.ctl_global_slope,
                   y = sl.ctl_global_slope,
                   yend =  sl.sg.ctl_global_slope),
               colour= "#046C9A",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.sgain.ctl_global_slope, # point
                                      y= sl.sg.ctl_global_slope ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% #segment
                 distinct( sloss.sgain.ctl_global_slope,sl.sg.ctl_global_slope,
                           sl.sg.cde.ctl_global_slope),
               aes(x = sloss.sgain.ctl_global_slope,
                   xend = sloss.sgain.ctl_global_slope,
                   y = sl.sg.ctl_global_slope,
                   yend = sl.sg.cde.ctl_global_slope), 
               colour=  "#F98400",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope,aes(x=0, # points
                                     y=  sl.sg.cde.ctl_global_slope),
             colour="#F98400",size=0.1,alpha = 0.4) +
  # posterior uncertainty samples for treatments (NPK) (thin solid lines)
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs.slope, # segment
               aes(x = 0,
                   xend = sloss.trt.slope.p  ,
                   y = 0,
                   yend = sl.trt.slope.p   ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.trt.slope.p , # points
                                      y=  sl.trt.slope.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope,  #segment
               aes(x = sloss.trt.slope.p ,
                   xend = sloss.sgain.trt.slope.p ,
                   y = sl.trt.slope.p ,
                   yend = sl.sg.trt.slope.p ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.sgain.trt.slope.p , #points
                                      y= sl.sg.trt.slope.p),
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope, #segment
               aes(x = sloss.sgain.trt.slope.p,
                   xend = sloss.sgain.trt.slope.p,
                   y = sl.sg.trt.slope.p,
                   yend = sl.sg.cde.trt.slope.p ),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope,aes(x=0, # point
                                     y= sl.sg.cde.trt.slope.p ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  #   # Overall effects in Treatments (NPK) (thick solid lines) 
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.trt_global_slope , sloss.ctl_global_slope,
                                                sl.trt_global_slope, sl.ctl_global_slope),
               aes(x = 0,
                   xend = sloss.trt_global_slope ,
                   y = 0,
                   yend = sl.trt_global_slope ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.trt_global_slope , #loss
                                      y=  sl.trt_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.trt_global_slope ,sloss.sgain.trt_global_slope, 
                                                sl.trt_global_slope, sl.sg.trt_global_slope ),
               aes(x = sloss.trt_global_slope,
                   xend = sloss.sgain.trt_global_slope,
                   y = sl.trt_global_slope ,
                   yend = sl.sg.trt_global_slope ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x=  sloss.sgain.trt_global_slope, #losses
                                      y=  sl.sg.trt_global_slope,
  ), colour="#046C9A", size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.trt_global_slope ,sloss.sgain.trt_global_slope, 
                                                sl.trt_global_slope, sl.sg.trt_global_slope,sl.sg.cde.trt_global_slope ),
               aes(x = sloss.sgain.trt_global_slope,
                   xend = sloss.sgain.trt_global_slope,
                   y = sl.sg.trt_global_slope,
                   yend = sl.sg.cde.trt_global_slope ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope,aes(x=0, #persistent
                                     y=  sl.sg.cde.trt_global_slope ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  scale_y_continuous(breaks=c(-10,-5,0,5,10,15)) +
  scale_x_continuous(breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.05,0.1)) +
  # annotate("text", x = -0.015, y = 0.75, label = "t0") +
  # annotate("text", x = -0.415, y = 7.25, label = "tn") +
  # annotate("text", x = 0.03, y = -1.5, label = "tn") +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       subtitle = 'b) Rate of change in species and biomass/year (slopes)')


fig_4b


# LANDSCAPE 9X13
fig_4 <- ( (fig_4a_trt ) | (fig_4b) ) / (f.legend.c) / (f.legend.n) / (p.legend) + plot_layout(heights = c(10,0.5,0.5,0.5))

fig_4


# Fig S - something something
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # plot.bm.3_p
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # plot.rich.3_p




rich.fixed.p <- posterior_samples(rich.3_p, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
bm.fixed.p <- posterior_samples(bm.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 

head(rich.fixed.p)

rich.fixed.p2 <- rich.fixed.p %>% 
  # slope
  mutate(rich.trt.slope.p =`b_year_trt` + `b_trtNPK:year_trt`) %>%
  mutate(rich.ctl.slope.p =`b_year_trt`) %>%
  #  trt
  mutate(rich.trt.p = `b_Intercept` + `b_trtNPK`) %>%
  mutate(rich.ctl.p =`b_Intercept`) %>%
  select(rich.trt.slope.p, rich.ctl.slope.p,
         rich.ctl.p, rich.trt.p)


bm.fixed.p2 <- bm.fixed.p %>% 
  # slope
  mutate(bm.trt.slope.p =`b_year_trt` + `b_trtNPK:year_trt`) %>%
  mutate(bm.ctl.slope.p =`b_year_trt`) %>%
  #  trt
  mutate(bm.trt.p = `b_Intercept` + `b_trtNPK`) %>%
  mutate(bm.ctl.p =`b_Intercept`) %>%
  select(bm.trt.slope.p, bm.ctl.slope.p,
         bm.ctl.p, bm.trt.p)

bef.effs <- rich.fixed.p2 %>% bind_cols(bm.fixed.p2) 

head(bef.effs)

rich.ctl.effs <- bef.effs %>%
  select(rich.ctl.slope.p, rich.ctl.p) %>%
  # rich control
  mutate( rich.ctl_global_slope = mean(rich.ctl.slope.p),
          rich.ctl_lower_slope = quantile(rich.ctl.slope.p, probs=0.025),
          rich.ctl_upper_slope = quantile(rich.ctl.slope.p, probs=0.975) ,
          #trt
          rich.ctl_global = mean(rich.ctl.p),
          rich.ctl_lower = quantile(rich.ctl.p, probs=0.025),
          rich.ctl_upper = quantile(rich.ctl.p, probs=0.975) ,)  %>%
  select(-rich.ctl.slope.p, -rich.ctl.p) %>% distinct()

rich.ctl.effs

rich.trt.effs <- bef.effs %>%
  select(rich.trt.slope.p, rich.trt.p) %>%
  # rich control
  mutate(  rich.trt_global_slope = mean(rich.trt.slope.p),
           rich.trt_lower_slope = quantile(rich.trt.slope.p, probs=0.025),
           rich.trt_upper_slope = quantile(rich.trt.slope.p, probs=0.975),
    #trt
    rich.trt_global = mean(rich.trt.p),
          rich.trt_lower = quantile(rich.trt.p, probs=0.025),
          rich.trt_upper = quantile(rich.trt.p, probs=0.975) )  %>%
  select(-rich.trt.slope.p, -rich.trt.p) %>% distinct()

rich.trt.effs

bm.ctl.effs <- bef.effs %>%
  select(bm.ctl.slope.p, bm.ctl.p) %>%
  # bm control
  mutate( bm.ctl_global_slope = mean(bm.ctl.slope.p),
          bm.ctl_lower_slope = quantile(bm.ctl.slope.p, probs=0.025),
          bm.ctl_upper_slope = quantile(bm.ctl.slope.p, probs=0.975) ,
          bm.ctl_global = mean(bm.ctl.p),
          bm.ctl_lower = quantile(bm.ctl.p, probs=0.025),
          bm.ctl_upper = quantile(bm.ctl.p, probs=0.975) )  %>%
  select(-bm.ctl.slope.p, -bm.ctl.p) %>% distinct()

bm.ctl.effs

bm.trt.effs <- bef.effs %>%
  select(bm.trt.slope.p, bm.trt.p) %>%
  # bm control
  mutate( bm.trt_global_slope = mean(bm.trt.slope.p),
          bm.trt_lower_slope = quantile(bm.trt.slope.p, probs=0.025),
          bm.trt_upper_slope = quantile(bm.trt.slope.p, probs=0.975),
          bm.trt_global = mean(bm.trt.p),
          bm.trt_lower = quantile(bm.trt.p, probs=0.025),
          bm.trt_upper = quantile(bm.trt.p, probs=0.975),)  %>%
  select(-bm.trt.slope.p, -bm.trt.p) %>% distinct()


trt.effs <- rich.trt.effs %>% bind_cols(bm.trt.effs)

ctl.effs <- rich.ctl.effs %>% bind_cols(bm.ctl.effs)

head(effs_calc)

bm_trt_effs <- effs_calc_slope %>% cbind(effs_calc) %>%
  select(sl.sg.cde.trt.slope.p, sl.sg.cde.trt.p) %>%
  # sl + sg + cde treatment
  mutate( sl.sg.cde.trt_global_slope = mean(sl.sg.cde.trt.slope.p),
          sl.sg.cde.trt_lower_slope = quantile(sl.sg.cde.trt.slope.p, probs=0.025),
          sl.sg.cde.trt_upper_slope = quantile(sl.sg.cde.trt.slope.p, probs=0.975),
          sl.sg.cde.trt_global = mean(sl.sg.cde.trt.p),
          sl.sg.cde.trt_lower = quantile(sl.sg.cde.trt.p, probs=0.025),
          sl.sg.cde.trt_upper = quantile(sl.sg.cde.trt.p, probs=0.975) ) %>%
  select(-sl.sg.cde.trt.slope.p, -sl.sg.cde.trt.p) %>% distinct()


sp_trt_effs <- effs_calc_slope %>% cbind(effs_calc) %>%
  select(sloss.sgain.trt.slope.p, sloss.sgain.trt.p) %>%
  # sloss + sgain treatment
  mutate( sloss.sgain.trt_global_slope = mean(sloss.sgain.trt.slope.p),
          sloss.sgain.trt_lower_slope = quantile(sloss.sgain.trt.slope.p, probs=0.025),
          sloss.sgain.trt_upper_slope = quantile(sloss.sgain.trt.slope.p, probs=0.975),
          sloss.sgain.trt_global = mean(sloss.sgain.trt.p),
          sloss.sgain.trt_lower = quantile(sloss.sgain.trt.p, probs=0.025),
          sloss.sgain.trt_upper = quantile(sloss.sgain.trt.p, probs=0.975),) %>%
  select(-sloss.sgain.trt.slope.p, -sloss.sgain.trt.p) %>% distinct()

price_trt_effs <- bm_trt_effs %>% bind_cols(sp_trt_effs)

price_trt_effs

bm_ctl_effs<- effs_calc_slope %>% cbind(effs_calc) %>%
  select(sl.sg.cde.ctl.slope.p, sl.sg.cde.ctl.p) %>%
  # sl + sg + cde treatment
  mutate( sl.sg.cde.ctl_global_slope = mean(sl.sg.cde.ctl.slope.p),
          sl.sg.cde.ctl_lower_slope = quantile(sl.sg.cde.ctl.slope.p, probs=0.025),
          sl.sg.cde.ctl_upper_slope = quantile(sl.sg.cde.ctl.slope.p, probs=0.975),
          sl.sg.cde.ctl_global = mean(sl.sg.cde.ctl.p),
          sl.sg.cde.ctl_lower = quantile(sl.sg.cde.ctl.p, probs=0.025),
          sl.sg.cde.ctl_upper = quantile(sl.sg.cde.ctl.p, probs=0.975) ) %>%
  select(-sl.sg.cde.ctl.slope.p, -sl.sg.cde.ctl.p) %>% distinct()


sp_ctl_effs <- effs_calc_slope %>% cbind(effs_calc) %>%
  select(sloss.sgain.ctl.slope.p, sloss.sgain.ctl.p) %>%
  # sloss + sgain treatment
  mutate( sloss.sgain.ctl_global_slope = mean(sloss.sgain.ctl.slope.p),
          sloss.sgain.ctl_lower_slope = quantile(sloss.sgain.ctl.slope.p, probs=0.025),
          sloss.sgain.ctl_upper_slope = quantile(sloss.sgain.ctl.slope.p, probs=0.975) ,
          sloss.sgain.ctl_global = mean(sloss.sgain.ctl.p),
          sloss.sgain.ctl_lower = quantile(sloss.sgain.ctl.p, probs=0.025),
          sloss.sgain.ctl_upper = quantile(sloss.sgain.ctl.p, probs=0.975) ) %>%
  select(-sloss.sgain.ctl.slope.p, -sloss.sgain.ctl.p) %>% distinct()

price_ctl_effs <- bm_ctl_effs %>% bind_cols(sp_ctl_effs)

price_ctl_effs

price_effs<- price_trt_effs %>% bind_cols(price_ctl_effs)

head(added.p.effs)

fig_ssa <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
 # overall effects
  geom_point(data = trt.effs, aes(x= rich.trt_global,
                                       y=  bm.trt_global ),
             fill="#0B775E",color="#0B775E",size=4, alpha=0.5)+
  geom_errorbar(data = trt.effs, aes(x=rich.trt_global,
                                         ymin = bm.trt_lower, ymax = bm.trt_upper),width=0,colour = "#0B775E", size = 2,alpha=0.7) +
  geom_errorbarh(data = trt.effs, aes(y= bm.trt_global,
                                          xmin = rich.trt_lower, xmax = rich.trt_upper),height=0,colour = "#0B775E", size = 2, alpha=0.7) +
   # price
  geom_point(data = price_effs, aes(x= sloss.sgain.trt_global,
                                  y=  sl.sg.cde.trt_global ),
             fill="#F98400",color="#F98400",size=4, alpha=0.5)+
  geom_errorbar(data = price_effs, aes(x= sloss.sgain.trt_global,
                                       ymin = sl.sg.cde.trt_lower, ymax = sl.sg.cde.trt_upper),width=0,colour = "#F98400", size = 2,alpha=0.7) +
  geom_errorbarh(data = price_effs, aes(y= sl.sg.cde.trt_global,
                                        xmin = sloss.sgain.trt_lower, xmax = sloss.sgain.trt_upper),height=0,colour = "#F98400", size = 2, alpha=0.7) +
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global , sloss.ctl_global,
                                                      sl.trt_global, sl.ctl_global),
               aes(x = 0,
                   xend = sloss.trt_global ,
                   y = 0,
                   yend = sl.trt_global ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global , sloss.sgain.trt_global,
                                                      sl.trt_global, sl.sg.trt_global ),
               aes(x = sloss.trt_global,
                   xend = sloss.sgain.trt_global,
                   y = sl.trt_global ,
                   yend = sl.sg.trt_global ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% distinct(sloss.trt_global ,sloss.sgain.trt_global,
                                                      sl.trt_global, sl.sg.trt_global,sl.sg.cde.trt_global ),
               aes(x = sloss.sgain.trt_global,
                   xend = sloss.sgain.trt_global,
                   y = sl.sg.trt_global,
                   yend = sl.sg.cde.trt_global ),
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
   #   scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  # scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  ylim(-100,400) + xlim(-6,15)+
  labs(x = 'Average species change',
       y = expression(paste('Average plot biomass (g/' ,m^2, ')')),
       title = 'Average change in species and biomass',
       subtitle= 'a) NPK') + theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

fig_ssa


fig_ssb <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
   #ctl
  geom_point(data = ctl.effs, aes(x= rich.ctl_global,
                                  y=  bm.ctl_global ),
             fill="black",color="black",size=8, alpha=0.5)+
  geom_errorbar(data = ctl.effs, aes(x=rich.ctl_global,
                                     ymin = bm.ctl_lower, ymax = bm.ctl_upper),width=0,colour = "black", size = 2,alpha=0.9) +
  geom_errorbarh(data = ctl.effs, aes(y= bm.ctl_global,
                                      xmin = rich.ctl_lower, xmax = rich.ctl_upper),height=0,colour = "black", size = 2, alpha=0.9) +
 #price ctl
  geom_point(data = price_effs, aes(x= sloss.sgain.ctl_global,
                                    y=  sl.sg.cde.ctl_global ),
             fill="#F98400",color="#F98400",size=4, alpha=0.5)+
  geom_errorbar(data = price_effs, aes(x= sloss.sgain.ctl_global,
                                       ymin = sl.sg.cde.ctl_lower, ymax = sl.sg.cde.ctl_upper),width=0,colour = "#F98400", size = 2,alpha=0.7) +
  geom_errorbarh(data = price_effs, aes(y= sl.sg.cde.ctl_global,
                                        xmin = sloss.sgain.ctl_lower, xmax = sloss.sgain.ctl_upper),height=0,colour = "#F98400", size = 2, alpha=0.7) +
  geom_segment(data = added.p.effs %>% distinct(sloss.ctl_global , sloss.ctl_global,
                                                      sl.ctl_global, sl.ctl_global),
               aes(x = 0,
                   xend = sloss.ctl_global ,
                   y = 0,
                   yend = sl.ctl_global ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% distinct(sloss.ctl_global ,sloss.sgain.ctl_global, 
                                                      sl.ctl_global, sl.sg.ctl_global ),
               aes(x = sloss.ctl_global,
                   xend = sloss.sgain.ctl_global,
                   y = sl.ctl_global ,
                   yend = sl.sg.ctl_global ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% distinct(sloss.ctl_global ,sloss.sgain.ctl_global, 
                                                      sl.ctl_global, sl.sg.ctl_global,sl.sg.cde.ctl_global ),
               aes(x = sloss.sgain.ctl_global,
                   xend = sloss.sgain.ctl_global,
                   y = sl.sg.ctl_global,
                   yend = sl.sg.cde.ctl_global ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  #   scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  # scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
 ylim(-100,400) + xlim(-6,15)+
  labs(x = 'Average species change',
       y = expression(paste('Average plot biomass (g/' ,m^2, ')')),
       subtitle = 'b) Control') + theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

fig_ssb

(fig_ssa | fig_ssb)

#c d

fig_ssc <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = trt.effs, aes(x= rich.trt_global_slope,
                                  y=  bm.trt_global_slope ),
             fill="#0B775E",color="#0B775E",size=4, alpha=0.5)+
  geom_errorbar(data = trt.effs, aes(x=rich.trt_global_slope,
                                     ymin = bm.trt_lower_slope, ymax = bm.trt_upper_slope),width=0,colour = "#0B775E", size = 2,alpha=0.7) +
  geom_errorbarh(data = trt.effs, aes(y= bm.trt_global_slope,
                                      xmin = rich.trt_lower_slope, xmax = rich.trt_upper_slope),height=0,colour = "#0B775E", size = 2, alpha=0.7) +
  # price
  geom_point(data = price_effs, aes(x= sloss.sgain.trt_global_slope,
                                    y=  sl.sg.cde.trt_global_slope ),
             fill="#F98400",color="#F98400",size=4, alpha=0.5)+
  geom_errorbar(data = price_effs, aes(x= sloss.sgain.trt_global_slope,
                                       ymin = sl.sg.cde.trt_lower_slope, ymax = sl.sg.cde.trt_upper_slope),width=0,colour = "#F98400", size = 2,alpha=0.7) +
  geom_errorbarh(data = price_effs, aes(y= sl.sg.cde.trt_global_slope,
                                        xmin = sloss.sgain.trt_lower_slope, xmax = sloss.sgain.trt_upper_slope),height=0,colour = "#F98400", size = 2, alpha=0.7) +
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.trt_global_slope , sloss.ctl_global_slope,
                                                      sl.trt_global_slope, sl.ctl_global_slope),
               aes(x = 0,
                   xend = sloss.trt_global_slope ,
                   y = 0,
                   yend = sl.trt_global_slope ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.trt_global_slope ,sloss.sgain.trt_global_slope, 
                                                      sl.trt_global_slope, sl.sg.trt_global_slope ),
               aes(x = sloss.trt_global_slope,
                   xend = sloss.sgain.trt_global_slope,
                   y = sl.trt_global_slope ,
                   yend = sl.sg.trt_global_slope ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.trt_global_slope ,sloss.sgain.trt_global_slope, 
                                                      sl.trt_global_slope, sl.sg.trt_global_slope,sl.sg.cde.trt_global_slope ),
               aes(x = sloss.sgain.trt_global_slope,
                   xend = sloss.sgain.trt_global_slope,
                   y = sl.sg.trt_global_slope,
                   yend = sl.sg.cde.trt_global_slope ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  #   scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  # scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  ylim(-8,35) + xlim(-0.8,0.1)+
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       title = 'Rate of change in species and biomass/year (slopes)',
       subtitle = 'c) NPK') + theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

fig_ssc


fig_ssd <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  #ctl
  geom_point(data = ctl.effs, aes(x= rich.ctl_global_slope,
                                  y=  bm.ctl_global_slope ),
             fill="black",color="black",size=8, alpha=0.5)+
  geom_errorbar(data = ctl.effs, aes(x=rich.ctl_global_slope,
                                     ymin = bm.ctl_lower_slope, ymax = bm.ctl_upper_slope),width=0,colour = "black", size = 2,alpha=0.9) +
  geom_errorbarh(data = ctl.effs, aes(y= bm.ctl_global_slope,
                                      xmin = rich.ctl_lower_slope, xmax = rich.ctl_upper_slope),height=0,colour = "black", size = 2, alpha=0.9) +
  #price ctl
  geom_point(data = price_effs, aes(x= sloss.sgain.ctl_global_slope,
                                    y=  sl.sg.cde.ctl_global_slope ),
             fill="#F98400",color="#F98400",size=4, alpha=0.5)+
  geom_errorbar(data = price_effs, aes(x= sloss.sgain.ctl_global_slope,
                                       ymin = sl.sg.cde.ctl_lower_slope, ymax = sl.sg.cde.ctl_upper_slope),width=0,colour = "#F98400", size = 2,alpha=0.7) +
  geom_errorbarh(data = price_effs, aes(y= sl.sg.cde.ctl_global_slope,
                                        xmin = sloss.sgain.ctl_lower_slope, xmax = sloss.sgain.ctl_upper_slope),height=0,colour = "#F98400", size = 2, alpha=0.7) +
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.ctl_global_slope , sloss.ctl_global_slope,
                                                      sl.ctl_global_slope, sl.ctl_global_slope),
               aes(x = 0,
                   xend = sloss.ctl_global_slope ,
                   y = 0,
                   yend = sl.ctl_global_slope ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.ctl_global_slope ,sloss.sgain.ctl_global_slope, 
                                                      sl.ctl_global_slope, sl.sg.ctl_global_slope ),
               aes(x = sloss.ctl_global_slope,
                   xend = sloss.sgain.ctl_global_slope,
                   y = sl.ctl_global_slope ,
                   yend = sl.sg.ctl_global_slope ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% distinct(sloss.ctl_global_slope ,sloss.sgain.ctl_global_slope, 
                                                      sl.ctl_global_slope, sl.sg.ctl_global_slope,sl.sg.cde.ctl_global_slope ),
               aes(x = sloss.sgain.ctl_global_slope,
                   xend = sloss.sgain.ctl_global_slope,
                   y = sl.sg.ctl_global_slope,
                   yend = sl.sg.cde.ctl_global_slope ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  #   scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  # scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  ylim(-8,35) + xlim(-0.8,0.1)+
  labs(x = 'Rate of change in species  (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       subtitle = 'd)  Control') + theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

fig_ssd

(fig_ssa | fig_ssb)/(fig_ssc | fig_ssd)

