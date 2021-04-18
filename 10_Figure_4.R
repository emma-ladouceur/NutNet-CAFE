
# Authors: Emma Ladouceur & Shane A. Blowes
# Title:
# Last Updated April 17, 2021

# 10_Figure 4.R
# This workflow uses data pulled out of Modelsbelow to produce Figure 4

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

meta <- meta %>% group_by(site_code) %>% filter(max.year >= 3) %>%
ungroup()

colnames(meta)

# Similarly  to '7_Model_Data_Posteriors.R' we 
# Extract 1000 posterior samples from Fixed Effects (Overall/Population/Global Effects) 
# for the price partitions
sloss.fixed.p <- posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p <- posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 

cde.fixed.p <- posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )
sl.fixed.p <- posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p <- posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 

# except here, we take 50 samples of the posterior distribution from overall effects
# within the range of 95 % probability to represent uncertainty around these effects

# note to self/Shane: here i take the mean of each global slope and then add them together in the fig
# but, should we instead take 1000 samps, then add all together, then take the mean
# i think the approach i currently take was meant to be a test and i was gonna fix it later but i didnt
# ask shane to check and for thoughts before i change everything
sl.fixed.p2 <- sl.fixed.p %>% 
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


cde.s <- cde.fixed.p2  

loss.s <- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2) 

gains.s <- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2) 

loss.gain <- loss.s %>% bind_cols(gains.s)

all.effs <- loss.gain %>% bind_cols(cde.fixed.p2)

head(all.effs)
colnames(all.effs)

fig_4 <- ggplot()+
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
# posterior uncertainty samples for Controls (small dashed lines)
  # species loss (x-axis) & SL (y-axis)
geom_segment(data = all.effs,  # segments
             aes(x = 0,
                 xend = sloss.ctl.rate.p ,
                 y = 0,
                 yend = sl.ctl.rate.p  ),
             colour= "#B40F20", linetype=2,
             size = 0.2,  alpha = 0.2,
             arrow= arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate.p, # points
                                  y=  sl.ctl.rate.p  ),
             colour="black",size=0.2,alpha = 0.2)+
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = all.effs, # segment
               aes(x = sloss.ctl.rate.p,
                   xend =  sloss.ctl.rate.p + sgain.ctl.rate.p ,
                   y = sl.ctl.rate.p,
                   yend = sl.ctl.rate.p+ sg.ctl.rate.p ),
               colour= "#046C9A",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate.p+ sgain.ctl.rate.p , # points
                                  y=  sl.ctl.rate.p + sg.ctl.rate.p ) ,
             colour="black",
             size=0.2,alpha = 0.2)+
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = all.effs, # segment
               aes(x =  sloss.ctl.rate.p + sgain.ctl.rate.p,
                   xend =  sloss.ctl.rate.p + sgain.ctl.rate.p,
                   y =  sl.ctl.rate.p+sg.ctl.rate.p,
                   yend = sl.ctl.rate.p+sg.ctl.rate.p + cde.ctl.rate.p ), 
               colour=  "#F98400",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, # points
                                 y= sl.ctl.rate.p+sg.ctl.rate.p+cde.ctl.rate.p ),
             colour="#F98400",size=0.1,alpha = 0.2) +
  # Overall effects in Controls (thick dashed lines) 
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = all.effs %>% distinct(sloss.ctl.rate_global_slope,sl.ctl.rate_global_slope), # segments
               aes(x = 0,
                   xend = sloss.ctl.rate_global_slope,
                   y = 0,
                   yend = sl.ctl.rate_global_slope  ),
               colour= "#B40F20", linetype=2,
               size = 1.5, alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate_global_slope, # points
                                  y=  sl.ctl.rate_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = all.effs %>% # segment
                 distinct(sloss.ctl.rate_global_slope,sgain.ctl.rate_global_slope,sl.ctl.rate_global_slope,sg.ctl.rate_global_slope),
               aes(x = sloss.ctl.rate_global_slope,
                   xend = sloss.ctl.rate_global_slope +  sgain.ctl.rate_global_slope,
                   y = sl.ctl.rate_global_slope,
                   yend =  sl.ctl.rate_global_slope+sg.ctl.rate_global_slope),
               colour= "#046C9A",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope, # point
                                  y= sl.ctl.rate_global_slope+ sg.ctl.rate_global_slope ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = all.effs %>% #segment
                 distinct(sloss.ctl.rate_global_slope,sgain.ctl.rate_global_slope,sl.ctl.rate_global_slope,sg.ctl.rate_global_slope,cde.ctl.rate_global_slope ),
               aes(x = sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope,
                   xend = sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope,
                   y = sl.ctl.rate_global_slope+sg.ctl.rate_global_slope,
                   yend = sl.ctl.rate_global_slope+sg.ctl.rate_global_slope+cde.ctl.rate_global_slope ), 
               colour=  "#F98400",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs,aes(x=0, # points
                                 y=  sloss.ctl.rate_global_slope+sgain.ctl.rate_global_slope+cde.ctl.rate_global_slope),
             colour="#F98400",size=0.1,alpha = 0.4) +
 # posterior uncertainty samples for treatments (NPK) (thin solid lines)
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = all.effs, # segment
               aes(x = 0,
                   xend = sloss.trt.rate.p  ,
                   y = 0,
                   yend = sl.trt.rate.p   ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= sloss.trt.rate.p , # points
                                  y=  sl.trt.rate.p  ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = all.effs,  #segment
               aes(x = sloss.trt.rate.p ,
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p ) ,
                   y = sl.trt.rate.p ,
                   yend = (sl.trt.rate.p)+(sg.trt.rate.p  ) ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= (sloss.trt.rate.p)+(sgain.trt.rate.p ) , #points
                                  y= (sl.trt.rate.p)+(sg.trt.rate.p ) ),
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = all.effs, #segment
               aes(x = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   y = (sl.trt.rate.p)+(sg.trt.rate.p ),
                   yend =(sl.trt.rate.p)+(sg.trt.rate.p)+ (cde.trt.rate.p  )),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=0, # point
                                 y= (sl.trt.rate.p)+(sg.trt.rate.p)+(cde.trt.rate.p) ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  #   # Overall effects in Treatments (NPK) (thick solid lines) 
  # species loss (x-axis) and SL (y-axis)
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
  # species gain (x-axis) & SG (y-axis)
    geom_segment(data = all.effs %>% distinct(sloss.trt.rate_global_slope , sloss.ctl.rate_global_slope,sgain.trt.rate_global_slope , sgain.ctl.rate_global_slope,sl.trt.rate_global_slope, sl.ctl.rate_global_slope, sg.trt.rate_global_slope , sg.ctl.rate_global_slope),
               aes(x = sloss.trt.rate_global_slope,
                   xend = (sloss.trt.rate_global_slope ) + (sgain.trt.rate_global_slope ),
                   y = sl.trt.rate_global_slope ,
                   yend = (sl.trt.rate_global_slope) + (sg.trt.rate_global_slope ) ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = all.effs, aes(x=  (sloss.trt.rate_global_slope ) + (sgain.trt.rate_global_slope ) , #losses
                                  y=  (sl.trt.rate_global_slope) + (sg.trt.rate_global_slope ),
  ), colour="#046C9A", size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
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
  scale_y_continuous(breaks=c(-10,-5,0,5,10,15)) +
  scale_x_continuous(breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.05,0.1)) +
   annotate("text", x = -0.015, y = 0.75, label = "t0") +
   annotate("text", x = -0.415, y = 7.25, label = "tn") +
  annotate("text", x = 0.03, y = -1.5, label = "tn") +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
      title = '')

  
fig_4
  

# GET LEGENDS
cde.s <- cde.fixed.p2  %>%
  mutate( Vector = "Persistent Sp.")

loss.s <- sl.fixed.p2 %>% bind_cols(sloss.fixed.p2) %>%
  mutate( Vector = "Losses")

gains.s <- sg.fixed.p2 %>% bind_cols(sgain.fixed.p2) %>%
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
                   yend = sl.trt.rate_global_slope, colour= Vector,),
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

# legend for posterior samples (thin lines)
post.leg <- ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
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




