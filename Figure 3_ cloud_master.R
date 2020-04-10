
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


sl.trt.i_fixef <- fixef(sl.s)
sg.trt.i_fixef <- fixef(sg.s)
CDE.trt.i_fixef <- fixef(CDE.s)
sloss.trt.i_fixef <- fixef(s.loss.s)
sgain.trt.i_fixef <- fixef(s.gain.s)


sl.fixed.p<-posterior_samples(sl.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
CDE.fixed.p<-posterior_samples(CDE.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
sloss.fixed.p<-posterior_samples(s.loss.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.s, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 

colnames(sl.fixed.p)
sl.fixed.p2 <-sl.fixed.p %>% 
  filter(`b_trt.yNPK:year.y.m` > quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.025),
         `b_trt.yNPK:year.y.m` < quantile(sl.fixed.p$`b_trt.yNPK:year.y.m`, probs=0.975)) %>%
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

loss.ss <- loss.s %>% sample_n(50) 
loss.ss$Vector="Losses"
gains.ss <- gains.s  %>% sample_n(50)
gains.ss$Vector="Gains"
cde.s <- cde.fixed.p2  %>% sample_n(50)
cde.s$Vector="Persistent Sp."
loss.gain <- loss.ss %>% bind_cols(gains.ss)

all.effs <- loss.gain %>% bind_cols(cde.s)

colnames(all.effs)


price.cloud<-ggplot()+
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
  ylim(-11,30) +
  labs(x = 'Effect of NPK on Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= 'b)')

price.cloud

# RICHNESS AND BIOMASS

plot.rich.im_fixef <- fixef(plot.rich.g)
plot.bm.im_fixef <- fixef(plot.bm.s)


rich.fixed.p<-posterior_samples(plot.rich.g, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(plot.bm.s, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 

plot.rich.im_fixef 
plot.bm.im_fixef 

rich.fixed.p2 <-rich.fixed.p %>% 
  filter(`b_trtNPK:year_trt` > quantile(rich.fixed.p$`b_trtNPK:year_trt`, probs=0.025),
         `b_trtNPK:year_trt` < quantile(rich.fixed.p$`b_trtNPK:year_trt`, probs=0.975)) %>%
  mutate(rich.trt.p=`b_trtNPK:year_trt`) %>%
  mutate(response = 'sl',
         rich.trt_global_slope = plot.rich.im_fixef ['trtNPK:year_trt','Estimate'],
        rich.trt_upper_slope = plot.rich.im_fixef ['trtNPK:year_trt','Q97.5'],
         rich.trt_lower_slope = plot.rich.im_fixef ['trtNPK:year_trt','Q2.5'],)  %>%
  select(rich.trt.p,rich.trt_global_slope, rich.trt_upper_slope,rich.trt_lower_slope)

nrow(rich.fixed.p2)

bm.fixed.p2 <-bm.fixed.p %>% 
  filter(`b_trtNPK:year_trt` > quantile(bm.fixed.p$`b_trtNPK:year_trt`, probs=0.025),
         `b_trtNPK:year_trt` < quantile(bm.fixed.p$`b_trtNPK:year_trt`, probs=0.975)) %>%
  mutate(bm.trt.p=`b_trtNPK:year_trt`) %>%
  mutate(response = 'sl',
         bm.trt_global_slope = plot.bm.im_fixef ['trtNPK:year_trt','Estimate'],
         bm.trt_upper_slope = plot.bm.im_fixef ['trtNPK:year_trt','Q97.5'],
         bm.trt_lower_slope = plot.bm.im_fixef ['trtNPK:year_trt','Q2.5'],)  %>%
  select(bm.trt.p,bm.trt_global_slope, bm.trt_upper_slope,bm.trt_lower_slope)

nrow(bm.fixed.p2)


rich.ss <- rich.fixed.p2 %>% sample_n(50) 
rich.ss$Vector="Species Richness"
biomass.ss <- bm.fixed.p2  %>% sample_n(50)
biomass.ss$Vector="Biomass"


r.bm.effs <- biomass.ss%>% bind_cols(rich.ss)

colnames(r.bm.effs)

#"#F98400","#0B775E"
bef.cloud<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = r.bm.effs,
               aes(x = 0,
                   xend = rich.trt.p ,
                   y = 0,
                   yend = bm.trt.p  ),
               colour= "#0B775E",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = r.bm.effs, aes(x= rich.trt.p, #loss
                                  y= bm.trt.p  ),
             colour="#0B775E",size=0.2,alpha = 0.4)+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
    geom_segment(data = r.bm.effs,
               aes(x = 0,
                   xend = rich.trt_global_slope,
                   y = 0,
                   yend = bm.trt_global_slope),
               colour= "#0B775E",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = r.bm.effs, aes(x= rich.trt_global_slope, #loss
                                  y=  bm.trt_global_slope ),
             colour="#0B775E",size=0.2,alpha = 0.4)+
  geom_errorbar(data = r.bm.effs,aes(x=rich.trt_global_slope,
                                    ymin = bm.trt_lower_slope, ymax = bm.trt_upper_slope),width=0,colour = "#0B775E", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = r.bm.effs,aes(y=bm.trt_global_slope,
                                     xmin = rich.trt_lower_slope, xmax = rich.trt_upper_slope),height=0,colour = "#0B775E", size = 0.55,alpha=0.3) +
  ylim(-11,30) +
  labs(x = 'Effect of NPK on Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= 'a)')

bef.cloud

grid.arrange(bef.cloud,price.cloud,nrow=1,ncol=2)
