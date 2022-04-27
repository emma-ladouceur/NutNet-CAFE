
# packages
library(tidyverse)
library(brms)
library(tidybayes)
library(gridExtra)
library(grid)
library(patchwork)


# load model objects
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


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.gain.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sl.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sg.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.cde.Rdata')


head(s.loss.fitted.df)


s.loss.fitted <- s.loss.fitted.df %>% mutate(s.loss = .epred) %>% select(-.epred)
s.gain.fitted <- s.gain.fitted.df %>% mutate(s.gain = .epred) %>% select(-.epred)
sl.fitted <- s.sl.fitted.df %>% mutate(sl = .epred) %>% select(-.epred)
sg.fitted <- s.sg.fitted.df %>% mutate(sg = .epred) %>% select(-.epred)
cde.fitted <- s.cde.fitted.df %>% mutate(cde = .epred) %>% select(-.epred)


fitted.effs <- s.loss.fitted %>% left_join(s.gain.fitted) %>%
  left_join(sl.fitted) %>% left_join(sg.fitted) %>%
  left_join(cde.fitted)

head(all.effs)

effs_calc <- fitted.effs %>%
  # add pfitted sampkles together
  mutate( 
    sloss.sgain = (s.loss + s.gain),
    sl.sg = (sl + sg),
    sl.sg.cde = (sl + sg + cde) ) 

head(effs_calc)


added.p.effs <- effs_calc %>%
  # sloss control
  mutate( sloss_global = mean(s.loss),
          sloss_lower = quantile(s.loss, probs=0.025),
          sloss_upper = quantile(s.loss, probs=0.975),
          # loss and gain
          sloss.sgain_global = mean(sloss.sgain),
          sloss.sgain_lower = quantile(sloss.sgain, probs=0.025),
          sloss.sgain_upper = quantile(sloss.sgain, probs=0.975),
          # sl
          sl_global = mean(sl),
          sl_lower = quantile(sl, probs=0.025),
          sl_upper = quantile(sl, probs=0.975),
          # sl.sg
          sl.sg_global = mean(sl.sg),
          sl.sg_lower = quantile(sl.sg, probs=0.025),
          sl.sg_upper = quantile(sl.sg, probs=0.975),
          # sl.sg
          sl.sg.cde_global = mean(sl.sg.cde),
          sl.sg.cde_lower = quantile(sl.sg.cde, probs=0.025),
          sl.sg.cde_upper = quantile(sl.sg.cde, probs=0.975),
          )  %>%
  filter(s.loss > quantile(s.loss, probs=0.025),
         s.loss < quantile(s.loss, probs=0.975),
         sloss.sgain > quantile(sloss.sgain, probs=0.025),
         sloss.sgain < quantile(sloss.sgain, probs=0.975),
         sl > quantile(sl, probs=0.025),
         sl < quantile(sl, probs=0.975),
         sl.sg > quantile(sl.sg, probs=0.025),
         sl.sg < quantile(sl.sg, probs=0.975),
         sl.sg.cde > quantile(sl.sg.cde, probs=0.025),
         sl.sg.cde < quantile(sl.sg.cde, probs=0.975),
         ) %>% sample_n(50) 



View(added.p.effs)



# Fig 4a: Average Effects
#____________________________________


fig_4a <- ggplot() +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  # posterior uncertainty samples for treatments (NPK) (thin solid lines)
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs %>% filter(trt.y == "NPK"), # segment
               aes(x = 0,
                   xend = s.loss,
                   y = 0,
                   yend = sl ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs  %>% filter(trt.y == "NPK"), 
             aes(x= s.loss, # points
                                      y=  sl ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs  %>% filter(trt.y == "NPK"),  #segment
               aes(x = s.loss,
                   xend = sloss.sgain,
                   y = sl,
                   yend = sl.sg ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs  %>% filter(trt.y == "NPK"),
             aes(x= sloss.sgain, #points
                                      y= sl.sg),
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs  %>% filter(trt.y == "NPK"), #segment
               aes(x = sloss.sgain,
                   xend = sloss.sgain,
                   y = sl.sg,
                   yend = sl.sg.cde),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs  %>% filter(trt.y == "NPK"),
             aes(x=0, # point
                                     y= sl.sg.cde),
             colour="#F98400",size=0.1,alpha = 0.4) +
  #  Overall effects in Treatments (NPK) (thick solid lines)
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs %>% filter(trt.y == "NPK") %>%
                 distinct(sloss_global , sl_global),
               aes(x = 0,
                   xend = sloss_global ,
                   y = 0,
                   yend = sl_global ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs  %>% filter(trt.y == "NPK"),
             aes(x= sloss_global , #loss
                                      y=  sl_global ),
             colour="#B40F20",size=0.2, alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% filter(trt.y == "NPK") %>% 
                 distinct(sloss_global , sloss.sgain_global,
                                                sl_global, sl.sg_global ),
               aes(x = sloss_global,
                   xend = sloss.sgain_global,
                   y = sl_global ,
                   yend = sl.sg_global ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs  %>% filter(trt.y == "NPK"), 
             aes(x=  sloss.sgain_global, #losses
                                      y=  sl.sg_global,
  ), colour="#046C9A", size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>%  filter(trt.y == "NPK") %>%
                 distinct(sloss_global ,sloss.sgain_global,
                                                sl_global, sl.sg_global, sl.sg.cde_global ),
               aes(x = sloss.sgain_global,
                   xend = sloss.sgain_global,
                   y = sl.sg_global,
                   yend = sl.sg.cde_global ),
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs  %>% filter(trt.y == "NPK"),
             aes(x=0, #persistent
                                     y=  sl.sg.cde_global ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  ##############################################
# posterior uncertainty samples for Controls (small dashed lines)
# species loss (x-axis) & SL (y-axis)
geom_segment(data = added.p.effs  %>% filter(trt.y == "Control"),  # segments
             aes(x = 0,
                 xend = s.loss,
                 y = 0,
                 yend = sl  ),
             colour= "#B40F20", linetype=2,
             size = 0.2,  alpha = 0.2,
             arrow= arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs %>% filter(trt.y == "Control"),
             aes(x= s.loss, # points
                                      y=  sl  ),
             colour="black",size=0.2,alpha = 0.2) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% filter(trt.y == "Control"), # segment
               aes(x = s.loss,
                   xend =  sloss.sgain ,
                   y = sl,
                   yend = sl.sg ),
               colour= "#046C9A",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs %>% filter(trt.y == "Control"), 
             aes(x= sloss.sgain, # points
                                      y=  sl.sg ) ,
             colour="black",
             size=0.2,alpha = 0.2) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% filter(trt.y == "Control"), # segment
               aes(x =  sloss.sgain,
                   xend =  sloss.sgain,
                   y =  sl.sg,
                   yend = sl.sg.cde ),
               colour=  "#F98400",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs %>% filter(trt.y == "Control"),
             aes(x=0, # points
                                     y= sl.sg.cde),
             colour="#F98400",size=0.1,alpha = 0.2) +
  # Overall effects in Controls (thick dashed lines)
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs %>% filter(trt.y == "Control") %>%
                 distinct(sloss_global,
                               sl_global), # segments
               aes(x = 0,
                   xend = sloss_global,
                   y = 0,
                   yend = sl_global  ),
               colour= "#B40F20", linetype=2,
               size = 1.5, alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs %>% filter(trt.y == "Control"),
             aes(x= sloss_global, # points
                                      y=  sl_global ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs %>% filter(trt.y == "Control") %>%
                 distinct(sloss_global,sloss.sgain_global,
                          sl_global, sl.sg_global),
               aes(x = sloss_global,
                   xend = sloss.sgain_global,
                   y = sl_global,
                   yend =  sl.sg_global),
               colour= "#046C9A",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs %>% filter(trt.y == "Control"),
             aes(x= sloss.sgain_global, # point
                                      y= sl.sg_global ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs %>% filter(trt.y == "Control") %>% #segment
                 distinct( sloss.sgain_global,sl.sg_global,
                           sl.sg.cde_global),
               aes(x = sloss.sgain_global,
                   xend = sloss.sgain_global,
                   y = sl.sg_global,
                   yend = sl.sg.cde_global),
               colour=  "#F98400",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs %>% filter(trt.y == "Control"),
             aes(x=0, # points
                                     y =  sl.sg.cde_global),
             colour="#F98400",size=0.1,alpha = 0.4) +
  labs(x = 'Average total change in species',
       y = expression(paste('Average total change in biomass (g/' ,m^2, ')')),
       subtitle = 'a) Average total change in species and biomass')


fig_4a


# CREATE CUSTOM LEGENDS

colnames(added.p.effs)

leg.dat <- added.p.effs %>% select(s.loss, s.gain, sl, sg, cde) %>%
  gather(Vector, Estimate, s.loss:cde) %>%
  mutate(Treatment = Trt_group) %>% ungroup() %>% select(-Trt_group)
  
head(leg.dat)


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

head(leg.dat)
# legend for posterior samples (thin lines)
post.leg <- ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = leg.dat %>% filter(Vector == "cde"),
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate ,colour= Vector),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = leg.dat %>% filter(Vector == "s.loss"),
               aes(x = 0,
                   xend = Estimate ,
                   y = 0,
                   yend = Estimate ,colour= Vector ),
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = leg.dat %>% filter(Vector == "s.gain"),
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

# fixed effect for controls
f.legend.c <- g_legend(fixed.leg.ctl)
# fixed effect for NPK
f.legend.n <- g_legend(fixed.leg.npk)
# posterior samples of uncertainty
p.legend < g_legend(post.leg)

# see how it looks
(fig_4a ) / (f.legend.c)/(f.legend.n) / (p.legend) + plot_layout(heights = c(10,0.5,0.5,0.5))



# Fig 4b: The Slopes
#______________________________________________________

# species loss & gained
sloss.fixed.p <- posterior_samples(sloss.3_p, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sgain.fixed.p <- posterior_samples(sgain.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 
# price components
cde.fixed.p <- posterior_samples(cde.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) )
sl.fixed.p <- posterior_samples(sl.3_p, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sg.fixed.p <- posterior_samples(sg.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 

# we take 50 samples of the posterior distribution from overall effects
# within the range of 95 % probability to represent uncertainty around these effects

head(sl.fixed.p)

# here we take 1000 samps, then add all together, then take the mean
sl.fixed <- sl.fixed.p %>% 
  # slope
  mutate(sl.trt.slope.p =`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sl.ctl.slope.p =`b_year.y.m`) %>%
  #  trt
  mutate(sl.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sl.ctl.p =`b_Intercept`) %>%
  select(sl.trt.slope.p, sl.ctl.slope.p,
         sl.ctl.p, sl.trt.p) %>%
  gather(Response, Estimate, sl.trt.slope.p:sl.trt.p) %>%
  mutate( Vector = "sl") %>%
  rownames_to_column( var = "n") %>%
  mutate( Treatment = case_when(Response == "sl.trt.slope.p" ~ "NPK",
                               Response == "sl.ctl.slope.p" ~ "Control",
                               Response == "sl.ctl.p" ~ "Control",
                               Response == "sl.trt.p" ~ "NPK")) %>%
  mutate( Response = case_when(Response == "sl.trt.slope.p" ~ "Slope",
                               Response == "sl.ctl.slope.p" ~ "Slope",
                               Response == "sl.ctl.p" ~ "Intercept",
                               Response == "sl.trt.p" ~ "Intercept")) 
  

head(sl.fixed)


sg.fixed <- sg.fixed.p %>% 
  # slope
  mutate(sg.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sg.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(sg.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sg.ctl.p =`b_Intercept`) %>%
  select(sg.trt.slope.p, sg.ctl.slope.p,
         sg.ctl.p, sg.trt.p) %>%
  gather(Response, Estimate, sg.trt.slope.p:sg.trt.p) %>%
  mutate( Vector = "sg") %>%
  rownames_to_column( var = "n") %>%
  mutate( Treatment = case_when(Response == "sg.trt.slope.p" ~ "NPK",
                                Response == "sg.ctl.slope.p" ~ "Control",
                                Response == "sg.ctl.p" ~ "Control",
                                Response == "sg.trt.p" ~ "NPK")) %>%
  mutate( Response = case_when(Response == "sg.trt.slope.p" ~ "Slope",
                               Response == "sg.ctl.slope.p" ~ "Slope",
                               Response == "sg.ctl.p" ~ "Intercept",
                               Response == "sg.trt.p" ~ "Intercept")) 

head(sg.fixed)

cde.fixed <- cde.fixed.p %>% 
  # slope
  mutate(cde.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(cde.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(cde.ctl.p =`b_Intercept`) %>%
  select(cde.trt.slope.p, cde.ctl.slope.p,
         cde.ctl.p,  cde.trt.p) %>%
  gather(Response, Estimate, cde.trt.slope.p:cde.trt.p) %>%
  mutate( Vector = "cde") %>%
  rownames_to_column( var = "n") %>%
  mutate( Treatment = case_when(Response == "cde.trt.slope.p" ~ "NPK",
                                Response == "cde.ctl.slope.p" ~ "Control",
                                Response == "cde.ctl.p" ~ "Control",
                                Response == "cde.trt.p" ~ "NPK")) %>%
  mutate( Response = case_when(Response == "cde.trt.slope.p" ~ "Slope",
                               Response == "cde.ctl.slope.p" ~ "Slope",
                               Response == "cde.ctl.p" ~ "Intercept",
                               Response == "cde.trt.p" ~ "Intercept")) 


sloss.fixed <- sloss.fixed.p %>% 
  mutate(sloss.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sloss.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(sloss.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sloss.ctl.p =`b_Intercept`) %>%
  select(sloss.trt.slope.p, sloss.ctl.slope.p,
         sloss.ctl.p, sloss.trt.p) %>%
  gather(Response, Estimate, sloss.trt.slope.p:sloss.trt.p) %>%
  mutate( Vector = "sloss") %>%
  rownames_to_column( var = "n") %>%
  mutate( Treatment = case_when(Response == "sloss.trt.slope.p" ~ "NPK",
                                Response == "sloss.ctl.slope.p" ~ "Control",
                                Response == "sloss.ctl.p" ~ "Control",
                                Response == "sloss.trt.p" ~ "NPK")) %>%
  mutate( Response = case_when(Response == "sloss.trt.slope.p" ~ "Slope",
                               Response == "sloss.ctl.slope.p" ~ "Slope",
                               Response == "sloss.ctl.p" ~ "Intercept",
                               Response == "sloss.trt.p" ~ "Intercept")) 


sgain.fixed <-sgain.fixed.p %>% 
  mutate(sgain.trt.slope.p=`b_year.y.m` + `b_trt.yNPK:year.y.m`) %>%
  mutate(sgain.ctl.slope.p=`b_year.y.m`) %>%
  #  trt
  mutate(sgain.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sgain.ctl.p =`b_Intercept`) %>%
  select(sgain.trt.slope.p, sgain.ctl.slope.p,
         sgain.ctl.p, sgain.trt.p)%>%
  gather(Response, Estimate, sgain.trt.slope.p:sgain.trt.p) %>%
  mutate( Vector = "sgain") %>%
  rownames_to_column( var = "n") %>%
  mutate( Treatment = case_when(Response == "sgain.trt.slope.p" ~ "NPK",
                                Response == "sgain.ctl.slope.p" ~ "Control",
                                Response == "sgain.ctl.p" ~ "Control",
                                Response == "sgain.trt.p" ~ "NPK")) %>%
  mutate( Response = case_when(Response == "sgain.trt.slope.p" ~ "Slope",
                               Response == "sgain.ctl.slope.p" ~ "Slope",
                               Response == "sgain.ctl.p" ~ "Intercept",
                               Response == "sgain.trt.p" ~ "Intercept")) 



all.effs <- sl.fixed %>% bind_rows(sg.fixed) %>%
  bind_rows(cde.fixed) %>%
  bind_rows(sgain.fixed)  %>%
  bind_rows(sloss.fixed)  %>% 
  spread(Vector, Estimate) %>%
  arrange(n) %>% filter(Response == "Slope")

head(all.effs)
nrow(all.effs)


effs_calc_slope <- all.effs %>%
  # add posterior samples together for plotting together
  mutate( 
    sloss.sgain = (sloss + sgain),
    sl.sg = (sl + sg),
    sl.sg.cde = (sl + sg + cde),
) 

head(effs_calc_slope)

# take mean and quantiles and take 50 samps within probs
added.p.effs.slope <- effs_calc_slope %>%
  group_by(Treatment) %>%
  mutate( # sloss
          sloss_global_slope = mean(sloss),
          sloss_lower_slope = quantile(sloss, probs=0.025),
          sloss_upper_slope = quantile(sloss, probs=0.975),
          # loss + gains
          sloss.sgain_global_slope = mean(sloss.sgain),
          sloss.sgain_lower_slope = quantile(sloss.sgain, probs=0.025),
          sloss.sgain_upper_slope = quantile(sloss.sgain, probs=0.975) ,
          # sl
          sl_global_slope = mean(sl),
          sl_lower_slope = quantile(sl, probs=0.025),
          sl_upper_slope = quantile(sl, probs=0.975) ,
          # sl + sg
          sl.sg_global_slope = mean(sl.sg),
          sl.sg_lower_slope = quantile(sl.sg, probs=0.025),
          sl.sg_upper_slope = quantile(sl.sg, probs=0.975),
          # sl + sg + cde
          sl.sg.cde_global_slope = mean(sl.sg.cde),
          sl.sg.cde_lower_slope = quantile(sl.sg.cde, probs=0.025),
          sl.sg.cde_upper_slope = quantile(sl.sg.cde, probs=0.975)
          )  %>%
  filter( # sloss
        sloss > quantile(sloss, probs=0.025),
         sloss < quantile(sloss, probs=0.975),
    # sloss + sgain
         sloss.sgain > quantile(sloss.sgain, probs=0.025),
         sloss.sgain < quantile(sloss.sgain, probs=0.975),
    # sl 
    sl > quantile(sl, probs=0.025),
             sl < quantile(sl, probs=0.975),
    # sl + sg
    sl.sg > quantile(sl.sg, probs=0.025),
    sl.sg < quantile(sl.sg, probs=0.975),
    # sl + sg + cde
    sl.sg.cde > quantile(sl.sg.cde, probs=0.025),
    sl.sg.cde < quantile(sl.sg.cde, probs=0.975)
         ) %>% sample_n(50) %>% 
  ungroup() %>%
  select(-c(sg, cde, sgain))


View(added.p.effs.slope)
head(added.p.effs.slope)


fig_4b <- ggplot() +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  theme_classic(base_size=14 )+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  # posterior uncertainty samples for Controls (small dashed lines)
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "Control"),  # segments
               aes(x = 0,
                   xend = sloss,
                   y = 0,
                   yend = sl  ),
               colour= "#B40F20", linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow= arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "Control"), 
             aes(x= sloss, # points
                                            y=  sl  ),
             colour="black",size=0.2,alpha = 0.2) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "Control"), # segment
               aes(x = sloss,
                   xend =  sloss.sgain ,
                   y = sl,
                   yend = sl.sg ),
               colour= "#046C9A",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "Control")
             , aes(x= sloss.sgain, # points
                                            y=  sl.sg ) ,
             colour="black",
             size=0.2,alpha = 0.2) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "Control"), # segment
               aes(x =  sloss.sgain,
                   xend =  sloss.sgain,
                   y =  sl.sg,
                   yend = sl.sg.cde ), 
               colour=  "#F98400",linetype=2,
               size = 0.2,  alpha = 0.2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "Control"),
             aes(x=0, # points
                                           y= sl.sg.cde),
             colour="#F98400",size=0.1,alpha = 0.2) +
  # Overall effects in Controls (thick dashed lines) 
  # species loss (x-axis) & SL (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "Control") %>% 
                 distinct(sloss_global_slope,sl_global_slope), # segments
               aes(x = 0,
                   xend = sloss_global_slope,
                   y = 0,
                   yend = sl_global_slope  ),
               colour= "#B40F20", linetype=2,
               size = 1.5, alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "Control"), 
             aes(x= sloss_global_slope, # points
                                            y=  sl_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "Control") %>% 
                 distinct(sloss_global_slope, sloss.sgain_global_slope,
                          sl_global_slope, sl.sg_global_slope),
               aes(x = sloss_global_slope,
                   xend = sloss.sgain_global_slope,
                   y = sl_global_slope,
                   yend =  sl.sg_global_slope),
               colour= "#046C9A",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope, aes(x= sloss.sgain_global_slope, # point
                                            y= sl.sg_global_slope ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "Control") %>% #segment
                 distinct( sloss.sgain_global_slope,sl.sg_global_slope,
                           sl.sg.cde_global_slope),
               aes(x = sloss.sgain_global_slope,
                   xend = sloss.sgain_global_slope,
                   y = sl.sg_global_slope,
                   yend = sl.sg.cde_global_slope), 
               colour=  "#F98400",linetype=2,
               size = 1.5,alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "Control") ,
             aes(x=0, # points
                                           y=  sl.sg.cde_global_slope),
             colour="#F98400",size=0.1,alpha = 0.4) +
  # posterior uncertainty samples for treatments (NPK) (thin solid lines)
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "NPK"), # segment
               aes(x = 0,
                   xend = sloss  ,
                   y = 0,
                   yend = sl   ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "NPK"),
             aes(x= sloss , # points
                                            y=  sl  ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "NPK"),  #segment
               aes(x = sloss ,
                   xend = sloss.sgain ,
                   y = sl ,
                   yend = sl.sg ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "NPK"),
             aes(x= sloss.sgain , y= sl.sg),
             colour="#046C9A",
             size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "NPK"), #segment
               aes(x = sloss.sgain,
                   xend = sloss.sgain,
                   y = sl.sg,
                   yend = sl.sg.cde ),
               colour= "#F98400",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "NPK"),
             aes(x=0, # point
                                           y= sl.sg.cde ),
             colour="#F98400",size=0.1,alpha = 0.4) +
  #  Overall effects in Treatments (NPK) (thick solid lines) 
  # species loss (x-axis) and SL (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "NPK") %>% 
                 distinct(sloss_global_slope , sloss_global_slope,
                           sl_global_slope, sl_global_slope),
               aes(x = 0,
                   xend = sloss_global_slope ,
                   y = 0,
                   yend = sl_global_slope ),
               colour= "#B40F20",
               size = 1.5, #alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "NPK"), 
             aes(x= sloss_global_slope ,  y=  sl_global_slope ),
             colour="#B40F20",size=0.2,alpha = 0.4) +
  # species gain (x-axis) & SG (y-axis)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "NPK") %>% 
                 distinct(sloss_global_slope ,sloss.sgain_global_slope, 
                           sl_global_slope, sl.sg_global_slope ),
               aes(x = sloss_global_slope,
                   xend = sloss.sgain_global_slope,
                   y = sl_global_slope ,
                   yend = sl.sg_global_slope ),
               colour= "#046C9A",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "NPK"), aes(x=  sloss.sgain_global_slope, #losses
                                            y=  sl.sg_global_slope,
  ), colour="#046C9A", size=0.2,alpha = 0.4) +
  # persistent species (cde/ps) (y axis only)
  geom_segment(data = added.p.effs.slope %>% filter( Treatment == "NPK") %>% 
                 distinct(sloss.sgain_global_slope, 
                          sl.sg_global_slope, sl.sg.cde_global_slope ),
               aes(x = sloss.sgain_global_slope,
                   xend = sloss.sgain_global_slope,
                   y = sl.sg_global_slope,
                   yend = sl.sg.cde_global_slope ), 
               colour= "#F98400",
               size = 1.5,#alpha=0.7,
               arrow=arrow(type="closed",length=unit(0.4,"cm"))) +
  geom_point(data = added.p.effs.slope %>% filter( Treatment == "NPK"),
             aes(x=0, #persistent
                                           y=  sl.sg.cde_global_slope ),
             colour="#F98400",size=0.1,alpha = 0.4) +
   scale_y_continuous(breaks=c(-10,-5,0,5,10,15)) +
   scale_x_continuous(breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.05,0.1)) +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       subtitle = 'b) Rate of change in species and biomass/year (slopes)')


fig_4b


# LANDSCAPE 9X13
fig_4 <- ( (fig_4a ) | (fig_4b) ) / (f.legend.c) / (f.legend.n) / (p.legend) + plot_layout(heights = c(10,0.5,0.5,0.5))

fig_4




