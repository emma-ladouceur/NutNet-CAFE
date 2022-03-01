
# packages
library(tidyverse)
library(brms)
library(tidybayes)
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


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.gain.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sl.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sg.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.cde.Rdata')


head(s.loss.fitted.df)

# left join
s.loss.fitted <- s.loss.fitted.df %>% mutate(s.loss = .epred) %>% select(-.epred)
s.gain.fitted <- s.gain.fitted.df %>% mutate(s.gain = .epred) %>% select(-.epred)
sl.fitted <- s.sl.fitted.df %>% mutate(sl = .epred) %>% select(-.epred)
sg.fitted <- s.sg.fitted.df %>% mutate(sg = .epred) %>% select(-.epred)
cde.fitted <- s.cde.fitted.df %>% mutate(cde = .epred) %>% select(-.epred)




all.effs <- s.loss.fitted %>% left_join(s.gain.fitted) %>%
  left_join(sl.fitted) %>% left_join(sg.fitted) %>%
  left_join(cde.fitted)

head(all.effs)

effs_calc <- all.effs %>%
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


fig_4a_trt <- ggplot() +
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
  #   # Overall effects in Treatments (NPK) (thick solid lines)
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
#posterior uncertainty samples for Controls (small dashed lines)
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
  #scale_y_continuous(breaks=c(-10,-5,0,5,10,15)) +
  #scale_x_continuous(breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.05,0.1)) +
  # annotate("text", x = -0.015, y = 0.75, label = "t0") +
  # annotate("text", x = -0.415, y = 7.25, label = "tn") +
  # annotate("text", x = 0.03, y = -1.5, label = "tn") +
  labs(x = 'Average change in species',
       y = expression(paste('Average change in biomass (g/' ,m^2, ')')),
       subtitle = 'a) Average change in species and biomass')


fig_4a_trt






