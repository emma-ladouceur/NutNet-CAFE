

library(ggplot2)
library(tidyverse)
library(patchwork)

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/study.p.effs.Rdata')

sloss.t <- study.sloss.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sloss.trt.rate.p = eff) %>%
  select(-response,-eff)

sloss.c <- study.sloss.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sloss.ctl.rate.p = eff) %>%
  select(-response,-eff)

sloss.eff <- left_join(sloss.t,sloss.c)

sgain.t <- study.sgain.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sgain.trt.rate.p = eff) %>%
  select(-response,-eff)

sgain.c <- study.sgain.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sgain.ctl.rate.p = eff) %>%
  select(-response,-eff)

sgain.eff <- left_join(sgain.t,sgain.c)


sl.t <- study.sl.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sl.trt.rate.p = eff) %>%
  select(-response,-eff)

sl.c <- study.sl.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sl.ctl.rate.p = eff) %>%
  select(-response,-eff)

sl.eff <- left_join(sl.t,sl.c)

sg.t <- study.sg.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sg.trt.rate.p = eff) %>%
  select(-response,-eff)

sg.c <- study.sg.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sg.ctl.rate.p = eff) %>%
  select(-response,-eff)

sg.eff <- left_join(sg.t,sg.c)

cde.t <- study.cde.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(cde.trt.rate.p = eff) %>%
  select(-response,-eff)

cde.c <- study.cde.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(cde.ctl.rate.p = eff) %>%
  select(-response,-eff)

cde.eff <- left_join(cde.t,cde.c)


sloss.sgain.effs<- left_join(sloss.eff,sgain.eff)

sg.sl.eff<-left_join(sg.eff,sl.eff)


price.eff<-left_join(sg.sl.eff,cde.eff)


all.effs <- left_join(price.eff,sloss.sgain.effs)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects//NutNet/Data/')
save(all.effs, file = 'study.price.p.effs.Rdata')

quads <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv", stringsAsFactors = FALSE)

all.effs <- all.effs %>% left_join(quads)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/')
save(all.effs, file = 'study.price.p.effs.q.Rdata')


study.price.cloud<-ggplot()+
  facet_wrap(~Quadrant) +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  #treatment effects
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt.rate.p  ,
                   y = 0,
                   yend = sl.trt.rate.p   ),
               colour= "#B40F20",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate.p ,
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p ) ,
                   y = sl.trt.rate.p ,
                   yend = (sl.trt.rate.p)+(sg.trt.rate.p  ) ),
               colour= "#046C9A",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = all.effs,
               aes(x = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   y = (sl.trt.rate.p)+(sg.trt.rate.p ),
                   yend =(sl.trt.rate.p)+(sg.trt.rate.p)+ (cde.trt.rate.p  )),
               colour= "#F98400",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  # #CTLS
  # geom_segment(data = all.effs,
  #              aes(x = 0,
  #                  xend = sloss.ctl.rate.p  ,
  #                  y = 0,
  #                  yend = sl.ctl.rate.p   ),
  #              colour= "black", alpha=0.2,
  #              arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  # geom_segment(data = all.effs,
  #              aes(x = sloss.ctl.rate.p ,
  #                  xend = (sloss.ctl.rate.p)+(sgain.ctl.rate.p ) ,
  #                  y = sl.ctl.rate.p ,
  #                  yend = (sl.ctl.rate.p)+(sg.ctl.rate.p  ) ),
  #              colour= "black", alpha=0.2,
  #              arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  # geom_segment(data = all.effs,
  #              aes(x = (sloss.ctl.rate.p)+(sgain.ctl.rate.p),
  #                  xend = (sloss.ctl.rate.p)+(sgain.ctl.rate.p),
  #                  y = (sl.ctl.rate.p)+(sg.ctl.rate.p ),
  #                  yend =(sl.ctl.rate.p)+(sg.ctl.rate.p)+ (cde.ctl.rate.p  )),
  #              colour= "black", alpha=0.2,
  #              arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       # title= 'Rate of change / year '
       title = '')


study.price.cloud


study.price.cloud.ctl<-ggplot()+
  # facet_wrap(~site_code) +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  #treatment effects
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.ctl.rate.p  ,
                   y = 0,
                   yend = sl.ctl.rate.p   ),
               colour= "#B40F20", linetype=2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = all.effs,
               aes(x = sloss.ctl.rate.p ,
                   xend = (sloss.ctl.rate.p)+(sgain.ctl.rate.p ) ,
                   y = sl.ctl.rate.p ,
                   yend = (sl.ctl.rate.p)+(sg.ctl.rate.p  ) ),
               colour= "#046C9A",linetype=2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = all.effs,
               aes(x = (sloss.ctl.rate.p)+(sgain.ctl.rate.p),
                   xend = (sloss.ctl.rate.p)+(sgain.ctl.rate.p),
                   y = (sl.ctl.rate.p)+(sg.ctl.rate.p ),
                   yend =(sl.ctl.rate.p)+(sg.ctl.rate.p)+ (cde.ctl.rate.p  )),
               colour= "#F98400",linetype=2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       # title= 'Rate of change / year '
       title = '')


study.price.cloud.ctl

(study.price.cloud | study.price.cloud.ctl )


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/study.price.p.effs.q.Rdata')


colnames(all.effs)
all.effs$Quadrant<-as.factor(as.character(all.effs$Quadrant))
levels(all.effs$Quadrant)

npk.effs <- all.effs %>% select(site_code,sg.trt.rate.p, sl.trt.rate.p,cde.trt.rate.p,sloss.trt.rate.p,sgain.trt.rate.p, Quadrant)# %>%
 # gather(model ,effect, sg.trt.rate.p:sgain.trt.rate.p) %>%
  #filter(!Quadrant == "-biomass +rich")

head(npk.effs)
npk.effs$Quadrant<-factor(npk.effs$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich","-biomass +rich"))
  
  
  
sloss.se<-ggplot() + 
 # facet_grid(~model)+
  geom_point(data =npk.effs, aes(y = Quadrant, x = sloss.trt.rate.p),
             colour= "#B40F20",size = 2, position = position_jitter(height = 0.15 )) +
  # geom_errorbar(data = rich.p, aes(x = response, ymin = eff_lower,
  #                                  ymax = eff_upper, color=response),
             #   width = 0, size = 0.7) +
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_vline(xintercept = 0, lty = 2) +
 # scale_y_continuous(breaks=c(-100,50)) +
  #scale_color_manual(values = c("#000000","#0B775E")) +
 # coord_flip()+
  labs(title= "Species Loss")+ 
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                               #axis.title.y = element_blank(),
                              # axis.title.x = element_blank(),
                               axis.text.y = element_text(size=8),
                               axis.text.x = element_text(size=8),
                               title=element_text(size=8),
                               strip.background = element_blank(),legend.position="none") +
   scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

sloss.se

sl.se<-ggplot() + 
  # facet_grid(~model)+
  geom_point(data =npk.effs, aes(y = Quadrant, x = sl.trt.rate.p),
             colour= "#B40F20",size = 2, position = position_jitter(height = 0.15 )) +
  # geom_errorbar(data = rich.p, aes(x = response, ymin = eff_lower,
  #                                  ymax = eff_upper, color=response),
  #   width = 0, size = 0.7) +
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_vline(xintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(-100,50)) +
  #scale_color_manual(values = c("#000000","#0B775E")) +
 # coord_flip()+
  labs(title= "Species Loss: Biomass")+ 
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank(),
                               #axis.text.y = element_text(size=6),
                               axis.text.x = element_text(size=8),
                               title=element_text(size=8),
                               strip.background = element_blank(),legend.position="none")

sl.se


sgain.se<-ggplot() + 
  # facet_grid(~model)+
  geom_point(data =npk.effs, aes(y = Quadrant, x = sgain.trt.rate.p),
             colour= "#046C9A",size = 2, position = position_jitter(height = 0.15 )) +
  # geom_errorbar(data = rich.p, aes(x = response, ymin = eff_lower,
  #                                  ymax = eff_upper, color=response),
  #   width = 0, size = 0.7) +
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_vline(xintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(-100,50)) +
  #scale_color_manual(values = c("#000000","#0B775E")) +
  #coord_flip()+
  labs(title= "Species Gain", x= 'Study-Level Slope')+ 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_text(size=8),
                               axis.text.x = element_text(size=8),
                               title=element_text(size=8),
                               strip.background = element_blank(),legend.position="none")

sgain.se

sg.se<-ggplot() + 
  # facet_grid(~model)+
  geom_point(data =npk.effs, aes(y = Quadrant, x = sg.trt.rate.p),
             colour= "#046C9A",size = 2, position = position_jitter(height = 0.15 )) +
  # geom_errorbar(data = rich.p, aes(x = response, ymin = eff_lower,
  #                                  ymax = eff_upper, color=response),
  #   width = 0, size = 0.7) +
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_vline(xintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(-100,50)) +
  #scale_color_manual(values = c("#000000","#0B775E")) +
 # coord_flip()+
  labs(title= "Species Gain: Biomass")+ 
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank(),
                               axis.text.x = element_text(size=8),
                               title=element_text(size=8),
                               strip.background = element_blank(),legend.position="none")

sg.se


cde.se<-ggplot() + 
  # facet_grid(~model)+
  geom_point(data =npk.effs, aes(y = Quadrant, x = cde.trt.rate.p),
             colour= "#F98400",size = 2, position = position_jitter(height = 0.15 )) +
  # geom_errorbar(data = rich.p, aes(x = response, ymin = eff_lower,
  #                                  ymax = eff_upper, color=response),
  #   width = 0, size = 0.7) +
  labs(x = '',
       # y= expression(paste('Effect of NPK on Species Richness'))
       y='')+
  geom_vline(xintercept = 0, lty = 2) +
 scale_x_continuous(breaks=c(-100,-50,50)) +
  #scale_color_manual(values = c("#000000","#0B775E")) +
  #coord_flip()+
  labs(title= "Persistent Species") + 
  geom_text(data = npk.effs %>%
              group_by(Quadrant) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_study, .keep_all = T),
            aes(x=-45, y=Quadrant,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y =0.20, parse = T) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank(),
                               axis.text.x = element_text(size=8),
                               title=element_text(size=8),
                               strip.background = element_blank(),legend.position="none")


cde.se

(sloss.se | sl.se | sgain.se | sg.se | cde.se)
