



library(ggplot2)
library(tidyverse)
library(patchwork)


p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/meta.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(p.all)


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/study.price.p.effs.q.Rdata')
npk.effs <- all.effs %>% select(site_code,sg.trt.rate.p, sl.trt.rate.p,cde.trt.rate.p,sloss.trt.rate.p,sgain.trt.rate.p, Quadrant) %>%
  filter(!Quadrant == "-biomass +rich")

head(npk.effs)
npk.effs$Quadrant<-factor(npk.effs$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich"))


colnames(meta)
p.all <- p.all %>% left_join(meta, by = "site_code") %>%
  filter(!Quadrant == "-biomass +rich")


sp.fig <- p.all %>% filter(trt.y == "NPK") %>%
  ggplot() + 
 # facet_wrap(~Quadrant ) +
  # geom_abline(intercept = 0, slope = 1)+
  geom_point( aes(x= s.loss , y= s.gain))+
  geom_smooth(aes(x = s.loss , y = s.gain), method='lm', height=0, color= "darkorchid4", linetype=2, size = 2) + theme_classic() +
  geom_smooth(aes(x = s.loss , y = s.gain, group= Quadrant, color= Quadrant), method='lm') +
  theme(legend.position="none")
sp.fig

bm.fig <- p.all %>% filter(trt.y == "NPK") %>%
  ggplot() +
  #facet_wrap(~Quadrant ) +
 # geom_abline(intercept = 0, slope = -1)+
  geom_point( aes(x= SL , y= SG)) +
  geom_smooth(aes(x = SL , y = SG, group= Quadrant, color= Quadrant), method='lm')+
  geom_smooth(aes(x = SL , y = SG), method='lm', color= "darkorchid4", linetype=2, size = 2)+
  theme_classic() +
  theme(legend.position="bottom")
bm.fig

ps.bm.fig <- p.all %>% filter(trt.y == "NPK") %>%
  ggplot() + 
  #facet_wrap(~Quadrant ) +
  #geom_abline(intercept = 0, slope = 1)+
  geom_point(aes(x= SL , y= CDE)) +
  geom_smooth(aes(x = SL , y = CDE), method='lm',color= "darkorchid4", linetype=2, size = 2)+
  geom_smooth(aes(x = SL , y = CDE, group= Quadrant, color= Quadrant), method='lm')+
  theme_classic() +
  theme(legend.position="none")
ps.bm.fig

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg <- g_legend(bm.fig)

(sp.fig)/( bm.fig + theme(legend.position="none"))/( ps.bm.fig) / (leg) +
  plot_layout( heights = c(10,10,10,1)) + plot_annotation(
    title = 'Absolute values'
  )





price.std <- p.all %>% group_by(site_code, trt.y, Quadrant) %>%
  summarise( gain.std = (SG/s.gain),
             loss.std = (SL/s.loss),
             cde.std = (CDE/c.rich)) 

bm.fig <- price.std %>% filter(trt.y == "NPK") %>%
  ggplot() + 
  #facet_wrap(~Quadrant ) +
 # geom_abline(intercept = 0, slope = -1)+
  geom_point( aes(x= loss.std , y= gain.std)) +
  geom_smooth(aes(x = loss.std , y = gain.std), method='lm',color= "darkorchid4", linetype=2, size = 2)+
  geom_smooth(aes(x = loss.std , y = gain.std, group= Quadrant, color= Quadrant), method='lm')+
  theme_classic() +
  theme(legend.position="none")

bm.fig

ps.bm.fig <- price.std %>% filter(trt.y == "NPK") %>%
  ggplot() + 
 # facet_wrap(~Quadrant ) +
 # geom_abline(intercept = 0, slope = 1)+
  geom_point(aes(x= loss.std , y= cde.std)) +
  geom_smooth(aes(x = loss.std , y = cde.std), method='lm',color= "darkorchid4", linetype=2, size = 2)+ 
  geom_smooth(aes(x = loss.std , y = cde.std, group= Quadrant, color= Quadrant), method='lm')+ 
  theme_classic() +
  theme(legend.position="bottom")

leg2 <- g_legend(ps.bm.fig)

(bm.fig )/( ps.bm.fig +   theme(legend.position="none")) /(leg2) +  
plot_layout( heights = c(10,10,1)) + plot_annotation(
  title = 'Biomass Change Standardized by number of species'
)








price.m <- p.all %>% group_by(site_code, trt.y, Quadrant) %>%
  summarise( gain.m = mean(s.gain),
             loss.m = mean(s.loss),
             SG.m = mean(SG),
             SL.m = mean(SL),
             CDE.m = mean(CDE))



sp.fig.m <- price.m %>% filter(trt.y == "NPK") %>%
  ggplot() + geom_point( aes(x= loss.m , y= gain.m))+
  geom_smooth(aes(x = loss.m , y = gain.m, group= Quadrant, color= Quadrant), method='lm')

bm.fig.m <- price.m %>% filter(trt.y == "NPK") %>%
  ggplot() + geom_point(aes(x= SL.m , y= SG.m)) +
  geom_smooth(aes(x = SL.m , y = SG.m, group= Quadrant, color= Quadrant), method='lm')


ps.bm.fig.m <- price.m %>% filter(trt.y == "NPK") %>%
  ggplot() + geom_point(aes(x= SL.m , y= CDE.m)) +
  geom_smooth(aes(x = SL.m , y = CDE.m, group= Quadrant, color= Quadrant), method='lm')

(sp.fig.m | bm.fig.m | ps.bm.fig.m)




colnames(npk.effs)

sp.fig.effs<- npk.effs %>% 
  ggplot() + geom_point( aes(x = sloss.trt.rate.p , y = sgain.trt.rate.p)) +
 geom_smooth(aes(x = sloss.trt.rate.p , y = sgain.trt.rate.p, group= Quadrant, color= Quadrant), method='lm')

bm.fig.effs <- npk.effs %>% 
  ggplot() + geom_point(aes(x = sl.trt.rate.p, y = sg.trt.rate.p)) +
  geom_smooth(aes(x = sloss.trt.rate.p , y = sgain.trt.rate.p, group= Quadrant, color= Quadrant), method='lm',)

ps.bm.fig.effs <- npk.effs %>% 
  ggplot() + geom_point(aes(x = sl.trt.rate.p, y = cde.trt.rate.p)) +
  geom_smooth(aes(x = sloss.trt.rate.p , y = sgain.trt.rate.p, group= Quadrant, color= Quadrant),method='lm')

(sp.fig.effs | bm.fig.effs | ps.bm.fig.effs)







