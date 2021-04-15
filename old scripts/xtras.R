

library(ggplot2)
library(tidyverse)
library(patchwork)


p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



colnames(p.all)


gain.fig <- p.all %>% filter(trt.y == "NPK") %>%
  ggplot() + geom_point( aes(x= s.gain , y= SG))

loss.fig <- p.all %>% filter(trt.y == "NPK") %>%
  ggplot() + geom_point( aes(x= s.loss , y= SL)) 

(gain.fig | loss.fig)


 price.m <- p.all %>% group_by(site_code, trt.y) %>%
   summarise( gain.m = mean(s.gain),
              loss.m = mean(s.loss),
              SG.m = mean(SG),
              SL.m = mean(SL),
              CDE.m = mean(CDE))

 

gain.fig.m <- price.m %>% filter(trt.y == "NPK") %>%
  ggplot() + geom_point( aes(x= gain.m , y= SG.m))
 
 loss.fig.m <- price.m %>% filter(trt.y == "NPK") %>%
   ggplot() + geom_point(aes(x= loss.m , y= SL.m)) 

(gain.fig.m | loss.fig.m)

 
 
 

 price.std <- p.all %>% group_by(site_code, trt.y) %>%
   summarise( gain.p = (SG/s.gain),
              loss.p = (SL/s.loss)) 
 
  
 colnames(price.std)

 
 
 
 ggplot() +
  # facet_grid(~trt.y) +
   geom_point(data = price.std,
              aes(x = trt.y, y = gain.p,
                  colour = `trt.y`, shape=`trt.y`), alpha=0.6,
              size = 1.3, position = position_jitter(width = 0.15 ))+
   stat_summary(data = price.std,
                aes(x = trt.y, y = gain.p),fun.data=mean_sdl, fun.args = list(mult=1), 
                geom="pointrange", color="black", shape=16,size=0.25)+
   scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
   ylim(-50,450)+
   theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=2,1,1,1, "lines"),
                         legend.direction = "horizontal", legend.position = "none" ) 
 
 
 
 ggplot() +
   # facet_grid(~trt.y) +
   geom_point(data = price.std,
              aes(x = trt.y, y = loss.p,
                  colour = `trt.y`, shape=`trt.y`), alpha=0.6,
              size = 1.3, position = position_jitter(width = 0.15 ))+
   stat_summary(data = price.std,
                aes(x = trt.y, y = loss.p),fun.data=mean_sdl, fun.args = list(mult=1), 
                geom="pointrange", color="black", shape=16,size=0.25)+
   scale_color_manual(values =  c("#228B22", 	"#6B8E23"))  + 
   theme_classic()+theme(axis.text.x = element_text(size=7), plot.margin=margin(t=2,1,1,1, "lines"),
                         legend.direction = "horizontal", legend.position = "none" ) 
 
 