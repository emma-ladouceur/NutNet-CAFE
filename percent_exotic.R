


rm(list=ls())

library(tidyverse)
library(ggplot2)




clim <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv", stringsAsFactors = FALSE)
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) 

head(plot)

p2<-distinct(plot,elevation,latitude,longitude,continent,country,site_code,region,grazed,burned,managed,anthropogenic,habitat,site_richness,site_native_richness,site_introduced_richness)

p2$site_percent_native_rich<- (p2$site_native_richness/p2$site_richness) * 100
p2$site_percent_int_rich<- (p2$site_introduced_richness/p2$site_richness) * 100

p2<-p2 %>% mutate(site_percent_int_rich= ifelse(is.na(site_percent_int_rich), 0, site_percent_int_rich),
                  site_percent_native_rich= ifelse(is.na(site_percent_native_rich), 0, site_percent_native_rich) )

p2$site_dom <- ifelse(p2$site_percent_native_rich >= p2$site_percent_int_rich , 'native dominated',
                      ifelse(p2$site_percent_int_rich  >= p2$site_percent_native_rich, 'introduced dominated','other'))


colnames(p2)



sl.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')

colnames(sl.ps)


sl<-  
  ggplot() +
  geom_rect(data = sl.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = sl.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_rich)) ,
                      aes(x = sl.trt.study + sl.trt.global, 
                          y = site_code, fill=site_percent_int_rich,
                      ), 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis(name = 
                       'site_percent_int_sl',
                     discrete=FALSE) +
  geom_vline(data = sl.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Change in Species slness/Year',
        title= 'a) Species slness',
        y= ' multilimited sites'
        #color= ''
  )+
  # geom_text(data = sl.ps %>%
  #             group_by(multilimited) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(multilimited, n_study, .keep_all = T),
  #           aes(x=2.5, y=multilimited,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="bottom")

sl

