


rm(list=ls())


library(ggplot2)


library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)




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



summary(p2)

View(p2)


p2 <- p2 %>% summarise(site_percent_int_rich = round(site_percent_int_rich))

p2$site_ex_p <- ifelse(p2$site_percent_int_rich >= 75 & p2$site_percent_int_rich <= 95, '75-93',
                          ifelse(p2$site_percent_int_rich >= 50 & p2$site_percent_int_rich <= 74, '50-74',
                                 ifelse(p2$site_percent_int_rich >= 25 & p2$site_percent_int_rich <= 49.5, '25-49',
                                        ifelse(p2$site_percent_int_rich <24 & p2$site_percent_int_rich >= 0, '0-24','other'))))


p3<- p2 %>% select(site_code,site_ex_p)



View(p3)

rich.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/rich_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')


rich.p

rich.e<-   ggplot() +
  geom_rect(data = rich.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sl.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_rich)) ,
    data = rich.ps ,
    aes(x = rich.trt.study + rich.trt.global, 
        y = site_code, fill=site_percent_int_rich,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  scale_fill_viridis(name = 
                       'Percentage Exotic',
                     discrete=FALSE) +
  geom_vline(data = rich.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Richness',
        title= ' Species Richness',
        y= 'Site Code'
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
        legend.position="none")

rich.e



bm.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/bm_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


head(bm.ps)
bm.e<-   ggplot() +
  geom_rect(data = bm.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sl.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_bm)) ,
    data = bm.ps ,
    aes(x = bm.trt.study + bm.trt.global, 
        y = site_code, fill=site_percent_int_rich,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  scale_fill_viridis(name = 
                       'Percentage Exotic',
                     discrete=FALSE) +
  geom_vline(data = bm.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Biomass',
        title= ' Biomass',
        y= 'Site Code'
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
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

bm.e

sl.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')

colnames(sl.ps)

summary(sl.ps)


sl.e<-  
  ggplot() +
  geom_rect(data = sl.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sl.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_rich)) ,
    data = sl.ps ,
                      aes(x = sl.trt.study + sl.trt.global, 
                          y = site_code, fill=site_percent_int_rich,
                      ), 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis(name = 
                       'Percentage Exotic',
                     discrete=FALSE) +
  geom_vline(data = sl.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Change in Biomss Due to Species Loss/Year',
        title= 'Biomass Change due to Species Loss',
        y= ' Site Code'
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
        legend.position="none")

sl.e



sg.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sg.e<-  
  ggplot() +
  geom_rect(data = sg.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf),alpha = 0.3) +
  geom_density_ridges(#data = sg.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_rich)) ,
    data = sg.ps ,
    aes(x = sg.trt.study + sg.trt.global, 
        y = site_code, fill=site_percent_int_rich,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  scale_fill_viridis(name = 
                       'Percentage Exotic',
                     discrete=FALSE) +
  geom_vline(data = sg.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Change in Biomass Due to Species Gain/Year',
        title= 'Biomass Change due to Species Gain',
        y= ' Site Code'
        #color= ''
  )+
  # geom_text(data = sg.ps %>%
  #             group_by(multilimited) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(multilimited, n_study, .keep_all = T),
  #           aes(x=2.5, y=multilimited,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="bottom")
sg.e



cde.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))





cde.e<-  
  ggplot() +
  geom_rect(data = cde.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = cde.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_rich)) ,
    data = cde.ps ,
    aes(x = cde.trt.study + cde.trt.global, 
        y = site_code, fill=site_percent_int_rich,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  scale_fill_viridis(name = 
                       'Percentage Exotic',
                     discrete=FALSE) +
  geom_vline(data = cde.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Change in Biomass in Persistent Species',
        title= 'Biomass Change in Persistent Species',
        y= ' Site Code'
        #color= ''
  )+
  # geom_text(data = cde.ps %>%
  #             group_by(multilimited) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(multilimited, n_study, .keep_all = T),
  #           aes(x=2.5, y=multilimited,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")
cde.e




(rich.e|bm.e)/(sl.e|sg.e|cde.e)





rich.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/rich_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')


rich.ps <- rich.ps %>% left_join(p3)


View(rich.ps)

rich.e.p<-   ggplot() +
  geom_rect(data = rich.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sl.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_rich)) ,
    data = rich.ps ,
    aes(x = rich.trt.study + rich.trt.global, 
        y = site_ex_p#, fill=site_percent_int_rich,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  # scale_fill_viridis(name = 
  #                      'Percentage Exotic',
  #                    discrete=FALSE) +
  geom_vline(data = rich.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Richness',
        title= ' Species Richness',
        y= 'Site Percent Exotic'
        #color= ''
  )+
  geom_text(data = rich.ps %>%
              group_by(site_ex_p) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(site_ex_p, n_study, .keep_all = T),
            aes(x=2.5, y=site_ex_p,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

rich.e.p




bm.ps <- bm.ps %>% left_join(p3)




bm.e.p<-   ggplot() +
  geom_rect(data = bm.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sl.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_bm)) ,
    data = bm.ps ,
    aes(x = bm.trt.study + bm.trt.global, 
        y = site_ex_p#, fill=site_percent_int_bm,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  # scale_fill_viridis(name = 
  #                      'Percentage Exotic',
  #                    discrete=FALSE) +
  geom_vline(data = bm.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on bmness',
        title= 'Biomass',
        y= 'Site Percent Exotic'
        #color= ''
  )+
  geom_text(data = bm.ps %>%
              group_by(site_ex_p) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(site_ex_p, n_study, .keep_all = T),
            aes(x=150, y=site_ex_p,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

bm.e.p



sl.ps <- sl.ps %>% left_join(p3)




sl.e.p<-   ggplot() +
  geom_rect(data = sl.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sl.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_sl)) ,
    data = sl.ps ,
    aes(x = sl.trt.study + sl.trt.global, 
        y = site_ex_p#, fill=site_percent_int_sl,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  # scale_fill_viridis(name = 
  #                      'Percentage Exotic',
  #                    discrete=FALSE) +
  geom_vline(data = sl.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(x='Effect of NPK on Change in Biomss Due to Species Loss/Year',
       title= 'Biomass Change due to Species Loss',
        y= 'Site Percent Exotic'
        #color= ''
  )+
  geom_text(data = sl.ps %>%
              group_by(site_ex_p) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(site_ex_p, n_study, .keep_all = T),
            aes(x=15, y=site_ex_p,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sl.e.p

sg.ps <- sg.ps %>% left_join(p3)




sg.e.p<-   ggplot() +
  geom_rect(data = sg.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sg.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_sg)) ,
    data = sg.ps ,
    aes(x = sg.trt.study + sg.trt.global, 
        y = site_ex_p#, fill=site_percent_int_sg,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  # scale_fill_viridis(name = 
  #                      'Percentage Exotic',
  #                    discrete=FALSE) +
  geom_vline(data = sg.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(x='Effect of NPK on Change in Biomss Due to Species Gain/Year',
       title= 'Biomass Change due to Species Gain',
       y= 'Site Percent Exotic'
       #color= ''
  )+
  geom_text(data = sg.ps %>%
              group_by(site_ex_p) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(site_ex_p, n_study, .keep_all = T),
            aes(x=75, y=site_ex_p,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sg.e.p

cde.ps <- cde.ps %>% left_join(p3)



cde.e.p<-   ggplot() +
  geom_rect(data = cde.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = cde.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_cde)) ,
    data = cde.ps ,
    aes(x = cde.trt.study + cde.trt.global, 
        y = site_ex_p#, fill=site_percent_int_cde,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  # scale_fill_viridis(name = 
  #                      'Percentage Exotic',
  #                    discrete=FALSE) +
  geom_vline(data = cde.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Change in Biomass of Persistent Species',
        title= 'Biomass Change in Persistent Species',
        y= 'Site Percent Exotic'
        #color= ''
  )+
  geom_text(data = cde.ps %>%
              group_by(site_ex_p) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(site_ex_p, n_study, .keep_all = T),
            aes(x=150, y=site_ex_p,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

cde.e.p



(rich.e.p|bm.e.p)/(sl.e.p|sg.e.p|cde.e.p)




load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')
sloss.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/sloss_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sgain.ps <- read.csv("~/Dropbox/Projects/NutNet/Data/sgain_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sloss.ps <- sloss.ps %>% left_join(p3)
sgain.ps <- sgain.ps %>% left_join(p3)


View(sloss.p)

sloss.e.p<-   ggplot() +
  geom_rect(data = sloss.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sloss.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_sloss)) ,
    data = sloss.ps ,
    aes(x = sloss.trt.study + sloss.trt.global, 
        y = site_ex_p#, fill=site_percent_int_sloss,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  # scale_fill_viridis(name = 
  #                      'Percentage Exotic',
  #                    discrete=FALSE) +
  geom_vline(data = sloss.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(x='Effect of NPK on Change in Biomss Due to Species Loss/Year',
       title= 'Biomass Change due to Species Loss',
       y= 'Site Percent Exotic'
       #color= ''
  )+
  geom_text(data = sloss.ps %>%
              group_by(site_ex_p) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(site_ex_p, n_study, .keep_all = T),
            aes(x=4, y=site_ex_p,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sloss.e.p

sgain.ps <- sgain.ps %>% left_join(p3)




sgain.e.p<-   ggplot() +
  geom_rect(data = sgain.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(#data = sgain.ps  %>% mutate(site_code = fct_reorder(site_code, site_percent_int_sgain)) ,
    data = sgain.ps ,
    aes(x = sgain.trt.study + sgain.trt.global, 
        y = site_ex_p#, fill=site_percent_int_sgain,
    ), 
    scale = 1, alpha = 0.6,
    linetype = 0) +
  # scale_fill_viridis(name = 
  #                      'Percentage Exotic',
  #                    discrete=FALSE) +
  geom_vline(data = sgain.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(x='Effect of NPK on Change in Biomss Due to Species Gain/Year',
       title= 'Biomass Change due to Species Gain',
       y= 'Site Percent Exotic'
       #color= ''
  )+
  geom_text(data = sgain.ps %>%
              group_by(site_ex_p) %>%
              mutate(n_study = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(site_ex_p, n_study, .keep_all = T),
            aes(x=4, y=site_ex_p,
                label=paste('n[study] == ', n_study)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sgain.e.p



View(sloss.ps)

sloss.ps.c <- sloss.ps %>% select(sloss.trt.study,sloss.trt.global,site_code,site_percent_int_rich) %>% group_by(site_code) %>%
  filter(sloss.trt.study > quantile(sloss.trt.study, probs=0.025),
         sloss.trt.study < quantile(sloss.trt.study, probs=0.975)) %>%
  filter(sloss.trt.global > quantile(sloss.trt.global, probs=0.025),
         sloss.trt.global < quantile(sloss.trt.global, probs=0.975)) %>%
  sample_n(500) 
  

sloss.e.p.c<-   ggplot() +
  geom_point(
    data = sloss.ps.c ,
    aes(x = site_percent_int_rich  , 
        y = sloss.trt.study + sloss.trt.global), alpha=0.3) +
  geom_smooth(data = sloss.ps.c ,
              aes(x = site_percent_int_rich , 
                  y = sloss.trt.study + sloss.trt.global ), method='lm', formula=y~x)+
  theme_bw() +
  labs(x='Site Percent Exotic',
       title= 'Species Loss',
       y= 'Effect of NPK on  Species Loss/Year' 
       #color= ''
  )+ #ylim(0,100) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sloss.e.p.c


sgain.ps.c <- sgain.ps %>% select(sgain.trt.study,sgain.trt.global,site_code,site_percent_int_rich) %>% group_by(site_code) %>%
  filter(sgain.trt.study > quantile(sgain.trt.study, probs=0.025),
         sgain.trt.study < quantile(sgain.trt.study, probs=0.975)) %>%
  filter(sgain.trt.global > quantile(sgain.trt.global, probs=0.025),
         sgain.trt.global < quantile(sgain.trt.global, probs=0.975)) %>%
  sample_n(500) 



sgain.e.p.c<-   ggplot() +
  geom_point(
    data = sgain.ps ,
    aes(x = site_percent_int_rich  , 
        y = sgain.trt.study + sgain.trt.global, alpha=0.3)) +
  geom_smooth(data = sgain.ps ,
              aes(x = site_percent_int_rich , 
                  y = sgain.trt.study + sgain.trt.global ), method='lm', formula=y~x)+
  theme_bw() +
  labs(x='Site Percent Exotic',
       title= 'Species Gain',
       y= 'Effect of NPK on  Species Gain/Year' 
       #color= ''
  )+ #ylim(0,100) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sgain.e.p.c

(sloss.e.p.c)/(sgain.e.p.c)

