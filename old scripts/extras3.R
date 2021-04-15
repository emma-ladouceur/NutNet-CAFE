


library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)


meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/meta.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

rich.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/p.effs.Rdata')

View(meta)
colnames(meta)
#meta <- meta %>% select(site_code, Quadrant)


rich.ps <- rich.ps %>% left_join(meta, by ="site_code")
View(rich.ps)

rich<-ggplot() +
  # geom_density_ridges(data = rich.ps,
  #                     aes(x = rich.trt.study + rich.trt.global,
  #                         y = site_percent_native_rich,
  #                         group=site_percent_native_rich,
  #                         fill= site_percent_native_rich
  #                     ),
  #                     scale = 1, alpha = 0.6,
  #                     linetype = 0) +
  geom_density_ridges(data = rich.ps,
                      aes(x = rich.trt.study + rich.trt.global,
                          y = site_percent_int_rich,
                          group=site_percent_int_rich,
                          fill= site_percent_native_rich
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis(name = #'site_rich_range',
                       #'starting.richness' ,
                      # 'site_rich_range',
                       'site_percent_int_rich',
                      # 'site_percent_native_rich',
                   #  'anthropogenic',
                     #'NDep.cats',
                     #'biome' ,
                     #'site_dom',
                     #'Realm',
                     #'colimitation',
                     discrete=FALSE) +
  # geom_vline(data = rich.ps %>% filter(response=="NPK"),
  #            aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x='Effect of NPK on Change in Species Richness/Year',
        title= 'a) Species Richness',
        y= 'Slope'
        #color= ''
  )+
  # geom_text(data = rich.ps %>%
  #             group_by(site_dom) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(site_dom, n_study, .keep_all = T),
  #           aes(x=2.5, y=site_dom,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
 # scale_x_continuous(breaks=c(-4,-2,0,2), limits=c(-4,2))+
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none") + 
  coord_cartesian(clip = "off")

rich


bm.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/bm_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



bm.ps <- bm.ps %>% left_join(meta, by ="site_code")
colnames(bm.p)
bm.ps


bm<-ggplot() +
  geom_rect(data = bm.p %>% filter(response=="NPK"),
            aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
                alpha = 0.3)) +
  geom_density_ridges(data = bm.ps,
                      aes(x = bm.trt.study + bm.trt.global, 
                          y = Quadrant,
                      ), fill="#0B775E",
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = bm.ps,
                      aes(x = bm.ctl.study + bm.ctl.global, 
                          y = Quadrant 
                      ), colour ="black", 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  #' scale_fill_viridis(name = #'site_bm_range',
  #'                      #'starting.bmness' ,
  #'                      'site_bm_range',
  #'                    # 'anthropogenic',
  #'                    #'NDep.cats',
  #'                    #'biome' ,
  #'                    #'site_dom',
  #'                    #'Realm',
  #'                    #'colimitation',
  #'                    discrete=TRUE) +
  geom_vline(data = bm.p %>% filter(response=="NPK"),
             aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/Year')),
        title= 'a) Biomass',
        y= ''
        #color= ''
  )+
  geom_text(data = bm.ps %>%
              group_by(Quadrant) %>%
              mutate(n_sites = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_sites, .keep_all = T),
            aes(x=175, y=Quadrant,
                label=paste('n[sites] == ', n_sites)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

bm


(rich | bm )





meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/meta.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- meta %>% select(site_code, Quadrant)


sl.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sl.ps <- sl.ps %>% left_join(meta, by ="site_code") #%>%
  #filter(!Quadrant == "-biomass +rich")

#sl.ps$Quadrant<-factor(sl.ps$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/study.price.p.effs.q.Rdata')
npk.effs <- all.effs %>% select(site_code,sg.trt.rate.p, sl.trt.rate.p,cde.trt.rate.p,sloss.trt.rate.p,sgain.trt.rate.p, Quadrant) %>%
  filter(!Quadrant == "-biomass +rich")

head(npk.effs)
#npk.effs$Quadrant<-factor(npk.effs$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

sl.ps$anthropogenic<- as.factor(as.character(sl.ps$anthropogenic))
sl.ps$site_percent_int_rich<- as.factor(as.character(sl.ps$site_percent_int_rich))

sl<-ggplot() +
  geom_density_ridges(data = sl.ps,
                      aes(x = sl.trt.study + sl.trt.global,
                          y = anthropogenic,
                         # group=site_percent_int_rich,
                          fill= site_percent_int_rich
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis(name = #'site_rich_range',
                       #'starting.richness' ,
                       # 'site_rich_range',
                       'site_percent_int_rich',
                     # 'site_percent_native_rich',
                     #  'anthropogenic',
                     #'NDep.cats',
                     #'biome' ,
                     #'site_dom',
                     #'Realm',
                     #'colimitation',
                     discrete=FALSE) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  scale_x_continuous(breaks=c(-40,-20,-10,-5,0,10), limits=c(-40,15))+
  labs( #x='',
    x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/Year')),
    title= 'b) Change in Biomass Due to Species Loss',
    y= ''
    #color= ''
  )+
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="bottom") 

sl


sg.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sg.ps <- sg.ps %>% left_join(meta, by ="site_code")%>%
  filter(!Quadrant == "-biomass +rich")

sg.ps %>% distinct(site_code, Quadrant)

sg.ps$Quadrant<-factor(sg.ps$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

colnames(sg.p)
sg.p


sg<-ggplot() +
  # geom_rect(data = sg.p %>% filter(response=="NPK"),
  #           aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
  #               alpha = 0.3)) +
  geom_density_ridges(data = sg.ps,
                      aes(x = sg.trt.study + sg.trt.global, 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = sg.trt.rate.p),
             colour= "#3B9AB2",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sg.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(sg.trt.study + sg.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  # geom_vline(data = sg.p %>% filter(response=="NPK"),
  #            aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( #x = '',
    x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/Year')),
    title= 'd) Change in Biomass Due to Species Gain',
    y= ' '
    #color= ''
  )+
  scale_x_continuous(breaks=c(-10,0,5,10,20,40,60,80), limits=c(-20,80))+
  # geom_text(data = sg.ps %>%
  #             group_by(Quadrant) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(Quadrant, n_study, .keep_all = T),
  #           aes(x=75, y=Quadrant,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

sg



cde.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

cde.ps <- cde.ps %>% left_join(meta, by ="site_code")%>%
  filter(!Quadrant == "-biomass +rich")

cde.ps$Quadrant<-factor(cde.ps$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

colnames(cde.p)
cde.p


cde<-ggplot() +
  # geom_rect(data = cde.p %>% filter(response=="NPK"),
  #           aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
  #               alpha = 0.3)) +
  geom_density_ridges(data = cde.ps,
                      aes(x = cde.trt.study + cde.trt.global, 
                          y = Quadrant,
                      ), fill="#F98400",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = cde.trt.rate.p),
             colour= "#F98400",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= cde.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(cde.trt.study + cde.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  # geom_vline(data = cde.p %>% filter(response=="NPK"),
  #            aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(#x='', 
    x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/Year')),
    title= 'e) Persistent Species Change in Biomass',
    y= ''
    #color= ''
  )+
  scale_x_continuous(breaks=c(-150,-100,-50,-25,0,25,50,100), limits=c(-175,100))+
  geom_text(data = cde.ps %>%
              group_by(Quadrant) %>%
              mutate(n_sites = n_distinct(site_code)) %>%
              ungroup() %>%
              distinct(Quadrant, n_sites, .keep_all = T),
            aes(x=80, y=Quadrant,
                label=paste('n[sites] == ', n_sites)),
            size=3.5,
            nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")

cde


(sl | sg | cde)




(rich | bm )/(sl | sg | cde)





sloss.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sloss.ps <- sloss.ps %>% left_join(meta, by ="site_code") #%>%
 # filter(!Quadrant == "-biomass +rich")

#sloss.ps$Quadrant<-factor(sloss.ps$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

View(sloss.ps)
View(sloss.p)
sloss.p


sloss<-ggplot() +
  # geom_rect(data = sloss.p %>% filter(response=="NPK"),
  #           aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
  #               alpha = 0.3)) +
  geom_density_ridges(data = sloss.ps,
                      aes(x = sloss.trt.study + sloss.trt.global, 
                          y = Quadrant,
                      ), fill="#B40F20",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = sloss.trt.rate.p),
             colour= "#B40F20", shape=1, size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sloss.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(sloss.trt.study + sloss.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  # geom_vline(data = sloss.p %>% filter(response=="NPK"),
  #            aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x = expression(paste('Effect of NPK on Species Loss / Year')),
        title= 'a) Species Loss',
        y= ' Slope'
        #color= ''
  )+
  scale_x_continuous(breaks=c(-2,-1,0,1,2), limits=c(-2,2))+
  # geom_text(data = sl.ps %>%
  #             group_by(multilimited) %>%
  #             mutate(n_study = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(multilimited, n_study, .keep_all = T),
  #           aes(x=20, y=multilimited,
  #               label=paste('n[study] == ', n_study)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none")+  scale_y_discrete(labels = function(x) str_wrap(x, width = 8))

sloss


sgain.ps <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sgain.ps <- sgain.ps %>% left_join(meta, by ="site_code")%>%
  filter(!Quadrant == "-biomass +rich")

sgain.ps$Quadrant<-factor(sgain.ps$Quadrant,  levels=c("+biomass +rich",  "-biomass -rich" , "+biomass -rich"))

colnames(sgain.ps)
sgain.p


sgain<-ggplot() +
  # geom_rect(data = sgain.p %>% filter(response=="NPK"),
  #           aes(xmin = eff_lower, xmax =  eff_upper, ymin = -Inf, ymax = Inf,
  #               alpha = 0.3)) +
  geom_density_ridges(data = sgain.ps,
                      aes(x = sgain.trt.study + sgain.trt.global, 
                          y = Quadrant,
                      ), fill="#3B9AB2",
                      scale = 1, alpha = 0.3,
                      linetype = 0) +
  geom_point(data =npk.effs, aes(y = Quadrant, x = sgain.trt.rate.p),
             colour= "#3B9AB2",shape=1,size = 2,  position = position_jitter(height = 0.02 )) +
  geom_point(data= sgain.ps %>% group_by(Quadrant) %>%
               summarise(mean.s.eff = median(sgain.trt.study + sgain.trt.global)), 
             aes(x= mean.s.eff, y= Quadrant),  size=4, shape=5)+
  # geom_vline(data = sgain.p %>% filter(response=="NPK"),
  #            aes(xintercept = eff)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( x = expression(paste('Effect of NPK on Species Gain / Year')),
        title= 'c) Species Gain',
        y= ' '
        #color= ''
  )+
  scale_x_continuous(breaks=c(-1,0,1), limits=c(-1.5,1.5))+
  # geom_text(data = sg.ps %>%
  #             group_by(Quadrant) %>%
  #             mutate(n_sites = n_distinct(site_code)) %>%
  #             ungroup() %>%
  #             distinct(Quadrant, n_sites, .keep_all = T),
  #           aes(x=2.75, y=Quadrant,
  #               label=paste('n[sites] == ', n_sites)),
  #           size=3.5,
  #           nudge_y = 0.1, parse = T) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=9),
        axis.title.x = element_text(size=9, hjust = 0.5),
        title=element_text(size=9),
        legend.key = element_blank(),
        legend.position="none") 

sgain


( rich | bm )/( sloss | sgain )/( sl | sg | cde )


# LANDSCAPE 7 X 17
( sloss | sl | sgain | sg |  cde  )

