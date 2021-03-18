


# Fig 1

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)
library(viridis)
library(ggrepel)

# emmas links
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
start.rich <-read.csv("~/Dropbox/Projects/NutNet/Data/start.rich.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

dat2<-distinct(plot, site_code,site_name, country, continent,habitat, latitude, longitude,elevation, experiment_type)

plot$year_trt<-as.numeric(as.character(plot$year_trt))
plot$continent<-as.factor(plot$continent)
plot$habitat<-as.factor(plot$habitat)
plot$site<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot$f.year_trt<-as.factor(as.character(plot$year_trt))


# Make Figure 1
plot2<-plot[plot$trt %in% c('NPK'),]
plot2$unique.id<-as.character(with(plot2, paste(site_code,block,plot, sep=".")))
#remove NAS
plot3<-plot2 %>% drop_na(plot.mass)

plot3$rich<-plot3$all.div

plot4<-plot3 %>% group_by(site_code, block,plot,year_trt) %>%
  select(continent,unique.id,site_code,block, plot,year_trt,rich,plot.mass)


plotzero<-plot4[plot4$year_trt %in% c('0'),]

s.check<-plotzero %>% arrange(site_code)
View(s.check)

plotmax<-plot4 %>% group_by(site_code) %>% top_n(1, year_trt)


s.check<-plotmax %>% arrange(site_code)


plot5<-bind_rows(plotmax,plotzero) %>% arrange(site_code)


s.check<-plot5 %>% arrange(site_code)



plot6<-plot5 %>%
  group_by(site_code,year_trt) %>%
  summarise(m.rich = mean(rich),
            r.rich = round(m.rich),
            sd.rich = sd(rich),
            m.mass = mean(plot.mass),
            sd.mass = sd(plot.mass))

s.check<-plot6 %>% arrange(site_code)
View(s.check)

zerorich<-plot6[plot6$year_trt %in% c('0'),]

zrich<-zerorich %>% left_join(start.rich,by="site_code") %>% 
  select(site_code,starting.richness) 

plot7<-inner_join(plot6,zrich)

plot7$starting.richness <- factor(plot7$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

site<-plot7 %>% distinct(site_code)



plot7$f.year_trt<-as.factor(plot7$year_trt)

ggplot(plot7, aes(x=m.rich, y=m.mass,group=site_code))+
  geom_point(size=2,shape=1)+ geom_line(aes(color=f.year_trt))+theme_classic()

yrdat <- plot7 %>% filter(year_trt >= 1) %>% 
  droplevels() %>% 
  select(site_code,year_trt)



yrdat$maxyr<-as.factor(yrdat$year_trt)
yrdat2 <- yrdat %>%
  select(site_code,maxyr)
plot8<-inner_join(plot7,yrdat2)


site<-plot8 %>% distinct(site_code)
View(site)

plot8$startend <- ifelse(plot8$year_trt < 1 , 'start',
                         ifelse(plot8$year_trt >=1, 'end', 'other'))

plot9 <- plot8 %>%
  select(site_code,m.rich,startend) %>%
  spread(startend,m.rich) %>%
  as_tibble() %>% 
  mutate(rich.start = start,
         rich.end = end ) %>% 
  select(-start, -end)


plot10 <- plot8 %>%
  select(site_code,m.mass,startend) %>%
  spread(startend,m.mass) %>%
  as_tibble() %>% 
  mutate(mass.start = start,
         mass.end = end ) %>% 
  select(-start, -end)

plot11<-inner_join(plot9,plot10)
plot12<-inner_join(plot11,yrdat2)


plot13<-inner_join(plot12,zrich)


s.check<-plot13 %>% arrange(site_code)



plot13$maxyr<- as.numeric(plot13$maxyr)

plot13$Experiment.Length <- ifelse(plot13$maxyr >= 1 & plot13$maxyr <= 4, '1-4 years',
                                   ifelse(plot13$maxyr >= 5 & plot13$maxyr <= 8,  '5-8 years',
                                     ifelse(plot13$maxyr >= 9 & plot13$maxyr <= 11,  '9-10 years', 'other')))

View(plot13)

plot13$Experiment.Length2 <- ifelse(plot13$maxyr >= 1 & plot13$maxyr <= 3, '1-3 years',
                                   ifelse(plot13$maxyr >= 4 & plot13$maxyr <= 6,  '4-6',
                                          ifelse(plot13$maxyr >= 7 & plot13$maxyr <= 9,  '7-9',
                                                 ifelse(plot13$maxyr >= 10 & plot13$maxyr <= 12,  '10-12','other'))))


plot13$Experiment.Length2 <- factor(plot13$Experiment.Length2 , levels=c("1-3 years","4-6","7-9","10-12"))

write.csv(plot13, "~/Dropbox/Projects/NutNet/Data/Figure1_dat.csv")



fig1_dat <-read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Figure1_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
t_s1 <-read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_S1.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(fig1_dat)

fig1_dat <- fig1_dat %>% group_by(site_code) %>% filter(maxyr >= 3) %>%
  ungroup() %>% select(-X)

head(fig1_dat)
head(t_s1)

fig1_dat <- fig1_dat %>% left_join(t_s1, by="site_code") 

write.csv(fig1_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/site_dat.csv")


fig1_dat$Experiment.Length2 <- factor(fig1_dat$Experiment.Length2 , levels=c("1-3 years","4-6","7-9","10-12"))



# FIGURE S2
# BIOMASS RICHNESS DIRECTIONAL ARROWS / VARIATION
ggplot() +
  facet_wrap(~maxyr, scales="free")+
  #facet_wrap(~Experiment.Length2, scales="free")+
  geom_text_repel(data=fig1_dat, aes(x=rich.end, y=mass.end,  label = site_n), size=3 ) +
  geom_point(data=fig1_dat,aes(x=rich.start, y=mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data=fig1_dat,aes(x=rich.end,y=mass.end),size=1.5, colour="white", shape=2) +
  geom_segment(data=fig1_dat,aes(x=rich.start,
                               xend=rich.end,
                               y=mass.start,
                               yend=mass.end,
                               group = site_code,
                               color=maxyr),  
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  scale_color_viridis(discrete=F,name="Length of Study") +
  labs(x = 'Species Richness',
       y = expression(paste('Biomass (g/' ,m^2, ')')), 
       title= '') +
   scale_y_continuous(limits=c(0,1500)) +
  scale_x_continuous(limits=c(0,35)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     strip.background = element_blank(),plot.title = element_text(size=12),
                          legend.position="bottom")




ggplot() +
  #facet_wrap(~maxyr, scales="free")+
  #facet_wrap(~Experiment.Length2, scales="free")+
 # geom_text_repel(data=fig1_dat, aes(x=rich.end, y=mass.end,  label = site_n), size=3 ) +
  geom_point(data=fig1_dat,aes(x=rich.start, y=mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data=fig1_dat,aes(x=rich.end,y=mass.end),size=1.5, colour="white", shape=2) +
  geom_segment(data=fig1_dat,aes(x=rich.start,
                                 xend=rich.end,
                                 y=mass.start,
                                 yend=mass.end,
                                 group = site_code,
                                 color=maxyr),  
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  scale_color_viridis(discrete=F,name="Length of Study") +
  labs(x = 'Species Richness',
       y = expression(paste('Biomass (g/' ,m^2, ')')), 
       title= '') +
  scale_y_continuous(limits=c(0,1500)) +
  scale_x_continuous(limits=c(0,35)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     strip.background = element_blank(),plot.title = element_text(size=12),
                     legend.position="bottom")


