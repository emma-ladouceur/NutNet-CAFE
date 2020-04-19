


# Figs 1-2

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)

#emmas links
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )

dat2<-distinct(plot, site_code,site_name, country, continent,habitat, latitude, longitude,elevation, experiment_type)

plot<-plot[plot$trt %in% c('NPK', 'Control'),]

plot$year_trt<-as.numeric(as.character(plot$year_trt))
plot$continent<-as.factor(plot$continent)
plot$habitat<-as.factor(plot$habitat)
plot$site<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot$f.year_trt<-as.factor(as.character(plot$year_trt))

#log
plot$log.rich<-log(plot$rich)
plot$log.live.mass<-log(plot$live_mass)

plot$log.year.trt<-log(plot$year_trt + 1)



# Make Figure 1
plot2<-plot[plot$trt %in% c('NPK'),]
plot2$unique.id<-as.character(with(plot2, paste(site_code,block,plot, sep=".")))
#remove NAS
plot3<-plot2 %>% drop_na(live_mass)


plot4<-plot3 %>% group_by(site_code, block,plot,year_trt) %>%
  select(continent,unique.id,site_code,block, plot,year_trt,rich,live_mass)

s.check<-plotzero %>% arrange(site_code)
View(s.check)

plotzero<-plot4[plot4$year_trt %in% c('0'),]

plotmax<-plot4 %>% group_by(site_code) %>% top_n(1, year_trt)


s.check<-plotmax %>% arrange(site_code)
View(s.check)


plot5<-bind_rows(plotmax,plotzero) %>% arrange(site_code)


s.check<-plot5 %>% arrange(site_code)
View(s.check)
View(s.check)

ggplot(plot5, aes(x=rich, y=live_mass, group=unique.id))+
  geom_point(size=2,shape=1)+ geom_line()+theme_classic()



plot6<-plot5 %>%
  group_by(site_code,year_trt) %>%
  summarise(m.rich = mean(rich),
            r.rich = round(m.rich),
            sd.rich = sd(rich),
            m.mass = mean(live_mass),
            sd.mass = sd(live_mass))

s.check<-plot6 %>% arrange(site_code)
View(s.check)

zerorich<-plot6[plot6$year_trt %in% c('0'),]

zerorich$starting.richness <- ifelse(zerorich$r.rich >= 1 & zerorich$r.rich <= 5, '1-5 species',
                                     ifelse(zerorich$r.rich >=6 & zerorich$r.rich <=10, '6-10',
                                            ifelse(zerorich$r.rich >=11 & zerorich$r.rich <=15, '11-15',    
                                                   ifelse(zerorich$r.rich >=16 & zerorich$r.rich <=20, '16-20',
                                                          ifelse(zerorich$r.rich >=21 & zerorich$r.rich <=25, '21-25',
                                                                 ifelse(zerorich$r.rich >=26, '>26', 'other'))))))

zrich<-zerorich %>% 
  select(site_code,starting.richness)

plot7<-inner_join(plot6,zrich)

plot7$starting.richness <- factor(plot7$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

site<-plot7 %>% distinct(site_code)
View(site)

ggplot(plot7, aes(x=m.rich, y=m.mass, color=starting.richness,group=site_code))+
  geom_point(size=2,shape=1)+ geom_line()+
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+theme_classic()


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
View(s.check)

ggplot() +
  geom_point(data=plot13,aes(x=rich.start, y=mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data=plot13,aes(x=rich.end,y=mass.end),size=1.5, fill="white", shape=2) +
  #geom_point(size=1.5, fill="white", shape=2)+
  geom_segment(data=plot13,aes(x=rich.start,
                               xend=rich.end,
                               y=mass.start,
                               yend=mass.end,
                               group = site_code,
                               colour=maxyr), 
               arrow=arrow(length=unit(0.3,"cm"))) +
  theme_classic()

plot13$starting.richness <- factor(plot13$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


plot13$maxyr<- as.numeric(plot13$maxyr)

plot13$Experiment.Length <- ifelse(plot13$maxyr >= 1 & plot13$maxyr <= 4, '1-4 years',
                                   ifelse(plot13$maxyr >= 5 & plot13$maxyr <= 8,  '5-8 years',
                                     ifelse(plot13$maxyr >= 9 & plot13$maxyr <= 11,  '9-10 years', 'other')))

View(plot13)

plot13$Experiment.Length2 <- ifelse(plot13$maxyr >= 1 & plot13$maxyr <= 3, '1-3 years',
                                   ifelse(plot13$maxyr >= 4 & plot13$maxyr <= 6,  '4-6',
                                          ifelse(plot13$maxyr >= 7 & plot13$maxyr <= 9,  '7-9',
                                                 ifelse(plot13$maxyr >= 10 & plot13$maxyr <= 11,  '10-11','other'))))


plot13$Experiment.Length2 <- factor(plot13$Experiment.Length2 , levels=c("1-3 years","4-6","7-9","10-11"))



# FIGURE 1
# BIOMASS RICHNESS DIRECTIONAL ARROWS / VARIATION
ggplot() +
  #facet_wrap(~Experiment.Length2, scales="free")+
  geom_point(data=plot13,aes(x=rich.start, y=mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data=plot13,aes(x=rich.end,y=mass.end),size=1.5, colour="white", shape=2) +
  #geom_point(size=1.5, fill="white", shape=2)+
  geom_segment(data=plot13,aes(x=rich.start,
                               xend=rich.end,
                               y=mass.start,
                               yend=mass.end,
                               group = site_code,
                               colour=starting.richness,# linetype= Experiment.Length 
                               ), 
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  #scale_linetype_manual(values=c("solid","dashed", "dotted"))+
  labs(x = 'Species Richness',
       y = expression(paste('Biomass (g/' ,m^2, ')')), 
       title= 'Experiment Length', color=" Starting Richness", linetype="Experiment Length") +
   scale_y_continuous(limits=c(0,2500)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     strip.background = element_blank(),plot.title = element_text(size=12),
                          legend.position="bottom")

library(ggrepel)


ggplot() +
  facet_wrap(~maxyr, scales="free")+
  #geom_text_repel(data=plot13, aes(x=rich.end, y=mass.end,  label = site_code), size=3 ) +
  geom_point(data=plot13,aes(x=rich.start, y=mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data=plot13,aes(x=rich.end,y=mass.end),size=1.5, colour="white", shape=2) +
  #geom_point(size=1.5, fill="white", shape=2)+
  geom_segment(data=plot13,aes(x=rich.start,
                               xend=rich.end,
                               y=mass.start,
                               yend=mass.end,
                               group = site_code,
                               colour=starting.richness,# linetype= Experiment.Length 
  ), 
  arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  #scale_linetype_manual(values=c("solid","dashed", "dotted"))+
  labs(x = 'Species Richness',
       y = expression(paste('Biomass (g/' ,m^2, ')')), 
       title= 'Experiment Length', color=" Starting Richness", linetype="Experiment Length") +
  scale_y_continuous(limits=c(0,2500)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     strip.background = element_blank(),plot.title = element_text(size=12),
                     legend.position="bottom")


