

# Figs 1-2

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)


# NOTE TO SELF: ALSO UPDATE THE MODELS & FIGS WITH THE NEW DATASET
# DOUBLE CHECK THAT FINALAND SAANA STARTS FROM YEAR 0, IF NOT CHANGE


#emmas links
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

#plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
#plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )

dat2<-distinct(p, continent, site_code, year_trt)

dat2<-distinct(p, continent, site_code, year_trt)
View(dat2)
colnames(plot)
dat2<-distinct(plot, site_code, country, continent,habitat, latitude, longitude,elevation, experiment_type)
View(dat2)
#write.csv(dat2,"~/Dropbox/Projects/NutNet/Data/nutnet_distinct.csv")

#plot<-p[p$trt %in% c('NPK'),]
#or
#plot<-plot[plot$trt %in% c('NPK', 'Control'),]
nrow(plot)

colnames(plot)
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


par(mfrow=c(2,2))
hist(plot$rich,breaks =40, main="rich", xlab= "rich")
hist(plot$live_mass, breaks=40, main="bm", xlab= "bm")

hist(plot$log.rich,breaks =40, main="rich", xlab= "rich")
hist(plot$log.live.mass, breaks=40, main="bm", xlab= "bm")


# Make Figure 1
plot2<-plot[plot$trt %in% c('NPK'),]
plot2$unique.id<-as.character(with(plot2, paste(site_code,block,plot, sep=".")))
#remove NAS
plot3<-plot2 %>% drop_na(plot.mass)

colnames(plot3)
plot4<-plot3 %>% group_by(site_code, block,plot,year_trt) %>%
  select(continent,id,site_code,block, plot,year_trt,rich,plot.mass)

plotzero<-plot4[plot3$year_trt %in% c('0'),]
View(plotzero)
plotmax<-plot4 %>% group_by(site_code) %>% top_n(1, year_trt)
View(plotmax)
plot5<-bind_rows(plotmax,plotzero)

View(plot5)
ggplot(plot5, aes(x=rich, y=plot.mass, group=id))+
  geom_point(size=2,shape=1)+ geom_line()+theme_classic()



plot6<-plot5 %>%
  group_by(site_code,year_trt) %>%
  summarise(m.rich = mean(rich),
            r.rich = round(m.rich),
            sd.rich = sd(rich),
            m.mass = mean(plot.mass),
            sd.mass = sd(plot.mass))

View(plot6)
zerorich<-plot6[plot6$year_trt %in% c('0'),]

zerorich$starting.richness <- ifelse(zerorich$r.rich >= 1 & zerorich$r.rich <= 5, '1-5 species',
                                     ifelse(zerorich$r.rich >=6 & zerorich$r.rich <=10, '6-10',
                                            ifelse(zerorich$r.rich >=11 & zerorich$r.rich <=15, '11-15',    
                                                   ifelse(zerorich$r.rich >=16 & zerorich$r.rich <=20, '16-20',
                                                          ifelse(zerorich$r.rich >=21 & zerorich$r.rich <=25, '21-25',
                                                                 ifelse(zerorich$r.rich >=26, '>26', 'other'))))))

zrich<-zerorich %>% ungroup() %>%
  select(site_code,starting.richness)

plot7<-inner_join(plot6,zrich)

plot7$starting.richness <- factor(plot7$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

ggplot(plot7, aes(x=m.rich, y=m.mass, color=starting.richness,group=site_code))+
  geom_point(size=2,shape=1)+ geom_line()+
scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                               "6-10" = "#75B41EFF",
                               "11-15" ="#5AC2F1FF",
                               "16-20"= "#0C5BB0FF",
                               "21-25" = "#972C8DFF",
                               ">26" = "#E0363AFF", drop =FALSE))+theme_classic()

View(plot7)
plot7$f.year_trt<-as.factor(plot7$year_trt)

ggplot(plot7, aes(x=m.rich, y=m.mass,group=site_code))+
  geom_point(size=2,shape=1)+ geom_line(aes(color=f.year_trt))+theme_classic()

yrdat <- plot7 %>% filter(year_trt > 1) %>% 
  droplevels() %>% 
  select(site_code,year_trt)

yrdat$maxyr<-as.factor(yrdat$year_trt)
yrdat2 <- yrdat %>%
  select(site_code,maxyr)
plot8<-inner_join(plot7,yrdat2)
View(plot8)

plot8$startend <- ifelse(plot8$year_trt < 1 , 'start',
                         ifelse(plot8$year_trt >=1, 'end', 'other'))

plot9 <- plot8 %>%
  select(site_code,m.rich,startend) %>%
  spread(startend,m.rich) %>%
  as_tibble() %>% 
  mutate(rich.start = start,
         rich.end = end ) %>% 
  select(-start, -end)
View(plot9)

 plot10 <- plot8 %>%
  select(site_code,m.mass,startend) %>%
  spread(startend,m.mass) %>%
  as_tibble() %>% 
  mutate(mass.start = start,
         mass.end = end ) %>% 
  select(-start, -end)

plot11<-inner_join(plot9,plot10)
plot12<-inner_join(plot11,yrdat2)
View(plot13)

plot13<-inner_join(plot12,zrich)


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

View(plot13)


# FIGURE 1
# BIOMASS RICHNESS DIRECTIONAL ARROWS / VARIATION
ggplot() +
  geom_point(data=plot13,aes(x=rich.start, y=mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data=plot13,aes(x=rich.end,y=mass.end),size=1.5, colour="white", shape=2) +
  #geom_point(size=1.5, fill="white", shape=2)+
  geom_segment(data=plot13,aes(x=rich.start,
                               xend=rich.end,
                               y=mass.start,
                               yend=mass.end,
                               group = site_code,
                               colour=starting.richness), 
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
labs(x = 'Species Richness',
     y = expression(paste('Biomass (g/' ,m^2, ')')), 
     title= '', color=" Starting Richness") +
  theme_classic()


# Supplementary figure s1 plot level version of fig 1

ggplot(plot5, aes(x=rich, y=plot.mass, group=id))+
  geom_point(size=2,shape=1)+ geom_line()+theme_classic()

plot5$unique.id<- as.character(with(plot5, paste(site_code,block,plot, sep="_")))

View(plot5)
head(plot5)
zerorich<-plot5[plot5$year_trt %in% c('0'),]

zerorich$starting.richness <- ifelse(zerorich$rich >= 1 & zerorich$rich <= 5, '1-5 species',
                                     ifelse(zerorich$rich >=6 & zerorich$rich <=10, '6-10',
                                            ifelse(zerorich$rich >=11 & zerorich$rich <=15, '11-15',    
                                                   ifelse(zerorich$rich >=16 & zerorich$rich <=20, '16-20',
                                                          ifelse(zerorich$rich >=21 & zerorich$rich <=25, '21-25',
                                                                 ifelse(zerorich$rich >=26, '>26', 'other'))))))
View(zerorich)
zrich<-zerorich %>% 
  select(unique.id,site_code,block,plot,starting.richness)

View(zrich)
plot7<-inner_join(plot5,zrich)

plot7$starting.richness <- factor(plot7$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

View(plot7)

yrdat <- plot7 %>% filter(year_trt > 1) %>% 
  droplevels() %>% 
  select(unique.id,site_code,year_trt)

yrdat$maxyr<-as.factor(yrdat$year_trt)
yrdat2 <- yrdat %>%
  select(unique.id,site_code,maxyr)
plot8<-inner_join(plot7,yrdat2)
View(plot8)

plot8$startend <- ifelse(plot8$year_trt < 1 , 'start',
                         ifelse(plot8$year_trt >=1, 'end', 'other'))

View(plot8)
head(plot8)
plot8$unique.id<- as.character(with(plot8, paste(site_code,block,plot,year_trt, sep="_")))

plot9 <- plot8 %>%
  select(unique.id,rich,startend) %>%
  spread(startend,rich) %>%
  as_tibble() %>% 
  mutate(rich.start = start,
         rich.end = end ) %>% 
  select(-start, -end)
View(plot9)

plot10 <- plot8 %>%
  select(unique.id,site_code,plot.mass,startend) %>%
  spread(startend,plot.mass) %>%
  as_tibble() %>% 
  mutate(mass.start = start,
         mass.end = end ) %>% 
  select(-start, -end)

plot11<-inner_join(plot9,plot10)
plot12<-inner_join(plot11,yrdat2)
View(plot13)

plot13<-inner_join(plot12,zrich)


ggplot() +
  geom_point(data=plot13,aes(x=rich.start, y=mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data=plot13,aes(x=rich.end,y=mass.end),size=1.5, colour="white", shape=2) +
  #geom_point(size=1.5, fill="white", shape=2)+
  geom_segment(data=plot13,aes(x=rich.start,
                               xend=rich.end,
                               y=mass.start,
                               yend=mass.end,
                               group = site_code,
                               colour=starting.richness), 
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  labs(x = 'Species Richness',
       y = expression(paste('Biomass (g/' ,m^2, ')')), 
       title= '', color=" Starting Richness") +
  theme_classic()


# FIGURE 2
# QUADRANT PLOT DELTA BIOMASS DELTA RICH ?
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
start.rich <-read.csv("~/Dropbox/Projects/NutNet/Data/start.rich.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



#models
load('~/Dropbox/Projects/NutNet/Model_fits/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.g

#RICHNESS

plot.rich_coeff <- coef(plot.rich.g)

plot.rich_coef<-as.data.frame(plot.rich_coeff$site_code)
#names(plot.rich_coef) <- gsub(":", ".", names(plot.rich_coef), fixed = TRUE)



plot.rich_coef2 <-  bind_cols(plot.rich_coef %>% 
                                as_tibble() %>% 
                                mutate(
                                  # Estimate.trtNPK.year_trt = "Estimate.trtNPK:year_trt",
                                  #trtNPK.year_trt_lower = "Q2.5.trtNPK:year_trt",
                                  #trtNPK.year_trt_upper = "Q97.5.trtNPK:year_trt",
                                  #    Slope = Estimate.year_trt ,
                                  #   Slope_lower =Q2.5.year_trt ,
                                  #  Slope_upper = Q97.5.year_trt,
                                  site_code = rownames(plot.rich_coef)) %>% 
                                as_tibble() %>%
                                # join with min and max of the x-values
                                inner_join(plot %>% 
                                             group_by(site_code) %>% 
                                             summarise(xmin = min(year_trt),
                                                       xmax = max(year_trt)),
                                           by = 'site_code') %>%
                                left_join(start.rich,
                                           by = 'site_code'))





colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"




# View(plot.rich_coef2)

dat<-distinct(plot, site_code, continent,habitat)
plot.rich_coef3<-full_join(plot.rich_coef2,dat)


plot.rich_fitted <- cbind(plot.rich.g$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(plot.rich.g, re_formula = NA)) %>% 
  as_tibble() %>% left_join(start.rich, by="site_code")
# View(plot.rich_fitted)

plot.rich_fitted2<-full_join(plot.rich_fitted,dat)
plot.rich_fitted.npk<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('NPK'),]
plot.rich_fitted.ctl<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('Control'),]
# View(plot.rich_fitted2)


plot.rich_fitted.npk<-plot.rich_fitted.npk[complete.cases(plot.rich_fitted.npk$starting.richness), ]
plot.rich_coef3<-plot.rich_coef3[complete.cases(plot.rich_coef3$starting.richness), ]

plot.rich_fitted.npk$starting.richness <- factor(plot.rich_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.rich_coef3$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


#BIOMASS
plot.bm_coef <- coef(plot.bm.s)
plot.bm_coef 
colnames(plot.bm_coef)
plot.bm_coef<-as.data.frame(plot.bm_coef$site_code)

plot.bm_coef2 <-  bind_cols(plot.bm_coef %>% 
                              as_tibble() %>% 
                              mutate(
                                # Estimate.trtNPK.year_trt = "Estimate.trtNPK:year_trt",
                                #trtNPK.year_trt_lower = "Q2.5.trtNPK:year_trt",
                                #trtNPK.year_trt_upper = "Q97.5.trtNPK:year_trt",
                                #    Slope = Estimate.year_trt ,
                                #   Slope_lower =Q2.5.year_trt ,
                                #  Slope_upper = Q97.5.year_trt,
                                site_code = rownames(plot.bm_coef)) %>% 
                              as_tibble() %>%
                              # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code') %>%
  inner_join(startrich %>%
               group_by(site_code) %>%
               summarise(m.rich = mean(rich),
                         r.rich = round(m.rich)),
             by = 'site_code'))

colnames(plot.bm_coef2)[colnames(plot.bm_coef2)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"
colnames(plot.bm_coef2)[colnames(plot.bm_coef2)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.bm_coef2)[colnames(plot.bm_coef2)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"



plot.bm_coef2$starting.richness <- ifelse(plot.bm_coef2$r.rich >= 1 & plot.bm_coef2$r.rich <= 5, '1-5 species',
                                          ifelse(plot.bm_coef2$r.rich >=6 & plot.bm_coef2$r.rich <=10, '6-10',
                                                 ifelse(plot.bm_coef2$r.rich >=11 & plot.bm_coef2$r.rich <=15, '11-15',    
                                                        ifelse(plot.bm_coef2$r.rich >=16 & plot.bm_coef2$r.rich <=20, '16-20',
                                                               ifelse(plot.bm_coef2$r.rich >=21 & plot.bm_coef2$r.rich <=25, '21-25',
                                                                      ifelse(plot.bm_coef2$r.rich >=26, '>26', 'other'))))))
View(plot.bm_coef2)



#dat<-distinct(plot, site_code, continent,habitat)

plot.bm_coef3<-full_join(plot.bm_coef2,dat)


load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')


View(plot.rich_coef3)
View(plot.bm_coef3)
colnames(plot.rich_coef3)
colnames(plot.bm_coef3)
plot.rich_coef4<-plot.rich_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18)]
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18)]
colnames(plot.rich_coef4)
View(plot.bm_coef4)

names(plot.rich_coef4) <- c("site_code","IR.Slope","IR.Slope_lower","IR.Slope_upper","R.Slope","R.Slope_lower","R.Slope_upper","starting.richness","continent","habitat")
names(plot.bm_coef4) <- c("site_code","IB.Slope","IB.Slope_lower","IB.Slope_upper","B.Slope","B.Slope_lower","B.Slope_upper","starting.richness","continent","habitat")
#plot.bm_coef5<-plot.bm_coef4[complete.cases(plot.bm_coef4$B.Slope),]
delta.coefs<-left_join(plot.rich_coef4,plot.bm_coef4)
View(delta.coefs)

# DELTA QUDRANT PLOT Model
# GROUP VARIATION
levels(delta.coefs$starting.richness)
delta.coefs$starting.richness <- factor(delta.coefs$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

mod.fig<-ggplot(data=delta.coefs, aes(x= IR.Slope+R.Slope, y= IB.Slope+B.Slope,color=starting.richness)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  #facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = IB.Slope_lower+B.Slope_lower, ymax = IB.Slope_upper+B.Slope_upper,colour = starting.richness), width = 0, size = 0.45,alpha=0.3) +
  geom_errorbarh(aes(xmin = IR.Slope_lower+R.Slope_lower, xmax = IR.Slope_upper+R.Slope_upper,colour = starting.richness), width = 0, size = 0.45,alpha=0.3) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year')),
       title= '', color= "Starting Richness") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

mod.fig

#RAW DELTA PLOT

plot13$starting.richness <- factor(plot13$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
colnames(plot13)
plot13$delta.rich=plot13$rich.end-plot13$rich.start
plot13$delta.mass=plot13$mass.end-plot13$mass.start
View(plot13)

# add in standard dev's

raw.fig<-ggplot(data=plot13, aes(x=delta.rich, y=delta.mass,color=starting.richness)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  #facet_grid(continent~., scales= 'free', space='free')+
  #geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.5) +
  #geom_errorbarh(aes(xmin = R.Slope_lower, xmax = R.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  labs(x = 'Richness Change',
       y = 'Biomass Change',
       title= 'Raw Data') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

raw.fig
grid_arrange_shared_legend(mod.fig,raw.fig,nrow=1)

