
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
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
#shanes links
sp <- read.csv("~/Dropbox/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

dat2<-distinct(p, continent, site_code, year_trt)

dat2<-distinct(p, continent, site_code, year_trt)
View(dat2)
colnames(plot)
dat2<-distinct(plot, site_code,site_name, country, continent,habitat, latitude, longitude,elevation, experiment_type)
View(dat2)
#write.csv(dat2,"~/Dropbox/Projects/NutNet/Data/nutnet_distinct.csv")

#plot<-p[p$trt %in% c('NPK'),]
#or
plot<-p[p$trt %in% c('NPK', 'Control'),]
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
plot3<-plot2 %>% drop_na(live_mass)

plot4<-plot3 %>% group_by(site_code, block,plot,year_trt) %>%
  select(continent,unique.id,site_code,block, plot,year_trt,rich,live_mass)

plotzero<-plot4[plot4$year_trt %in% c('0'),]
View(plotzero)
plotmax<-plot4 %>% group_by(site_code) %>% top_n(1, year_trt)
View(plotmax)
plot5<-bind_rows(plotmax,plotzero)


ggplot(plot5, aes(x=rich, y=live_mass, group=unique.id))+
  geom_point(size=2,shape=1)+ geom_line()+theme_classic()

plot6<-plot5 %>%
  group_by(site_code,year_trt) %>%
  summarise(m.rich = mean(rich),
            r.rich = round(m.rich),
            sd.rich = sd(rich),
            m.mass = mean(live_mass),
            sd.mass = sd(live_mass))

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

ggplot(plot7, aes(x=m.rich, y=m.mass, color=starting.richness,group=site_code))+
  geom_point(size=2,shape=1)+ geom_line()
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
                                 ">26" = "#E0363AFF", drop =FALSE))+theme_classic()




# FIGURE 2
# QUADRANT PLOT DELTA BIOMASS DELTA RICH


load('~/Dropbox/Projects/NutNet/Model_fits/plot.nutnet.i.models.Rdata')

#RICHNESS

plot.rich_coeff <- coef(plot.rich.im)

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
                                inner_join(startrich %>%
                                             group_by(site_code) %>%
                                             summarise(m.rich = mean(rich),
                                                       r.rich = round(m.rich)),
                                           by = 'site_code'))



plot.rich_coef2$starting.richness <- ifelse(plot.rich_coef2$r.rich >= 1 & plot.rich_coef2$r.rich <= 5, '1-5 species',
                                            ifelse(plot.rich_coef2$r.rich >=6 & plot.rich_coef2$r.rich <=10, '6-10',
                                                   ifelse(plot.rich_coef2$r.rich >=11 & plot.rich_coef2$r.rich <=15, '11-15',    
                                                          ifelse(plot.rich_coef2$r.rich >=16 & plot.rich_coef2$r.rich <=20, '16-20',
                                                                 ifelse(plot.rich_coef2$r.rich >=21 & plot.rich_coef2$r.rich <=25, '21-25',
                                                                        ifelse(plot.rich_coef2$r.rich >=26, '>26', 'other'))))))
#View(plot.rich_coef2)                       

colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"




View(plot.rich_coef2)

dat<-distinct(plot, site_code, continent,habitat)
plot.rich_coef3<-full_join(plot.rich_coef2,dat)

View(plot.rich_fitted)
summary(plot.rich_fitted)
plot.rich_fitted$starting.richness <- ifelse(plot.rich_fitted$rich >= 1 & plot.rich_fitted$rich <= 5, '1-5 species',
                                             ifelse(plot.rich_fitted$rich >=6 & plot.rich_fitted$rich <=10, '6-10',
                                                    ifelse(plot.rich_fitted$rich >=11 & plot.rich_fitted$rich <=15, '11-15',    
                                                           ifelse(plot.rich_fitted$rich >=16 & plot.rich_fitted$rich <=20, '16-20',
                                                                  ifelse(plot.rich_fitted$rich >=21 & plot.rich_fitted$rich <=25, '21-25',
                                                                         ifelse(plot.rich_fitted$rich >=26, '>26', 'other'))))))
plot.rich_fitted2<-full_join(plot.rich_fitted,dat)
plot.rich_fitted.npk<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('NPK'),]
plot.rich_fitted.ctl<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('Control'),]
View(plot.rich_fitted2)

View(plot.rich_coef3)
View(plot.rich_fitted.npk)

plot.rich_fixef

levels(plot.rich_fitted.npk$rich.cat)
levels(plot.rich_coef3$rich.cat)

plot.rich_fitted.npk<-plot.rich_fitted.npk[complete.cases(plot.rich_fitted.npk$starting.richness), ]
plot.rich_coef3<-plot.rich_coef3[complete.cases(plot.rich_coef3$starting.richness), ]

plot.rich_fitted.npk$starting.richness <- factor(plot.rich_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.rich_coef3$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


#BIOMASS
plot.bm_coef <- coef(plot.bm.im)
plot.bm_coef 

plot.bm_coef2 <-  bind_cols(plot.bm_coef$site_code[,,'Intercept'] %>% 
                              as_tibble() %>% 
                              mutate(Intercept = Estimate,
                                     Intercept_lower = Q2.5,
                                     Intercept_upper = Q97.5,
                                     site_code = rownames(plot.bm_coef$site_code[,,'Intercept'])) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            plot.bm_coef$site_code[,,'year_trt'] %>% 
                              as_tibble() %>% 
                              mutate(ISlope = Estimate,
                                     ISlope_lower = Q2.5,
                                     ISlope_upper = Q97.5) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            plot.bm_coef$site_code[,,'trtNPK'] %>% 
                              as_tibble() %>% 
                              mutate(TE = Estimate,
                                     TE_lower = Q2.5,
                                     TE_upper = Q97.5) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            plot.bm_coef$site_code[,,'trtNPK:year_trt'] %>% 
                              as_tibble() %>% 
                              mutate(TESlope = Estimate,
                                     TESlope_lower = Q2.5,
                                     TESlope_upper = Q97.5) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
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
             by = 'site_code')


plot.bm_coef2$starting.richness <- ifelse(plot.bm_coef2$r.rich >= 1 & plot.bm_coef2$r.rich <= 5, '1-5 species',
                                          ifelse(plot.bm_coef2$r.rich >=6 & plot.bm_coef2$r.rich <=10, '6-10',
                                                 ifelse(plot.bm_coef2$r.rich >=11 & plot.bm_coef2$r.rich <=15, '11-15',    
                                                        ifelse(plot.bm_coef2$r.rich >=16 & plot.bm_coef2$r.rich <=20, '16-20',
                                                               ifelse(plot.bm_coef2$r.rich >=21 & plot.bm_coef2$r.rich <=25, '21-25',
                                                                      ifelse(plot.bm_coef2$r.rich >=26, '>26', 'other'))))))
View(plot.bm_coef2)



#dat<-distinct(plot, site_code, continent,habitat)

plot.bm_coef3<-full_join(plot.bm_coef2,dat)

View(plot.rich_coef3)
View(plot.bm_coef3)
colnames(plot.rich_coef3)
colnames(plot.bm_coef3)
plot.rich_coef4<-plot.rich_coef3[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-16,-17,-18,-20)]
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9,-10,-12)]
colnames(plot.rich_coef4)
colnames(plot.bm_coef4)
names(plot.rich_coef4) <- c("R.Slope","R.Slope_lower","R.Slope_upper","site_code","continent")
names(plot.bm_coef4) <- c("site_code","B.Slope","B.Slope_lower","B.Slope_upper","continent")
delta.coefs<-bind_cols(plot.rich_coef4,plot.bm_coef4)
View(delta.coefs)

# DELTA QUDRANT PLOT
# GROUP VARIATION


ggplot(data=delta.coefs, aes(x=R.Slope, y=B.Slope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  #facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = R.Slope_lower, xmax = R.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'Richness Slope',
       y = 'Biomass Slope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

# FIGURE 3
# DERIVATIVES DELTA OVER TIME
# CAFE BAYES VECTORS

# SP LOSS VECTOR, SP GAIN VECTOR, CDE VECTOR




# FIGURE 4
# POSTERIORS ACROSS GROUPS

# SITE DIVERSITY
# CO-LIMITED (STAND'S PAPER HOW DID HE DO IT) PLOTS WITH EFFECT VS. PLOTS WITH NO EFFECT
# EXOTIC VS. NATIVE DOMINATED
# ANTHROPOGENIC
# HERBIVORY
# BIOGEO / CLIMATE
# N. DEPOSITION

# FACETED AS LOSSES, GAINS, CDE? WITH POSTERIORS GROUPED AS ABOVE IN DIFF COLOURS
# CONTROLS GREY IN THE BACKGROUND






