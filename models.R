
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)

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




#native<-sp[sp$local_provenance %in% c('NAT'),]
#introduced<-sp[sp$local_provenance %in% c('INT'),]
head(sp)
View(plot)
View(p)
#site
#plot2 <- plot[!(is.na(plot$log.live.mass)),]
#View(p2)


head(plot2)

#plot all
plot.rich.m <- brm(rich ~  year_trt + (year_trt | site_code), 
                   data = plot, family = poisson() ,  cores = 4, chains = 4)

plot.rich.im <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
                   data = plot,   cores = 4, chains = 4)


plot.bm.m <- brm(live_mass ~ year_trt + (year_trt | site_code), 
                 data = plot, family = lognormal() , cores = 4, chains = 4)

plot.bm.im <- brm(log.live.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                   data = plot , cores = 4, chains = 4)



setwd('~/Dropbox/Projects/NutNet/Model_fits/')
#save(plot.rich.m,plot.bm.m,file = 'plot.nutnet.models.Rdata')
#load('~/Dropbox/Projects/NutNet/Model_fits/plot.nutnet.models.Rdata')

#save(plot.rich.im,plot.bm.im,file = 'plot.nutnet.i.models.Rdata')
#em


#shane
load('~/Dropbox/NutNet/Model_fits/plot.nutnet.i.models.Rdata')

summary(plot.rich.im)


# inspection of chain diagnostic
plot(plot.rich.im)


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
color_scheme_set("purple")
pp_check(plot.rich.im)



#residuals
m1<-residuals(plot.rich.im)
m1<-as.data.frame(m1)
nrow(m1)
nrow(plot)
rr.plot<-cbind(plot,m1$Estimate)
View(rr.plot)

head(rr.plot)
par(mfrow=c(3,2))
with(rr.plot, plot(continent, m1$Estimate))
with(rr.plot, plot(habitat, m1$Estimate))
with(rr.plot, plot(site_code, m1$Estimate))
with(rr.plot, plot(block, m1$Estimate))
with(rr.plot, plot(plot, m1$Estimate))
with(rr.plot, plot(f.year_trt, m1$Estimate))


# #------plot richness model all sp----------------
# fixed effects

plot.rich_fitted <- cbind(plot.rich.im$data,
                        # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                        fitted(plot.rich.im, re_formula = NA)) %>% 
  as_tibble() 
#%>% 
  # get the seed.rich values for plotting
 # inner_join(plot2 %>%
            #   distinct(site_code, rich ,log.rich, continent, habitat),
             #by = c('site_code', 'log.rich'))

View(plot.rich_fitted)
# fixed effect coefficients (I want these for the coefficient plot)
plot.rich_fixef <- fixef(plot.rich.im)

# coefficients for experiment-level (random) effects
plot.rich_coeff <- coef(plot.rich.im)

plot.rich_coef<-as.data.frame(plot.rich_coeff$site_code)
#names(plot.rich_coef) <- gsub(":", ".", names(plot.rich_coef), fixed = TRUE)


startrich<-plot[plot$year_trt %in% c('0'),]
View(startrich)

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


library(yarrr)
piratepal(palette = "all", plot.result = TRUE)
piratepal(palette = "info2")
piratepal(palette = "cars")
# pal "#E5BA3AFF", "#75B41EFF","#5AC2F1FF","#0C5BB0FF","#972C8DFF","#E0363AFF"

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))

r1<-ggplot() +
  # data
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = rich,
                 colour = starting.richness, alpha=0.1),
             size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=plot.rich_fitted.npk,
  #          aes(x = year_trt, y = rich,
  #            colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = plot.rich_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Estimate.Intercept + Estimate.trtNPK + (Estimate.year_trt + Estimate.trtNPK.year_trt) * xmin),
                   yend = (Estimate.Intercept + Estimate.trtNPK + (Estimate.year_trt + Estimate.trtNPK.year_trt) * xmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = plot.rich_fitted.npk,
            aes(x = year_trt, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = plot.rich_fitted.ctl,
            aes(x = year_trt, y = Estimate),
            size = 1.5,linetype= "dashed") +
  #scale_y_continuous(trans = 'log10') +
  # # sanity check
  # geom_abline(data = plot.rich_fixef2,
  #             aes(intercept = plot.rich_fixef2['Intercept','Estimate'],
  #                 slope = plot.rich_fixef2['year_trt', 'Estimate']),
  #             colour = 'pink') +
  labs(x = 'Years',
       y = 'Species richness', title= 'a) Plot Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
 theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
                               #+ theme(legend.position="bottom")
r1



#plot biomass


summary(plot.bm.im)


# inspection of chain diagnostic
plot(plot.bm.im)  


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(plot.bm.im)


#residuals
bm1<-residuals(plot.bm.im)
bm1<-as.data.frame(bm1)
nrow(bm1)
nrow(plot)
plot2 <- plot[!(is.na(plot$live_mass)),]
rb.plot<-cbind(plot2,bm1$Estimate)
View(rb.plot)

head(rb.plot)
par(mfrow=c(3,2))
with(rb.plot, plot(continent, bm1$Estimate))
with(rb.plot, plot(habitat, bm1$Estimate))
with(rb.plot, plot(site_code, bm1$Estimate))
with(rb.plot, plot(block, bm1$Estimate))
with(rb.plot, plot(plot, bm1$Estimate))
with(rb.plot, plot(f.year_trt, bm1$Estimate))

# #------plot richness model all sp----------------
# fixed effects
plot.bm_fitted <- cbind(plot.bm.im$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(plot.bm.im, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.bm_fixef <- fixef(plot.bm.im)

# coefficients for experiment-level (random) effects
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

View(plot.bm_coef3)
View(plot.bm_fitted)
dat2<-distinct(plot,habitat, continent,site_code, year_trt,block, plot,log.live.mass,live_mass,rich)
plot.bm_fitted2<-full_join(plot.bm_fitted,dat2)
View(plot.bm_fitted2)
View(plot)


plot.bm_fitted2$starting.richness <- ifelse(plot.bm_fitted2$rich >= 1 & plot.bm_fitted2$rich <= 5, '1-5 species',
                                    ifelse(plot.bm_fitted2$rich >=6 & plot.bm_fitted2$rich <=10, '6-10',
                                           ifelse(plot.bm_fitted2$rich >=11 & plot.bm_fitted2$rich <=15, '11-15',    
                                                  ifelse(plot.bm_fitted2$rich >=16 & plot.bm_fitted2$rich <=20, '16-20',
                                                         ifelse(plot.bm_fitted2$rich >=21 & plot.bm_fitted2$rich <=25, '21-25',
                                                                ifelse(plot.bm_fitted2$rich >=26, '>26', 'other'))))))

plot.bm_fitted.npk<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('NPK'),]
plot.bm_fitted.ctl<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('Control'),]




plot.bm_fitted.npk<-plot.bm_fitted.npk[complete.cases(plot.bm_fitted.npk$starting.richness), ]
plot.bm_coef3<-plot.bm_coef3[complete.cases(plot.bm_coef3$starting.richness), ]


plot.bm_fitted.npk$starting.richness <- factor(plot.bm_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.bm_coef3$starting.richness <- factor(plot.bm_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

#are low and lowest even there?
# plot.bm_fitted.npkt<-plot.bm_fitted.npk[plot.bm_fitted.npk$starting.richness %in% c('lowest', 'low'),]
# plot.bm_coef3t<-plot.bm_coef3[plot.bm_coef3$starting.richness %in% c('lowest', 'low'),]
# plot.bm_fitted.npkt<-plot.bm_fitted.npk[plot.bm_fitted.npk$starting.richness %in% c('highest', 'high'),]
# plot.bm_coef3t<-plot.bm_coef3[plot.bm_coef3$starting.richness %in% c('highest', 'high'),]
# plot.bm_fitted.npkt<-plot.bm_fitted.npk[plot.bm_fitted.npk$starting.richness %in% c('medium-high', 'medium'),]
# plot.bm_coef3t<-plot.bm_coef3[plot.bm_coef3$starting.richness %in% c('medium-high', 'medium'),]
# #yep!
#finland there? saana montane
# plot.bm_coef3t<-plot.bm_coef3[plot.bm_coef3$site_code %in% c('saana.fi'),]
# plot.bm_fitted.npkt<-plot.bm_fitted.npk[plot.bm_fitted.npk$site_code %in% c('saana.fi'),]

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
b1<-ggplot() +
  # data
  geom_point(data = plot.bm_fitted.npk,
             aes(x = year_trt, y = live_mass,
                 colour = starting.richness, alpha=0.1),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=plot.bm_fitted.npk,
  #             aes(x = year_trt, y = live_mass,
  #               colour = starting.richness), height=0.45,width = 0.45)+
 
   #experiment (random) effects
  geom_segment(data = plot.bm_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code,
                   colour = factor(starting.richness)),
               size = 0.7) +
 # uncertainy in fixed effect
  geom_ribbon(data = plot.bm_fitted.npk,
              aes(x = year_trt, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = plot.bm_fitted.npk,
            aes(x = year_trt, y = exp(Estimate)),
            size = 1.5) +
    geom_ribbon(data = plot.bm_fitted.ctl,
                aes(x = year_trt, ymin = exp(Q2.5), ymax = exp(Q97.5)),
                alpha = 0.5) +
    # fixed effect
    geom_line(data = plot.bm_fitted.ctl,
              aes(x = year_trt, y = exp(Estimate)),
              size = 1.5,linetype= "dashed") +
  scale_y_continuous(trans = 'log10', #breaks = c(8, 64, 512, 1024, 2048, 4096)
                     ) +
  labs(x = 'Years',
       y = expression(paste('Biomass (g/',m^2, ')')), title= 'b) Plot Biomass') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")
  
b1


grid_arrange_shared_legend(r1,b1,nrow=1)
#scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+





#coefficients

View(plot.bm_coef2)
View(plot.rich_coef2)
colnames(plot.rich_coef2)
colnames(plot.bm_coef2)
#biomass_exp_coef3<-biomass_exp_coef2[,c(-10,-11)]
plot.rich_coef2$Model<-'_Richness'
plot.bm_coef2$Model<-'Biomass'
coef.all<-bind_rows(plot.rich_coef3,plot.bm_coef3)
coef.all<-inner_join(coef.all,dat2)
View(coef.all)

#fixed
fixef_r<-as.data.frame(plot.rich_fixef)
fixef_b<-as.data.frame(plot.bm_fixef)
fixef_r$Model<-'_Richness'
fixef_b$Model<-'Biomass'
fixedf_df<-bind_rows(fixef_r,fixef_b)
View(fixedf_df)

coef.all$site_code<-as.factor(coef.all$site_code)

View(coef.all)
write.csv(coef.all,"~/Dropbox/Projects/SeedAdd/Data/mm_coef.all.csv")



#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot() + 
  geom_point(data = coef.all, aes(x = reorder(site_code, Slope), y = Slope,colour = site_code),size = 4) +
  geom_errorbar(data = coef.all, aes(x = site_code,ymin = Slope_lower,
                                     ymax = Slope_upper,colour = site_code),
                width = 0, size = 1.5) + facet_grid(~Model,scales="free")+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixedf_df, Model=='_Richness'),
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixedf_df, Model=='_Richness'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  geom_hline(data = filter(fixedf_df, Model=='Biomass'),
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixedf_df, Model=='Biomass'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  labs(x = 'site_code',
       y = 'Slope') +
  #scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", 
                                # "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+
  #scale_x_discrete(limits = rev(levels(coef.all$site_code)))+
  coord_flip() + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")




plot.rich_coef3<-full_join(plot.rich_coef2,dat)
plot.rich_fixef2<-as.data.frame(plot.rich_fixef)
#plot.rich_coef3$Slope<-as.numeric(plot.rich_coef3$Slope)
is.numeric(plot.rich_coef3$Slope)

View(plot.rich_fixef2)
View(plot.rich_coef3)
colnames(plot.rich_coef3)

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
r2<-ggplot() + 
  geom_point(data = plot.rich_coef3, aes(x = reorder(site_code,Estimate.trtNPK.year_trt), y = Estimate.trtNPK.year_trt,colour = continent),size = 2) +
  geom_errorbar(data = plot.rich_coef3, aes(x = reorder(site_code,Estimate.trtNPK.year_trt),ymin = Q2.5.trtNPK.year_trt,
                                      ymax = Q97.5.trtNPK.year_trt,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = plot.rich_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = plot.rich_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  ylim(-0.3, 0.3) +
  labs(x = 'Site',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'a) Plot Richness') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

r2

View(plot)

plot.bm_coef3<-full_join(plot.bm_coef2,dat)
View(plot.bm_coef3)
plot.rich_coef3$slope.rich<-plot.rich_coef3$Estimate.trtNPK.year_trt
View(plot.rich_coef3)
rich.slope<-select(plot.rich_coef3,site_code,slope.rich)
plot.bm_coef4<-inner_join(plot.bm_coef3,rich.slope)
is.numeric(plot.bm_coef3$slope.rich)
plot.bm_coef3$site_code <- reorder(plot.bm_coef3$site_code, plot.bm_coef3$slope.rich)

View(fixef_b)
View(plot.bm_coef4)
summary(plot.bm_coef4)
colnames(plot.bm_coef4)

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
b2<-ggplot() + 
  geom_point(data = plot.bm_coef4, aes(x = reorder(site_code, slope.rich), y = TESlope, colour = continent),size = 2) +
  geom_errorbar(data = plot.bm_coef4, aes(x = reorder(site_code, slope.rich),ymin = TESlope_lower,
                                            ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixef_b,),
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = filter(fixef_b, ),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  ylim(-0.3, 0.3) +
  labs(x = 'Site',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'b) Plot Biomass') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

b2

grid_arrange_shared_legend(r2,b2,nrow=1)

grid_arrange_shared_legend(r1,b1,r2,b2,nrow=2,ncol=2)


#delta

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

#By Experiment
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(data=delta.coefs, aes(x=R.Slope, y=B.Slope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = R.Slope_lower, xmax = R.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'Richness Slope',
       y = 'Biomass Slope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


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






#biomass by CDE delta plot

View(cde_coef3)
View(plot.bm_coef3)
colnames(cde_coef3)
colnames(plot.bm_coef3)
cde_coef4<-cde_coef3[,c(-1,-2,-3,-8,-9,-10,-11)]
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9)]
colnames(cde_coef4)
colnames(plot.bm_coef4)
names(cde_coef4) <- c("site_code","C.Slope","C.Slope_lower","C.Slope_upper","continent","habitat")
names(plot.bm_coef4) <- c("site_code","B.Slope","B.Slope_lower","B.Slope_upper","continent","habitat")
delta.coefs<-bind_cols(cde_coef4,plot.bm_coef4)
View(delta.coefs)

#By Experiment
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(data=delta.coefs, aes(x=C.Slope, y=B.Slope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = C.Slope_lower, xmax = C.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'Biomass Change Slope',
       y = 'Overall Biomass Slope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")





