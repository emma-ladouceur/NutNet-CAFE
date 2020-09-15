


library(tidyverse)
library(brms)
library(sjstats)
library(bayesplot)


# Emmas links
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
start.rich <-read.csv("~/Dropbox/Projects/NutNet/Data/start.rich.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# PLOT 
plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

 plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) 
  ungroup()


# PARTITION DATA
p.all<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all$f.year.y<-as.factor(p.all$year.y)
p.all$plot<-as.factor(p.all$plot)
p.all$site_code<-as.factor(p.all$site_code)

p.all <- p.all %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()

dat<- plot %>% distinct(site_code, continent,habitat)
p.dat2<-inner_join(p.all,dat)


# START RICH
startrich<-plot[plot$year_trt %in% c('0'),]

startrich2<-startrich %>%
  group_by(site_code) %>%
  summarise(m.rich = mean(rich),
            r.rich = round(m.rich))


startrich2$starting.richness <- ifelse(startrich2$r.rich >= 1 & startrich2$r.rich <= 5, '1-5 species',
                                       ifelse(startrich2$r.rich >=6 & startrich2$r.rich <=10, '6-10',
                                              ifelse(startrich2$r.rich >=11 & startrich2$r.rich <=15, '11-15',
                                                     ifelse(startrich2$r.rich >=16 & startrich2$r.rich <=20, '16-20',
                                                            ifelse(startrich2$r.rich >=21 & startrich2$r.rich <=25, '21-25',
                                                                   ifelse(startrich2$r.rich >=26, '>26', 'other'))))))

write.csv(startrich2, "~/Dropbox/Projects/NutNet/Data/start.rich.csv")


# load models


load('~/Dropbox/Projects/NutNet/Model_fits/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/rich.Rdata') # plot.rich.g

load('~/Dropbox/Projects/NutNet/Model_fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/cde.Rdata') # CDE.s


load('~/Dropbox/Projects/NutNet/Model_fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sgain.Rdata') # s.gain.s



summary(plot.rich.3)
# inspection of chain diagnostic
#plot(plot.rich.g)
color_scheme_set("gray")
pp_check(plot.rich.3) + theme_classic() + labs(x='Species Richness',
                                               y = 'Frequency') 


plot(plot.rich.3)

# residuals
m1<-residuals(plot.rich.g)
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


# fixed effects

plot.rich_fitted <- cbind(plot.rich.3$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(plot.rich.3, re_formula = NA)) %>% 
  as_tibble() 

View(plot.rich_fitted)
# fixed effect coefficients 
plot.rich_fixef <- fixef(plot.rich.3)

# coefficients for experiment-level (random) effects
plot.rich_coef <- coef(plot.rich.3)

View(plot.rich_coef)

plot.rich_coef2 <-  bind_cols(plot.rich_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(plot.rich_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'year_trt'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK:year_trt'] %>% 
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
             by = 'site_code') 


View(plot.rich_coef2)
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"


dat<-distinct(plot, site_code, continent,habitat)
plot.rich_coef3<-full_join(plot.rich_coef2,dat)

plot.rich_fitted2<-left_join(plot.rich_fitted,start.rich)

plot.rich_fitted.npk<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('NPK'),]
plot.rich_fitted.ctl<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('Control'),]


setwd('~/Dropbox/Projects/NutNet/Data/')
save(plot.rich_fixef,plot.rich_fitted.npk,plot.rich_fitted.ctl,plot.rich_coef3,file = 'rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')


# BIOMASS

summary(plot.bm.3)

#plot(plot.bm.s)  
pp_check(plot.bm.3) + theme_classic()+ scale_x_continuous(limits = c(-1000, 2000))+ labs(x=expression(paste('Biomass (g/',m^2, ')')),
                                                                                         y = 'Frequency') 


#residuals
bm1<-residuals(plot.bm.s)
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
plot.bm_fitted <- cbind(plot.bm.3$data,
                        # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                        fitted(plot.bm.3, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.bm_fixef <- fixef(plot.bm.3)

# coefficients for experiment-level (random) effects
plot.bm_coef <- coef(plot.bm.3)
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
             by = 'site_code') %>% left_join(start.rich, by="site_code")


dat<-distinct(plot, site_code, continent,habitat)
#
plot.bm_coef3<-left_join(plot.bm_coef2,dat)

dat2$block<-as.numeric(dat2$block)
plot.bm_fitted$block<-as.numeric(plot.bm_fitted$block)
dat2$plot<-as.numeric(dat2$plot)
plot.bm_fitted$plot<-as.numeric(plot.bm_fitted$plot)


plot.bm_fitted2<-left_join(plot.bm_fitted,start.rich, by= "site_code")

plot.bm_fitted.npk<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('NPK'),]
plot.bm_fitted.ctl<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('Control'),]



setwd('~/Dropbox/Projects/NutNet/Data/')
save(plot.bm_fixef,plot.bm_fitted.npk,plot.bm_fitted.ctl,plot.bm_coef3,file = 'bm.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')



# PARTITIONS



# SL : sl.s
summary(sl.3)
#plot(sl.s)  
pp_check(sl.3) + theme_classic() + scale_x_continuous(limits = c(-700, 50))+ labs(x=expression(paste('Change in Biomass due to Species Loss (g/',m^2, ')')), 
                                                                                     y = 'Frequency') 


# residuals
sl1<-residuals(sl.s)
sl1<-as.data.frame(sl1)
nrow(sl1)
nrow(p.dat2)
p.dat3 <- p.dat2[!(is.na(p.dat2$SL)),]
sl.plot<-cbind(p.dat3,sl1$Estimate)
View(sl.plot)

head(sl.plot)
par(mfrow=c(3,3))
with(sl.plot, p.dat3(continent, sl1$Estimate))
with(sl.plot, p.dat3(habitat, sl1$Estimate))
with(sl.plot, p.dat3(site_code, sl1$Estimate))
with(sl.plot, p.dat3(block, sl1$Estimate))
with(sl.plot, p.dat3(plot, sl1$Estimate))
with(sl.plot, p.dat3(f.year_trt, sl1$Estimate))

# fixed effects
sl.trt_fitted <- cbind(sl.3$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(sl.3, re_formula = NA)) %>% 
  as_tibble()


p.dat3<-p.dat2 %>% 
  group_by(site_code,continent,block,plot,trt.y,year.x,year.y,year.y.m) %>% 
  left_join(start.rich,by="site_code")

View(p.dat2)
View(p.dat3)
View(sl.trt_fitted)

sl.trt_fitted$plot<-as.factor(as.character(sl.trt_fitted$plot))
sl.trt_fitted$block<-as.factor(as.character(sl.trt_fitted$block))
sl.trt_fitted$trt.y<-as.factor(as.character(sl.trt_fitted$trt.y))
p.dat3$plot<-as.factor(as.character(p.dat3$plot))
p.dat3$block<-as.factor(as.character(p.dat3$block))
p.dat3$trt.y<-as.factor(as.character(p.dat3$trt.y))

sl.trt_fitted3<-left_join(sl.trt_fitted,p.dat3)

View(sl.trt_fitted3)


sl.trt_fitted.npk<-sl.trt_fitted3[sl.trt_fitted3$trt.y %in% c('NPK'),]
sl.trt_fitted.ctl<-sl.trt_fitted3[sl.trt_fitted3$trt.y %in% c('Control'),]
View(sl.trt_fitted.npk)

# fixed effect coefficients -coefficient plot
sl.trt_fixef <- fixef(sl.3)

# coefficients for experiment-level (random) effects
sl.trt_coef <- coef(sl.3)

sl.trt_coef2 <-  bind_cols(sl.trt_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(sl.trt_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sl.trt_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sl.trt_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sl.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(TESlope = Estimate,
                                    TESlope_lower = Q2.5,
                                    TESlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') %>%  left_join(start.rich, by="site_code")

View(p.dat3)
View(sl.trt_coef2)



summary(sl.s)

dat<-distinct(p.dat2, site_code, continent,habitat)

sl.trt_coef3<-inner_join(sl.trt_coef2,dat)
View(sl.trt_coef3)

setwd('~/Dropbox/Projects/NutNet/Data/')
save(sl.trt_fitted.npk,sl.trt_fitted.ctl,sl.trt_coef3,file = 'sl.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')


# SG : sg.s

summary(sg.3)
#plot(sg.s)  
pp_check(sg.3) + theme_classic() + scale_x_continuous(limits = c(-50, 600))+ labs(x=expression(paste('Change in Biomass due to Species Gain (g/',m^2, ')')), 
                                                                y = 'Frequency') 

# residuals
sg1<-residuals(sg.s)
sg1<-as.data.frame(sg1)
nrow(sg1)
nrow(p.dat2)
p.dat3 <- p.dat2[!(is.na(p.dat2$sg)),]
sg.plot<-cbind(p.dat3,sg1$Estimate)
View(sg.plot)

head(sg.plot)
par(mfrow=c(3,3))
with(sg.plot, p.dat3(continent, sg1$Estimate))
with(sg.plot, p.dat3(habitat, sg1$Estimate))
with(sg.plot, p.dat3(site_code, sg1$Estimate))
with(sg.plot, p.dat3(block, sg1$Estimate))
with(sg.plot, p.dat3(plot, sg1$Estimate))
with(sg.plot, p.dat3(f.year_trt, sg1$Estimate))

# fixed effects
sg.trt_fitted <- cbind(sg.3$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(sg.3, re_formula = NA)) %>% 
  as_tibble() 

as.data.frame(sg.trt_fitted)
p.dat3<-p.dat2 %>% 
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  left_join(start.rich, by="site_code")

View(p.dat3)
View(sg.trt_fitted)
sg.trt_fitted3<-left_join(sg.trt_fitted,p.dat3)


sg.trt_fitted.npk<-sg.trt_fitted3[sg.trt_fitted3$trt.y %in% c('NPK'),]
sg.trt_fitted.ctl<-sg.trt_fitted3[sg.trt_fitted3$trt.y %in% c('Control'),]
View(sg.trt_fitted.npk)

View(p.dat3)
View(sg.trt_fitted3)


# fixed effect coefficients (I want these for the coefficient plot)
sg.trt_fixef <- fixef(sg.3)

# coefficients for experiment-level (random) effects
sg.trt_coef <- coef(sg.3)
sg.trt_coef 
sg.trt_coef2 <-  bind_cols(sg.trt_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(sg.trt_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sg.trt_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sg.trt_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           sg.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(TESlope = Estimate,
                                    TESlope_lower = Q2.5,
                                    TESlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') %>% left_join(start.rich, by="site_code")


View(sg.trt_coef3)

View(sg.trt_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

sg.trt_coef3<-full_join(sg.trt_coef2,dat)

summary(sg.s)

rm(sg.trt.i)
setwd('~/Dropbox/Projects/NutNet/Data/')
save(sg.trt_fitted.npk,sg.trt_fitted.ctl,sg.trt_coef3,file = 'sg_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/sg_dat.Rdata')



# CDE
summary(CDE.3)
#plot(CDE.s)  
pp_check(CDE.3) + theme_classic() + scale_x_continuous(limits = c(-1000, 1000))+ labs(x=expression(paste('Change in Biomass in Persistent Species (g/',m^2, ')')), 
                                                                                   y = 'Frequency') 


#residuals
cde1<-residuals(CDE.s)
cde1<-as.data.frame(cde1)
nrow(cde1)
nrow(p.dat2)
p.dat3 <- p.dat2[!(is.na(p.dat2$cde)),]
cde.plot<-cbind(p.dat3,cde1$Estimate)
View(cde.plot)

head(cde.plot)
par(mfrow=c(3,3))
with(cde.plot, p.dat3(continent, cde1$Estimate))
with(cde.plot, p.dat3(habitat, cde1$Estimate))
with(cde.plot, p.dat3(site_code, cde1$Estimate))
with(cde.plot, p.dat3(block, cde1$Estimate))
with(cde.plot, p.dat3(plot, cde1$Estimate))
with(cde.plot, p.dat3(f.year_trt, cde1$Estimate))

# fixed effects
cde_fitted <- cbind(CDE.3$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(CDE.3, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(cde_fitted)

View(p.dat2)
p.dat3<-p.dat2 %>%  
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  left_join(start.rich, by="site_code")

View(p.dat3)
p.dat3$block<-as.factor(p.dat3$block)
#cde_fitted2<-full_join(cde_fitted,dat)
nrow(cde_fitted)
cde_fitted3<-inner_join(cde_fitted,p.dat3)
View(cde_fitted3)
nrow(cde_fitted3)



cde_fitted.npk<-cde_fitted3[cde_fitted3$trt.y %in% c('NPK'),]
cde_fitted.ctl<-cde_fitted3[cde_fitted3$trt.y %in% c('Control'),]
View(cde_fitted.npk)
View(cde_fitted.ctl)

cde_fixef <- fixef(CDE.3)

cde_coef <- coef(CDE.3)
cde_coef
cde_coef2 <-  bind_cols(cde_coef$site_code[,,'Intercept'] %>% 
                          as_tibble() %>% 
                          mutate(Intercept = Estimate,
                                 Intercept_lower = Q2.5,
                                 Intercept_upper = Q97.5,
                                 site_code = rownames(cde_coef$site_code[,,'Intercept'])) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'year.y.m'] %>% 
                          as_tibble() %>% 
                          mutate(ISlope = Estimate,
                                 ISlope_lower = Q2.5,
                                 ISlope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'trt.yNPK'] %>% 
                          as_tibble() %>% 
                          mutate(TE = Estimate,
                                 TE_lower = Q2.5,
                                 TE_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                          as_tibble() %>% 
                          mutate(TESlope = Estimate,
                                 TESlope_lower = Q2.5,
                                 TESlope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat3 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') %>% left_join(start.rich)


View(cde_coef3)


cde_coef3<-full_join(cde_coef2,dat)


rm(cde.trt.i)
setwd('~/Dropbox/Projects/NutNet/Data/')
save(cde_fitted.npk,cde_fitted.ctl,cde_coef3,file = 'cde.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')


# SGAIN : s.gain.s

summary(s.gain.3)
#plot(s.gain.s)  
pp_check(s.gain.3)+ theme_classic() + scale_x_continuous(limits = c(-25, 25))+ labs(x='Species Gain', 
                                                                                     y = 'Frequency') 


#residuals
s.gain1<-residuals(s.gain.s)
s.gain1<-as.data.frame(s.gain1)
nrow(s.gain1)
nrow(p.dat2)
p.dat3 <- p.dat2[!(is.na(p.dat2$s.gain)),]
s.gain.plot<-cbind(p.dat3,s.gain1$Estimate)
View(s.gain.plot)

head(s.gain.plot)
par(mfrow=c(3,3))
with(s.gain.plot, p.dat3(continent, s.gain1$Estimate))
with(s.gain.plot, p.dat3(habitat, s.gain1$Estimate))
with(s.gain.plot, p.dat3(site_code, s.gain1$Estimate))
with(s.gain.plot, p.dat3(block, s.gain1$Estimate))
with(s.gain.plot, p.dat3(plot, s.gain1$Estimate))
with(s.gain.plot, p.dat3(f.year_trt, s.gain1$Estimate))

 


sgain.trt_fitted <- cbind(s.gain.3$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(s.gain.3, re_formula = NA)) %>% 
  as_tibble() 

as.data.frame(sgain.trt_fitted)
p.dat3<-p.dat2 %>% 
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  left_join(start.rich, by="site_code")

View(p.dat3)
View(sgain.trt_fitted)
sgain.trt_fitted3<-left_join(sgain.trt_fitted,p.dat3)

sgain.trt_fitted.npk<-sgain.trt_fitted3[sgain.trt_fitted3$trt.y %in% c('NPK'),]
sgain.trt_fitted.ctl<-sgain.trt_fitted3[sgain.trt_fitted3$trt.y %in% c('Control'),]
View(sgain.trt_fitted.npk)

View(p.dat3)
View(sgain.trt_fitted3)


# fixed effect coefficients (I want these for the coefficient plot)
sgain.trt_fixef <- fixef(s.gain.3)

# coefficients for experiment-level (random) effects
sgain.trt_coef <- coef(s.gain.3)
sgain.trt_coef 
sgain.trt_coef2 <-  bind_cols(sgain.trt_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(sgain.trt_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sgain.trt_coef$site_code[,,'year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sgain.trt_coef$site_code[,,'trt.yNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sgain.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(TESlope = Estimate,
                                       TESlope_lower = Q2.5,
                                       TESlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m),
                         by = 'site_code')) %>% left_join(start.rich,by="site_code")

View(sgain.trt_coef2)

View(sgain.trt_coef3)

View(sgain.trt_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

sgain.trt_coef3<-full_join(sgain.trt_coef2,dat)

setwd('~/Dropbox/Projects/NutNet/Data/')
save(sgain.trt_fitted.npk,sgain.trt_fitted.ctl,sgain.trt_coef3,file = 'sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')


# SLOSS

# NEGATIVE OR POSITIVE MODEL? ....FIND AND REPLACE AS APPROPRIATE

summary(s.loss.3)
#plot(s.loss.s)  
pp_check(s.loss.3)+ theme_classic() + scale_x_continuous(limits = c(-25, 25))+ labs(x='Species Loss', 
                                                                                    y = 'Frequency') 


#residuals
s.loss1<-residuals(s.loss.n.s)
s.loss1<-as.data.frame(s.loss1)
nrow(s.loss1)
nrow(p.dat2)
p.dat3 <- p.dat2[!(is.na(p.dat2$s.loss)),]
s.loss.plot<-cbind(p.dat3,s.loss1$Estimate)
View(s.loss.plot)

head(s.loss.plot)
par(mfrow=c(3,3))
with(s.loss.plot, p.dat3(continent, s.loss1$Estimate))
with(s.loss.plot, p.dat3(habitat, s.loss1$Estimate))
with(s.loss.plot, p.dat3(site_code, s.loss1$Estimate))
with(s.loss.plot, p.dat3(block, s.loss1$Estimate))
with(s.loss.plot, p.dat3(plot, s.loss1$Estimate))
with(s.loss.plot, p.dat3(f.year_trt, s.loss1$Estimate))

sloss.trt_fitted <- cbind(s.loss.3$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(s.loss.3, re_formula = NA)) %>% 
  as_tibble()


p.dat3<-p.dat2 %>% 
  group_by(site_code,continent,block,plot,trt.y,year.x,year.y,year.y.m) %>% 
  left_join(start.rich,by="site_code")

View(p.dat2)
View(p.dat3)
View(sloss.trt_fitted)

sloss.trt_fitted$plot<-as.factor(as.character(sloss.trt_fitted$plot))
sloss.trt_fitted$block<-as.factor(as.character(sloss.trt_fitted$block))
sloss.trt_fitted$trt.y<-as.factor(as.character(sloss.trt_fitted$trt.y))
p.dat3$plot<-as.factor(as.character(p.dat3$plot))
p.dat3$block<-as.factor(as.character(p.dat3$block))
p.dat3$trt.y<-as.factor(as.character(p.dat3$trt.y))

sloss.trt_fitted3<-left_join(sloss.trt_fitted,p.dat3)

View(sloss.trt_fitted3)

sloss.trt_fitted.npk<-sloss.trt_fitted3[sloss.trt_fitted3$trt.y %in% c('NPK'),]
sloss.trt_fitted.ctl<-sloss.trt_fitted3[sloss.trt_fitted3$trt.y %in% c('Control'),]
View(sloss.trt_fitted.npk)

# fixed effect coefficients -coefficient plot
sloss.trt_fixef <- fixef(s.loss.3)

# coefficients for experiment-level (random) effects
sloss.trt_coef <- coef(s.loss.3)

sloss.trt_coef2 <-  bind_cols(sloss.trt_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(sloss.trt_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sloss.trt_coef$site_code[,,'year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sloss.trt_coef$site_code[,,'trt.yNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sloss.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
                                as_tibble() %>% 
                                mutate(TESlope = Estimate,
                                       TESlope_lower = Q2.5,
                                       TESlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code') %>% left_join(start.rich, by="site_code")

View(p.dat3)
View(sloss.trt_coef2)

dat<-distinct(p.dat2, site_code, continent,habitat)

sloss.trt_coef3<-inner_join(sloss.trt_coef2,dat)
View(sloss.trt_coef3)

setwd('~/Dropbox/Projects/NutNet/Data/')
save(sloss.trt_fitted.npk,sloss.trt_fitted.ctl,sloss.trt_coef3,file = 'sloss.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


# EFFS


# SEPERATELY



sl.trt.i_fixef <-  as.data.frame(fixef(sl.3))
sg.trt.i_fixef <- as.data.frame(fixef(sg.3))
CDE.trt.i_fixef <- as.data.frame( fixef(CDE.3))
plot.rich.im_fixef <- as.data.frame(fixef(plot.rich.3))
plot.bm.im_fixef <- as.data.frame(fixef(plot.bm.3))
sloss.trt.i_fixef <- as.data.frame(fixef(s.loss.3))
sgain.trt.i_fixef <-as.data.frame( fixef(s.gain.3))

plot.rich.im_fixef

rich.f <-bind_rows(
  plot.rich.im_fixef['year_trt',] %>%
    mutate(response='Control Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


rich.f$Model <- "Species Richness"
rich.f

bm.f <-bind_rows(
  plot.bm.im_fixef['year_trt',] %>%
  mutate(response='Control Slope',
         eff = Estimate,
         eff_upper = Q97.5,
         eff_lower = Q2.5) %>%
  select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

bm.f$Model <- "Biomass"

sl.trt.i_fixef 

sl.f <-bind_rows(
  sl.trt.i_fixef['year.y.m',] %>%
    mutate(response='Control Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


sl.f$Model <- "Species Loss Effect on Biomass"

sg.f <-bind_rows(
  sg.trt.i_fixef['year.y.m',] %>%
    mutate(response='Control Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sg.f$Model <- "Species Gain Effect on Biomass"

CDE.trt.i_fixef

cde.f <-bind_rows(
  CDE.trt.i_fixef['year.y.m',] %>%
    mutate(response='Control Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  CDE.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

cde.f$Model <- "Persistent Species Change in Biomass"

cde.f

sloss.f <-bind_rows(
  sloss.trt.i_fixef['year.y.m',] %>%
    mutate(response='Control Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sloss.f$Model <- "Species Loss"

sgain.f <-bind_rows(
  sgain.trt.i_fixef['year.y.m',] %>%
    mutate(response='Control Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sgain.f$Model <- "Species Gain"

setwd('~/Dropbox/Projects/NutNet/Data/')
save(rich.f,bm.f,sloss.f,sgain.f,sl.f,sg.f,cde.f,file = 'effs.Rdata')







# posterior effects


load('~/Dropbox/Projects/NutNet/Model_fits/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Model_fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Model_fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sgain.Rdata') # s.gain.s



# site level meta data for posrteriors
# calculated to site level details found in Climate_Data.R
# latitude and longitude dont match due to decimal rounding
# lat.x long.x is nutnet site, lat.y long.y is world clim
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


meta <- meta %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()


colnames(meta)
View(meta)


rich_fixef <- fixef(plot.rich.3)
bm_fixef <- fixef(plot.bm.3)

sl.trt.i_fixef <- fixef(sl.3)
sg.trt.i_fixef <- fixef(sg.3)
CDE.trt.i_fixef <- fixef(CDE.3)
sloss.trt.i_fixef <- fixef(s.loss.3)
sgain.trt.i_fixef <- fixef(s.gain.3)

rich.fixed.p<-posterior_samples(plot.rich.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))
rich.fixed.p

bm.fixed.p<-posterior_samples(plot.bm.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 

sl.fixed.p<-posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p<-posterior_samples(sg.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p<-posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )

sloss.fixed.p<-posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p<-posterior_samples(s.gain.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 



head(rich.fixed.p)
rich_fixef



rich.p.ctl <-  rich.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope)) %>%
 mutate( response="Control", eff = mean(ctl.slope),
          eff_lower = quantile(ctl.slope, probs=0.025),
          eff_upper = quantile(ctl.slope, probs=0.975))  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   
  
rich.p.npk <-  rich.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope)) %>%
  mutate( response="NPK", eff = mean(trt.slope),
          eff_lower = quantile(trt.slope, probs=0.025),
          eff_upper = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

rich.p <- bind_rows(rich.p.ctl,rich.p.npk)
head(rich.p)

bm.p.ctl <-  bm.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope)) %>%
  mutate( response="Control", eff = mean(ctl.slope),
          eff_lower = quantile(ctl.slope, probs=0.025),
          eff_upper = quantile(ctl.slope, probs=0.975))  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

bm.p.npk <-  bm.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope)) %>%
  mutate( response="NPK", eff = mean(trt.slope),
          eff_lower = quantile(trt.slope, probs=0.025),
          eff_upper = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

bm.p <- bind_rows(bm.p.ctl,bm.p.npk)

bm.p

head(sl.fixed.p)
head(sl.trt.i_fixef)


sl.p.ctl <-  sl.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="Control", eff = mean(ctl.slope),
          eff_lower = quantile(ctl.slope, probs=0.025),
          eff_upper = quantile(ctl.slope, probs=0.975))  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sl.p.npk <-  sl.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="NPK", eff = mean(trt.slope),
          eff_lower = quantile(trt.slope, probs=0.025),
          eff_upper = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sl.p <- bind_rows(sl.p.ctl,sl.p.npk)
sl.p 

sg.p.ctl <-  sg.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="Control", eff = mean(ctl.slope),
          eff_lower = quantile(ctl.slope, probs=0.025),
          eff_upper = quantile(ctl.slope, probs=0.975))  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sg.p.npk <-  sg.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="NPK", eff = mean(trt.slope),
          eff_lower = quantile(trt.slope, probs=0.025),
          eff_upper = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sg.p <- bind_rows(sg.p.ctl,sg.p.npk)
sg.p


cde.p.ctl <-  cde.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="Control", eff = mean(ctl.slope),
          eff_lower = quantile(ctl.slope, probs=0.025),
          eff_upper = quantile(ctl.slope, probs=0.975))  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

cde.p.npk <- cde.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="NPK", eff = mean(trt.slope),
          eff_lower = quantile(trt.slope, probs=0.025),
          eff_upper = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

cde.p <- bind_rows(cde.p.ctl,cde.p.npk)
cde.p

sloss.p.ctl <-  sloss.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="Control", eff = mean(ctl.slope),
          eff_lower = quantile(ctl.slope, probs=0.025),
          eff_upper = quantile(ctl.slope, probs=0.975))  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sloss.p.npk <-  sloss.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="NPK", eff = mean(trt.slope),
          eff_lower = quantile(trt.slope, probs=0.025),
          eff_upper = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sloss.p <- bind_rows(sloss.p.ctl,sloss.p.npk)
sloss.p

sgain.p.ctl <-  sgain.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="Control", eff = mean(ctl.slope),
          eff_lower = quantile(ctl.slope, probs=0.025),
          eff_upper = quantile(ctl.slope, probs=0.975))  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sgain.p.npk <-  sgain.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope)) %>%
  mutate( response="NPK", eff = mean(trt.slope),
          eff_lower = quantile(trt.slope, probs=0.025),
          eff_upper = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(ctl.slope,trt.slope)) %>% distinct()   

sgain.p <- bind_rows(sgain.p.ctl,sgain.p.npk)
sgain.p

setwd('~/Dropbox/Projects/NutNet/Data/')
save(rich.p,bm.p,sloss.p,sgain.p,sl.p,sg.p,cde.p,file = 'p.effs.Rdata')







