




rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(bayesplot)
library(priceTools)

#emmas links
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/progressive_time_only.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
#p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


#shanes links
sp <- read.csv("~/Dropbox/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/NutNet/Data/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(p.all)
levels(p.all3$trt_year)

p.all<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all$f.year.y<-as.factor(p.all$year.y)
p.all$plot<-as.factor(p.all$plot)
p.all$site_code<-as.factor(p.all$site_code)

dat<-distinct(plot, site_code, continent,habitat)
p.dat2<-inner_join(p.all,dat)

View(p.dat2)
par(mfrow=c(2,3))
hist(p.dat2$s.loss.p,breaks =10, main="Species Loss", xlab= "Species Loss")
hist(p.dat2$s.gain, breaks=10, main="Species Gains", xlab= "Species Gains")
hist(p.dat2$s.change, breaks=10, main="Species Change", xlab= "Species Change")
hist(p.dat2$s.loss.p.log,breaks =10, main="Log Species Loss", xlab= "Log Species Loss")
hist(p.dat2$s.gain.log, breaks=10, main="Log Species Gains", xlab= "Log Species Gains")
hist(p.dat2$s.change.log, breaks=10, main="Log Species Change", xlab= "Log Species Change")



summary(p.dat2)
p.dat2$site_code<-as.factor(p.dat2$site_code)
p.dat2$site.year.id<-as.factor(p.dat2$site.year.id)

write.csv(p.dat2,"~/Desktop/price_time_only.csv")

#try out models on a sample of data?
#levels(p.dat5$site_code)
#samp<-p.dat5[p.dat5$site_code %in% c('arch.us','azi.cn'),]

colnames(p.dat5) 

#treat interaction
# sl.trt.i <- brm(SL.p ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
#                 data = p.dat5, family=hurdle_lognormal(),cores = 4, chains = 4)
# 
# #, control = list(adapt_delta = 0.999, max_treedepth = 15)
# 
# sg.trt.i <- brm(SG ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
#                 data = p.dat5, family=hurdle_lognormal(),cores = 4, chains = 4)
# 
# 
# CDE.trt.i <- brm(CDE ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
#                  data = p.dat5, family=asym_laplace(),cores = 4, chains = 4)


summary(CDE.trt.i)

setwd('~/Dropbox/Projects/NutNet/Model_fits/')


#setwd('~/Dropbox/NutNet/Model_fits/')
#save(sl.trt,sg.trt,CDE.trt,file = 'price_trt_time.Rdata')
#load('~/Dropbox/Projects/NutNet/Model_fits/price_trt_time.Rdata')


#load('~/Dropbox/NutNet/Model_fits/price_trt_time.Rdata')

#save(sl.trt.i,sg.trt.i,CDE.trt.i,file = 'price_trt_interact_time.Rdata')
#emma
#!!!!!!!!
#we now call these the old cumulative models 
#where we compare everything to year 0
load('~/Dropbox/Projects/NutNet/Model_fits/price_trt_interact_time.Rdata')

#!!!!!!!!
#then next we call these progressive
#where everything is compared to the year before it instead of year 0
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sl.Rdata')
#nn_time.sl-5241651.Rdata removed site.year.id
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sl-progressive.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sg-progressive.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.cde-progressive.Rdata')
#models have the same names(oops)



summary(p.CDE.trt.i)

summary(p.sg.trt.i)

summary(p.sl.trt.i)

color_scheme_set("purple")
plot(p.sl.trt.i)
pp_check(p.sl.trt.i)+ scale_x_continuous(trans="log") +theme_bw()

plot(p.sg.trt.i)
pp_check(p.sg.trt.i)+ scale_x_continuous(trans="log")+theme_bw()

plot(p.CDE.trt.i)
pp_check(p.CDE.trt.i)

#residuals
sl.trtm1<-residuals(p.sl.trt.i)
sl.trtm1<-as.data.frame(sl.trtm1)
View(sl.trtm1)
nrow(sl.trtm1)
nrow(p.dat2)
p.dat4<-p.dat2[complete.cases(p.dat2$SL.p), ]
nrow(p.dat4)
sl.trt.plot<-cbind(p.dat4,sl.trtm1$Estimate)
sl.trt.plot2<-inner_join(sl.trt.plot,dat)

View(sl.trt.plot2)
p.dat2$habitat<-as.character(as.factor(p.dat2$habitat))
is.character(p.dat2$habitat)
is.character(p.dat2$site_code)

par(mfrow=c(3,2))
with(sl.trt.plot2, plot(continent, sl.trtm1$Estimate))
with(sl.trt.plot2, plot(habitat, sl.trtm1$Estimate))
with(sl.trt.plot2, plot(site_code, sl.trtm1$Estimate))
with(sl.trt.plot2, plot(block, sl.trtm1$Estimate))
with(sl.trt.plot2, plot(plot, sl.trtm1$Estimate))
with(sl.trt.plot2, plot(f.year.y, sl.trtm1$Estimate))

# #------plot richness model all sp----------------
# fixed effects
p.sl.trt_fitted <- cbind(p.sl.trt.i$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(p.sl.trt.i, re_formula = NA)) %>% 
  as_tibble()



p.sl.trt_fitted.start<-p.sl.trt_fitted3[p.sl.trt_fitted3$year.x %in% c('0'),]

p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)



p.dat3<-p.datss %>% 
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich),
            r.rich = round(r.rich))


p.sl.trt_fitted3<-inner_join(p.sl.trt_fitted,p.dat3)

p.sl.trt_fitted3$starting.richness <- ifelse(p.sl.trt_fitted3$r.rich >= 1 & p.sl.trt_fitted3$r.rich <= 5, '1-5 species',
                                           ifelse(p.sl.trt_fitted3$r.rich >=6 & p.sl.trt_fitted3$r.rich <=10, '6-10',
                                                  ifelse(p.sl.trt_fitted3$r.rich >=11 & p.sl.trt_fitted3$r.rich <=15, '11-15',    
                                                         ifelse(p.sl.trt_fitted3$r.rich >=16 & p.sl.trt_fitted3$r.rich <=20, '16-20',
                                                                ifelse(p.sl.trt_fitted3$r.rich >=21 & p.sl.trt_fitted3$r.rich <=25, '21-25',
                                                                       ifelse(p.sl.trt_fitted3$r.rich >=26, '>26', 'other'))))))



levels(p.sl.trt_fitted3$trt.y)
colnames(p.sl.trt_fitted3)
p.sl.trt_fitted.npk<-p.sl.trt_fitted3[p.sl.trt_fitted3$trt.y %in% c('NPK'),]
p.sl.trt_fitted.ctl<-p.sl.trt_fitted3[p.sl.trt_fitted3$trt.y %in% c('Control'),]
View(p.sl.trt_fitted.npk)
View(p.sl.trt_fitted.ctl)

#either subset NPK and/or have two seperate lines for NPK and control

# fixed effect coefficients -coefficient plot
p.sl.trt_fixef <- fixef(p.sl.trt.i)

# coefficients for experiment-level (random) effects
p.sl.trt_coef
p.sl.trt_coef <- coef(p.sl.trt.i)
View(p.sl.trt_coef)

p.sl.trt_coef2 <-  bind_cols(p.sl.trt_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(p.sl.trt_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.sl.trt_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.sl.trt_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.sl.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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
             by = 'site_code')


p.sl.trt_coef2<-inner_join(p.sl.trt_coef2,p.dat.s)


p.sl.trt_coef2$starting.richness <- ifelse(p.sl.trt_coef2$r.rich >= 1 & p.sl.trt_coef2$r.rich <= 5, '1-5 species',
                                         ifelse(p.sl.trt_coef2$r.rich >=6 & p.sl.trt_coef2$r.rich <=10, '6-10',
                                                ifelse(p.sl.trt_coef2$r.rich >=11 & p.sl.trt_coef2$r.rich <=15, '11-15',    
                                                       ifelse(p.sl.trt_coef2$r.rich >=16 & p.sl.trt_coef2$r.rich <=20, '16-20',
                                                              ifelse(p.sl.trt_coef2$r.rich >=21 & p.sl.trt_coef2$r.rich <=25, '21-25',
                                                                     ifelse(p.sl.trt_coef2$r.rich >=26, '>26', 'other'))))))
View(p.sl.trt_coef2)
dat<-distinct(p.dat2, site_code, continent,habitat)

p.sl.trt_coef3<-inner_join(p.sl.trt_coef2,dat)
View(p.sl.trt_coef3)

rm(sl.trt.i)
save(sl.trt_fitted.npk,sl.trt_fitted.ctl,sl.trt_coef3,file = 'sl_dat.new.Rdata')
load('~/Desktop/Academic/R code/NutNet/sl_dat.Rdata')



library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


View(p.sl.trt_fitted.ctl)
View(p.sl.trt_fitted.npk)
View(p.sl.trt_coef3)

as.factor(as.character(p.sl.trt_fitted.npk$starting.richness))
as.factor(as.character(p.sl.trt_coef3$starting.richness))

# p.sl.trt_fitted.npk<-p.sl.trt_fitted.npk[complete.cases(p.sl.trt_fitted.npk$starting.richness), ]
# p.sl.trt_coef3<-p.sl.trt_coef3[complete.cases(p.sl.trt_coef3$starting.richness), ]

p.sl.trt_fitted.npk$starting.richness <- factor(p.sl.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.sl.trt_coef3$starting.richness <- factor(p.sl.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

View(p.sl.trt_fitted.npk)
View(p.sl.trt_coef3)
levels(p.sl.trt_fitted.npk$starting.richness)
summary(p.sl.trt_fitted.npk)


p.sl.trtm<-ggplot() +
  # data
  geom_point(data = p.sl.trt_fitted.npk,
             aes(x = year.y, y = SL.p,
                 colour = starting.richness, alpha=0.01),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=p.sl.trt_fitted.npk,
  #             aes(x = year.y, y = SL.p,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = p.sl.trt_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope) * cxmax),
                   #group = site_code,
                   colour = factor(starting.richness)),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.sl.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.sl.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = p.sl.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.sl.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_y_continuous(trans = reverselog_trans(), breaks=c(1,2,4,6,8,16,24,64,512,1024,2048,4096)) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'b) Change in Biomass due to SL') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")


p.sl.trtm

grid_arrange_shared_legend(p.s.loss.im,p.sl.trtm,nrow=1)


# #########################################GAINS ####################################################


color_scheme_set("purple")
pp_check(p.sg.trt.i)
pp_check(p.sg.trt.i, type = "hist")
#marginal effects
marginal_effects(p.sg.trt.i, surface = TRUE)
marginal_smooths(p.sg.trt.i)


summary(p.sg.trt.i)
#residuals
p.sg.trtm1<-residuals(p.sg.trt.i)
p.sg.trtm1<-as.data.frame(p.sg.trtm1)
nrow(p.sg.trtm1)
nrow(p.dat2)
p.sg.trt.plot<-cbind(p.dat2,p.sg.trtm1$Estimate)
p.sg.trt.plot2<-inner_join(p.sg.trt.plot,dat)

par(mfrow=c(3,2))
with(p.sg.trt.plot2, plot(continent, p.sg.trtm1$Estimate))
with(p.sg.trt.plot2, plot(habitat, p.sg.trtm1$Estimate))
with(p.sg.trt.plot, plot(site_code, p.sg.trtm1$Estimate))
with(p.sg.trt.plot2, plot(block, p.sg.trtm1$Estimate))
with(p.sg.trt.plot2, plot(plot, p.sg.trtm1$Estimate))
with(p.sg.trt.plot2, plot(f.year.y, p.sg.trtm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects

p.sg.trt_fitted <- cbind(p.sg.trt.i$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(p.sg.trt.i, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(p.sg.trt_fitted)


p.sg.trt_fitted.start<-p.sg.trt_fitted3[p.sg.trt_fitted3$year.x %in% c('0'),]

p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)



p.dat3<-p.datss %>% 
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich),
            r.rich = round(r.rich))


p.sg.trt_fitted3<-inner_join(p.sg.trt_fitted,p.dat3)

p.sg.trt_fitted3$starting.richness <- ifelse(p.sg.trt_fitted3$r.rich >= 1 & p.sg.trt_fitted3$r.rich <= 5, '1-5 species',
                                             ifelse(p.sg.trt_fitted3$r.rich >=6 & p.sg.trt_fitted3$r.rich <=10, '6-10',
                                                    ifelse(p.sg.trt_fitted3$r.rich >=11 & p.sg.trt_fitted3$r.rich <=15, '11-15',    
                                                           ifelse(p.sg.trt_fitted3$r.rich >=16 & p.sg.trt_fitted3$r.rich <=20, '16-20',
                                                                  ifelse(p.sg.trt_fitted3$r.rich >=21 & p.sg.trt_fitted3$r.rich <=25, '21-25',
                                                                         ifelse(p.sg.trt_fitted3$r.rich >=26, '>26', 'other'))))))



levels(p.sg.trt_fitted3$trt.y)
colnames(p.sg.trt_fitted3)
p.sg.trt_fitted.npk<-p.sg.trt_fitted3[p.sg.trt_fitted3$trt.y %in% c('NPK'),]
p.sg.trt_fitted.ctl<-p.sg.trt_fitted3[p.sg.trt_fitted3$trt.y %in% c('Control'),]
p.sg.trt_fitted.npk

# fixed effect coefficients (I want these for the coefficient plot)
p.sg.trt_fixef <- fixef(p.sg.trt.i)

# coefficients for experiment-level (random) effects
p.sg.trt_coef <- coef(p.sg.trt.i)
p.sg.trt_coef 
p.sg.trt_coef2 <-  bind_cols(p.sg.trt_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(p.sg.trt_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.sg.trt_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.sg.trt_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.sg.trt_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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
             by = 'site_code')

p.sg.trt_coef2<-inner_join(p.sg.trt_coef2,p.dat.s)


p.sg.trt_coef2$starting.richness <- ifelse(p.sg.trt_coef2$r.rich >= 1 & p.sg.trt_coef2$r.rich <= 5, '1-5 species',
                                           ifelse(p.sg.trt_coef2$r.rich >=6 & p.sg.trt_coef2$r.rich <=10, '6-10',
                                                  ifelse(p.sg.trt_coef2$r.rich >=11 & p.sg.trt_coef2$r.rich <=15, '11-15',    
                                                         ifelse(p.sg.trt_coef2$r.rich >=16 & p.sg.trt_coef2$r.rich <=20, '16-20',
                                                                ifelse(p.sg.trt_coef2$r.rich >=21 & p.sg.trt_coef2$r.rich <=25, '21-25',
                                                                       ifelse(p.sg.trt_coef2$r.rich >=26, '>26', 'other'))))))

View(p.sg.trt_coef3)

View(p.sg.trt_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

p.sg.trt_coef3<-full_join(p.sg.trt_coef2,dat)


rm(p.sg.trt.i)
save(p.sg.trt_fitted.npk,p.sg.trt_fitted.ctl,p.sg.trt_coef3,file = 'sg_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/sg_dat.Rdata')




summary(p.sg.trt_fitted3)
summary(p.sg.trt_coef3)

is.factor(p.sg.trt_fitted.npk$starting.richness)
is.factor(p.sg.trt_coef3$starting.richness)

p.sg.trt_fitted.npk<-p.sg.trt_fitted.npk[complete.cases(p.sg.trt_fitted.npk$starting.richness), ]
p.sg.trt_coef3<-p.sg.trt_coef3[complete.cases(p.sg.trt_coef3$starting.richness), ]


p.sg.trt_fitted.npk$starting.richness <- factor(p.sg.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.sg.trt_coef3$starting.richness <- factor(p.sg.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


#gai
p.sg.trtm<-ggplot()  +
  # data
  geom_point(data = p.sg.trt_fitted.npk,
             aes(x = year.y, y = SG,
                 colour = starting.richness, alpha=0.1),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=p.sg.trt_fitted.npk,
  #             aes(x = year.y, y = SG,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  # uncertainy in fixed effect
  geom_segment(data = p.sg.trt_coef3,
               aes(x = xmin,
                   xend = xmax,
                   y = exp(Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope)  * cxmax),
                   # group = starting.richness,
                   colour = starting.richness),
               size = .7) +
  geom_ribbon(data = p.sg.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.sg.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = p.sg.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.sg.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_y_continuous(trans = 'log', breaks=c(1,2,4,6,8,16,24,64,512,1024,2048,4096)) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'd) Change in Biomass due to SG') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")

p.sg.trtm


grid_arrange_shared_legend(sl.trtm,p.sg.trtm,nrow=1)
grid_arrange_shared_legend(s.loss.im,s.gain.im,nrow=1)

grid_arrange_shared_legend(s.loss.im,sl.trtm,nrow=1)
grid_arrange_shared_legend(s.gain.im,p.sg.trtm,nrow=1)

grid_arrange_shared_legend(p.s.loss.im,p.sl.trtm,p.s.gain.im,p.sg.trtm,nrow=2,ncol=2)


########################################################################################################################
#####################################CDE model###################################################################################
########################################################################################################################

summary(p.cde.trt.i)
color_scheme_set("purple")
pairs(p.cde.trt)

# inspection of chain diagnostic
plot(p.cde.trt)  


pp_check(p.cde.trt)

pp_check(p.cde.trt, type = "hist")
#marginal effects
marginal_effects(p.cde.trt, surface = TRUE)
marginal_smooths(p.cde.trt)



#residuals
cm1<-residuals(p.cde.trt)
cm1<-as.data.frame(cm1)
nrow(cm1)
nrow(p.dat3)
p.dat4<-p.dat3[complete.cases(p.dat3$CDE), ]
cde.plot<-cbind(p.dat3,cm1$Estimate)
cde.plot2<-inner_join(cde.plot,dat)

par(mfrow=c(3,2))
with(cde.plot2, plot(continent, cm1$Estimate))
with(cde.plot2, plot(habitat, cm1$Estimate))
with(cde.plot, plot(site_code, cm1$Estimate))
with(cde.plot2, plot(block, cm1$Estimate))
with(cde.plot2, plot(plot, cm1$Estimate))
with(cde.plot2, plot(f.year.y, cm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
p.cde_fitted <- cbind(p.CDE.trt.i$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(p.CDE.trt.i, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(p.cde_fitted)

p.cde_fitted3<-inner_join(p.cde_fitted,p.dat3)
View(p.cde_fitted)
p.cde.trt_fitted.start<-p.cde_fitted3[p.cde_fitted3$year.x %in% c('0'),]

p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)



p.dat3<-p.datss %>% 
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich),
            r.rich = round(r.rich))


p.cde.trt_fitted3<-inner_join(p.cde_fitted,p.dat3)

p.cde_fitted3$starting.richness <- ifelse(p.cde.trt_fitted3$r.rich >= 1 & p.cde.trt_fitted3$r.rich <= 5, '1-5 species',
                                             ifelse(p.cde.trt_fitted3$r.rich >=6 & p.cde.trt_fitted3$r.rich <=10, '6-10',
                                                    ifelse(p.cde.trt_fitted3$r.rich >=11 & p.cde.trt_fitted3$r.rich <=15, '11-15',    
                                                           ifelse(p.cde.trt_fitted3$r.rich >=16 & p.cde.trt_fitted3$r.rich <=20, '16-20',
                                                                  ifelse(p.cde.trt_fitted3$r.rich >=21 & p.cde.trt_fitted3$r.rich <=25, '21-25',
                                                                         ifelse(p.cde.trt_fitted3$r.rich >=26, '>26', 'other'))))))



levels(p.cde.trt_fitted3$trt.y)
colnames(p.cde.trt_fitted3)
p.cde_fitted.npk<-p.cde_fitted3[p.cde_fitted3$trt.y %in% c('NPK'),]
p.cde_fitted.ctl<-p.cde_fitted3[p.cde_fitted3$trt.y %in% c('Control'),]
View(p.cde_fitted.npk)


# fixed effect coefficients (I want these for the coefficient plot)
p.cde_fixef <- fixef(p.CDE.trt.i)

# coefficients for experiment-level (random) effects
p.cde_coef <- coef(p.CDE.trt.i)
p.cde_coef
p.cde_coef2 <-  bind_cols(p.cde_coef$site_code[,,'Intercept'] %>% 
                          as_tibble() %>% 
                          mutate(Intercept = Estimate,
                                 Intercept_lower = Q2.5,
                                 Intercept_upper = Q97.5,
                                 site_code = rownames(p.cde_coef$site_code[,,'Intercept'])) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        p.cde_coef$site_code[,,'year.y.m'] %>% 
                          as_tibble() %>% 
                          mutate(ISlope = Estimate,
                                 ISlope_lower = Q2.5,
                                 ISlope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        p.cde_coef$site_code[,,'trt.yNPK'] %>% 
                          as_tibble() %>% 
                          mutate(TE = Estimate,
                                 TE_lower = Q2.5,
                                 TE_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        p.cde_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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
             by = 'site_code')

p.cde_coef2<-inner_join(p.cde_coef2,p.dat.s)



p.cde_coef2$starting.richness <- ifelse(p.cde_coef2$r.rich >= 1 & p.cde_coef2$r.rich <= 5, '1-5 species',
                                      ifelse(p.cde_coef2$r.rich >=6 & p.cde_coef2$r.rich <=10, '6-10',
                                             ifelse(p.cde_coef2$r.rich >=11 & p.cde_coef2$r.rich <=15, '11-15',    
                                                    ifelse(p.cde_coef2$r.rich >=16 & p.cde_coef2$r.rich <=20, '16-20',
                                                           ifelse(p.cde_coef2$r.rich >=21 & p.cde_coef2$r.rich <=25, '21-25',
                                                                  ifelse(p.cde_coef2$r.rich >=26, '>26', 'other'))))))

View(p.cde_coef2)

View(p.cde_fitted)


p.cde_coef3<-full_join(p.cde_coef2,dat)


rm(p.p.cde.trt.i)
save(p.cde_fitted.npk,p.cde_fitted.ctl,p.cde_coef3,file = 'p.cde_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/sg_dat.Rdata')


p.cde_fitted.npk<-p.cde_fitted.npk[complete.cases(p.cde_fitted.npk$starting.richness), ]
p.cde_coef3<-p.cde_coef3[complete.cases(p.cde_coef3$starting.richness), ]

p.cde_fitted.npk$starting.richness <- factor(p.cde_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.cde_coef3$starting.richness <- factor(p.cde_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))




library(scales)
sign_sqrt <- scales::trans_new('sign_sqrt',
                               transform = function(x) {sign(x) * sqrt(abs(x))},
                               inverse = function(x){sign(x) * abs(x)^2})


View(p.cde_fitted.npk)
#p.cde
p.cde_coef3$xs<-1
pcdem<-ggplot() +
  # data
  geom_point(data = p.cde_fitted.npk,
             aes(x = year.y, y = CDE,
                 colour = starting.richness, alpha=0.1),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=cde_fitted.npk,
  #             aes(x = year.y, y = CDE,
  #                 colour = starting.richness), height=0.25,width = 0.25)+
  #experiment (random) effects
  geom_segment(data = p.cde_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope)  * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.cde_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.cde_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = p.cde_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.cde_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_y_continuous(trans = sign_sqrt #, breaks=c(8,64,512,1024,2048,4096)
  ) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'Biomass Change in Persistent Species') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")

pcdem

grid_arrange_shared_legend(p.c.rich.im,pcdem,nrow=1)

grid_arrange_shared_legend(sl.trtm,sg.trtm,cdem,nrow=1)





#coefs


sl.trt_fixef2<-as.data.frame(sl.trt_fixef)
sg.trt_fixef2<-as.data.frame(sg.trt_fixef)
cde_fixef2<-as.data.frame(cde_fixef)
View(sl.trt_fixef2)

sl.trt_coef4<-sl.trt_coef3[complete.cases(sl.trt_coef3), ]
sg.trt_coef4<-sg.trt_coef3[complete.cases(sg.trt_coef3), ]
cde_coef4<-cde_coef3[complete.cases(cde_coef3), ]

View(sl.trt_coef4)

sl.trt_fixef2$Estimate.n<-(sl.trt_fixef2$Estimate * -1)
sl.trt_fixef2$Est.Error.n<-(sl.trt_fixef2$Est.Error * -1)
sl.trt_fixef2$Q2.5.n<-(sl.trt_fixef2$Q2.5 * -1)
sl.trt_fixef2$Q97.5.n<-(sl.trt_fixef2$Q97.5 * -1)

View(sl.trt_coef4)
View(sl.trt_fixef2)
sl.trtm2<-ggplot() + 
  geom_point(data = sl.trt_coef4, aes(x =reorder(site_code,TESlope), y = TESlope, colour = continent),size = 2) +
  geom_errorbar(data = sl.trt_coef4, aes(x = reorder(site_code,TESlope),ymin = TESlope_6-10er,
                                         ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  #facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = sl.trt_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = sl.trt_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  ylim(-0.2,0.4) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'a) EF: SL') +
  scale_y_continuous(trans = reverse_trans()) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


sl.trtm2

sl.trt_coef4$Slope.sl<-sl.trt_coef4$TESlope
sl.trt.Slope<-select(sl.trt_coef4,site_code,Slope.sl)
sg.trt_coef5<-inner_join(sg.trt_coef4,sl.trt.Slope)
is.numeric(sg.trt_coef5$Slope.sl)


View(sg.trt_coef5)
View(sg.trt_fixef2)
sg.trtm2<-ggplot() + 
  geom_point(data = sg.trt_coef5, aes(x = reorder(site_code,Slope.sl), y = TESlope,colour = continent),size = 2) +
  geom_errorbar(data = sg.trt_coef5, aes(x = reorder(site_code,Slope.sl),ymin = TESlope_6-10er,
                                         ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = sg.trt_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = sg.trt_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  ylim(-0.2,0.4) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'b) EF : SG') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")
sg.trtm2


#sl.trt_coef4$Slope.sl<-sl.trt_coef4$Slope
#sl.trt.Slope<-select(sl.trt_coef4,site_code,Slope.sl)
cde_coef5<-inner_join(cde_coef4,sl.trt.Slope)
is.numeric(sg.trt_coef5$Slope.sl)

cde_fixef2
cdem2<-ggplot() + 
  geom_point(data = cde_coef5, aes(x = reorder(site_code,Slope.sl), y = TESlope,colour = continent),size = 2) +
  geom_errorbar(data = cde_coef5, aes(x = reorder(site_code,Slope.sl),ymin = TESlope_6-10er,
                                      ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = cde_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = cde_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  #ylim(-100,100) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'c) EF: Persistent Species') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

cdem2

grid_arrange_shared_legend(sl.trtm2,sg.trtm2,cdem2,nrow=1)
#sl.trtm,sg.trtm,cdem,
grid_arrange_shared_legend(sl.trtm,sg.trtm,cdem,sl.trtm2,sg.trtm2,cdem2,ncol=3,nrow=2)






View(sl.c_coef3)
View(sg.c_coef3)
colnames(sl.c_coef3)
colnames(sg.c_coef3)
sl.c_coef4<-sl.c_coef3[,c(-1,-2,-3)]
sg.c_coef4<-sg.c_coef3[,c(-1,-2,-3)]
colnames(sl.c_coef4)
colnames(sg.c_coef4)
sl.c_coef5<-sl.c_coef4[,c(-9,-10,-11,-12,-13,-14,-15,-16,-17,-18)]
names(sl.c_coef4) <- c("site_code","sl.c.Slope","sl.c.Slope_6-10er","sl.c.Slope_upper","xmin","xmax","cxmin","cxmax","continent","habitat")
names(sg.c_coef4) <- c("site_code","sg.c.Slope","sg.c.Slope_6-10er","sg.c.Slope_upper","xmin","xmax","cxmin","cxmax","continent","habitat")
delta.coefs<-bind_cols(sl.c_coef4,sg.c_coef4)
View(delta.coefs)

#By Experiment
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(data=delta.coefs, aes(x=sl.c.sl.cope, y=sg.c.sl.cope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = sg.c.sl.cope_6-10er, ymax = sg.c.sl.cope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = sl.c.sl.cope_6-10er, xmax = sl.c.sl.cope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'sl.c sl.cope',
       y = 'sg.c sl.cope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")






ps.sl.c<-as.data.frame(posterior_samples(sl.c))
ps.sg.c<-as.data.frame(posterior_samples(sg.c))
ps.cde<-as.data.frame(posterior_samples(CDE.c))

View(ps.sl.c)


devtools::install_github("mvuorre/brmstools")
library(brmstools)
forest(sl.c)




# MULTIVARIATE MODEL

summary(multi_sg.c.sl.c)

color_scheme_set("purple")
pp_check(multi_sg.c.sl.c, resp = 'sl.clog')
pp_check(multi_sg.c.sl.c, resp = 'sg.clog')

plot(multi_sg.c.sl.c)


m1<-residuals(multi_sg.c.sl.c)
head(m1)
nrow(m1)
nrow(m2)
nrow(p.all4)
p.all5 <- p.all4[!(is.na(p.all4$sl.c)),]
p.all6 <- p.all5[!(is.na(p.all5$sg.c)),]
nrow(p.all6)
plot <- cbind(p.all6,m1)
m1<-as.data.frame(m1)
levels(plot$site_code)


par(mfrow=c(2,2))
with(plot, plot(site_code, m1$Estimate.sl.c))
with(plot, plot(block, m1$Estimate.sl.c))
with(plot, plot(plot, m1$Estimate.sl.c))
with(plot, plot(f.year.y, m1$Estimate.sl.c))

par(mfrow=c(2,2))
with(plot, plot(site_code, m1$Estimate.sg.c))
with(plot, plot(block, m1$Estimate.sg.c))
with(plot, plot(plot, m1$Estimate.sg.c))
with(plot, plot(f.year.y, m1$Estimate.sg.c))


mm_fitted <- cbind(multi_sg.c.sl.c$data,
                   # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                   fitted(multi_sg.c.sl.c, re_formula = NA)) %>% 
  as_tibble() 

dat2<-distinct(p.all6, site.year.id,site_code)
View(dat2)
dat3<-inner_join(dat2,dat,by='site_code')
View(dat3)
mm_fitted2<-inner_join(mm_fitted, dat3, by='site.year.id')

View(p.all6)
View(mm_fitted)


# fixed effect coefficients (I want these for the coefficient plot)
mm_fixef <- fixef(multi_sg.c.sl.c)
View(mm_fixef)


# coefficients for experiment-level (random) effects
mm_coef <- coef(multi_sg.c.sl.c)
View(mm_coef)
mm_coef
mm_coef.sl.c <-  bind_cols(mm_coef$site.year.id[,,'sl.c_Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_6-10er = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site.year.id = rownames(mm_coef$site.year.id[,,'sl.c_Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           mm_coef$site.year.id[,,'sl.c_year.y'] %>% 
                             as_tibble() %>% 
                             mutate(sl.cope = Estimate,
                                    sl.cope_6-10er = Q2.5,
                                    sl.cope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all6 %>% 
               group_by(site.year.id) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site.year.id') 


View(mm_coef.sl.c)


mm_coef.sg.c <-  bind_cols(mm_coef$site.year.id[,,'sg.c_Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_6-10er = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site.year.id = rownames(mm_coef$site.year.id[,,'sg.c_Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           mm_coef$site.year.id[,,'sg.c_year.y'] %>% 
                             as_tibble() %>% 
                             mutate(sl.cope = Estimate,
                                    sl.cope_6-10er = Q2.5,
                                    sl.cope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all6 %>% 
               group_by(site.year.id) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site.year.id')



mm_coef.sl.c2<-full_join(mm_coef.sl.c,dat3)

mm_coef.sg.c2<-full_join(mm_coef.sg.c,dat3)

colnames(mm_fitted2)
View(mm_coef.sl.c2)

#loss
ggplot() +
  # data
  geom_point(data = mm_fitted2,
             aes(x = year.y, y = sl.c,
                 colour = continent, alpha=0.5),
             size = 1.2) +
  geom_jitter(data=mm_fitted2,
              aes(x = year.y, y = sl.c,
                  colour = continent), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = mm_coef.sl.c2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + sl.cope * xmin,
                   yend = Intercept + sl.cope * xmax,
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = mm_fitted2,
              aes(x = year.y, ymin = Q2.5.sl.c, ymax = Q97.5.sl.c),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = mm_fitted2,
            aes(x = year.y, y = Estimate.sl.c),
            size = 1.5) +
  #scale_y_continuous(trans = 'log10') +
  labs(x = 'Years',
       y = 'Function Loss due to Species Loss', title= 'a) Species Loss') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

#gain
ggplot() +
  # data
  geom_point(data = mm_fitted2,
             aes(x = year.y, y = sg.c,
                 colour = continent, alpha=0.5),
             size = 1.2) +
  geom_jitter(data=mm_fitted2,
              aes(x = year.y, y = sg.c,
                  colour = continent), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = mm_coef.sg.c2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + sl.cope * xmin,
                   yend = Intercept + sl.cope * xmax,
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = mm_fitted2,
              aes(x = year.y, ymin = Q2.5.sg.c, ymax = Q97.5.sg.c),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = mm_fitted2,
            aes(x = year.y, y = Estimate.sg.c),
            size = 1.5) +
  #scale_y_continuous(trans = 'log10') +
  labs(x = 'Years',
       y = 'Function Loss due to Species Gain', title= 'b) Species Gain') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

