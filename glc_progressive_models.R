
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
#shanes links

sp <- read.csv("~/Dropbox/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/NutNet/Data/cumulative_time_only.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(p.all)
levels(p.all3$trt_year)

p.all<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all$f.year.y<-as.factor(p.all$year.y)
p.all$plot<-as.factor(p.all$plot)
p.all$site_code<-as.factor(p.all$site_code)

dat<-distinct(plot, site_code, continent,habitat)
p.dat2<-inner_join(p.all,dat)

nrow(p.dat)
#p.dat2<-p.dat[complete.cases(p.dat), ]
nrow(p.dat2)

View(p.all)
colnames(p.dat2)
# s loss, gain and change metrics
# p.dat2$s.loss <- -1*(p.dat2$x.rich - p.dat2$p.c.rich)
# p.dat2$p.s.gain <- p.dat2$y.rich - p.dat2$p.c.rich
# p.dat2$p.s.change <- p.dat2$y.rich - p.dat2$x.rich

p.dat2$c.rich.log<-log1p(p.dat2$c.rich)
as.numeric(p.all$c.rich)
par(mfrow=c(1,2))
hist(p.dat2$c.rich, breaks=10, main="C. Rich", xlab= "C. Rich")
hist(p.dat2$c.rich.log, breaks=10, main="Log C. Rich", xlab= "Log C. Rich")

p.dat2$c.func <- p.dat2$y.func - p.dat2$x.func
par(mfrow=c(1,1))
hist(p.dat2$c.func, breaks=10, main="C. Func", xlab= "Log C. Func")


summary(p.dat2)
par(mfrow=c(2,3))
hist(p.dat2$s.loss.p,breaks =20, main="Species Loss", xlab= "Species Loss")
hist(p.dat2$p.s.gain, breaks=20, main="Species Gains", xlab= "Species Gains")
hist(p.dat2$p.s.change, breaks=10, main="Species Change", xlab= "Species Change")
hist(p.dat2$s.loss.p.log,breaks =10, main="Log Species Loss", xlab= "Log Species Loss")
hist(p.dat2$p.s.gain.log, breaks=10, main="Log Species Gains", xlab= "Log Species Gains")
hist(p.dat2$p.s.change.log, breaks=10, main="Log Species Change", xlab= "Log Species Change")


# `
# p.s.loss.i <- brm(s.loss.p~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot),
#               data = p.dat2, family=hurdle_lognormal(),cores = 4, chains = 4)
# 
# 
# p.s.gain.i <- brm(s.gain ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot),
#               data = p.dat2,family=hurdle_lognormal(), cores = 4, chains = 4)
# 
# 
# # p.s.change.i <- brm(p.s.change ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id/block/plot), 
# #                 data = p.dat3, family=asym_laplace(),cores = 4, chains = 4)
# 
# 
# p.c.rich.i <- brm(c.rich ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
#                 data = p.dat2,family=hurdle_lognormal(),cores = 4, chains = 4)
# 
# 
# p.c.func.i <- brm(c.func ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
#                 data = p.dat2,family=asym_laplace(),cores = 4, chains = 4)


#save mods
# setwd('~/Dropbox/Projects/NutNet/Model_fits/')
# save(p.s.loss.i,file = 'nn_time.sloss-progressive-hurdle.Rdata.Rdata')
# save(p.s.gain.i,file = 'nn_time.gain-progressive-hurdle.Rdata.Rdata')
# save(p.c.rich.i,file = 'nn_time.crich-progressive.Rdata.Rdata')
# save(p.c.func.i,file = 'nn_time.cfunc-progressive.Rdata.Rdata')

#progressive models with time only
#These do converge so we use these
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sloss-progressive.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sgain-progressive.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.schange-progressive.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.crich-progressive.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.cfunc-progressive.Rdata')

#hurdle log normal models
#models have same names as above-oops
#sloss does not converge
#load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sloss-progressive-hurdle.Rdata')
#sgain does not converge
# load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sgain-progressive-hurdle.Rdata')
     
summary(p.s.loss.i)

summary(p.s.gain.i)

summary(p.s.change.i)

summary(p.c.rich.i)

summary(p.c.func.i)


plot(p.s.loss.i)

plot(p.s.gain.i)

plot(p.s.change.i)

pp_check(p.s.loss.i)
pp_check(p.s.loss.i)+ scale_x_continuous(trans="log") +theme_bw()

pp_check(p.s.gain.i)
pp_check(p.s.gain.i)+ scale_x_continuous(trans="log") +theme_bw()

pp_check(p.s.change.i)

pp_check(p.c.rich.i)
pp_check(p.c.rich.i)+ scale_x_continuous(trans="log") +theme_bw()


pp_check(p.c.func.i)


dat<-distinct(p.dat, site_code, continent,habitat)


p.s.loss.i_fitted <- cbind(p.s.loss.i$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(p.s.loss.i, re_formula = NA)) %>% 
  as_tibble()

p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)


p.dat3<-p.datss %>% 
  group_by(continent,habitat,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich))

p.dat4<-p.dat3[p.dat3$year.x %in% c('0'),]
p.dat5<-distinct(p.dat4, site_code, trt.xy, block, plot,s.rich)
p.dat5
p.dat6<-subset(p.dat5, select = - c( year.x,year.y))
View(p.s.loss.i_fitted)
View(p.dat6)

p.s.loss.i_fitted2<-inner_join(p.s.loss.i_fitted,p.dat2)
p.s.loss.i_fitted3<-inner_join(p.s.loss.i_fitted2,p.dat6)



View(p.s.loss.i_fitted3)

p.s.loss.i_fitted3$starting.richness <- ifelse(p.s.loss.i_fitted3$s.rich >= 1 & p.s.loss.i_fitted3$s.rich <= 5, '1-5 species',
                                  ifelse(p.s.loss.i_fitted3$s.rich >=6 & p.s.loss.i_fitted3$s.rich <=10, '6-10',
                                         ifelse(p.s.loss.i_fitted3$s.rich >=11 & p.s.loss.i_fitted3$s.rich <=15, '11-15',    
                                                ifelse(p.s.loss.i_fitted3$s.rich >=16 & p.s.loss.i_fitted3$s.rich <=20, '16-20',
                                                       ifelse(p.s.loss.i_fitted3$s.rich >=21 & p.s.loss.i_fitted3$s.rich <=25, '21-25',
                                                              ifelse(p.s.loss.i_fitted3$s.rich >=26, '>26', 'other'))))))



p.s.loss.i_fitted.npk<-p.s.loss.i_fitted3[p.s.loss.i_fitted3$trt.y %in% c('NPK'),]
p.s.loss.i_fitted.ctl<-p.s.loss.i_fitted3[p.s.loss.i_fitted3$trt.y %in% c('Control'),]



# fixed effect coefficients -coefficient plot
p.s.loss.i_fixef <- fixef(p.s.loss.i)


p.s.loss.i_coef <- coef(p.s.loss.i)
p.s.loss.i_coef

p.s.loss.i_coef2 <-  bind_cols(p.s.loss.i_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(p.s.loss.i_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.s.loss.i_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.s.loss.i_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           p.s.loss.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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

View(p.s.loss.i_coef2)
View(p.dat6)

p.s.loss.i_coef3<-p.s.loss.i_coef2 %>% inner_join(p.dat6 %>%
                               group_by(site_code) %>%
                               summarise(ss.rich = mean(s.rich),
                                          r.rich = round(ss.rich)),
                               by = 'site_code')
                               
View(p.s.loss.i_coef3)
p.s.loss.i_coef3$starting.richness <- ifelse(p.s.loss.i_coef3$r.rich >= 1 & p.s.loss.i_coef3$r.rich <= 5, '1-5 species',
                                ifelse(p.s.loss.i_coef3$r.rich >=6 & p.s.loss.i_coef3$r.rich <=10, '6-10',
                                       ifelse(p.s.loss.i_coef3$r.rich >=11 & p.s.loss.i_coef3$r.rich <=15, '11-15',    
                                              ifelse(p.s.loss.i_coef3$r.rich >=16 & p.s.loss.i_coef3$r.rich <=20, '16-20',
                                                     ifelse(p.s.loss.i_coef3$r.rich >=21 & p.s.loss.i_coef3$r.rich <=25, '21-25',
                                                            ifelse(p.s.loss.i_coef3$r.rich >=26, '>26', 'other'))))))

p.s.loss.i_coef3<-inner_join(p.s.loss.i_coef3,dat)
View(p.s.loss.i_coef3)


rm(p.s.loss.i)
save(p.s.loss.i_fitted.npk,p.s.loss.i_fitted.ctl,p.s.loss.i_coef3,file = 's.loss_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/s.loss_dat.Rdata')



View(p.s.loss.i_fitted.npk)

p.s.loss.i_fitted.npk<-p.s.loss.i_fitted.npk[complete.cases(p.s.loss.i_fitted.npk$starting.richness), ]
p.s.loss.i_coef3<-p.s.loss.i_coef3[complete.cases(p.s.loss.i_coef3$starting.richness), ]

p.s.loss.i_fitted.npk$starting.richness <- factor(p.s.loss.i_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.s.loss.i_coef3$starting.richness <- factor(p.s.loss.i_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
is.factor(p.s.loss.i_coef3$starting.richness)

library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


p.s.loss.i_coef3$xs<-1
p.s.loss.im<-ggplot() +
  # data
  geom_point(data = p.s.loss.i_fitted.npk,
             aes(x = year.y, y = s.loss.p,
                 colour = starting.richness, alpha=0.01),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=p.s.loss.i_fitted.npk,
  #             aes(x = year.y, y = s.loss.p,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = p.s.loss.i_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = exp(Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.s.loss.i_fitted.npk,
              aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.s.loss.i_fitted.npk,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5) +
  geom_ribbon(data = p.s.loss.i_fitted.ctl,
              aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.5) +
  # fixed effect
  geom_line(data =  p.s.loss.i_fitted.ctl,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5, linetype= "dashed") +
  scale_y_continuous(trans = reverselog_trans(), breaks=c(1,2,4,6,8,16,24) ) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Plot Species Loss')), title= 'a) Species Loss') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))

p.s.loss.im



########################################################################################################################
##################################### GAINS ##################################################################################
########################################################################################################################


p.s.gain.im1<-residuals(p.s.gain.i)
p.s.gain.im1<-as.data.frame(p.s.gain.im1)
nrow(p.s.gain.im1)
nrow(p.dat)
p.s.gain.i.plot<-cbind(p.dat,p.s.gain.im1$Estimate)
p.s.gain.i.plot2<-inner_join(p.s.gain.i.plot,dat)

par(mfrow=c(3,2))
with(p.s.gain.i.plot2, plot(continent, p.s.gain.im1$Estimate))
with(p.s.gain.i.plot2, plot(habitat, p.s.gain.im1$Estimate))
with(p.s.gain.i.plot, plot(site_code, p.s.gain.im1$Estimate))
with(p.s.gain.i.plot2, plot(block, p.s.gain.im1$Estimate))
with(p.s.gain.i.plot2, plot(plot, p.s.gain.im1$Estimate))
with(p.s.gain.i.plot2, plot(f.year.y, p.s.gain.im1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
p.s.gain.i_fitted <- cbind(p.s.gain.i$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(p.s.gain.i, re_formula = NA)) %>% 
  as_tibble() 

as.data.frame(p.s.gain.i_fitted)
p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)


p.dat3<-p.datss %>% 
  group_by(continent,habitat,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich))

p.dat4<-p.dat3[p.dat3$year.x %in% c('0'),]
p.dat5<-distinct(p.dat4, site_code, trt.xy, block, plot,s.rich)
p.dat5
p.dat6<-subset(p.dat5, select = - c( year.x,year.y))
View(p.s.gain.i_fitted)
View(p.dat6)

p.s.gain.i_fitted2<-inner_join(p.s.gain.i_fitted,p.dat2)
p.s.gain.i_fitted3<-inner_join(p.s.gain.i_fitted2,p.dat6)



p.s.gain.i_fitted3$starting.richness <- ifelse(p.s.gain.i_fitted3$s.rich >= 1 & p.s.gain.i_fitted3$s.rich <= 5, '1-5 species',
                                    ifelse(p.s.gain.i_fitted3$s.rich >=6 & p.s.gain.i_fitted3$s.rich <=10, '6-10',
                                           ifelse(p.s.gain.i_fitted3$s.rich >=11 & p.s.gain.i_fitted3$s.rich <=15, '11-15',    
                                                  ifelse(p.s.gain.i_fitted3$s.rich >=16 & p.s.gain.i_fitted3$s.rich <=20, '16-20',
                                                         ifelse(p.s.gain.i_fitted3$s.rich >=21 & p.s.gain.i_fitted3$s.rich <=25, '21-25',
                                                                ifelse(p.s.gain.i_fitted3$s.rich >=26, '>26', 'other'))))))



p.s.gain.i_fitted.npk<-p.s.gain.i_fitted3[p.s.gain.i_fitted3$trt.y %in% c('NPK'),]
p.s.gain.i_fitted.ctl<-p.s.gain.i_fitted3[p.s.gain.i_fitted3$trt.y %in% c('Control'),]


View(p.dat3)
View(p.s.gain.i_fitted.npk)


# fixed effect coefficients (I want these for the coefficient plot)
p.s.gain.i_fixef <- fixef(p.s.gain.i)

# coefficients for experiment-level (random) effects
p.s.gain.i_coef <- coef(p.s.gain.i)
p.s.gain.i_coef 

p.s.gain.i_coef2 <-  bind_cols(p.s.gain.i_coef$site_code[,,'Intercept'] %>% 
                               as_tibble() %>% 
                               mutate(Intercept = Estimate,
                                      Intercept_lower = Q2.5,
                                      Intercept_upper = Q97.5,
                                      site_code = rownames(p.s.gain.i_coef$site_code[,,'Intercept'])) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.s.gain.i_coef$site_code[,,'year.y.m'] %>% 
                               as_tibble() %>% 
                               mutate(ISlope = Estimate,
                                      ISlope_lower = Q2.5,
                                      ISlope_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.s.gain.i_coef$site_code[,,'trt.yNPK'] %>% 
                               as_tibble() %>% 
                               mutate(TE = Estimate,
                                      TE_lower = Q2.5,
                                      TE_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.s.gain.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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


p.s.gain.i_coef3<-p.s.gain.i_coef2 %>% inner_join(p.dat6 %>%
                                                    group_by(site_code) %>%
                                                    summarise(ss.rich = mean(s.rich),
                                                              r.rich = round(ss.rich)),
                                                  by = 'site_code')

p.s.gain.i_coef3$starting.richness <- ifelse(p.s.gain.i_coef3$r.rich >= 1 & p.s.gain.i_coef3$r.rich <= 5, '1-5 species',
                                  ifelse(p.s.gain.i_coef3$r.rich >=6 & p.s.gain.i_coef3$r.rich <=10, '6-10',
                                         ifelse(p.s.gain.i_coef3$r.rich >=11 & p.s.gain.i_coef3$r.rich <=15, '11-15',    
                                                ifelse(p.s.gain.i_coef3$r.rich >=16 & p.s.gain.i_coef3$r.rich <=20, '16-20',
                                                       ifelse(p.s.gain.i_coef3$r.rich >=21 & p.s.gain.i_coef3$r.rich <=25, '21-25',
                                                              ifelse(p.s.gain.i_coef3$r.rich >=26, '>26', 'other'))))))


View(p.s.gain.i_coef3)

View(p.s.gain.i_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

p.s.gain.i_coef3<-full_join(p.s.gain.i_coef3,dat)

rm(p.s.gain.i)
save(p.s.gain.i_fitted.npk,p.s.gain.i_fitted.ctl,p.s.gain.i_coef3, file = 'p.s.gain_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/p.s.gain_dat.Rdata')

p.s.gain.i_fitted.npk<-p.s.gain.i_fitted.npk[complete.cases(p.s.gain.i_fitted.npk$starting.richness), ]
p.s.gain.i_coef3<-p.s.gain.i_coef3[complete.cases(p.s.gain.i_coef3$starting.richness), ]

p.s.gain.i_fitted.npk$starting.richness <- factor(p.s.gain.i_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.s.gain.i_coef3$starting.richness <- factor(p.s.gain.i_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

dt<-distinct(p.s.gain.i_fitted.npk,site_code,block,plot,s.rich,starting.richness)
View(dt)
p.s.gain.i_coef3$xs<-1
#gai
p.s.gain.im<-ggplot() +
  # data
  geom_point(data = p.s.gain.i_fitted.npk,
             aes(x = year.y, y = s.gain,
                 colour = starting.richness, alpha=0.1),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=p.s.gain.i_fitted.npk,
  #             aes(x = year.y, y = p.s.gain,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = p.s.gain.i_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = exp(Intercept + TE + (ISlope+TESlope) * cxmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.s.gain.i_fitted.npk,
              aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.s.gain.i_fitted.npk,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5) +
  geom_ribbon(data = p.s.gain.i_fitted.ctl,
             aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
             alpha = 0.5) +
  # fixed effect
  geom_line(data =  p.s.gain.i_fitted.ctl,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5, linetype= "dashed") +
  scale_y_continuous(trans = 'log' , breaks=c(1,2,4,6,8,16,24)  ) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Plot Species Gain')), title= 'c) Species Gain') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))

p.s.gain.im



grid_arrange_shared_legend(p.s.loss.im,p.s.gain.im,nrow=1)





########################################################################################################################
##################################### SPECIES CHANGE ###################################################################################
########################################################################################################################

summary(p.s.change.i)
color_scheme_set("purple")
pairs(p.s.change.i)

# inspection of chain diagnostic
plot(p.s.change.i)  


pp_check(p.s.change.i)

pp_check(p.s.change.i, type = "hist")
#marginal effects
marginal_effects(p.s.change.i, surface = TRUE)
marginal_smooths(p.s.change.i)



#residuals
cm1<-residuals(p.s.change.i)
cm1<-as.data.frame(cm1)
nrow(cm1)
nrow(p.dat3)
#p.dat4<-p.dat3[complete.cases(p.dat3$CDE), ]
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
p.s.change.i_fitted <- cbind(p.s.change.i$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(p.s.change.i, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(p.s.change.i_fitted)

p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)


p.dat3<-p.datss %>% 
  group_by(continent,habitat,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich))

p.dat4<-p.dat3[p.dat3$year.x %in% c('0'),]
p.dat5<-distinct(p.dat4, site_code, trt.xy, block, plot,s.rich)
p.dat5
p.dat6<-subset(p.dat5, select = - c( year.x,year.y))
View(p.s.gain.i_fitted)
View(p.dat6)

p.s.change.i_fitted2<-inner_join(p.s.change.i_fitted,p.dat2)
p.s.change.i_fitted3<-inner_join(p.s.change.i_fitted2,p.dat6)



p.s.change.i_fitted3$starting.richness <- ifelse(p.s.change.i_fitted3$s.rich >= 1 & p.s.change.i_fitted3$s.rich <= 5, '1-5 species',
                                             ifelse(p.s.change.i_fitted3$s.rich >=6 & p.s.change.i_fitted3$s.rich <=10, '6-10',
                                                    ifelse(p.s.change.i_fitted3$s.rich >=11 & p.s.change.i_fitted3$s.rich <=15, '11-15',    
                                                           ifelse(p.s.change.i_fitted3$s.rich >=16 & p.s.change.i_fitted3$s.rich <=20, '16-20',
                                                                  ifelse(p.s.change.i_fitted3$s.rich >=21 & p.s.change.i_fitted3$s.rich <=25, '21-25',
                                                                         ifelse(p.s.change.i_fitted3$s.rich >=26, '>26', 'other'))))))



p.s.change.i_fitted.npk<-p.s.change.i_fitted3[p.s.change.i_fitted3$trt.y %in% c('NPK'),]
p.s.change.i_fitted.ctl<-p.s.change.i_fitted3[p.s.change.i_fitted3$trt.y %in% c('Control'),]

# fixed effect coefficients (I want these for the coefficient plot)
p.s.change.i_fixef <- fixef(p.s.change.i)

# coefficients for experiment-level (random) effects
p.s.change.i_coef <- coef(p.s.change.i)
p.s.change.i

p.s.change.i_coef2 <-  bind_cols(p.s.change.i_coef$site_code[,,'Intercept'] %>% 
                               as_tibble() %>% 
                               mutate(Intercept = Estimate,
                                      Intercept_lower = Q2.5,
                                      Intercept_upper = Q97.5,
                                      site_code = rownames(p.s.change.i_coef$site_code[,,'Intercept'])) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.s.change.i_coef$site_code[,,'year.y.m'] %>% 
                               as_tibble() %>% 
                               mutate(ISlope = Estimate,
                                      ISlope_lower = Q2.5,
                                      ISlope_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.s.change.i_coef$site_code[,,'trt.yNPK'] %>% 
                               as_tibble() %>% 
                               mutate(TE = Estimate,
                                      TE_lower = Q2.5,
                                      TE_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.s.change.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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


p.s.change.i_coef3<-p.s.change.i_coef2 %>% inner_join(p.dat6 %>%
                                                    group_by(site_code) %>%
                                                    summarise(ss.rich = mean(s.rich),
                                                              r.rich = round(ss.rich)),
                                                  by = 'site_code')


p.s.change.i_coef3$starting.richness <- ifelse(p.s.change.i_coef3$r.rich >= 1 & p.s.change.i_coef3$r.rich <= 5, '1-5 species',
                                           ifelse(p.s.change.i_coef3$r.rich >=6 & p.s.change.i_coef3$r.rich <=10, '6-10',
                                                  ifelse(p.s.change.i_coef3$r.rich >=11 & p.s.change.i_coef3$r.rich <=15, '11-15',    
                                                         ifelse(p.s.change.i_coef3$r.rich >=16 & p.s.change.i_coef3$r.rich <=20, '16-20',
                                                                ifelse(p.s.change.i_coef3$r.rich >=21 & p.s.change.i_coef3$r.rich <=25, '21-25',
                                                                       ifelse(p.s.change.i_coef3$r.rich >=26, '>26', 'other'))))))

View(p.s.change.i_coef2)

View(p.s.change.i_fitted)


p.s.change.i_coef3<-full_join(p.s.change.i_coef3,dat)



rm(p.s.change.i)
save(p.s.change.i_fitted.npk,p.s.change.i_fitted.ctl,p.s.change.i_coef3,file = 'p.s.change_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/p.s.change_dat.Rdata')

library(scales)
sign_sqrt <- scales::trans_new('sign_sqrt',
                               transform = function(x) {sign(x) * sqrt(abs(x))},
                               inverse = function(x){sign(x) * abs(x)^2})


#p.s.change.i_fitted.npk<-p.s.change.i_fitted.npk[complete.cases(p.s.change.i_fitted.npk$starting.richness), ]
#p.s.change.i_coef3<-p.s.change.i_coef3[complete.cases(p.s.change.i_coef3$starting.richness), ]

p.s.change.i_fitted.npk$starting.richness <- factor(p.s.change.i_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.s.change.i_coef3$starting.richness <- factor(p.s.change.i_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


p.s.change.i_coef3$xs<-1

summary(p.s.change.i)
#p.s.change.i
p.s.change.im<-ggplot() +
  # data
  geom_point(data = p.s.change.i_fitted.npk,
             aes(x = year.y, y = s.change,
                 colour = starting.richness, alpha=0.1),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=p.s.change.i_fitted.npk,
  #             aes(x = year.y, y = p.s.gain,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = p.s.change.i_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.s.change.i_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.s.change.i_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = p.s.change.i_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data =  p.s.change.i_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5, linetype= "dashed") +
  scale_y_continuous(trans = sign_sqrt #, breaks=c(8,64,512,1024,2048,4096)
   ) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Richness Change')), title= 'a) Richness Change') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))

p.s.change.im

grid_arrange_shared_legend(p.s.change.im,p.c.rich.im,nrow=1)



########################################################################################################################
##################################### C RICH ###################################################################################
########################################################################################################################



p.c.rich.i_fitted <- cbind(p.c.rich.i$data,
                         # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                         fitted(p.c.rich.i, re_formula = NA)) %>% 
  as_tibble()

p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)


p.dat3<-p.datss %>% 
  group_by(continent,habitat,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich))

p.dat4<-p.dat3[p.dat3$year.x %in% c('0'),]
p.dat5<-distinct(p.dat4, site_code, trt.xy, block, plot,s.rich)
p.dat5
p.dat6<-subset(p.dat5, select = - c( year.x,year.y))
View(p.s.gain.i_fitted)
View(p.dat6)

p.c.rich.i_fitted2<-inner_join(p.c.rich.i_fitted,p.dat2)
p.c.rich.i_fitted3<-inner_join(p.c.rich.i_fitted2,p.dat6)



p.c.rich.i_fitted3$starting.richness <- ifelse(p.c.rich.i_fitted3$s.rich >= 1 & p.c.rich.i_fitted3$s.rich <= 5, '1-5 species',
                                             ifelse(p.c.rich.i_fitted3$s.rich >=6 & p.c.rich.i_fitted3$s.rich <=10, '6-10',
                                                    ifelse(p.c.rich.i_fitted3$s.rich >=11 & p.c.rich.i_fitted3$s.rich <=15, '11-15',    
                                                           ifelse(p.c.rich.i_fitted3$s.rich >=16 & p.c.rich.i_fitted3$s.rich <=20, '16-20',
                                                                  ifelse(p.c.rich.i_fitted3$s.rich >=21 & p.c.rich.i_fitted3$s.rich <=25, '21-25',
                                                                         ifelse(p.c.rich.i_fitted3$s.rich >=26, '>26', 'other'))))))


p.c.rich.i_fitted.npk<-p.c.rich.i_fitted3[p.c.rich.i_fitted3$trt.y %in% c('NPK'),]
p.c.rich.i_fitted.ctl<-p.c.rich.i_fitted3[p.c.rich.i_fitted3$trt.y %in% c('Control'),]



# fixed effect coefficients -coefficient plot
p.c.rich.i_fixef <- fixef(p.c.rich.i)


p.c.rich.i_coef <- coef(p.c.rich.i)
p.c.rich.i_coef

p.c.rich.i_coef2 <-  bind_cols(p.c.rich.i_coef$site_code[,,'Intercept'] %>% 
                               as_tibble() %>% 
                               mutate(Intercept = Estimate,
                                      Intercept_lower = Q2.5,
                                      Intercept_upper = Q97.5,
                                      site_code = rownames(p.c.rich.i_coef$site_code[,,'Intercept'])) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.c.rich.i_coef$site_code[,,'year.y.m'] %>% 
                               as_tibble() %>% 
                               mutate(ISlope = Estimate,
                                      ISlope_lower = Q2.5,
                                      ISlope_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.c.rich.i_coef$site_code[,,'trt.yNPK'] %>% 
                               as_tibble() %>% 
                               mutate(TE = Estimate,
                                      TE_lower = Q2.5,
                                      TE_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             p.c.rich.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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


p.c.rich.i_coef3<-p.c.rich.i_coef2 %>% inner_join(p.dat6 %>%
                                                        group_by(site_code) %>%
                                                        summarise(ss.rich = mean(s.rich),
                                                                  r.rich = round(ss.rich)),
                                                      by = 'site_code')

p.c.rich.i_coef3$starting.richness <- ifelse(p.c.rich.i_coef3$r.rich >= 1 & p.c.rich.i_coef3$r.rich <= 5, '1-5 species',
                                           ifelse(p.c.rich.i_coef3$r.rich >=6 & p.c.rich.i_coef3$r.rich <=10, '6-10',
                                                  ifelse(p.c.rich.i_coef3$r.rich >=11 & p.c.rich.i_coef3$r.rich <=15, '11-15',    
                                                         ifelse(p.c.rich.i_coef3$r.rich >=16 & p.c.rich.i_coef3$r.rich <=20, '16-20',
                                                                ifelse(p.c.rich.i_coef3$r.rich >=21 & p.c.rich.i_coef3$r.rich <=25, '21-25',
                                                                       ifelse(p.c.rich.i_coef3$r.rich >=26, '>26', 'other'))))))

p.c.rich.i_coef3<-inner_join(p.c.rich.i_coef3,dat)
View(p.c.rich.i_coef3)


rm(p.c.rich.i)
save(p.c.rich.i_fitted.npk,p.c.rich.i_fitted.ctl,p.c.rich.i_coef3,file = 'p.c.rich_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/p.c.rich_dat.Rdata')



View(p.c.rich.i_fitted.npk)

p.c.rich.i_fitted.npk<-p.c.rich.i_fitted.npk[complete.cases(p.c.rich.i_fitted.npk$starting.richness), ]
p.c.rich.i_coef3<-p.c.rich.i_coef3[complete.cases(p.c.rich.i_coef3$starting.richness), ]

p.c.rich.i_fitted.npk$starting.richness <- factor(p.c.rich.i_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.c.rich.i_coef3$starting.richness <- factor(p.c.rich.i_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
is.factor(p.c.rich.i_coef3$starting.richness)



View(p.c.rich.i_fitted3)
p.c.rich.i_coef3$xs<-1
p.c.rich.im<-ggplot() +
  # data
  geom_point(data = p.c.rich.i_fitted.npk,
             aes(x = year.y, y = c.rich,
                 colour = starting.richness, alpha=0.01),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=p.c.rich.i_fitted.npk,
  #             aes(x = year.y, y = p.c.rich.p,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = p.c.rich.i_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = exp(Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.c.rich.i_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.c.rich.i_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = p.c.rich.i_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data =  p.c.rich.i_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5, linetype= "dashed") +
  scale_y_continuous(trans = 'log', breaks=c(1,2,4,6,8,16,24,36) ) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Persistent Richness')), title= 'Persistent Species') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))

p.c.rich.im



 ########################################################################################################################
##################################### C FUNC ###################################################################################
########################################################################################################################

summary(p.c.func.i)
color_scheme_set("purple")
pairs(p.c.func.i)

# inspection of chain diagnostic
plot(p.c.func.i)  


pp_check(p.c.func.i)

pp_check(p.c.func.i, type = "hist")
#marginal effects
marginal_effects(p.c.func.i, surface = TRUE)
marginal_smooths(p.c.func.i)



#residuals
cm1<-residuals(p.c.func.i)
cm1<-as.data.frame(cm1)
nrow(cm1)
nrow(p.dat3)
#p.dat4<-p.dat3[complete.cases(p.dat3$CDE), ]
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
p.c.func.i_fitted <- cbind(p.c.func.i$data,
                           # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                           fitted(p.c.func.i, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(p.c.func.i_fitted)

p.dat.s<-p.dat2 %>% 
  group_by(site_code,block,plot) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))
p.datss<-inner_join(p.dat2,p.dat.s)


p.dat3<-p.datss %>% 
  group_by(continent,habitat,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(r.rich))

p.dat4<-p.dat3[p.dat3$year.x %in% c('0'),]
p.dat5<-distinct(p.dat4, site_code, trt.xy, block, plot,s.rich)
p.dat5
p.dat6<-subset(p.dat5, select = - c( year.x,year.y))
View(p.s.gain.i_fitted)
View(p.dat6)

p.c.func.i_fitted2<-inner_join(p.c.func.i_fitted,p.dat2)
p.c.func.i_fitted3<-inner_join(p.c.func.i_fitted2,p.dat6)



p.c.func.i_fitted3$starting.richness <- ifelse(p.c.func.i_fitted3$s.rich >= 1 & p.c.func.i_fitted3$s.rich <= 5, '1-5 species',
                                               ifelse(p.c.func.i_fitted3$s.rich >=6 & p.c.func.i_fitted3$s.rich <=10, '6-10',
                                                      ifelse(p.c.func.i_fitted3$s.rich >=11 & p.c.func.i_fitted3$s.rich <=15, '11-15',    
                                                             ifelse(p.c.func.i_fitted3$s.rich >=16 & p.c.func.i_fitted3$s.rich <=20, '16-20',
                                                                    ifelse(p.c.func.i_fitted3$s.rich >=21 & p.c.func.i_fitted3$s.rich <=25, '21-25',
                                                                           ifelse(p.c.func.i_fitted3$s.rich >=26, '>26', 'other'))))))



p.c.func.i_fitted.npk<-p.c.func.i_fitted3[p.c.func.i_fitted3$trt.y %in% c('NPK'),]
p.c.func.i_fitted.ctl<-p.c.func.i_fitted3[p.c.func.i_fitted3$trt.y %in% c('Control'),]


# fixed effect coefficients (I want these for the coefficient plot)
p.c.func.i_fixef <- fixef(p.c.func.i)

# coefficients for experiment-level (random) effects
p.c.func.i_coef <- coef(p.c.func.i)
p.c.func.i

p.c.func.i_coef2 <-  bind_cols(p.c.func.i_coef$site_code[,,'Intercept'] %>% 
                                 as_tibble() %>% 
                                 mutate(Intercept = Estimate,
                                        Intercept_lower = Q2.5,
                                        Intercept_upper = Q97.5,
                                        site_code = rownames(p.c.func.i_coef$site_code[,,'Intercept'])) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                               p.c.func.i_coef$site_code[,,'year.y.m'] %>% 
                                 as_tibble() %>% 
                                 mutate(ISlope = Estimate,
                                        ISlope_lower = Q2.5,
                                        ISlope_upper = Q97.5) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                               p.c.func.i_coef$site_code[,,'trt.yNPK'] %>% 
                                 as_tibble() %>% 
                                 mutate(TE = Estimate,
                                        TE_lower = Q2.5,
                                        TE_upper = Q97.5) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                               p.c.func.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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


p.c.func.i_coef3<-p.c.func.i_coef2 %>% inner_join(p.dat6 %>%
                                                    group_by(site_code) %>%
                                                    summarise(ss.rich = mean(s.rich),
                                                              r.rich = round(ss.rich)),
                                                  by = 'site_code')


p.c.func.i_coef3$starting.richness <- ifelse(p.c.func.i_coef3$r.rich >= 1 & p.c.func.i_coef3$r.rich <= 5, '1-5 species',
                                             ifelse(p.c.func.i_coef3$r.rich >=6 & p.c.func.i_coef3$r.rich <=10, '6-10',
                                                    ifelse(p.c.func.i_coef3$r.rich >=11 & p.c.func.i_coef3$r.rich <=15, '11-15',    
                                                           ifelse(p.c.func.i_coef3$r.rich >=16 & p.c.func.i_coef3$r.rich <=20, '16-20',
                                                                  ifelse(p.c.func.i_coef3$r.rich >=21 & p.c.func.i_coef3$r.rich <=25, '21-25',
                                                                         ifelse(p.c.func.i_coef3$r.rich >=26, '>26', 'other'))))))

View(p.c.func.i_coef2)

View(p.c.func.i_fitted)


p.c.func.i_coef3<-full_join(p.c.func.i_coef3,dat)



rm(p.c.func.i)
save(p.c.func.i_fitted.npk,p.c.func.i_fitted.ctl,p.c.func.i_coef3,file = 'p.c.func_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/p.c.func_dat.Rdata')

library(scales)
sign_sqrt <- scales::trans_new('sign_sqrt',
                               transform = function(x) {sign(x) * sqrt(abs(x))},
                               inverse = function(x){sign(x) * abs(x)^2})


#p.c.func.i_fitted.npk<-p.c.func.i_fitted.npk[complete.cases(p.c.func.i_fitted.npk$starting.richness), ]
#p.c.func.i_coef3<-p.c.func.i_coef3[complete.cases(p.c.func.i_coef3$starting.richness), ]

p.c.func.i_fitted.npk$starting.richness <- factor(p.c.func.i_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
p.c.func.i_coef3$starting.richness <- factor(p.c.func.i_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


p.c.func.i_coef3$xs<-1

summary(p.c.func.i)
#p.c.func.i
p.c.func.m<-ggplot() +
  # data
  geom_point(data = p.c.func.i_fitted.npk,
             aes(x = year.y, y = c.func,
                 colour = starting.richness, alpha=0.1),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=p.c.func_fitted.npk,
  #             aes(x = year.y, y = p.c.func,
  #                 colour = starting.richness), height=0.25,width = 0.25)+
  #experiment (random) effects
  geom_segment(data = p.c.func.i_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope)  * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = p.c.func.i_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.c.func.i_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = p.c.func.i_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = p.c.func.i_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  scale_y_continuous(trans = sign_sqrt #, breaks=c(8,64,512,1024,2048,4096)
  ) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'b) Biomass Change') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")

p.c.func.m


grid_arrange_shared_legend(p.s.change.im,p.c.func.m,nrow=1)




#coefs


p.s.loss.i_fixef2<-as.data.frame(p.s.loss.i_fixef)
p.s.gain.i_fixef2<-as.data.frame(p.s.gain.i_fixef)
p.s.change.i_fixef2<-as.data.frame(p.s.change.i_fixef)
View(p.s.loss.i_fixef2)

p.s.loss.i_coef4<-p.s.loss.i_coef3[complete.cases(p.s.loss.i_coef3), ]
p.s.gain.i_coef4<-p.s.gain.i_coef3[complete.cases(p.s.gain.i_coef3), ]
p.s.change.i_coef4<-p.s.change.i_coef3[complete.cases(p.s.change.i_coef3), ]

View(p.s.loss.i_coef4)

p.s.loss.i_fixef2$Estimate.n<-(p.s.loss.i_fixef2$Estimate * -1)
p.s.loss.i_fixef2$Est.Error.n<-(p.s.loss.i_fixef2$Est.Error * -1)
p.s.loss.i_fixef2$Q2.5.n<-(p.s.loss.i_fixef2$Q2.5 * -1)
p.s.loss.i_fixef2$Q97.5.n<-(p.s.loss.i_fixef2$Q97.5 * -1)

View(p.s.loss.i_coef4)
View(p.s.loss.i_fixef2)


p.s.loss.im2<-ggplot() + 
  geom_point(data = p.s.loss.i_coef4, aes(x =reorder(site_code,TESlope), y = TESlope, colour = continent),size = 2) +
  geom_errorbar(data = p.s.loss.i_coef4, aes(x = reorder(site_code,TESlope),ymin = TESlope_lower,
                                         ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  #facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = p.s.loss.i_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = p.s.loss.i_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  #ylim(-0.45,1.2) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'a) Species Loss') +
  scale_y_continuous(trans = reverse_trans()) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


p.s.loss.im2

p.s.loss.i_coef4$Slope.sl<-p.s.loss.i_coef4$TESlope
p.s.loss.i.Slope<-select(p.s.loss.i_coef4,site_code,Slope.sl)
p.s.gain.i_coef5<-inner_join(p.s.gain.i_coef4,p.s.loss.i.Slope)
is.numeric(p.s.gain.i_coef5$Slope.sl)


View(p.s.gain.i_coef3)
p.s.gain.im2<-ggplot() + 
  geom_point(data = p.s.gain.i_coef5, aes(x = reorder(site_code,Slope.sl), y = TESlope,colour = continent),size = 2) +
  geom_errorbar(data = p.s.gain.i_coef5, aes(x = reorder(site_code,Slope.sl),ymin = TESlope_lower,
                                         ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = p.s.gain.i_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = p.s.gain.i_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  #ylim(-0.45,1.2) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'b) Species Gains') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")
p.s.gain.im2


#p.s.loss.i_coef4$Slope.sl<-p.s.loss.i_coef4$Slope
#p.s.loss.i.Slope<-select(p.s.loss.i_coef4,site_code,Slope.sl)
p.s.change.i_coef5<-inner_join(p.s.change.i_coef4,p.s.loss.i.Slope)
is.numeric(p.s.gain.i_coef5$Slope.sl)

p.s.change.i_fixef2
p.s.change.im2<-ggplot() + 
  geom_point(data = p.s.change.i_coef5, aes(x = reorder(site_code,Slope.sl), y = TESlope,colour = continent),size = 2) +
  geom_errorbar(data = p.s.change.i_coef5, aes(x = reorder(site_code,Slope.sl),ymin = TESlope_lower,
                                      ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = p.s.change.i_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = p.s.change.i_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  #ylim(-100,100) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'c) Species Change') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")



grid_arrange_shared_legend(p.s.loss.im,p.s.gain.im,p.s.change.im,ncol=3,nrow=1)

grid_arrange_shared_legend(p.s.loss.im2,p.s.gain.im2,p.s.change.im2,ncol=3,nrow=1)


grid_arrange_shared_legend(p.s.loss.im,p.s.gain.im,p.s.loss.im2,p.s.gain.im2,ncol=2,nrow=2)


grid_arrange_shared_legend(p.s.loss.im,p.s.gain.im,p.s.change.im,p.s.loss.im2,p.s.gain.im2,p.s.change.im2,ncol=3,nrow=2)


grid_arrange_shared_legend(p.s.loss.im,p.s.gain.im,p.s.change.im,p.s.loss.im2,p.s.gain.im2,p.s.change.im2,ncol=3,nrow=2)



grid_arrange_shared_legend(p.s.loss.im,sl.trtm,p.s.gain.im,sg.trtm,p.s.loss.im2,sl.trtm2,p.s.gain.im2,sg.trtm2,ncol=4,nrow=2)



