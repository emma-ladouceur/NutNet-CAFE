

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(bayesplot)
library(priceTools)

sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(p.all)
#price stuff

p.all2<-p.all[p.all$trt.x %in% c('NPK'),]
p.all3<-p.all2[p.all2$trt.y %in% c('NPK'),]
levels(p.all3$trt_year)
View(p.all3)
p.all4<-separate(p.all3,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all4$f.year.y<-as.factor(p.all4$year.y)
p.all4$plot<-as.factor(p.all4$plot)
p.all4$site_code<-as.factor(p.all4$site_code)
nrow(p.all4)
#price vectors
p.all.c<-p.all4[complete.cases(p.all4), ]

# REMOVE ALL 0'S
#START PLOTS FROM YEAR 1 
#BECAUSE YEAR 1 IS A COMPARISON TO 0 YOU FOOL
p.all4<-p.all4 %>% filter(year.y != "0" )

p.all5<-p.all4[p.all4$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]

View(p.all4)


ctl<-p.all[p.all$trt_year %in% c('NPK_0 Control_0'),]
ctl2<-ctl[ctl$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]
View(ctl2)
ctl3<-separate(ctl2,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all4<-bind_rows(p.all5,ctl3)

colnames(p.all4)
head(p.all4)
summary(p.all4)
View(p.all4)

#write.csv(p.all4,"~/Desktop/pricel.csv")

summary(p.all4)

p.all4$SL.p<-abs(p.all4$SL)
p.all4$SL.p.log1<-log1p(p.all4$SL.p)
p.all4$SG.log1<-log1p(p.all4$SG)

p.all4$CDE.log<-log(p.all4$CDE)
p.all4$SL.p.log<-log(p.all4$SL.p)
p.all4$SG.log<-log(p.all4$SG)

p.all4$year.y.m<-p.all4$year.y-mean(p.all4$year.y)

#try shanes funky custom transformation for cde
p.all4$CDE.t<- sign(p.all4$CDE)*sqrt(abs(p.all4$CDE))
summary(p.all4)

View(p.all4)
par(mfrow=c(1,1))
hist(p.all4$CDE.t, breaks=40, main="CDE Funky Transformed", xlab= "CDE FT")


#histograms of sl & sg
par(mfrow=c(2,3))
hist(p.all4$SL,breaks =40, main="Species Loss", xlab= "Species Loss")
hist(p.all4$SG, breaks=40, main="Species Gains", xlab= "Species Gains")
hist(p.all4$CDE, breaks=40, main="CDE", xlab= "CDE")

hist(p.all4$SL.p.log1,breaks=40, main="Log Species Loss +1", xlab= "Log Species Loss +1")
hist(p.all4$SG.log1, breaks=40, main="Log Species Gains +1", xlab= "Log Species Gains +1")
hist(p.all4$CDE.log, breaks=40, main="CDE log", xlab= "CDE log")
hist(p.all4$CDE..t, breaks=40, main="CDE t", xlab= "CDE t")


# s loss, gain and change metrics
p.all4$s.loss <- -1*(p.all4$x.rich - p.all4$c.rich)
p.all4$s.gain <- p.all4$y.rich - p.all4$c.rich
p.all4$s.change <- p.all4$y.rich - p.all4$x.rich


p.all4$s.loss.log <- sign(p.all4$s.loss)*log1p(abs(p.all4$s.loss))
p.all4$s.gain.log<-log1p(p.all4$s.gain)

par(mfrow=c(2,2))
hist(p.all4$s.loss,breaks =10, main="Species Loss", xlab= "Species Loss")
hist(p.all4$s.gain, breaks=10, main="Species Gains", xlab= "Species Gains")
hist(p.all4$s.loss.log,breaks =10, main="Log Species Loss", xlab= "Log Species Loss")
hist(p.all4$s.gain.log, breaks=10, main="Log Species Gains", xlab= "Log Species Gains")

summary(p.all4)


head(p.all3)

is.numeric(p.all3$year.y)


distinct(p.all4,site_code,site.year.id,block,plot)


#multi_sg.sl <- brm(cbind(SL.log, SG.log) ~  year.y + (year.y | p | site.year.id), 
 #                  data = p.all4, cores = 4, chains = 4)

is.numeric(p.all4$year.y)
is.numeric(p.all4$SL.p)
is.numeric(p.all4$site_code)
is.numeric(p.all4$site.year.id)

sl <- brm(SL.p ~  year.y + (year.y | site_code/site.year.id), 
                           data = p.all4,family=hurdle_lognormal(), cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))

#now with only specific appropriate block comparisons
sl.c <- brm(SL.p ~  year.y.m + (year.y.m |  site_code/site.year.id), 
          data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))

#sl.c.i <- brm(SL.p ~  year.y.m * s.loss  + (year.y.m |  site_code/site.year.id), 
 #           data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))


#year.y.m

# * species loss?
summary(p.all4)
View(p.all4)
# Petr's explorations:
sl.preds <- predict(sl, type = "response")
summary(sl.preds)
hist(sl.preds[,'Estimate'])
plot(predict(sl, type="response"), p.all4$SL.p)
table(p.all4$site_code)
p.all4[p.all4$site_code == "marc.ar",]


sg <- brm(SG ~  year.y + (year.y |  site_code/site.year.id), 
          data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))
#centered years
sg.c <- brm(SG ~  year.y.m + (year.y.m |  site_code/site.year.id), 
            data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))

#interaction with species gain
#sg.c.i <- brm(SG ~  year.y.m * s.gain  + (year.y.m |  site_code/site.year.id), 
 #             data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))


#, control = list(adapt_delta = 0.999, max_treedepth = 15))
# iter=3000,

# assymetric laplace distribution
CDE.m <- brm(CDE ~   year.y + (year.y | site_code/site.year.id), 
             data = p.all4, family=asym_laplace(),cores = 4, chains = 4)

CDE.c <- brm(CDE ~  year.y.m + (year.y.m |  site_code/site.year.id), 
            data = p.all4, family=asym_laplace(),cores = 4, chains = 4)

#what about this one...... maybe an interaction with plot level biomass change?
#not sure theres an equivelent for this
#CDE.c.i <- brm(CDE ~  year.y.m * s.loss  + (year.y.m |  site_code/site.year.id), 
 #             data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))


#data with a funky transformation beforehand
#CDE.t.m <- brm(CDE.t ~   year.y + (year.y | site_code/site.year.id), 
 #            data = p.all4, cores = 4, chains = 4)


setwd('~/Dropbox/Projects/NutNet/Model_fits/')
#save(multi_sg.sl,CDE.m,file = 'price_models_time.Rdata')
#save(CDE.m,file = 'price_cde_laplace.Rdata')
save(sl.c,sg.c,CDE.c,file = 'price_models_new_time.Rdata')
#load('~/Dropbox/Projects/NutNet/Model_fits/price_models_time.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price_cde_laplace.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price_models_sg_time.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price_models_new_time.Rdata')


summary(CDE.c)

summary(sg.c)

summary(sl.c)

plot(sl.c)
pp_check(sl.c)+ scale_x_continuous(trans="log")

plot(sg.c)
pp_check(sg.c)+ scale_x_continuous(trans="log")

plot(CDE.c)
pp_check(CDE.c)


color_scheme_set("purple")
pp_check(sl.c)
pp_check(sl.c, type = "hist")
#marginal effects
marginal_effects(sl.c, surface = TRUE)
marginal_smooths(sl.c)

levels(p.all4$f.year.y)
#residuals
sl.cm1<-residuals(sl.c)
sl.cm1<-as.data.frame(sl.cm1)
View(sl.cm1)
nrow(sl.cm1)
nrow(p.all5)
p.all5<-p.all4[complete.cases(p.all4$SL.p), ]
sl.c.plot<-cbind(p.all5,sl.cm1$Estimate)
sl.c.plot2<-inner_join(sl.c.plot,dat)



par(mfrow=c(3,2))
with(sl.c.plot, plot(continent, sl.cm1$Estimate))
with(sl.c.plot, plot(habitat, sl.cm1$Estimate))
with(sl.c.plot, plot(site_code, sl.cm1$Estimate))
with(sl.c.plot, plot(block, sl.cm1$Estimate))
with(sl.c.plot, plot(plot, sl.cm1$Estimate))
with(sl.c.plot, plot(year.y, sl.cm1$Estimate))


par(mfrow=c(3,2))
with(sl.c.plot2, plot(continent, sl.cm1$Estimate))
with(sl.c.plot2, plot(habitat, sl.cm1$Estimate))
with(sl.c.plot, plot(site_code, sl.cm1$Estimate))
with(sl.c.plot2, plot(block, sl.cm1$Estimate))
with(sl.c.plot2, plot(plot, sl.cm1$Estimate))
with(sl.c.plot2, plot(f.year.y, sl.cm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
sl.c_fitted <- cbind(sl.c$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(sl.c, re_formula = NA)) %>% 
  as_tibble()

View(sl.c_fitted)
p.all5<-distinct(p.all4,site_code, year.y, SL,SL.p)
sl.c_fitted2<-inner_join(sl.c_fitted,p.all5)
View(sl.c_fitted2)



View(sl.c_fitted)
# fixed effect coefficients (I want these for the coefficient plot)
sl.c_fixef <- fixef(sl.c)

# coefficients for experiment-level (random) effects
sl.c_coef <- coef(sl.c)

sl.c_coef2 <-  bind_cols(sl.c_coef$site_code[,,'Intercept'] %>% 
                          as_tibble() %>% 
                          mutate(Intercept = Estimate,
                                 Intercept_lower = Q2.5,
                                 Intercept_upper = Q97.5,
                                 site_code = rownames(sl.c_coef$site_code[,,'Intercept'])) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        sl.c_coef$site_code[,,'year.y.m'] %>% 
                          as_tibble() %>% 
                          mutate(Slope = Estimate,
                                 Slope_lower = Q2.5,
                                 Slope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all4 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code')


View(sl.c_coef2)

View(sl.c_fitted)

dat<-distinct(plot, site_code, continent,habitat)
nrow(dat)

nrow(sl.c_coef2)


sl.c_coef3<-inner_join(sl.c_coef2,dat)
View(sl.c_coef3)
#convert to negative
#sl.c_coef3$Intercept<-log(sl.c_coef3$Intercept)
#sl.c_coef3$Intercept_upper<-log(sl.c_coef3$Intercept_upper)
#sl.c_coef3$Intercept_lower<-log(sl.c_coef3$Intercept_lower)
#sl.c_coef3$sl.cope<-log(sl.c_coef3$sl.cope)
#sl.c_coef3$sl.cope_upper<-log(sl.c_coef3$sl.cope_upper)
#sl.c_coef3$sl.cope_lower<-log(sl.c_coef3$sl.cope_lower)


sl.c_coef3$Intercept.n<-(sl.c_coef3$Intercept * -1)
sl.c_coef3$Intercept_upper.n<-(sl.c_coef3$Intercept_upper * -1)
sl.c_coef3$Intercept_lower.n<-(sl.c_coef3$Intercept_lower * -1)
sl.c_coef3$sl.cope.n<-(sl.c_coef3$sl.cope * -1)
sl.c_coef3$sl.cope_upper.n<-(sl.c_coef3$sl.cope_upper * -1)
sl.c_coef3$sl.cope_lower.n<-(sl.c_coef3$sl.cope_lower * -1)

sl.c_coef3$Intercept.log<-log(sl.c_coef3$Intercept)
sl.c_coef3$sl.cope.log<-log(sl.c_coef3$sl.cope)
sl.c_coef3$Intercept.log.n<-(sl.c_coef3$Intercept.log * -1)
sl.c_coef3$sl.cope.log.n<-(sl.c_coef3$sl.cope.log * -1)

View(sl.c_coef3)

dat2<-distinct(p.all4, site_code,year.y, year.y.m)

sl.c_fitted2<-inner_join(sl.c_fitted,dat)
sl.c_fitted3<-inner_join(sl.c_fitted2,dat2)

View(sl.c_fitted3)

#neg
sl.c_fitted2$sl.c.n<-(sl.c_fitted2$sl.c.p * -1)
sl.c_fitted2$Estimate.n<-(sl.c_fitted2$Estimate * -1)
sl.c_fitted2$Est.Error.n<-(sl.c_fitted2$Est.Error * -1)
sl.c_fitted2$Q2.5.n<-(sl.c_fitted2$Q2.5 * -1)
sl.c_fitted2$Q97.5.n<-(sl.c_fitted2$Q97.5 * -1)


View(p.all4)
sl.c_fitted2$year.y.f<-as.factor(sl.c_fitted2$year.y)
levels(sl.c_fitted2$year.y.f)


View(sl.c_fitted2)
View(p.all4)
dat<-distinct(plot,site_code,continent)
nrow(sl.cdat)
nrow(sl.c_fitted2)

#sl.c_fitted2<-inner_join(sl.c_fitted,dat)

View(sl.c_fitted3)
View(sl.c_coef3)

summary(sl_fitted)
summary(sl.c_fitted3)
summary(sl.c_coef3)


View(sl.c_fitted3)
sl.cm<-ggplot() +
  # data
  geom_point(data = sl.c_fitted3,
             aes(x = year.y, y = SL.p,
                 colour = continent, alpha=0.02),
             size = 1.2) +
  geom_jitter(data=sl.c_fitted3,
              aes(x = year.y, y = SL.p,
                  colour = continent), height=0.25,width = 0.25)+
  # experiment (random) effects
  geom_segment(data = sl.c_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + Slope * cxmin),
                   yend = exp(Intercept + Slope * cxmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sl.c_fitted3,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = sl.c_fitted3,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  scale_y_continuous(trans = 'log', breaks=c(8,64,512,1024,2048,4096)) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'a) Change in EF due to SL') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

sl.cm




# #########################################GAINS ####################################################


color_scheme_set("purple")
pp_check(sg.c)
pp_check(sg.c, type = "hist")
#marginal effects
marginal_effects(sg.c, surface = TRUE)
marginal_smooths(sg.c)


summary(sg.c)
#residuals
sg.cm1<-residuals(sg.c)
sg.cm1<-as.data.frame(sg.cm1)
nrow(sg.cm1)
nrow(p.all4)
p.all5<-p.all4[complete.cases(p.all4$SG), ]
sg.c.plot<-cbind(p.all5,sg.cm1$Estimate)
sg.c.plot2<-inner_join(sg.c.plot,dat)

par(mfrow=c(3,2))
with(sg.c.plot2, plot(continent, sg.cm1$Estimate))
with(sg.c.plot2, plot(habitat, sg.cm1$Estimate))
with(sg.c.plot, plot(site_code, sg.cm1$Estimate))
with(sg.c.plot2, plot(block, sg.cm1$Estimate))
with(sg.c.plot2, plot(plot, sg.cm1$Estimate))
with(sg.c.plot2, plot(f.year.y, sg.cm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
sg.c_fitted <- cbind(sg.c$data,
                   # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                   fitted(sg.c, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(sg.c_fitted)
View(sg.c_fitted)
p.all5<-distinct(p.all4,site_code, year.y, year.y.m, SG)
sg.c_fitted2<-inner_join(sg.c_fitted,dat)
#sg.c_fitted2$sg.c.log<-log(sg.c_fitted2$sg.c)

View(p.all4)
View(sg.c_fitted2)


# fixed effect coefficients (I want these for the coefficient plot)
sg.c_fixef <- fixef(sg.c)

# coefficients for experiment-level (random) effects
sg.c_coef <- coef(sg.c)
sg.c_coef 
sg.c_coef2 <-  bind_cols(sg.c_coef$site_code[,,'Intercept'] %>% 
                         as_tibble() %>% 
                         mutate(Intercept = Estimate,
                                Intercept_lower = Q2.5,
                                Intercept_upper = Q97.5,
                                site_code = rownames(sg.c_coef$site_code[,,'Intercept'])) %>% 
                         select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                       sg.c_coef$site_code[,,'year.y.m'] %>% 
                         as_tibble() %>% 
                         mutate(Slope = Estimate,
                                Slope_lower = Q2.5,
                                Slope_upper = Q97.5) %>% 
                         select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all4 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code')


View(sg.c_coef3)

View(sg.c_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

sg.c_coef3<-full_join(sg.c_coef2,dat)
#dat2<-distinct(p.all4, site.year.id,sg.c)
sg.c_fitted3<-inner_join(sg.c_fitted2,dat2)

View(sg.c_coef3)
sg.c_coef4<-sg.c_coef3[complete.cases(sg.c_coef3), ]
nrow(sg.c_coef4)
View(sg.c_coef4)


summary(sg.c_fitted3)
summary(sg.c_coef3)

#gai
sg.cm<-ggplot() +
  # data
  geom_point(data = sg.c_fitted3,
             aes(x = year.y, y = SG,
                 colour = continent, alpha=0.2),
             size = 1.2) +
  geom_jitter(data=sg.c_fitted3,
              aes(x = year.y, y = SG,
                  colour = continent), height=0.25,width = 0.25)+
  # experiment (random) effects
  geom_segment(data = sg.c_coef4,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + Slope * cxmin),
                   yend = exp(Intercept + Slope * cxmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sg.c_fitted3,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = sg.c_fitted3,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  scale_y_continuous(trans = 'log', breaks=c(8,64,512,1024,2048,4096)) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'b) Change in EF due to SG') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

sg.cm



########################################################################################################################
#####################################CDE model###################################################################################
########################################################################################################################

summary(CDE.c)

pairs(CDE.c)

# inspection of chain diagnostic
plot(CDE.c)  

color_scheme_set("purple")
pp_check(CDE.c)

pp_check(CDE.c, type = "hist")
#marginal effects
marginal_effects(CDE.c, surface = TRUE)
marginal_smooths(CDE.c)



#residuals
cm1<-residuals(CDE.c)
cm1<-as.data.frame(cm1)
nrow(cm1)
nrow(p.all4)
p.all5<-p.all4[complete.cases(p.all4$CDE), ]
cde.plot<-cbind(p.all5,cm1$Estimate)
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
cde_fitted <- cbind(CDE.c$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(CDE.c, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(cde_fitted)

#p.all5<-distinct(p.all4,site_code, year.y, continent)
#cde_fitted2<-inner_join(cde_fitted,p.all5,  by = c('site_code', 'year.y'))


View(cde_fitted)
# fixed effect coefficients (I want these for the coefficient plot)
cde_fixef <- fixef(CDE.c)

# coefficients for experiment-level (random) effects
cde_coef <- coef(CDE.c)

cde_coef2 <-  bind_cols(cde_coef$site_code[,,'Intercept'] %>% 
                          as_tibble() %>% 
                          mutate(Intercept = Estimate,
                                 Intercept_lower = Q2.5,
                                 Intercept_upper = Q97.5,
                                 site_code = rownames(cde_coef$site_code[,,'Intercept'])) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'year.y.m'] %>% 
                          as_tibble() %>% 
                          mutate(Slope = Estimate,
                                 Slope_lower = Q2.5,
                                 Slope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all4 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m)),
             by = 'site_code')


View(cde_coef2)

View(cde_fitted)


cde_coef3<-full_join(cde_coef2,dat)
cde_fitted2<-full_join(cde_fitted,dat)
cde_fitted3<-full_join(cde_fitted2,dat2)

library(scales)
sign_sqrt <- scales::trans_new('sign_sqrt',
                               transform = function(x) {sign(x) * sqrt(abs(x))},
                               inverse = function(x){sign(x) * abs(x)^2})


View(cde_fitted3)
#cde
cdem<-ggplot() +
  # data
  geom_point(data = cde_fitted3,
             aes(x = year.y, y = CDE,
                 colour = continent, alpha=0.2),
             size = 1.2) +
  geom_jitter(data=cde_fitted3,
             aes(x = year.y, y = CDE,
                 colour = continent), height=0.25,width = 0.25)+
  #experiment (random) effects
  geom_segment(data = cde_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * cxmin,
                   yend = Intercept + Slope * cxmax,
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = cde_fitted3,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = cde_fitted3,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  scale_y_continuous(trans = sign_sqrt #, breaks=c(8,64,512,1024,2048,4096)
                   ) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'c) EF Change in Persistent Species') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

cdem


grid_arrange_shared_legend(sl.cm,sg.cm,cdem,nrow=1)



library(tidybayes)
library(modelr)

get_variables(CDE.m)

??data_grid


p.all4 %>%
  data_grid(site_code) %>%
  add_fitted_draws(CDE.m) %>%
  head(10)


p.all4 %>%
  data_grid(site_code) %>%
  add_fitted_draws(CDE.c) %>%
  do(tibble(.value = quantile(.$.value, ppoints(100)))) %>%
  ggplot(aes(x = .value)) +
  geom_dotplot(binwidth = .04) +
  facet_grid(fct_rev(continent) ~ .) +
  scale_y_continuous(breaks = NULL)


library(sjstats)
library(sjmisc)
library(mediation)
library(sjPlot)
library(sjlabelled)

equi_test(CDE.c)
equi_test(m5, out = "plot")


#coefs


sl.c_fixef2<-as.data.frame(sl.c_fixef)
sg.c_fixef2<-as.data.frame(sg.c_fixef)
cde_fixef2<-as.data.frame(cde_fixef)
View(sl.c_coef2)

sl.c_coef4<-sl.c_coef3[complete.cases(sl.c_coef3), ]
sg.c_coef4<-sg.c_coef3[complete.cases(sg.c_coef3), ]
cde_coef4<-cde_coef3[complete.cases(cde_coef3), ]

View(sl.c_coef4)
View(sl.c_fixef2)
sl.c_fixef2$Estimate.n<-(sl.c_fixef2$Estimate * -1)
sl.c_fixef2$Est.Error.n<-(sl.c_fixef2$Est.Error * -1)
sl.c_fixef2$Q2.5.n<-(sl.c_fixef2$Q2.5 * -1)
sl.c_fixef2$Q97.5.n<-(sl.c_fixef2$Q97.5 * -1)



sl.cm2<-ggplot() + 
  geom_point(data = sl.c_coef4, aes(x =reorder(site_code,Slope), y = Slope, colour = continent),size = 2) +
  geom_errorbar(data = sl.c_coef4, aes(x = reorder(site_code,Slope),ymin = Slope_lower,
                                            ymax = Slope_upper,colour = continent),
                width = 0, size = 0.7) + 
  #facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = sl.c_fixef2,
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = sl.c_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  ylim(-0.45,1.2) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'a) EF: SL') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


sl.cm2

sl.c_coef4$Slope.sl<-sl.c_coef4$Slope
sl.c.Slope<-select(sl.c_coef4,site_code,Slope.sl)
sg.c_coef5<-inner_join(sg.c_coef4,sl.c.Slope)
is.numeric(sg.c_coef5$Slope.sl)


View(sg.c_coef3)
sg.cm2<-ggplot() + 
  geom_point(data = sg.c_coef5, aes(x = reorder(site_code,Slope.sl), y = Slope,colour = continent),size = 2) +
  geom_errorbar(data = sg.c_coef5, aes(x = reorder(site_code,Slope.sl),ymin = Slope_lower,
                                            ymax = Slope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = sg.c_fixef2,
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = sg.c_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  ylim(-0.45,1.2) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'b) EF : SG') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")
sg.cm2


#sl.c_coef4$Slope.sl<-sl.c_coef4$Slope
#sl.c.Slope<-select(sl.c_coef4,site_code,Slope.sl)
cde_coef5<-inner_join(cde_coef4,sl.c.Slope)
is.numeric(sg.c_coef5$Slope.sl)


cdem2<-ggplot() + 
  geom_point(data = cde_coef5, aes(x = reorder(site_code,Slope.sl), y = Slope,colour = continent),size = 2) +
  geom_errorbar(data = cde_coef5, aes(x = reorder(site_code,Slope.sl),ymin = Slope_lower,
                                     ymax = Slope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = cde_fixef2,
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = cde_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  ylim(-150,175) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'c) EF: Persistent Species') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


grid_arrange_shared_legend(sl.cm2,sg.cm2,cdem2,nrow=1)




View(sl.c_coef3)
View(sg.c_coef3)
colnames(sl.c_coef3)
colnames(sg.c_coef3)
sl.c_coef4<-sl.c_coef3[,c(-1,-2,-3)]
sg.c_coef4<-sg.c_coef3[,c(-1,-2,-3)]
colnames(sl.c_coef4)
colnames(sg.c_coef4)
sl.c_coef5<-sl.c_coef4[,c(-9,-10,-11,-12,-13,-14,-15,-16,-17,-18)]
names(sl.c_coef4) <- c("site_code","sl.c.Slope","sl.c.Slope_lower","sl.c.Slope_upper","xmin","xmax","cxmin","cxmax","continent","habitat")
names(sg.c_coef4) <- c("site_code","sg.c.Slope","sg.c.Slope_lower","sg.c.Slope_upper","xmin","xmax","cxmin","cxmax","continent","habitat")
delta.coefs<-bind_cols(sl.c_coef4,sg.c_coef4)
View(delta.coefs)

#By Experiment
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(data=delta.coefs, aes(x=sl.c.sl.cope, y=sg.c.sl.cope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = sg.c.sl.cope_lower, ymax = sg.c.sl.cope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = sl.c.sl.cope_lower, xmax = sl.c.sl.cope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
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
                                  Intercept_lower = Q2.5,
                                  Intercept_upper = Q97.5,
                                  site.year.id = rownames(mm_coef$site.year.id[,,'sl.c_Intercept'])) %>% 
                           select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                         mm_coef$site.year.id[,,'sl.c_year.y'] %>% 
                           as_tibble() %>% 
                           mutate(sl.cope = Estimate,
                                  sl.cope_lower = Q2.5,
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
                                  Intercept_lower = Q2.5,
                                  Intercept_upper = Q97.5,
                                  site.year.id = rownames(mm_coef$site.year.id[,,'sg.c_Intercept'])) %>% 
                           select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                         mm_coef$site.year.id[,,'sg.c_year.y'] %>% 
                           as_tibble() %>% 
                           mutate(sl.cope = Estimate,
                                  sl.cope_lower = Q2.5,
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

