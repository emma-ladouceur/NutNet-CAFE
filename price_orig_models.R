
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

View(p.all4)

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

sl.c <- brm(SL.p ~  year.y.m + (year.y.m |  site_code/site.year.id), 
            data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))

sl.c.i <- brm(SL.p ~  year.y.m * s.loss  + (year.y.m |  site_code/site.year.id), 
              data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))


#year.y.m

# * species loss?
summary(p.all4)

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

#interaction with species loss
sg.c.i <- brm(SG ~  year.y.m * s.gain  + (year.y.m |  site_code/site.year.id), 
              data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))


#, control = list(adapt_delta = 0.999, max_treedepth = 15))
# iter=3000,

# assymetric laplace distribution
CDE.m <- brm(CDE ~   year.y + (year.y | site_code/site.year.id), 
             data = p.all4, family=asym_laplace(),cores = 4, chains = 4)

CDE.c <- brm(CDE ~  year.y.m + (year.y.m |  site_code/site.year.id), 
             data = p.all4, family=asym_laplace(),cores = 4, chains = 4)

#what about this one...... maybe an interaction with plot level biomass change?
#CDE.c.i <- brm(CDE ~  year.y.m * s.loss  + (year.y.m |  site_code/site.year.id), 
#             data = p.all4, family=hurdle_lognormal(),cores = 4, chains = 4, control = list(adapt_delta = 0.999, max_treedepth = 15))


#data with a funky transformation beforehand
#CDE.t.m <- brm(CDE.t ~   year.y + (year.y | site_code/site.year.id), 
#            data = p.all4, cores = 4, chains = 4)


setwd('~/Dropbox/Projects/NutNet/Model_fits/')
#save(multi_sg.sl,CDE.m,file = 'price_models_time.Rdata')
#save(CDE.m,file = 'price_cde_laplace.Rdata')
save(sl,sg,CDE.m,file = 'price_models_no0_time.Rdata')
#load('~/Dropbox/Projects/NutNet/Model_fits/price_models_time.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price_cde_laplace.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price_models_sg_time.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price_models_sl_time.Rdata')




summary(sl.c)
summary(sl)

plot(sl)
pp_check(sl)+ scale_x_continuous(trans="log")


plot(sg)
pp_check(sg)
plot(sl)

color_scheme_set("purple")
pp_check(sl)
pp_check(sl, type = "hist")
#marginal effects
marginal_effects(sl, surface = TRUE)
marginal_smooths(sl)


#residuals
slm1<-residuals(sl)
slm1<-as.data.frame(slm1)
nrow(slm1)
nrow(p.all5)
p.all5<-p.all4[complete.cases(p.all4$SL.p), ]
sl.plot<-cbind(p.all5,slm1$Estimate)
sl.plot2<-inner_join(sl.plot,dat)

View(sl.plot)
par(mfrow=c(3,2))
with(sl.plot2, plot(continent, slm1$Estimate))
with(sl.plot2, plot(habitat, slm1$Estimate))
with(sl.plot, plot(site_code, slm1$Estimate))
with(sl.plot2, plot(block, slm1$Estimate))
with(sl.plot2, plot(plot, slm1$Estimate))
with(sl.plot2, plot(f.year.y, slm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
sl_fitted <- cbind(sl$data,
                   # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                   fitted(sl, re_formula = NA)) %>% 
  as_tibble()

View(sl_fitted)
p.all5<-distinct(p.all4,site_code, year.y, SL,SL.p)
sl_fitted2<-inner_join(sl_fitted,p.all5)
View(sl_fitted2)



View(sl_fitted)
# fixed effect coefficients (I want these for the coefficient plot)
sl_fixef <- fixef(sl)

# coefficients for experiment-level (random) effects
sl_coef <- coef(sl)

sl_coef2 <-  bind_cols(sl_coef$site_code[,,'Intercept'] %>% 
                         as_tibble() %>% 
                         mutate(Intercept = Estimate,
                                Intercept_lower = Q2.5,
                                Intercept_upper = Q97.5,
                                site_code = rownames(sl_coef$site_code[,,'Intercept'])) %>% 
                         select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                       sl_coef$site_code[,,'year.y'] %>% 
                         as_tibble() %>% 
                         mutate(Slope = Estimate,
                                Slope_lower = Q2.5,
                                Slope_upper = Q97.5) %>% 
                         select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all4 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site_code')


View(sl_coef2)

View(sl_fitted)

dat<-distinct(plot, site_code, continent,habitat)
nrow(dat)

nrow(sl_coef2)


sl_coef3<-full_join(sl_coef2,dat)
View(sl_coef3)
#convert to negative
#sl_coef3$Intercept<-log(sl_coef3$Intercept)
#sl_coef3$Intercept_upper<-log(sl_coef3$Intercept_upper)
#sl_coef3$Intercept_lower<-log(sl_coef3$Intercept_lower)
#sl_coef3$Slope<-log(sl_coef3$Slope)
#sl_coef3$Slope_upper<-log(sl_coef3$Slope_upper)
#sl_coef3$Slope_lower<-log(sl_coef3$Slope_lower)


sl_coef3$Intercept.n<-(sl_coef3$Intercept * -1)
sl_coef3$Intercept_upper.n<-(sl_coef3$Intercept_upper * -1)
sl_coef3$Intercept_lower.n<-(sl_coef3$Intercept_lower * -1)
sl_coef3$Slope.n<-(sl_coef3$Slope * -1)
sl_coef3$Slope_upper.n<-(sl_coef3$Slope_upper * -1)
sl_coef3$Slope_lower.n<-(sl_coef3$Slope_lower * -1)

sl_coef3$Intercept.log<-log(sl_coef3$Intercept)
sl_coef3$Slope.log<-log(sl_coef3$Slope)
sl_coef3$Intercept.log.n<-(sl_coef3$Intercept.log * -1)
sl_coef3$Slope.log.n<-(sl_coef3$Slope.log * -1)

View(sl_coef3)

#dat2<-distinct(p.all4, site.year.id,SL,SL.log)

sl_fitted2<-full_join(sl_fitted,dat)
#neg
sl_fitted2$SL.n<-(sl_fitted2$SL.p * -1)
sl_fitted2$Estimate.n<-(sl_fitted2$Estimate * -1)
sl_fitted2$Est.Error.n<-(sl_fitted2$Est.Error * -1)
sl_fitted2$Q2.5.n<-(sl_fitted2$Q2.5 * -1)
sl_fitted2$Q97.5.n<-(sl_fitted2$Q97.5 * -1)


View(sl_fitted2)
View(p.all4)
sl_fitted2$year.y.f<-as.factor(sl_fitted2$year.y)
levels(sl_fitted2$year.y.f)

#loss
#forget about making it negative!
slm<-ggplot() +
  # data
  geom_point(data = sl_fitted2,
             aes(x = year.y, y = SL.n,
                 colour = continent, alpha=0.5),
             size = 1.2) +
  geom_jitter(data=sl_fitted2,
              aes(x = year.y, y = SL.n,
                  colour = continent), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = sl_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept.n + Slope.n * xmin,
                   yend = Intercept.n + Slope.n * xmax,
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sl_fitted2,
              aes(x = year.y, ymin = Q2.5.n, ymax = Q97.5.n),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = sl_fitted2,
            aes(x = year.y, y = Estimate.n),
            size = 1.5) +
  #scale_y_continuous(trans = 'log' #, breaks=c(-8,-64,-512,-1024,-2048,-4096) 
  #                  ) +
  labs(x = 'Years',
       y = 'Effect of SL on Function ', title= 'a) EF: SL') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

slm

View(sl_fitted2)
View(p.all4)
dat<-distinct(plot,site_code,continent)
nrow(sldat)
nrow(sl_fitted2)

sl_fitted2<-inner_join(sl_fitted,dat)



#log
slm<-ggplot() +
  # data
  geom_point(data = sl_fitted2,
             aes(x = year.y, y = SL.p,
                 colour = continent, alpha=0.02),
             size = 1.2) +
  geom_jitter(data=sl_fitted2,
              aes(x = year.y, y = SL.p,
                  colour = continent), height=0.25,width = 0.25)+
  # experiment (random) effects
  geom_segment(data = sl_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + Slope * xmin),
                   yend = exp(Intercept + Slope * xmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sl_fitted2,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = sl_fitted2,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  scale_y_continuous(trans = 'log', breaks=c(8,64,512,1024,2048,4096)) +
  labs(x = 'Years',
       y = 'Effect of SL on Function ', title= 'a) EF: SL') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

slm




# #########################################GAINS ####################################################


color_scheme_set("purple")
pp_check(sg)
pp_check(sg, type = "hist")
#marginal effects
marginal_effects(sg, surface = TRUE)
marginal_smooths(sg)


summary(sg)
#residuals
sgm1<-residuals(sg)
sgm1<-as.data.frame(sgm1)
nrow(sgm1)
nrow(p.all4)
p.all5<-p.all4[complete.cases(p.all4$SG.log), ]
sg.plot<-cbind(p.all5,sgm1$Estimate)
sg.plot2<-inner_join(sg.plot,dat)

par(mfrow=c(3,2))
with(sg.plot2, plot(continent, sgm1$Estimate))
with(sg.plot2, plot(habitat, sgm1$Estimate))
with(sg.plot, plot(site_code, sgm1$Estimate))
with(sg.plot2, plot(block, sgm1$Estimate))
with(sg.plot2, plot(plot, sgm1$Estimate))
with(sg.plot2, plot(f.year.y, sgm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
sg_fitted <- cbind(sg$data,
                   # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                   fitted(sg, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(sg_fitted)
View(sg_fitted)
p.all5<-distinct(p.all4,site_code, year.y, SG)
sg_fitted2<-inner_join(sg_fitted,dat)
sg_fitted2$SG.log<-log(sg_fitted2$SG)

View(p.all4)
View(sg_fitted2)


# fixed effect coefficients (I want these for the coefficient plot)
sg_fixef <- fixef(sg)

# coefficients for experiment-level (random) effects
sg_coef <- coef(sg)
sg_coef 
sg_coef2 <-  bind_cols(sg_coef$site_code[,,'Intercept'] %>% 
                         as_tibble() %>% 
                         mutate(Intercept = Estimate,
                                Intercept_lower = Q2.5,
                                Intercept_upper = Q97.5,
                                site_code = rownames(sg_coef$site_code[,,'Intercept'])) %>% 
                         select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                       sg_coef$site_code[,,'year.y'] %>% 
                         as_tibble() %>% 
                         mutate(Slope = Estimate,
                                Slope_lower = Q2.5,
                                Slope_upper = Q97.5) %>% 
                         select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all4 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site_code')


View(sg_coef3)

View(sg_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

sg_coef3<-full_join(sg_coef2,dat)
#dat2<-distinct(p.all4, site.year.id,SG)


View(sg_coef3)
sg_coef4<-sg_coef3[complete.cases(sg_coef3), ]
nrow(sg_coef4)
View(sg_coef4)

#gai
sgm<-ggplot() +
  # data
  geom_point(data = sg_fitted2,
             aes(x = year.y, y = SG,
                 colour = continent, alpha=0.2),
             size = 1.2) +
  geom_jitter(data=sg_fitted2,
              aes(x = year.y, y = SG,
                  colour = continent), height=0.25,width = 0.25)+
  # experiment (random) effects
  geom_segment(data = sg_coef4,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + Slope * xmin),
                   yend = exp(Intercept + Slope * xmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sg_fitted2,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = sg_fitted2,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  scale_y_continuous(trans = 'log', breaks=c(8,64,512,1024,2048,4096)) +
  labs(x = 'Years',
       y = 'Effect of SG on Function ', title= 'b) EF: SG') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")



sgm



########################################################################################################################
#####################################CDE model###################################################################################
########################################################################################################################

summary(CDE.m)

pairs(CDE.m)

# inspection of chain diagnostic
plot(CDE.m)  



color_scheme_set("purple")
pp_check(CDE.m)

pp_check(CDE.m, type = "hist")
#marginal effects
marginal_effects(CDE.m, surface = TRUE)
marginal_smooths(CDE.m)



#residuals
cm1<-residuals(CDE.m)
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
cde_fitted <- cbind(CDE.m$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(CDE.m, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(cde_fitted)

#p.all5<-distinct(p.all4,site_code, year.y, continent)
#cde_fitted2<-inner_join(cde_fitted,p.all5,  by = c('site_code', 'year.y'))


View(cde_fitted)
# fixed effect coefficients (I want these for the coefficient plot)
cde_fixef <- fixef(CDE.m)

# coefficients for experiment-level (random) effects
cde_coef <- coef(CDE.m)
cde_coef2 <-  bind_cols(cde_coef$site_code[,,'Intercept'] %>% 
                          as_tibble() %>% 
                          mutate(Intercept = Estimate,
                                 Intercept_lower = Q2.5,
                                 Intercept_upper = Q97.5,
                                 site_code = rownames(cde_coef$site_code[,,'Intercept'])) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                        cde_coef$site_code[,,'year.y'] %>% 
                          as_tibble() %>% 
                          mutate(Slope = Estimate,
                                 Slope_lower = Q2.5,
                                 Slope_upper = Q97.5) %>% 
                          select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all4 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site_code')


View(cde_coef2)

View(cde_fitted)


cde_coef3<-full_join(cde_coef2,dat)
cde_fitted2<-full_join(cde_fitted,dat)

library(scales)
sign_sqrt <- scales::trans_new('sign_sqrt',
                               transform = function(x) {sign(x) * sqrt(abs(x))},
                               inverse = function(x){sign(x) * abs(x)^2})



#cde
cdem<-ggplot() +
  # data
  geom_point(data = cde_fitted2,
             aes(x = year.y, y = CDE,
                 colour = continent, alpha=0.2),
             size = 1.2) +
  geom_jitter(data=cde_fitted2,
              aes(x = year.y, y = CDE,
                  colour = continent), height=0.25,width = 0.25)+
  experiment (random) effects
geom_segment(data = cde_coef3,
             aes(x = xmin, 
                 xend = xmax,
                 y = Intercept + Slope * xmin,
                 yend = Intercept + Slope * xmax,
                 group = site_code,
                 colour = continent),
             size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = cde_fitted2,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = cde_fitted2,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  scale_y_continuous(trans = sign_sqrt #, breaks=c(8,64,512,1024,2048,4096)
  ) +
  labs(x = 'Years',
       y = 'Function Change in persistent Species', title= 'c) Context Dependent Change') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

cdem


grid_arrange_shared_legend(slm,sgm,cdem,nrow=1)

#coefs


sl_fixef2<-as.data.frame(sl_fixef)
sg_fixef2<-as.data.frame(sg_fixef)
cde_fixef2<-as.data.frame(cde_fixef)
View(sl_coef2)

sl_coef4<-sl_coef3[complete.cases(sl_coef3), ]
sg_coef4<-sg_coef3[complete.cases(sg_coef3), ]
cde_coef4<-cde_coef3[complete.cases(cde_coef3), ]

View(sl_coef4)
View(sl_fixef2)
sl_fixef2$Estimate.n<-(sl_fixef2$Estimate * -1)
sl_fixef2$Est.Error.n<-(sl_fixef2$Est.Error * -1)
sl_fixef2$Q2.5.n<-(sl_fixef2$Q2.5 * -1)
sl_fixef2$Q97.5.n<-(sl_fixef2$Q97.5 * -1)



slm2<-ggplot() + 
  geom_point(data = sl_coef4, aes(x =reorder(site_code,Slope), y = Slope, colour = continent),size = 2) +
  geom_errorbar(data = sl_coef4, aes(x = reorder(site_code,Slope),ymin = Slope_lower,
                                     ymax = Slope_upper,colour = continent),
                width = 0, size = 0.7) + 
  #facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = sl_fixef2,
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = sl_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  ylim(-0.25,0.70) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'a) EF: SL') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


slm2

sl_coef4$slope.sl<-sl_coef4$Slope
sl.slope<-select(sl_coef4,site_code,slope.sl)
sg_coef5<-inner_join(sg_coef4,sl.slope)
is.numeric(sg_coef5$slope.sl)


View(sg_coef3)
sgm2<-ggplot() + 
  geom_point(data = sg_coef5, aes(x = reorder(site_code,slope.sl), y = Slope,colour = continent),size = 2) +
  geom_errorbar(data = sg_coef5, aes(x = reorder(site_code,slope.sl),ymin = Slope_lower,
                                     ymax = Slope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = sg_fixef2,
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = sg_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  ylim(-0.25,0.70) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'b) EF : SG') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")
sgm2


#sl_coef4$slope.sl<-sl_coef4$Slope
#sl.slope<-select(sl_coef4,site_code,slope.sl)
cde_coef5<-inner_join(cde_coef4,sl.slope)
is.numeric(sg_coef5$slope.sl)


cdem2<-ggplot() + 
  geom_point(data = cde_coef5, aes(x = reorder(site_code,slope.sl), y = Slope,colour = continent),size = 2) +
  geom_errorbar(data = cde_coef5, aes(x = reorder(site_code,slope.sl),ymin = Slope_lower,
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
  ylim(-125,175) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'c) Context Dependent Effect') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


grid_arrange_shared_legend(slm2,sgm2,cdem2,nrow=1)




View(sl_coef3)
View(sg_coef3)
colnames(sl_coef3)
colnames(sg_coef3)
sl_coef4<-sl_coef3[,c(-1,-2,-3)]
sg_coef4<-sg_coef3[,c(-1,-2,-3)]
colnames(sl_coef4)
colnames(sg_coef4)
sl_coef5<-sl_coef4[,c(-9,-10,-11,-12,-13,-14,-15,-16,-17,-18)]
names(sl_coef5) <- c("site_code","SL.Slope","SL.Slope_lower","SL.Slope_upper","xmin","xmax","continent","habitat")
names(sg_coef4) <- c("site_code","SG.Slope","SG.Slope_lower","SG.Slope_upper","xmin","xmax","continent","habitat")
delta.coefs<-bind_cols(sl_coef5,sg_coef4)
View(delta.coefs)

#By Experiment
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(data=delta.coefs, aes(x=SL.Slope, y=SG.Slope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = SG.Slope_lower, ymax = SG.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = SL.Slope_lower, xmax = SL.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'SL Slope',
       y = 'SG Slope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")






ps.sl<-as.data.frame(posterior_samples(sl))
ps.sg<-as.data.frame(posterior_samples(sg))
ps.cde<-as.data.frame(posterior_samples(CDE.m))

View(ps.sl)


devtools::install_github("mvuorre/brmstools")
library(brmstools)
forest(sl)




# MULTIVARIATE MODEL

summary(multi_sg.sl)

color_scheme_set("purple")
pp_check(multi_sg.sl, resp = 'SLlog')
pp_check(multi_sg.sl, resp = 'SGlog')

plot(multi_sg.sl)


m1<-residuals(multi_sg.sl)
head(m1)
nrow(m1)
nrow(m2)
nrow(p.all4)
p.all5 <- p.all4[!(is.na(p.all4$SL)),]
p.all6 <- p.all5[!(is.na(p.all5$SG)),]
nrow(p.all6)
plot <- cbind(p.all6,m1)
m1<-as.data.frame(m1)
levels(plot$site_code)


par(mfrow=c(2,2))
with(plot, plot(site_code, m1$Estimate.SL))
with(plot, plot(block, m1$Estimate.SL))
with(plot, plot(plot, m1$Estimate.SL))
with(plot, plot(f.year.y, m1$Estimate.SL))

par(mfrow=c(2,2))
with(plot, plot(site_code, m1$Estimate.SG))
with(plot, plot(block, m1$Estimate.SG))
with(plot, plot(plot, m1$Estimate.SG))
with(plot, plot(f.year.y, m1$Estimate.SG))


mm_fitted <- cbind(multi_sg.sl$data,
                   # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                   fitted(multi_sg.sl, re_formula = NA)) %>% 
  as_tibble() 

dat2<-distinct(p.all6, site.year.id,site_code)
View(dat2)
dat3<-inner_join(dat2,dat,by='site_code')
View(dat3)
mm_fitted2<-inner_join(mm_fitted, dat3, by='site.year.id')

View(p.all6)
View(mm_fitted)


# fixed effect coefficients (I want these for the coefficient plot)
mm_fixef <- fixef(multi_sg.sl)
View(mm_fixef)


# coefficients for experiment-level (random) effects
mm_coef <- coef(multi_sg.sl)
View(mm_coef)
mm_coef
mm_coef.sl <-  bind_cols(mm_coef$site.year.id[,,'SL_Intercept'] %>% 
                           as_tibble() %>% 
                           mutate(Intercept = Estimate,
                                  Intercept_lower = Q2.5,
                                  Intercept_upper = Q97.5,
                                  site.year.id = rownames(mm_coef$site.year.id[,,'SL_Intercept'])) %>% 
                           select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                         mm_coef$site.year.id[,,'SL_year.y'] %>% 
                           as_tibble() %>% 
                           mutate(Slope = Estimate,
                                  Slope_lower = Q2.5,
                                  Slope_upper = Q97.5) %>% 
                           select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all6 %>% 
               group_by(site.year.id) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site.year.id') 


View(mm_coef.sl)


mm_coef.sg <-  bind_cols(mm_coef$site.year.id[,,'SG_Intercept'] %>% 
                           as_tibble() %>% 
                           mutate(Intercept = Estimate,
                                  Intercept_lower = Q2.5,
                                  Intercept_upper = Q97.5,
                                  site.year.id = rownames(mm_coef$site.year.id[,,'SG_Intercept'])) %>% 
                           select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                         mm_coef$site.year.id[,,'SG_year.y'] %>% 
                           as_tibble() %>% 
                           mutate(Slope = Estimate,
                                  Slope_lower = Q2.5,
                                  Slope_upper = Q97.5) %>% 
                           select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all6 %>% 
               group_by(site.year.id) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site.year.id')



mm_coef.sl2<-full_join(mm_coef.sl,dat3)

mm_coef.sg2<-full_join(mm_coef.sg,dat3)

colnames(mm_fitted2)
View(mm_coef.sl2)

#loss
ggplot() +
  # data
  geom_point(data = mm_fitted2,
             aes(x = year.y, y = SL,
                 colour = continent, alpha=0.5),
             size = 1.2) +
  geom_jitter(data=mm_fitted2,
              aes(x = year.y, y = SL,
                  colour = continent), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = mm_coef.sl2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * xmin,
                   yend = Intercept + Slope * xmax,
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = mm_fitted2,
              aes(x = year.y, ymin = Q2.5.SL, ymax = Q97.5.SL),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = mm_fitted2,
            aes(x = year.y, y = Estimate.SL),
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
             aes(x = year.y, y = SG,
                 colour = continent, alpha=0.5),
             size = 1.2) +
  geom_jitter(data=mm_fitted2,
              aes(x = year.y, y = SG,
                  colour = continent), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = mm_coef.sg2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * xmin,
                   yend = Intercept + Slope * xmax,
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = mm_fitted2,
              aes(x = year.y, ymin = Q2.5.SG, ymax = Q97.5.SG),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = mm_fitted2,
            aes(x = year.y, y = Estimate.SG),
            size = 1.5) +
  #scale_y_continuous(trans = 'log10') +
  labs(x = 'Years',
       y = 'Function Loss due to Species Gain', title= 'b) Species Gain') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

