
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
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
#shanes links
sp <- read.csv("~/Dropbox/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/PNutNet/Data/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(p.all)
levels(p.all3$trt_year)

p.all<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all$f.year.y<-as.factor(p.all$year.y)
p.all$plot<-as.factor(p.all$plot)
p.all$site_code<-as.factor(p.all$site_code)

dat<-distinct(plot, site_code, continent,habitat)
p.dat<-inner_join(p.all,dat)

nrow(p.dat)
p.dat2<-p.dat[complete.cases(p.dat), ]
nrow(p.dat2)


p.dat3<-p.dat2[p.dat2$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]
nrow(p.dat3)

View(p.dat3)

colnames(p.dat3)
head(p.dat3)
summary(p.dat3)
View(p.dat3)

#write.csv(p.all4,"~/Desktop/pricel.csv")

p.dat3$year.y.m<-p.dat3$year.y-mean(p.dat3$year.y)

# s loss, gain and change metrics
p.dat3$s.loss <- -1*(p.dat3$x.rich - p.dat3$c.rich)
p.dat3$s.gain <- p.dat3$y.rich - p.dat3$c.rich
p.dat3$s.change <- p.dat3$y.rich - p.dat3$x.rich

p.dat3$s.loss.p<-abs(p.dat3$s.loss)

p.dat3$s.loss.p.log <- log1p(abs(p.dat3$s.loss.p))
p.dat3$s.gain.log<-log1p(p.dat3$s.gain)
p.dat3$s.change.log<-log1p(p.dat3$s.change)

par(mfrow=c(2,3))
hist(p.dat3$s.loss.p,breaks =10, main="Species Loss", xlab= "Species Loss")
hist(p.dat3$s.gain, breaks=10, main="Species Gains", xlab= "Species Gains")
hist(p.dat3$s.change, breaks=10, main="Species Change", xlab= "Species Change")
hist(p.dat3$s.loss.p.log,breaks =10, main="Log Species Loss", xlab= "Log Species Loss")
hist(p.dat3$s.gain.log, breaks=10, main="Log Species Gains", xlab= "Log Species Gains")
hist(p.dat3$s.change.log, breaks=10, main="Log Species Change", xlab= "Log Species Change")

p.dat3$site_code<-as.factor(p.dat3$site_code)
p.dat3$site.year.id<-as.factor(p.dat3$site.year.id)


s.loss <- brm(s.loss.p.log ~  trt.y * year.y.m + (year.y.m |  site_code/site.year.id), 
              data = p.dat3, cores = 4, chains = 4)


s.gain <- brm(s.gain.log ~  trt.y * year.y.m + (year.y.m |  site_code/site.year.id), 
              data = p.dat3, cores = 4, chains = 4)


s.change <- brm(s.change ~  trt.y * year.y.m + (year.y.m |  site_code/site.year.id), 
               data = p.dat3, family=asym_laplace(),cores = 4, chains = 4)



setwd('~/Dropbox/Projects/NutNet/Model_fits/')
save(s.loss,s.gain,s.change,file = 'price_lgc.Rdata')
write.csv(p.dat3,file = 'price_models.csv')
load('~/Dropbox/Projects/NutNet/Model_fits/price_lgc.Rdata')

s.loss.i <- brm(s.loss.p.log ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id), 
              data = p.dat3, cores = 4, chains = 4)


s.gain.i <- brm(s.gain.log ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id), 
              data = p.dat3, cores = 4, chains = 4)


s.change.i <- brm(s.change ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/site.year.id), 
                data = p.dat3, family=asym_laplace(),cores = 4, chains = 4)

setwd('~/Dropbox/Projects/NutNet/Model_fits/')
save(s.loss.i,s.gain.i,s.change.i,file = 'price_trt_i_lgc.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price_trt_i_lgc.Rdata')

     
summary(s.loss.i)

summary(s.gain.i)

summary(s.change.i)




plot(s.loss.i)

plot(s.gain.i)

plot(s.change.i)


pp_check(s.loss.i)

pp_check(s.gain.i)

pp_check(s.change.i)

dat<-distinct(p.dat3, site_code, continent,habitat)


s.loss.i_fitted <- cbind(s.loss.i$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(s.loss.i, re_formula = NA)) %>% 
  as_tibble()

View(s.loss.i_fitted)
View(p.dat5)
p.dat5<-distinct(p.dat3,site_code, year.y,year.y.m, s.loss.p,s.loss.p.log)
s.loss.i_fitted2<-inner_join(s.loss.i_fitted,dat)
s.loss.i_fitted3<-inner_join(s.loss.i_fitted2,p.dat5)
View(s.loss.i_fitted3)

s.loss.i_fitted.npk<-s.loss.i_fitted3[s.loss.i_fitted3$trt.y %in% c('NPK'),]
s.loss.i_fitted.ctl<-s.loss.i_fitted3[s.loss.i_fitted3$trt.y %in% c('Control'),]

View(s.loss.i_fitted.npk)
nrow(s.loss.i_fitted)
nrow(s.gain.i_fitted)
nrow(s.loss.i_fitted.npk)
nrow(s.gain.i_fitted.npk)

# fixed effect coefficients -coefficient plot
s.loss.i_fixef <- fixef(s.loss.i)

# coefficients for experiment-level (random) effects
s.loss.i_coef
s.loss.i_coef <- coef(s.loss.i)
View(s.loss.i_coef)



s.loss.i_coef2 <-  bind_cols(s.loss.i_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(s.loss.i_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           s.loss.i_coef$site_code[,,'year.y.m'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           s.loss.i_coef$site_code[,,'trt.yNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           s.loss.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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


s.loss.i_coef3<-inner_join(s.loss.i_coef2,dat)
View(s.loss.i_coef3)


#convert to negative
#s.loss.i_coef3$Intercept<-log(s.loss.i_coef3$Intercept)
#s.loss.i_coef3$Intercept_upper<-log(s.loss.i_coef3$Intercept_upper)
#s.loss.i_coef3$Intercept_lower<-log(s.loss.i_coef3$Intercept_lower)
#s.loss.i_coef3$s.loss.iope<-log(s.loss.i_coef3$s.loss.iope)
#s.loss.i_coef3$s.loss.iope_upper<-log(s.loss.i_coef3$s.loss.iope_upper)
#s.loss.i_coef3$s.loss.iope_lower<-log(s.loss.i_coef3$s.loss.iope_lower)


s.loss.i_coef3$Intercept.n<-(s.loss.i_coef3$Intercept * -1)
s.loss.i_coef3$Intercept_upper.n<-(s.loss.i_coef3$Intercept_upper * -1)
s.loss.i_coef3$Intercept_lower.n<-(s.loss.i_coef3$Intercept_lower * -1)
s.loss.i_coef3$s.loss.iope.n<-(s.loss.i_coef3$s.loss.iope * -1)
s.loss.i_coef3$s.loss.iope_upper.n<-(s.loss.i_coef3$s.loss.iope_upper * -1)
s.loss.i_coef3$s.loss.iope_lower.n<-(s.loss.i_coef3$s.loss.iope_lower * -1)

s.loss.i_coef3$Intercept.log<-log(s.loss.i_coef3$Intercept)
s.loss.i_coef3$s.loss.iope.log<-log(s.loss.i_coef3$s.loss.iope)
s.loss.i_coef3$Intercept.log.n<-(s.loss.i_coef3$Intercept.log * -1)
s.loss.i_coef3$s.loss.iope.log.n<-(s.loss.i_coef3$s.loss.iope.log * -1)

View(s.loss.i_coef3)
View(p.dat3)
dat2<-distinct(p.dat3, site_code,year.y, year.y.m)
View(dat2)


View(s.loss.i_fitted2)
View(dat2)
#neg
s.loss.i_fitted2$s.loss.i.n<-(s.loss.i_fitted2$s.loss.i.p * -1)
s.loss.i_fitted2$Estimate.n<-(s.loss.i_fitted2$Estimate * -1)
s.loss.i_fitted2$Est.Error.n<-(s.loss.i_fitted2$Est.Error * -1)
s.loss.i_fitted2$Q2.5.n<-(s.loss.i_fitted2$Q2.5 * -1)
s.loss.i_fitted2$Q97.5.n<-(s.loss.i_fitted2$Q97.5 * -1)


View(p.dat3)
s.loss.i_fitted2$year.y.f<-as.factor(s.loss.i_fitted2$year.y)
levels(s.loss.i_fitted2$year.y.f)


View(s.loss.i_fitted2)
View(p.dat3)
dat<-distinct(plot,site_code,continent)
nrow(s.loss.idat)
nrow(s.loss.i_fitted2)

#s.loss.i_fitted2<-inner_join(s.loss.i_fitted,dat)

View(s.loss.i_fitted3)
View(s.loss.i_coef3)

summary(s.loss.i_fitted.npk)
summary(s.loss.i_coef3)

library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

View(s.loss.i_fitted3)
s.loss.im<-ggplot() +
  # data
  geom_point(data = s.loss.i_fitted.npk,
             aes(x = year.y, y = s.loss.p,
                 colour = continent, alpha=0.02),
             size = 1.2) +
  geom_jitter(data=s.loss.i_fitted.npk,
              aes(x = year.y, y = s.loss.p,
                  colour = continent), height=0.25,width = 0.25)+
  # experiment (random) effects
  geom_segment(data = s.loss.i_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = s.loss.i_fitted.npk,
              aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = s.loss.i_fitted.npk,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5) +
  geom_ribbon(data = s.loss.i_fitted.ctl,
              aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data =  s.loss.i_fitted.ctl,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5, linetype= "dashed") +
  scale_y_continuous(trans = reverselog_trans(), breaks=c(1,2,4,6,8,16,24) ) +
  labs(x = 'Years',
       y = expression(paste('Plot Species Loss')), title= 'a) Species Loss') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

s.loss.im



########################################################################################################################
##################################### GAINS ##################################################################################
########################################################################################################################


s.gain.im1<-residuals(s.gain.i)
s.gain.im1<-as.data.frame(s.gain.im1)
nrow(s.gain.im1)
nrow(p.dat3)
s.gain.i.plot<-cbind(p.dat3,s.gain.im1$Estimate)
s.gain.i.plot2<-inner_join(s.gain.i.plot,dat)

par(mfrow=c(3,2))
with(s.gain.i.plot2, plot(continent, s.gain.im1$Estimate))
with(s.gain.i.plot2, plot(habitat, s.gain.im1$Estimate))
with(s.gain.i.plot, plot(site_code, s.gain.im1$Estimate))
with(s.gain.i.plot2, plot(block, s.gain.im1$Estimate))
with(s.gain.i.plot2, plot(plot, s.gain.im1$Estimate))
with(s.gain.i.plot2, plot(f.year.y, s.gain.im1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
s.gain.i_fitted <- cbind(s.gain.i$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(s.gain.i, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(s.gain.i_fitted)
p.dat4<-distinct(p.dat3,site_code, year.y, year.y.m, s.gain,s.gain.log)
s.gain.i_fitted2<-inner_join(s.gain.i_fitted,dat)
s.gain.i_fitted3<-inner_join(s.gain.i_fitted2,p.dat4)

View(s.gain.i_fitted2)

s.gain.i_fitted.npk<-s.gain.i_fitted3[s.gain.i_fitted3$trt.y %in% c('NPK'),]
s.gain.i_fitted.ctl<-s.gain.i_fitted3[s.gain.i_fitted3$trt.y %in% c('Control'),]


View(p.dat3)
View(s.gain.i_fitted2)


# fixed effect coefficients (I want these for the coefficient plot)
s.gain.i_fixef <- fixef(s.gain.i)

# coefficients for experiment-level (random) effects
s.gain.i_coef <- coef(s.gain.i)
s.gain.i_coef 

s.gain.i_coef2 <-  bind_cols(s.gain.i_coef$site_code[,,'Intercept'] %>% 
                               as_tibble() %>% 
                               mutate(Intercept = Estimate,
                                      Intercept_lower = Q2.5,
                                      Intercept_upper = Q97.5,
                                      site_code = rownames(s.gain.i_coef$site_code[,,'Intercept'])) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             s.gain.i_coef$site_code[,,'year.y.m'] %>% 
                               as_tibble() %>% 
                               mutate(ISlope = Estimate,
                                      ISlope_lower = Q2.5,
                                      ISlope_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             s.gain.i_coef$site_code[,,'trt.yNPK'] %>% 
                               as_tibble() %>% 
                               mutate(TE = Estimate,
                                      TE_lower = Q2.5,
                                      TE_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             s.gain.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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



View(s.gain.i_coef3)

View(s.gain.i_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

s.gain.i_coef3<-full_join(s.gain.i_coef2,dat)


View(s.gain.i_coef3)
s.gain.i_coef4<-s.gain.i_coef3[complete.cases(s.gain.i_coef3), ]
nrow(s.gain.i_coef4)
View(s.gain.i_fitted.npk)


summary(s.gain.i_fitted3)
summary(s.gain.i_coef3)

#gai
s.gain.im<-ggplot() +
  # data
  geom_point(data = s.gain.i_fitted.npk,
             aes(x = year.y, y = s.gain,
                 colour = continent, alpha=0.2),
             size = 1.2) +
  geom_jitter(data=s.gain.i_fitted.npk,
              aes(x = year.y, y = s.gain,
                  colour = continent), height=0.25,width = 0.25)+
  # experiment (random) effects
  geom_segment(data = s.gain.i_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + TE + (ISlope+TESlope) * cxmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = s.gain.i_fitted.npk,
              aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = s.gain.i_fitted.npk,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5) +
  geom_ribbon(data = s.gain.i_fitted.ctl,
             aes(x = year.y, ymin = exp(Q2.5), ymax = exp(Q97.5)),
             alpha = 0.3) +
  # fixed effect
  geom_line(data =  s.gain.i_fitted.ctl,
            aes(x = year.y, y = exp(Estimate)),
            size = 1.5, linetype= "dashed") +
  scale_y_continuous(trans = 'log' , breaks=c(1,2,4,6,8,16,24)  ) +
  labs(x = 'Years',
       y = expression(paste('Plot Species Gain')), title= 'b) Species Gain') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

s.gain.im



grid_arrange_shared_legend(s.loss.im,s.gain.im,nrow=1)


########################################################################################################################
#####################################CDE model###################################################################################
########################################################################################################################

summary(s.change.i)
color_scheme_set("purple")
pairs(s.change.i)

# inspection of chain diagnostic
plot(s.change.i)  


pp_check(s.change.i)

pp_check(s.change.i, type = "hist")
#marginal effects
marginal_effects(s.change.i, surface = TRUE)
marginal_smooths(s.change.i)



#residuals
cm1<-residuals(s.change.i)
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
s.change.i_fitted <- cbind(s.change.i$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(s.change.i, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(s.change.i_fitted)

View(s.change.i_fitted)

p.dat4<-distinct(p.dat3,site_code, year.y, continent,s.change)
#s.change.i_fitted2<-inner_join(s.change.i_fitted,p.all5,  by = c('site_code', 'year.y'))


# fixed effect coefficients (I want these for the coefficient plot)
s.change.i_fixef <- fixef(s.change.i)

# coefficients for experiment-level (random) effects
s.change.i_coef <- coef(s.change.i)

s.change.i_coef2 <-  bind_cols(s.change.i_coef$site_code[,,'Intercept'] %>% 
                               as_tibble() %>% 
                               mutate(Intercept = Estimate,
                                      Intercept_lower = Q2.5,
                                      Intercept_upper = Q97.5,
                                      site_code = rownames(s.change.i_coef$site_code[,,'Intercept'])) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             s.change.i_coef$site_code[,,'year.y.m'] %>% 
                               as_tibble() %>% 
                               mutate(ISlope = Estimate,
                                      ISlope_lower = Q2.5,
                                      ISlope_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             s.change.i_coef$site_code[,,'trt.yNPK'] %>% 
                               as_tibble() %>% 
                               mutate(TE = Estimate,
                                      TE_lower = Q2.5,
                                      TE_upper = Q97.5) %>% 
                               select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                             s.change.i_coef$site_code[,,'trt.yNPK:year.y.m'] %>% 
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



View(s.change.i_coef2)

View(s.change.i_fitted)


s.change.i_coef3<-full_join(s.change.i_coef2,dat)
s.change.i_fitted2<-full_join(s.change.i_fitted,dat)
s.change.i_fitted3<-full_join(s.change.i_fitted2,dat2)

s.change.i_fitted.npk<-s.change.i_fitted3[s.change.i_fitted3$trt.y %in% c('NPK'),]
s.change.i_fitted.ctl<-s.change.i_fitted3[s.change.i_fitted3$trt.y %in% c('Control'),]



library(scales)
sign_sqrt <- scales::trans_new('sign_sqrt',
                               transform = function(x) {sign(x) * sqrt(abs(x))},
                               inverse = function(x){sign(x) * abs(x)^2})


View(s.change.i_fitted.npk)
View(s.change.i_coef3)
#s.change.i
s.change.im<-ggplot() +
  # data
  geom_point(data = s.change.i_fitted.npk,
             aes(x = year.y, y = s.change,
                 colour = continent, alpha=0.2),
             size = 1.2) +
  geom_jitter(data=s.change.i_fitted.npk,
              aes(x = year.y, y = s.change,
                  colour = continent), height=0.25,width = 0.25)+
  #experiment (random) effects
  geom_segment(data = s.change.i_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = s.change.i_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = s.change.i_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = s.change.i_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = s.change.i_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,linetype="dashed") +
  scale_y_continuous(trans = sign_sqrt #, breaks=c(8,64,512,1024,2048,4096)
  ) +
  labs(x = 'Years',
       y = expression(paste('Species Change')), title= 'c) Species Change') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()+ theme(legend.position="bottom")

s.change.im


grid_arrange_shared_legend(s.loss.im,s.gain.im,s.change.im,nrow=1)






#coefs


s.loss.i_fixef2<-as.data.frame(s.loss.i_fixef)
s.gain.i_fixef2<-as.data.frame(s.gain.i_fixef)
s.change.i_fixef2<-as.data.frame(s.change.i_fixef)
View(s.loss.i_fixef2)

s.loss.i_coef4<-s.loss.i_coef3[complete.cases(s.loss.i_coef3), ]
s.gain.i_coef4<-s.gain.i_coef3[complete.cases(s.gain.i_coef3), ]
s.change.i_coef4<-s.change.i_coef3[complete.cases(s.change.i_coef3), ]

View(s.loss.i_coef4)

s.loss.i_fixef2$Estimate.n<-(s.loss.i_fixef2$Estimate * -1)
s.loss.i_fixef2$Est.Error.n<-(s.loss.i_fixef2$Est.Error * -1)
s.loss.i_fixef2$Q2.5.n<-(s.loss.i_fixef2$Q2.5 * -1)
s.loss.i_fixef2$Q97.5.n<-(s.loss.i_fixef2$Q97.5 * -1)

View(s.loss.i_coef4)
View(s.loss.i_fixef2)


s.loss.im2<-ggplot() + 
  geom_point(data = s.loss.i_coef4, aes(x =reorder(site_code,TESlope), y = TESlope, colour = continent),size = 2) +
  geom_errorbar(data = s.loss.i_coef4, aes(x = reorder(site_code,TESlope),ymin = TESlope_lower,
                                         ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  #facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = s.loss.i_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = s.loss.i_fixef2,
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


s.loss.im2

s.loss.i_coef4$Slope.sl<-s.loss.i_coef4$TESlope
s.loss.i.Slope<-select(s.loss.i_coef4,site_code,Slope.sl)
s.gain.i_coef5<-inner_join(s.gain.i_coef4,s.loss.i.Slope)
is.numeric(s.gain.i_coef5$Slope.sl)


View(s.gain.i_coef3)
s.gain.im2<-ggplot() + 
  geom_point(data = s.gain.i_coef5, aes(x = reorder(site_code,Slope.sl), y = TESlope,colour = continent),size = 2) +
  geom_errorbar(data = s.gain.i_coef5, aes(x = reorder(site_code,Slope.sl),ymin = TESlope_lower,
                                         ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = s.gain.i_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = s.gain.i_fixef2,
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
s.gain.im2


#s.loss.i_coef4$Slope.sl<-s.loss.i_coef4$Slope
#s.loss.i.Slope<-select(s.loss.i_coef4,site_code,Slope.sl)
s.change.i_coef5<-inner_join(s.change.i_coef4,s.loss.i.Slope)
is.numeric(s.gain.i_coef5$Slope.sl)

s.change.i_fixef2
s.change.im2<-ggplot() + 
  geom_point(data = s.change.i_coef5, aes(x = reorder(site_code,Slope.sl), y = TESlope,colour = continent),size = 2) +
  geom_errorbar(data = s.change.i_coef5, aes(x = reorder(site_code,Slope.sl),ymin = TESlope_lower,
                                      ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = s.change.i_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = s.change.i_fixef2,
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



grid_arrange_shared_legend(s.loss.im,s.gain.im,s.change.im,ncol=3,nrow=1)

grid_arrange_shared_legend(s.loss.im2,s.gain.im2,s.change.im2,ncol=3,nrow=1)


grid_arrange_shared_legend(s.loss.im,s.gain.im,s.loss.im2,s.gain.im2,ncol=2,nrow=2)


grid_arrange_shared_legend(s.loss.im,s.gain.im,s.change.im,s.loss.im2,s.gain.im2,s.change.im2,ncol=3,nrow=2)


grid_arrange_shared_legend(s.loss.im,s.gain.im,s.change.im,s.loss.im2,s.gain.im2,s.change.im2,ncol=3,nrow=2)



grid_arrange_shared_legend(s.loss.im,sl.trtm,s.gain.im,sg.trtm,s.loss.im2,sl.trtm2,s.gain.im2,sg.trtm2,ncol=4,nrow=2)



