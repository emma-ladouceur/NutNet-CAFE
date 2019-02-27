
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)

sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



plot<-p[p$trt %in% c('NPK'),]

colnames(plot)
plot$year_trt<-as.numeric(as.character(plot$year_trt))
plot$continent<-as.factor(plot$continent)
plot$habitat<-as.factor(plot$habitat)
plot$site<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

native<-sp[sp$local_provenance %in% c('NAT'),]
introduced<-sp[sp$local_provenance %in% c('INT'),]


View(p)

p2 <- plot[!(is.na(p$live_mass)),]
View(p2)
s<-distinct(p2,site_code,continent, habitat,trt)
View(s)
site.r.bm<- p2 %>% group_by(site_code,year_trt) %>% dplyr::summarise(site.rich=sum(rich),site.bm=sum(live_mass))
site<-full_join(s,site.r.bm, by="site_code")
View(site)



#plot all
plot.rich.m <- brm(rich ~  year_trt + (year_trt | site_code/block/plot), 
data = plot, cores = 4, chains = 4)

plot.bm.m <- brm(live_mass ~  year_trt + (year_trt | site_code/block/plot), 
                   data = plot, cores = 4, chains = 4)

#plot native
plot.nat.rich.m <- brm(NAT_rich ~  year_trt + (year_trt | site_code/block/plot), 
                   data = native, cores = 4, chains = 4)

plot.nat.bm.m <- brm(nat_biomass ~  year_trt + (year_trt | site_code/block/plot), 
                       data = native, cores = 4, chains = 4)

#plot introduced
plot.int.rich.m <- brm(INT_rich ~  year_trt + (year_trt | site_code/block/plot), 
                          data = introduced, cores = 4, chains = 4)

plot.int.bm.m <- brm(int_biomass ~  year_trt + (year_trt | site_code/block/plot), 
                       data = introduced, cores = 4, chains = 4)


#site
site.rich.m <- brm(site.rich ~  year_trt + (year_trt | continent/habitat/site_code), 
                   data = site, cores = 4, chains = 4)

site.bm.m <- brm(site.bm ~  year_trt + (year_trt | continent/habitat/site_code), 
                 data = site, cores = 4, chains = 4)


setwd('~/Dropbox/Projects/NutNet/Model_fits/')
#save(plot.rich.m,plot.bm.m,file = 'plot.nutnet.models.Rdata')
save(site.rich.m,site.bm.m,file = 'site.nutnet.models.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/plot.nutnet.models.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/site.nutnet.models.Rdata')


summary(plot.rich.m)


# inspection of chain diagnostic
plot(plot.rich.m)# looks ok


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(plot.rich.m)

#residuals
m1<-residuals(plot.rich.m)
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


# #------plot richness model all sp----------------
# fixed effects
plot.rich_fitted <- cbind(plot.rich.m$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(plot.rich.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.rich_fixef <- fixef(plot.rich.m)

# coefficients for experiment-level (random) effects
plot.rich_coef <- coef(plot.rich.m)
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
                                 mutate(Slope = Estimate,
                                        Slope_lower = Q2.5,
                                        Slope_upper = Q97.5) %>% 
                                 select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code')

View(plot.rich_coef2)

View(plot.rich_fitted)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
r1<-ggplot() +
  # data
  geom_point(data = plot.rich_fitted,
             aes(x = year_trt, y = rich,
                 colour = site_code),
             size = 1.2) +
  # experiment (random) effects
  geom_segment(data = plot.rich_coef2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * xmin,
                   yend = Intercept + Slope * xmax,
                   group = site_code,
                   colour = site_code),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.rich_fitted,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = plot.rich_fitted,
            aes(x = year_trt, y = Estimate),
            size = 1.5) +
  labs(x = 'Years',
       y = 'Species richness', title= 'a) Richness') +
  #scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+
  theme_bw()+ theme(legend.position="none")





#plot biomass


summary(plot.bm.m)


# inspection of chain diagnostic
plot(plot.bm.m)  


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(plot.bm.m)


#residuals
bm1<-residuals(plot.bm.m)
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


# #------plot richness model all sp----------------
# fixed effects
plot.bm_fitted <- cbind(plot.bm.m$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(plot.bm.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.bm_fixef <- fixef(plot.bm.m)

# coefficients for experiment-level (random) effects
plot.bm_coef <- coef(plot.bm.m)
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
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code')

View(plot.bm_coef2)

View(plot.bm_fitted)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
b1<-ggplot() +
  # data
  geom_point(data = plot.bm_fitted,
             aes(x = year_trt, y = live_mass,
                 colour = site_code),
             size = 1.2) +
  # experiment (random) effects
  geom_segment(data = plot.bm_coef2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * xmin,
                   yend = Intercept + Slope * xmax,
                   group = site_code,
                   colour = site_code),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.bm_fitted,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = plot.bm_fitted,
            aes(x = year_trt, y = Estimate),
            size = 1.5) +
  labs(x = 'Years',
       y = 'Biomass', title= 'b) Biomass') +
  #scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+
  theme_bw()+ theme(legend.position="none")


grid.arrange(r1,b1,nrow=1)



#SITE

summary(site.rich.m)


# inspection of chain diagnostic
plot(site.rich.m)# looks ok


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(site.rich.m)

#residuals
srm1<-residuals(site.rich.m)
srm1<-as.data.frame(srm1)
nrow(srm1)
nrow(site2)
site2 <- site[!(is.na(site$site.rich)),]
rr.site<-cbind(site2,srm1$Estimate)
View(rr.site)

head(rr.site)
par(mfrow=c(3,1))
with(rr.site, plot(continent, srm1$Estimate))
with(rr.site, plot(habitat, srm1$Estimate))
with(rr.site, plot(site_code, srm1$Estimate))



# #------plot richness model all sp----------------
# fixed effects
site.rich_fitted <- cbind(site.rich.m$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(site.rich.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
site.rich_fixef <- fixef(site.rich.m)

# coefficients for experiment-level (random) effects
site.rich_coef <- coef(site.rich.m)
site.rich_coef
View(site2)
site.rich_coef2 <-  bind_cols(site.rich_coef$continent[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       continent = rownames(site.rich_coef$continent[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              site.rich_coef$continent[,,'year_trt'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(site2 %>% 
               group_by(continent) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'continent')

View(site.rich_coef2)

View(site.rich_fitted)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
r1<-ggplot() +
  # data
  geom_point(data = site.rich_fitted,
             aes(x = year_trt, y = rich,
                 colour = continent),
             size = 1.2) +
  # experiment (random) effects
  geom_segment(data = site.rich_coef2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * xmin,
                   yend = Intercept + Slope * xmax,
                   group = continent,
                   colour = continent),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = site.rich_fitted,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = site.rich_fitted,
            aes(x = year_trt, y = Estimate),
            size = 1.5) +
  labs(x = 'Years',
       y = 'Species richness', title= 'a) Richness') +
  #scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+
  theme_bw()+ theme(legend.position="none")





#plot biomass


summary(site.bm.m)


# inspection of chain diagnostic
plot(site.bm.m)  


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(site.bm.m)


#residuals
bm1<-residuals(site.bm.m)
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
with(rb.plot, plot(continent, bm1$Estimate))
with(rb.plot, plot(block, bm1$Estimate))
with(rb.plot, plot(plot, bm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
site.bm_fitted <- cbind(site.bm.m$data,
                        # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                        fitted(site.bm.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
site.bm_fixef <- fixef(site.bm.m)

# coefficients for experiment-level (random) effects
site.bm_coef <- coef(site.bm.m)
site.bm_coef 
site.bm_coef2 <-  bind_cols(site.bm_coef$continent[,,'Intercept'] %>% 
                              as_tibble() %>% 
                              mutate(Intercept = Estimate,
                                     Intercept_lower = Q2.5,
                                     Intercept_upper = Q97.5,
                                     continent = rownames(site.bm_coef$continent[,,'Intercept'])) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                            site.bm_coef$continent[,,'year_trt'] %>% 
                              as_tibble() %>% 
                              mutate(Slope = Estimate,
                                     Slope_lower = Q2.5,
                                     Slope_upper = Q97.5) %>% 
                              select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(continent) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'continent')

View(site.bm_coef2)

View(site.bm_fitted)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
b1<-ggplot() +
  # data
  geom_point(data = site.bm_fitted,
             aes(x = year_trt, y = live_mass,
                 colour = continent),
             size = 1.2) +
  # experiment (random) effects
  geom_segment(data = site.bm_coef2,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * xmin,
                   yend = Intercept + Slope * xmax,
                   group = continent,
                   colour = continent),
               size = 1.2) +
  # uncertainy in fixed effect
  geom_ribbon(data = site.bm_fitted,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = site.bm_fitted,
            aes(x = year_trt, y = Estimate),
            size = 1.5) +
  labs(x = 'Years',
       y = 'Biomass', title= 'b) Biomass') +
  #scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+
  theme_bw()+ theme(legend.position="none")


grid.arrange(r1,b1,nrow=1)

