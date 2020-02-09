

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
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only4.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )
summary(plot)



#shanes links
sp <- read.csv("~/Dropbox/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all  <- read.csv('~/Dropbox/Projects/NutNet/Data/cumulative_time_only2.csv', sep=',') %>% 
  as_tibble() 



p.all<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all$f.year.y<-as.factor(p.all$year.y)
p.all$plot<-as.factor(p.all$plot)
p.all$site_code<-as.factor(p.all$site_code)

dat<-distinct(plot, site_code, continent,habitat)
p.dat2<-inner_join(p.all,dat)
View(p.dat2)

#p.dat2$year.y.m<-p.dat2$year.y-mean(p.dat2$year.y)
#p.dat2$SL.p<-abs(p.dat2$SL)

# s loss, gain and change metrics
# p.dat2$s.loss <- -1*(p.dat2$x.rich - p.dat2$c.rich)
# p.dat2$s.gain <- p.dat2$y.rich - p.dat2$c.rich
# p.dat2$s.change <- p.dat2$y.rich - p.dat2$x.rich
# 
# p.dat2$s.loss.p<-abs(p.dat2$s.loss)

# models
load('~/Dropbox/Projects/NutNet/Model_fits/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.g

load('~/Dropbox/Projects/NutNet/Model_fits/sl.Rdata') # sl.s
# load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s


# #------plot richness model all sp----------------
# fixed effects
sl.trt_fitted <- cbind(sl.s$data,
                     # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                     fitted(sl.s, re_formula = NA)) %>% 
  as_tibble()


p.dat3<-p.dat2 %>% 
  group_by(site_code,continent,block,plot,trt.y,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))

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

sl.trt_fitted3$starting.richness <- ifelse(sl.trt_fitted3$r.rich >= 1 & sl.trt_fitted3$r.rich <= 5, '1-5 species',
                                   ifelse(sl.trt_fitted3$r.rich >=6 & sl.trt_fitted3$r.rich <=10, '6-10',
                                          ifelse(sl.trt_fitted3$r.rich >=11 & sl.trt_fitted3$r.rich <=15, '11-15',    
                                                 ifelse(sl.trt_fitted3$r.rich >=16 & sl.trt_fitted3$r.rich <=20, '16-20',
                                                        ifelse(sl.trt_fitted3$r.rich >=21 & sl.trt_fitted3$r.rich <=25, '21-25',
                                                               ifelse(sl.trt_fitted3$r.rich >=26, '>26', 'other'))))))

sl.trt_fitted.npk<-sl.trt_fitted3[sl.trt_fitted3$trt.y %in% c('NPK'),]
sl.trt_fitted.ctl<-sl.trt_fitted3[sl.trt_fitted3$trt.y %in% c('Control'),]
View(sl.trt_fitted.npk)

# fixed effect coefficients -coefficient plot
sl.trt_fixef <- fixef(sl.s)

# coefficients for experiment-level (random) effects
sl.trt_coef <- coef(sl.s)

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
                         cxmax = max(year.y.m),
                         s.rich = mean(x.rich),
                         r.rich = round(s.rich)
                         ),
             by = 'site_code')

View(p.dat3)
View(sl.trt_coef2)

sl.trt_coef2$starting.richness <- ifelse(sl.trt_coef2$r.rich >= 1 & sl.trt_coef2$r.rich <= 5, '1-5 species',
                                 ifelse(sl.trt_coef2$r.rich >=6 & sl.trt_coef2$r.rich <=10, '6-10',
                                        ifelse(sl.trt_coef2$r.rich >=11 & sl.trt_coef2$r.rich <=15, '11-15',    
                                               ifelse(sl.trt_coef2$r.rich >=16 & sl.trt_coef2$r.rich <=20, '16-20',
                                                      ifelse(sl.trt_coef2$r.rich >=21 & sl.trt_coef2$r.rich <=25, '21-25',
                                                             ifelse(sl.trt_coef2$r.rich >=26, '>26', 'other'))))))


dat<-distinct(p.dat2, site_code, continent,habitat)

sl.trt_coef3<-inner_join(sl.trt_coef2,dat)
View(sl.trt_coef3)

setwd('~/Dropbox/Projects/NutNet/Data/')
save(sl.trt_fitted.npk,sl.trt_fitted.ctl,sl.trt_coef3,file = 'sl.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sl.mod.dat.Rdata')



library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


View(sl.trt_fitted.ctl)
View(sl.trt_fitted.npk)
View(sl.trt_coef3)

as.factor(as.character(sl.trt_fitted.npk$starting.richness))
as.factor(as.character(sl.trt_coef3$starting.richness))

sl.trt_fitted.npk<-sl.trt_fitted.npk[complete.cases(sl.trt_fitted.npk$starting.richness), ]
sl.trt_coef3<-sl.trt_coef3[complete.cases(sl.trt_coef3$starting.richness), ]

sl.trt_fitted.npk$starting.richness <- factor(sl.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sl.trt_coef3$starting.richness <- factor(sl.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

View(sl.trt_fitted.npk)
View(sl.trt_coef3)

sl.trt_coef3$xs<-1

View(sl.trt_coef3)
sl.trtm<-ggplot() +
  geom_point(data = sl.trt_fitted.npk,
             aes(x = year.y, y = SL.p,
                  colour = starting.richness),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = sl.trt_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax),
                   # group = site_code,
                    colour = starting.richness
                   ),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sl.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sl.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sl.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sl.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  #scale_y_continuous(#trans = reverselog_trans(), breaks=c(1,2,4,6,8,16,24,64,512,1024)) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  #ylim(0,400) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'a) Change in Biomass due to SL') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")


sl.trtm




# #########################################GAINS ####################################################


color_scheme_set("purple")
pp_check(sg.trt.i)
pp_check(sg.trt.i, type = "hist")
#marginal effects
marginal_effects(sg.trt.i, surface = TRUE)
marginal_smooths(sg.trt.i)


summary(sg.trt.i)
#residuals
sg.trtm1<-residuals(sg.trt.i)
sg.trtm1<-as.data.frame(sg.trtm1)
nrow(sg.trtm1)
nrow(p.dat2)
sg.trt.plot<-cbind(p.dat2,sg.trtm1$Estimate)
sg.trt.plot2<-inner_join(sg.trt.plot,dat)

par(mfrow=c(3,2))
with(sg.trt.plot2, plot(continent, sg.trtm1$Estimate))
with(sg.trt.plot2, plot(habitat, sg.trtm1$Estimate))
with(sg.trt.plot, plot(site_code, sg.trtm1$Estimate))
with(sg.trt.plot2, plot(block, sg.trtm1$Estimate))
with(sg.trt.plot2, plot(plot, sg.trtm1$Estimate))
with(sg.trt.plot2, plot(f.year.y, sg.trtm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects

sg.trt_fitted <- cbind(sg.s$data,
                     # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                     fitted(sg.s, re_formula = NA)) %>% 
  as_tibble() 

as.data.frame(sg.trt_fitted)
p.dat3<-p.dat2 %>% 
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))

View(p.dat3)
View(sg.trt_fitted)
sg.trt_fitted3<-left_join(sg.trt_fitted,p.dat3)

sg.trt_fitted3$starting.richness <- ifelse(sg.trt_fitted3$r.rich >= 1 & sg.trt_fitted3$r.rich <= 5, '1-5 species',
                                  ifelse(sg.trt_fitted3$r.rich >=6 & sg.trt_fitted3$r.rich <=10, '6-10',
                                         ifelse(sg.trt_fitted3$r.rich >=11 & sg.trt_fitted3$r.rich <=15, '11-15',    
                                                ifelse(sg.trt_fitted3$r.rich >=16 & sg.trt_fitted3$r.rich <=20, '16-20',
                                                       ifelse(sg.trt_fitted3$r.rich >=21 & sg.trt_fitted3$r.rich <=25, '21-25',
                                                              ifelse(sg.trt_fitted3$r.rich >=26, '>26', 'other'))))))


sg.trt_fitted.npk<-sg.trt_fitted3[sg.trt_fitted3$trt.y %in% c('NPK'),]
sg.trt_fitted.ctl<-sg.trt_fitted3[sg.trt_fitted3$trt.y %in% c('Control'),]
View(sg.trt_fitted.npk)

View(p.dat3)
View(sg.trt_fitted3)


# fixed effect coefficients (I want these for the coefficient plot)
sg.trt_fixef <- fixef(sg.s)

# coefficients for experiment-level (random) effects
sg.trt_coef <- coef(sg.s)
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
                         cxmax = max(year.y.m),
                         s.rich = mean(x.rich),
                         r.rich = round(s.rich)),
             by = 'site_code')

View(sg.trt_coef2)

sg.trt_coef2$starting.richness <- ifelse(sg.trt_coef2$r.rich >= 1 & sg.trt_coef2$r.rich <= 5, '1-5 species',
                                ifelse(sg.trt_coef2$r.rich >=6 & sg.trt_coef2$r.rich <=10, '6-10',
                                       ifelse(sg.trt_coef2$r.rich >=11 & sg.trt_coef2$r.rich <=15, '11-15',    
                                              ifelse(sg.trt_coef2$r.rich >=16 & sg.trt_coef2$r.rich <=20, '16-20',
                                                     ifelse(sg.trt_coef2$r.rich >=21 & sg.trt_coef2$r.rich <=25, '21-25',
                                                            ifelse(sg.trt_coef2$r.rich >=26, '>26', 'other'))))))

View(sg.trt_coef3)

View(sg.trt_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

sg.trt_coef3<-full_join(sg.trt_coef2,dat)


rm(sg.trt.i)
setwd('~/Dropbox/Projects/NutNet/Data/')
save(sg.trt_fitted.npk,sg.trt_fitted.ctl,sg.trt_coef3,file = 'sg_dat.Rdata')
load('~/Desktop/Academic/R code/NutNet/sg_dat.Rdata')


summary(sg.trt_fitted3)
summary(sg.trt_coef3)

is.factor(sg.trt_fitted.npk$starting.richness)
is.factor(sg.trt_coef3$starting.richness)

sg.trt_fitted.npk<-sg.trt_fitted.npk[complete.cases(sg.trt_fitted.npk$starting.richness), ]
sg.trt_coef3<-sg.trt_coef3[complete.cases(sg.trt_coef3$starting.richness), ]
View(sg.trt_coef3)

sg.trt_fitted.npk$starting.richness <- factor(sg.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.trt_coef3$starting.richness <- factor(sg.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sg.trt_coef3$xs<-1
#gai
sg.trtm<-ggplot()  +
  # data
  geom_point(data = sg.trt_fitted.npk,
             aes(x = year.y, y = SG,
                 colour = starting.richness),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=sg.trt_fitted.npk,
  #             aes(x = year.y, y = SG,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  # uncertainy in fixed effect
    geom_segment(data = sg.trt_coef3,
                 aes(x = xs,
                     xend = xmax,
                     y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                     yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                     # group = starting.richness,
                     colour = starting.richness),
                 size = .7) +
  geom_ribbon(data = sg.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sg.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sg.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sg.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  #scale_y_continuous(trans = 'log', breaks=c(1,2,4,6,8,16,24,64,512,1024,2048,4096)) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(0,400) +
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

sg.trtm


grid_arrange_shared_legend(sl.trtm,sg.trtm,nrow=1)
grid_arrange_shared_legend(s.loss.im,s.gain.im,nrow=1)

grid_arrange_shared_legend(s.loss.im,sl.trtm,nrow=1)
grid_arrange_shared_legend(s.gain.im,sg.trtm,nrow=1)

grid_arrange_shared_legend(s.loss.im,sl.trtm,s.gain.im,sg.trtm,nrow=2,ncol=2)


########################################################################################################################
#####################################CDE model###################################################################################
########################################################################################################################

summary(CDE.trt.i)
color_scheme_set("purple")
pairs(CDE.trt)

# inspection of chain diagnostic
plot(CDE.trt)  


pp_check(CDE.trt)

pp_check(CDE.trt, type = "hist")
#marginal effects
marginal_effects(CDE.trt, surface = TRUE)
marginal_smooths(CDE.trt)



#residuals
cm1<-residuals(CDE.trt.i)
cm1<-as.data.frame(cm1)
nrow(cm1)
nrow(p.dat2)
#p.dat4<-p.dat3[complete.cases(p.dat3$CDE), ]
cde.plot<-cbind(p.dat2,cm1$Estimate)
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
cde_fitted <- cbind(CDE.s$data,
                    # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                    fitted(CDE.s, re_formula = NA)) %>% 
  as_tibble() 
as.data.frame(cde_fitted)

p.dat3<-p.dat2 %>% 
             group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
             summarise(s.rich = mean(x.rich),
                       r.rich = round(s.rich))

View(p.dat3)
p.dat3$block<-as.factor(p.dat3$block)
#cde_fitted2<-full_join(cde_fitted,dat)
nrow(cde_fitted)
cde_fitted3<-inner_join(cde_fitted,p.dat3)
View(cde_fitted3)
nrow(cde_fitted3)

#cde_fitted2<-inner_join(cde_fitted,p.all5,  by = c('site_code', 'year.y'))
cde_fitted3$starting.richness <- ifelse(cde_fitted3$r.rich >= 1 & cde_fitted3$r.rich <= 5, '1-5 species',
                                  ifelse(cde_fitted3$r.rich >=6 & cde_fitted3$r.rich <=10, '6-10',
                                         ifelse(cde_fitted3$r.rich >=11 & cde_fitted3$r.rich <=15, '11-15',    
                                                ifelse(cde_fitted3$r.rich >=16 & cde_fitted3$r.rich <=20, '16-20',
                                                       ifelse(cde_fitted3$r.rich >=21 & cde_fitted3$r.rich <=25, '21-25',
                                                              ifelse(cde_fitted3$r.rich >=26, '>26', 'other'))))))


cde_fitted.npk<-cde_fitted3[cde_fitted3$trt.y %in% c('NPK'),]
cde_fitted.ctl<-cde_fitted3[cde_fitted3$trt.y %in% c('Control'),]
View(cde_fitted.npk)
View(cde_fitted.ctl)

cde_fixef <- fixef(CDE.s)

cde_coef <- coef(CDE.s)
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
  inner_join(p.dat2 %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y),
                         cxmin = min(year.y.m),
                         cxmax = max(year.y.m),
                         s.rich = mean(x.rich),
                         r.rich = round(s.rich)),
             by = 'site_code')

cde_coef2$starting.richness <- ifelse(cde_coef2$r.rich >= 1 & cde_coef2$r.rich <= 5, '1-5 species',
                                ifelse(cde_coef2$r.rich >=6 & cde_coef2$r.rich <=10, '6-10',
                                       ifelse(cde_coef2$r.rich >=11 & cde_coef2$r.rich <=15, '11-15',    
                                              ifelse(cde_coef2$r.rich >=16 & cde_coef2$r.rich <=20, '16-20',
                                                     ifelse(cde_coef2$r.rich >=21 & cde_coef2$r.rich <=25, '21-25',
                                                            ifelse(cde_coef2$r.rich >=26, '>26', 'other'))))))

View(cde_coef2)

View(cde_fitted)


cde_coef3<-full_join(cde_coef2,dat)


rm(cde.trt.i)
setwd('~/Dropbox/Projects/NutNet/Data/')
save(cde_fitted.npk,cde_fitted.ctl,cde_coef3,file = 'cde.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')


cde_fitted.npk<-cde_fitted.npk[complete.cases(cde_fitted.npk$starting.richness), ]
cde_coef3<-cde_coef3[complete.cases(cde_coef3$starting.richness), ]

cde_fitted.npk$starting.richness <- factor(cde_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
cde_coef3$starting.richness <- factor(cde_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


View(cde_coef3)
cde_coef3$xs<-1
#cde
cdem<-ggplot() +
  # data
  geom_point(data = cde_fitted.npk,
             aes(x = year.y, y = CDE,
                 colour = starting.richness),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = cde_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope)  * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = cde_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = cde_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = cde_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = cde_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  #scale_y_continuous(trans = sign_sqrt #, breaks=c(8,64,512,1024,2048,4096)
  #) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(-500,1000)+
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'c) Biomass Change in Persistent Species') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")

cdem


grid_arrange_shared_legend(sl.trtm,sg.trtm,cdem,nrow=1)

grid_arrange_shared_legend(sl.trtm,cdem,nrow=1)



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
  geom_errorbar(data = sl.trt_coef4, aes(x = reorder(site_code,TESlope),ymin = TESlope,
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
  geom_errorbar(data = sg.trt_coef5, aes(x = reorder(site_code,Slope.sl),ymin = TESlope,
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
cde_coef4
cdem2<-ggplot() + 
  geom_point(data = cde_coef4, aes(x = reorder(site_code,TESlope), y = TESlope,colour = continent),size = 2) +
  geom_errorbar(data = cde_coef4, aes(x = reorder(site_code,TESlope),ymin = TESlope_lower,
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

