

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

View(p.all)
plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )
summary(plot)



#shanes links
sp <- read.csv("~/Dropbox/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all  <- read.csv('~/Dropbox/Projects/NutNet/Data/cumulative_time_only4.csv', sep=',') %>% 
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

load('~/Dropbox/Projects/NutNet/Model_fits/sl.n.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
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
save(sl.trt_fitted.npk,sl.trt_fitted.ctl,sl.trt_coef3,file = 'sl.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')



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


# sl.trt_fitted.npk$SL<- -abs(sl.trt_fitted.npk$SL.p)
# sl.trt_coef3$Intercept.n<- -(sl.trt_coef3$Intercept)
# sl.trt_coef3$TE.n<- -(sl.trt_coef3$TE)
# sl.trt_coef3$ISlope.n<- -(sl.trt_coef3$ISlope)
# sl.trt_coef3$TESlope.n<- -(sl.trt_coef3$TESlope)
# sl.trt_fitted.npk$Estimate.n<- -(sl.trt_fitted.npk$Estimate)
# 
# sl.trt_fitted.npk$Q2.5.n<- -abs(sl.trt_fitted.npk$Q2.5)
# sl.trt_fitted.npk$Q97.5.n<- -abs(sl.trt_fitted.npk$Q97.5)
# sl.trt_fitted.ctl$Estimate.n<- -(sl.trt_fitted.ctl$Estimate)
# 
# sl.trt_fitted.ctl$Q2.5.n<- -abs(sl.trt_fitted.ctl$Q2.5)
# sl.trt_fitted.ctl$Q97.5.n<- -abs(sl.trt_fitted.ctl$Q97.5)



sl.trt_coef3$xs<-1

#View(sl.trt_coef3)
sl.trtm<-ggplot() +
  geom_point(data = sl.trt_fitted.npk,
             aes(x = year.y, y = SL,
                  colour = starting.richness),alpha=0.6,
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
  #scale_y_reverse()+
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(-400,0) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'a) Species Loss', color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")


sl.trtm

# positive
sl.trtm<-ggplot() +
  geom_point(data = sl.trt_fitted.npk,
             aes(x = year.y, y = SL.p,
                 colour = starting.richness),alpha=0.6,
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
  #scale_y_reverse()+
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(0,400) +
  labs(x = 'Years',
       y = expression(paste('Change in Biomass (g/' ,m^2, ')')), title= 'b)  Species Gain',  color='Starting Richness') +
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
                 colour = starting.richness), alpha =0.6,
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
       y='',
       #y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'b) Species Gain', color='Starting Richness') +
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
                 colour = starting.richness), alpha=0.6,
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
       y= '',
      # y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
       title= 'c) Persistent Species',  color='Starting Richness') +
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



load('~/Dropbox/Projects/NutNet/Model_fits/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s




sgain.trt_fitted <- cbind(s.gain.s$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(s.gain.s, re_formula = NA)) %>% 
  as_tibble() 

as.data.frame(sgain.trt_fitted)
p.dat3<-p.dat2 %>% 
  group_by(continent,site_code,block,plot,trt.xy,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))

View(p.dat3)
View(sgain.trt_fitted)
sgain.trt_fitted3<-left_join(sgain.trt_fitted,p.dat3)

sgain.trt_fitted3$starting.richness <- ifelse(sgain.trt_fitted3$r.rich >= 1 & sgain.trt_fitted3$r.rich <= 5, '1-5 species',
                                           ifelse(sgain.trt_fitted3$r.rich >=6 & sgain.trt_fitted3$r.rich <=10, '6-10',
                                                  ifelse(sgain.trt_fitted3$r.rich >=11 & sgain.trt_fitted3$r.rich <=15, '11-15',    
                                                         ifelse(sgain.trt_fitted3$r.rich >=16 & sgain.trt_fitted3$r.rich <=20, '16-20',
                                                                ifelse(sgain.trt_fitted3$r.rich >=21 & sgain.trt_fitted3$r.rich <=25, '21-25',
                                                                       ifelse(sgain.trt_fitted3$r.rich >=26, '>26', 'other'))))))


sgain.trt_fitted.npk<-sgain.trt_fitted3[sgain.trt_fitted3$trt.y %in% c('NPK'),]
sgain.trt_fitted.ctl<-sgain.trt_fitted3[sgain.trt_fitted3$trt.y %in% c('Control'),]
View(sgain.trt_fitted.npk)

View(p.dat3)
View(sgain.trt_fitted3)


# fixed effect coefficients (I want these for the coefficient plot)
sgain.trt_fixef <- fixef(s.gain.s)

# coefficients for experiment-level (random) effects
sgain.trt_coef <- coef(s.gain.s)
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
                         s.rich = mean(x.rich),
                         r.rich = round(s.rich)),
             by = 'site_code')

View(sgain.trt_coef2)

sgain.trt_coef2$starting.richness <- ifelse(sgain.trt_coef2$r.rich >= 1 & sgain.trt_coef2$r.rich <= 5, '1-5 species',
                                         ifelse(sgain.trt_coef2$r.rich >=6 & sgain.trt_coef2$r.rich <=10, '6-10',
                                                ifelse(sgain.trt_coef2$r.rich >=11 & sgain.trt_coef2$r.rich <=15, '11-15',    
                                                       ifelse(sgain.trt_coef2$r.rich >=16 & sgain.trt_coef2$r.rich <=20, '16-20',
                                                              ifelse(sgain.trt_coef2$r.rich >=21 & sgain.trt_coef2$r.rich <=25, '21-25',
                                                                     ifelse(sgain.trt_coef2$r.rich >=26, '>26', 'other'))))))

View(sgain.trt_coef3)

View(sgain.trt_fitted2)

dat<-distinct(plot, site_code, continent,habitat)

sgain.trt_coef3<-full_join(sgain.trt_coef2,dat)


rm(sgain.trt.i)
setwd('~/Dropbox/Projects/NutNet/Data/')
save(sgain.trt_fitted.npk,sgain.trt_fitted.ctl,sgain.trt_coef3,file = 'sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')


summary(sgain.trt_fitted3)
summary(sgain.trt_coef3)

is.factor(sgain.trt_fitted.npk$starting.richness)
is.factor(sgain.trt_coef3$starting.richness)

sgain.trt_fitted.npk<-sgain.trt_fitted.npk[complete.cases(sgain.trt_fitted.npk$starting.richness), ]
sgain.trt_coef3<-sgain.trt_coef3[complete.cases(sgain.trt_coef3$starting.richness), ]
View(sgain.trt_coef3)
View(sgain.trt_fitted.npk)

sgain.trt_fitted.npk$starting.richness <- factor(sgain.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sgain.trt_coef3$starting.richness <- factor(sgain.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sgain.trt_coef3$xs<-1
#gai

sgain.trtm<-ggplot()  +
  # data
  geom_point(data = sgain.trt_fitted.npk,
             aes(x = year.y, y = s.gain,
                 colour = starting.richness), alpha=0.6,
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=sgain.trt_fitted.npk,
  #             aes(x = year.y, y = SG,
  #                 colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  # uncertainy in fixed effect
  geom_segment(data = sgain.trt_coef3,
               aes(x = xs,
                   xend = xmax,
                   y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                   # group = starting.richness,
                   colour = starting.richness),
               size = .7) +
  geom_ribbon(data = sgain.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sgain.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sgain.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sgain.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  #scale_y_continuous(trans = 'log', breaks=c(1,2,4,6,8,16,24,64,512,1024,2048,4096)) +
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  #ylim(0,400) +
  labs(x = 'Years',
       y = expression(paste('Species Gain')), title= 'b) Species Gain', color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")

sgain.trtm




sloss.trt_fitted <- cbind(s.loss.s$data,
                       # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                       fitted(s.loss.s, re_formula = NA)) %>% 
  as_tibble()


p.dat3<-p.dat2 %>% 
  group_by(site_code,continent,block,plot,trt.y,year.x,year.y,year.y.m) %>% 
  summarise(s.rich = mean(x.rich),
            r.rich = round(s.rich))

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

sloss.trt_fitted3$starting.richness <- ifelse(sloss.trt_fitted3$r.rich >= 1 & sloss.trt_fitted3$r.rich <= 5, '1-5 species',
                                           ifelse(sloss.trt_fitted3$r.rich >=6 & sloss.trt_fitted3$r.rich <=10, '6-10',
                                                  ifelse(sloss.trt_fitted3$r.rich >=11 & sloss.trt_fitted3$r.rich <=15, '11-15',    
                                                         ifelse(sloss.trt_fitted3$r.rich >=16 & sloss.trt_fitted3$r.rich <=20, '16-20',
                                                                ifelse(sloss.trt_fitted3$r.rich >=21 & sloss.trt_fitted3$r.rich <=25, '21-25',
                                                                       ifelse(sloss.trt_fitted3$r.rich >=26, '>26', 'other'))))))

sloss.trt_fitted.npk<-sloss.trt_fitted3[sloss.trt_fitted3$trt.y %in% c('NPK'),]
sloss.trt_fitted.ctl<-sloss.trt_fitted3[sloss.trt_fitted3$trt.y %in% c('Control'),]
View(sloss.trt_fitted.npk)

# fixed effect coefficients -coefficient plot
sloss.trt_fixef <- fixef(s.loss.s)

# coefficients for experiment-level (random) effects
sloss.trt_coef <- coef(s.loss.s)

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
                         cxmax = max(year.y.m),
                         s.rich = mean(x.rich),
                         r.rich = round(s.rich)
               ),
             by = 'site_code')

View(p.dat3)
View(sloss.trt_coef2)

sloss.trt_coef2$starting.richness <- ifelse(sloss.trt_coef2$r.rich >= 1 & sloss.trt_coef2$r.rich <= 5, '1-5 species',
                                         ifelse(sloss.trt_coef2$r.rich >=6 & sloss.trt_coef2$r.rich <=10, '6-10',
                                                ifelse(sloss.trt_coef2$r.rich >=11 & sloss.trt_coef2$r.rich <=15, '11-15',    
                                                       ifelse(sloss.trt_coef2$r.rich >=16 & sloss.trt_coef2$r.rich <=20, '16-20',
                                                              ifelse(sloss.trt_coef2$r.rich >=21 & sloss.trt_coef2$r.rich <=25, '21-25',
                                                                     ifelse(sloss.trt_coef2$r.rich >=26, '>26', 'other'))))))


dat<-distinct(p.dat2, site_code, continent,habitat)

sloss.trt_coef3<-inner_join(sloss.trt_coef2,dat)
View(sloss.trt_coef3)

setwd('~/Dropbox/Projects/NutNet/Data/')
save(sloss.trt_fitted.npk,sloss.trt_fitted.ctl,sloss.trt_coef3,file = 'sloss.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')



library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


View(sloss.trt_fitted.ctl)
View(sloss.trt_fitted.npk)
View(sloss.trt_coef3)

as.factor(as.character(sloss.trt_fitted.npk$starting.richness))
as.factor(as.character(sloss.trt_coef3$starting.richness))

sloss.trt_fitted.npk<-sloss.trt_fitted.npk[complete.cases(sloss.trt_fitted.npk$starting.richness), ]
sloss.trt_coef3<-sloss.trt_coef3[complete.cases(sloss.trt_coef3$starting.richness), ]

sloss.trt_fitted.npk$starting.richness <- factor(sloss.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sloss.trt_coef3$starting.richness <- factor(sloss.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

View(sloss.trt_fitted.npk)
View(sloss.trt_coef3)


# sloss.trt_fitted.npk$s.loss<- -abs(sloss.trt_fitted.npk$s.loss.p)
# sloss.trt_coef3$Intercept.n<- -(sloss.trt_coef3$Intercept)
# sloss.trt_coef3$TE.n<- -(sloss.trt_coef3$TE)
# sloss.trt_coef3$ISlope.n<- -(sloss.trt_coef3$ISlope)
# sloss.trt_coef3$TESlope.n<- -(sloss.trt_coef3$TESlope)
# sloss.trt_fitted.npk$Estimate.n<- -(sloss.trt_fitted.npk$Estimate)
# 
# sloss.trt_fitted.npk$Q2.5.n<- -abs(sloss.trt_fitted.npk$Q2.5)
# sloss.trt_fitted.npk$Q97.5.n<- -abs(sloss.trt_fitted.npk$Q97.5)
# sloss.trt_fitted.ctl$Estimate.n<- -(sloss.trt_fitted.ctl$Estimate)
# 
# sloss.trt_fitted.ctl$Q2.5.n<- -abs(sloss.trt_fitted.ctl$Q2.5)
# sloss.trt_fitted.ctl$Q97.5.n<- -abs(sloss.trt_fitted.ctl$Q97.5)
# 


sloss.trt_coef3$xs<-1

View(sloss.trt_coef3)
sloss.trtm<-ggplot() +
  geom_point(data = sloss.trt_fitted.npk,
             aes(x = year.y, y = s.loss,
                 colour = starting.richness), alpha=0.6,
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = sloss.trt_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax),
                   # group = site_code,
                   colour = starting.richness
               ),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sloss.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sloss.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sloss.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sloss.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  #scale_y_reverse()+
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  #ylim(-400,0) +
  labs(x = 'Years',
       y = expression(paste('Species Loss')), title= 'a) Species Loss', color='Starting Richness') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")


sloss.trtm

grid_arrange_shared_legend(sloss.trtm,sgain.trtm,nrow=1)



# positive
sloss.trtm<-ggplot() +
  geom_point(data = sloss.trt_fitted.npk,
             aes(x = year.y, y = SL.p,
                 colour = starting.richness),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  geom_segment(data = sloss.trt_coef3,
               aes(x = xs, 
                   xend = xmax,
                   y = (Intercept + TE  + (ISlope+TESlope) * cxmin),
                   yend = (Intercept + TE + (ISlope+TESlope) * cxmax),
                   # group = site_code,
                   colour = starting.richness
               ),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sloss.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sloss.trt_fitted.npk,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = sloss.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = sloss.trt_fitted.ctl,
            aes(x = year.y, y = Estimate),
            size = 1.5,  linetype= "dashed") +
  #scale_y_reverse()+
  scale_x_continuous(breaks=c(1,3,6,9,11)) +
  ylim(0,400) +
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


sloss.trtm
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





