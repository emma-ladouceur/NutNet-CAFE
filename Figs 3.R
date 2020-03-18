
library(brms)
library(tidyverse)
library(ggplot2)

# Figs 3-4


# FIGURE 3
# DERIVATIVES DELTA OVER TIME
# CAFE BAYES VECTORS

# SP LOSS VECTOR, SP GAIN VECTOR, CDE VECTOR... but what about....transformations?
# data
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )


#price partition data
pp <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only4.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(pp)

# models
load('~/Dropbox/Projects/NutNet/Model_fits/sl.Rdata') # sl..s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Model_fits/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s
#load('~/Dropbox/Projects/NutNet/Model_fits/rich3.Rdata') # plot.rich.g


summary(sl.trt.h.t)
s.loss.p.d
CDE.s.t
sloss_fixef

sloss_fixef <- fixef(s.loss.s)
sgain_fixef <- fixef(s.gain.s)
sl_fixef <- fixef(sl.s)
sg_fixef <- fixef(sg.s)
cde_fixef <- fixef(CDE.s)

sgain_fixef<-as.data.frame(sgain_fixef)
sloss_fixef<-as.data.frame(sloss_fixef)
sl_fixef<-as.data.frame(sl_fixef)
sg_fixef<-as.data.frame(sg_fixef)
cde_fixef<-as.data.frame(cde_fixef)

sgain_fixef$names <- rownames(sgain_fixef)
sloss_fixef$names <- rownames(sloss_fixef)
sl_fixef$names <- rownames(sl_fixef)
sg_fixef$names <- rownames(sg_fixef)
cde_fixef$names <- rownames(cde_fixef)
sgain_fixef
sloss_fixef
sl_fixef

# 
sloss_fixef$Estimate<- (sloss_fixef$Estimate)
sloss_fixef$Q2.5<- (sloss_fixef$Q2.5)
sloss_fixef$Q97.5<- (sloss_fixef$Q97.5)

sl_fixef$Estimate<- (sl_fixef$Estimate)
sl_fixef$Q2.5<- (sl_fixef$Q2.5)
sl_fixef$Q97.5<- (sl_fixef$Q97.5)
sl_fixef


 sgain_fixef

sg_fixef

sgain_fixef$Model<-'Sgain'
sloss_fixef$Model<-'Sloss'
sl_fixef$Model<-'SL'
sg_fixef$Model<-'SG'
cde_fixef$Model<-'CDE'
fixedf_pp<-bind_rows(sl_fixef,sg_fixef,cde_fixef,sloss_fixef,sgain_fixef)
View(fixedf_pp)


ggplot(data = fixedf_pp)+
  geom_point(aes(x=Estimate[16], #losses
                 y=Estimate[4]),
                 colour="red",
                 size=0.7)+
  geom_errorbar(aes(x=Estimate[16],
                    ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_errorbarh(aes(y=Estimate[4],
                    xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_point(aes(x=Estimate[16]+Estimate[20], #gains
             y= Estimate[8]),
                 colour="blue",size=0.7)+
  geom_errorbar(aes(x=Estimate[16]+Estimate[20],
                ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "blue", size = 0.35,alpha=0.3) +
  # geom_errorbarh(aes(y=Estimate[8],
  #                   xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "blue", size = 0.35,alpha=0.3) +
   geom_point(aes(x=Estimate[16]+Estimate[20], #persistent
                 y=Estimate[12]),
             colour="purple",size=0.7)+
  geom_errorbar(aes(x=Estimate[16]+Estimate[20],
                    ymin = Q2.5[8]+Q2.5[12], ymax = Q97.5[4]+Q97.5[8]+Q97.5[12]),width=0,colour = "purple", size = 0.35,alpha=0.3) +
  # geom_errorbarh(aes(y=Estimate[8]+Estimate[12],
  #                    xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "purple", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp, # cde
               aes(x = Estimate[16]+Estimate[20], #losses + gains
                   xend = Estimate[16]+Estimate[20], # losses + gains
                   y = Estimate[8],   # effect of sl + effect of sg on bm
                   yend = Estimate[12]), # effect of sl + sg + cde on biomass
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # gains
               aes(x = Estimate[16], # start at losses
                   xend = Estimate[16]+Estimate[20], #species losses + species gains
                   y = Estimate[4],    # effect of sl on biomass
                   yend = Estimate[8]),  # effect of sl + effect of sg on bm
               colour= "blue",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
   geom_segment(data = fixedf_pp, # losses
                            aes(x = 0,
                                xend = Estimate[16], # species losses
                                y = 0,
                                yend = Estimate[4]), # effect of sl on biomass
                            colour= "red",
                            size = 1.5,
                            arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
   labs(x = 'Effect on Species Richness Over Time',
       y = 'Effect on Biomass Change Over Time',
       title= 'CAFE Bayes Vector Plot') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")



# Try 2
ggplot(data = fixedf_pp)+
  geom_point(aes(x=Estimate[16], #losses
                 y=Estimate[4]),
             colour="red",
             size=0.7)+
  geom_errorbar(aes(x=Estimate[16],
                    ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_errorbarh(aes(y=Estimate[4],
                     xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_point(aes(x=Estimate[20], #gains
                 y= Estimate[8]),
             colour="blue",size=0.7)+
  geom_errorbar(aes(x=Estimate[20],
                    ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_errorbarh(aes(y=Estimate[8],
                     xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_point(aes(x=Estimate[20], #persistent
                 y=Estimate[12]),
             colour="purple",size=0.7)+
   geom_errorbar(aes(x=Estimate[20],
                     ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "purple", size = 0.35,alpha=0.3) +
   #geom_errorbarh(aes(y=Estimate[8]+Estimate[12],
     #                xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "purple", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp, # cde
               aes(x = Estimate[20], #gains
                   xend = Estimate[20], # gains
                   y = Estimate[8],   # effect of  sg on bm
                   yend = Estimate[12]), # effect of cde on biomass
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # gains
               aes(x = Estimate[16], # start at losses
                   xend = Estimate[20], # species gains effect
                   y = Estimate[4],    # effect of sl on biomass
                   yend = Estimate[8]),  # effect of sl + effect of sg on bm
               colour= "blue",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # losses
               aes(x = 0,
                   xend = Estimate[16], # species losses
                   y = 0,
                   yend = Estimate[4]), # effect of sl on biomass
               colour= "red",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  labs(x = 'Effect on Species Richness Over Time',
       y = 'Effect on Biomass Change Over Time',
       title= 'CAFE Vector Plot') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


#you could also start them all from 0?
ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate[12]),
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data = fixedf_pp,aes(x=0, #persistent
                 y=Estimate[12]),
             colour="purple",size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=0,
                    ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "purple", size = 0.55,alpha=0.3) +
   geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[20],
                   y = 0,
                   yend = Estimate[8]),
               colour= "blue",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data = fixedf_pp,aes(x=Estimate[20], #gains
                 y= Estimate[8]),
             colour="blue",size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                    ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                     xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "blue", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[16],
                   y = 0,
                   yend = Estimate[4]),
               colour= "red",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data = fixedf_pp,aes(x=Estimate[16], #losses
                 y=Estimate[4]),
             colour="red",
             size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                    ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "red", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                     xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "red", size = 0.35,alpha=0.3) 

  


##########################################
######SCRATCH THAT , GOING DOWN WRONG PATH

#fitted values
sl_fitted <- cbind(sl.trt.i$data,fitted(sl.trt.i, re_formula = NA)) %>%  as_tibble() 
sg_fitted <- cbind(sg.trt.i$data,fitted(sg.trt.i, re_formula = NA)) %>% as_tibble() 
cde_fitted <- cbind(p.CDE.trt.i$data,fitted(p.CDE.trt.i, re_formula = NA)) %>% as_tibble() 
View(sl_fitted)
head(pp)
dat<-distinct(pp,habitat,continent,site_code, trt.y,year.y.m,year.y,block, plot,x.rich,y.rich,x.func,y.func,SL.p,SL,SG,CDE,s.loss,s.gain,c.rich)
sl_fitted2<-full_join(sl_fitted,dat)
sg_fitted2<-full_join(sg_fitted,dat)
cde_fitted2<-full_join(cde_fitted,dat)

sl_fitted.npk<-sl_fitted2[sl_fitted2$trt.y %in% c('NPK'),]
sl_fitted_fitted.ctl<-sl_fitted2[sl_fitted2$trt.y %in% c('Control'),]

sg_fitted.npk<-sg_fitted2[sg_fitted2$trt.y %in% c('NPK'),]
View(sg_fitted.npk)

cde_fitted.npk<-cde_fitted2[cde_fitted2$trt.y %in% c('NPK'),]
cde_fitted_fitted.ctl<-cde_fitted2[cde_fitted2$trt.y %in% c('Control'),]



# CAFE component    richness    function
# base = c(x.rich,x.func)
# SL = c(c.rich,SL)
# SG = c(y.rich,SL+SG)
# CDE = c(y.rich,y.func)

ggplot() +
  # geom_point(data = sl_fitted.npk,
  #            aes(x = x.rich, y = x.func),
  #            colour = "black", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_point(data = sl_fitted.npk,
  #            aes(x = c.rich, y = x.func+SL),
  #            colour = "blue", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_point(data = sg_fitted.npk,
  #            aes(x = c.rich, y = x.func+SL+SG),
  #            colour = "red", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_point(data = cde_fitted.npk,
  #            aes(x = y.rich, y = y.func),
  #            colour = "purple", alpha=0.1,
  #            size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_segment(data = sl_fitted.npk,
  #              aes(x = x.rich,
  #                  xend = c.rich,
  #                  y = x.func,
  #                  yend =  x.func+SL),
  #              colour= "blue",
  #              size = 1.5,
  #              arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  # geom_segment(data = sg_fitted.npk,
  #              aes(x = c.rich,
  #                  xend = y.rich,
  #                  y = SL,
  #                  yend = x.func+SL+SG),
  #              colour= "red",
  #              size = 1.5,
  #              arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  # geom_segment(data = cde_fitted.npk,
  #              aes(x = y.rich,
  #                  xend = y.rich,
  #                  y = x.func+SL+SG,
  #                  yend = y.func),
  #              colour= "purple",
  #              size = 1.5,
  #              arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  labs(x = 'Species Richness',
       y = 'Function', title= 'Manual CAFE Plot') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     strip.background = element_rect(colour="black", fill="white"))






# posteriors


meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(meta)
View(meta)


#  mods study level dat
study_levels <- plot.rich.g$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

parnames(plot.rich.g)
study_sample_posterior <- study_levels %>%
  mutate(sloss.ctl = purrr::map(data, ~posterior_samples(s.loss.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sgain.ctl = purrr::map(data, ~posterior_samples(s.gain.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
                 sloss.trt = purrr::map(data, ~posterior_samples(s.loss.s, 
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sgain.trt = purrr::map(data, ~posterior_samples(s.gain.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()))
        


sloss.trt.i_fixef <- fixef(s.loss.s)
sgain.trt.i_fixef <- fixef(s.gain.s)




sloss_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sloss.ctl,sloss.trt) %>% 
  mutate(response = 'sloss',
         sloss.ctl_global_slope = sloss.trt.i_fixef['year.y.m','Estimate'],
         sloss.ctl_upper_slope = sloss.trt.i_fixef['year.y.m','Q97.5'],
         sloss.ctl_lower_slope = sloss.trt.i_fixef['year.y.m','Q2.5'],
         sloss.trt_global_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sloss.trt_upper_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sloss.trt_lower_slope = sloss.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) 


head(sl_posterior)
# sl_posterior$sl<-sl_posterior$sl.ctl+sl_posterior$sl.trt
# sl_posterior$sl.global<-sl_posterior$sl.ctl_global_slope + sl_posterior$sl.trt_global_slope
# sl_posterior$sl.upper<-sl_posterior$sl.ctl_upper_slope + sl_posterior$sl.trt_upper_slope
# sl_posterior$sl.lower<-sl_posterior$sl.ctl_lower_slope + sl_posterior$sl.trt_lower_slope
sloss.p<-sloss_posterior %>% inner_join(meta, by = 'site_code')

sloss.p$starting.richness <- ifelse(sloss.p$r.rich >= 1 & sloss.p$r.rich <= 5, '1-5 species',
                                 ifelse(sloss.p$r.rich >=6 & sloss.p$r.rich <=10, '6-10',
                                        ifelse(sloss.p$r.rich >=11 & sloss.p$r.rich <=15, '11-15',    
                                               ifelse(sloss.p$r.rich >=16 & sloss.p$r.rich <=20, '16-20',
                                                      ifelse(sloss.p$r.rich >=21 & sloss.p$r.rich <=25, '21-25',
                                                             ifelse(sloss.p$r.rich >=26, '>26', 'other'))))))

sloss.p$starting.richness <- factor(sloss.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sloss.p$anthropogenic<-as.factor(sloss.p$anthropogenic)
sloss.p$grazed<-as.factor(as.character(sloss.p$grazed))
sloss.p$managed<-as.factor(as.character(sloss.p$managed))
sloss.p$burned<-as.factor(as.character(sloss.p$burned))

sloss.p$site_rich_range <- ifelse(sloss.p$site_richness >= 2 & sloss.p$site_richness <= 44, '2-40 species',
                               ifelse(sloss.p$site_richness >=45 & sloss.p$site_richness <=69, '45-69',
                                      ifelse(sloss.p$site_richness >=70 & sloss.p$site_richness <=90, '70-90',    
                                             ifelse(sloss.p$site_richness >=91 & sloss.p$site_richness <=119, '90-119',
                                                    ifelse(sloss.p$site_richness >=120 & sloss.p$site_richness <=144, '120-144',
                                                           ifelse(sloss.p$site_richness >=145, '>145', 'other'))))))


write.csv(sloss.p,"~/Dropbox/Projects/NutNet/Data/sloss_posteriors.csv")




sgain_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sgain.ctl,sgain.trt) %>% 
  mutate(response = 'sgain',
         sgain.ctl_global_slope = sgain.trt.i_fixef['year.y.m','Estimate'],
         sgain.ctl_upper_slope = sgain.trt.i_fixef['year.y.m','Q97.5'],
         sgain.ctl_lower_slope = sgain.trt.i_fixef['year.y.m','Q2.5'],
         sgain.trt_global_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sgain.trt_upper_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sgain.trt_lower_slope = sgain.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) 


head(sl_posterior)
# sl_posterior$sl<-sl_posterior$sl.ctl+sl_posterior$sl.trt
# sl_posterior$sl.global<-sl_posterior$sl.ctl_global_slope + sl_posterior$sl.trt_global_slope
# sl_posterior$sl.upper<-sl_posterior$sl.ctl_upper_slope + sl_posterior$sl.trt_upper_slope
# sl_posterior$sl.lower<-sl_posterior$sl.ctl_lower_slope + sl_posterior$sl.trt_lower_slope
sgain.p<-sgain_posterior %>% inner_join(meta, by = 'site_code')

sgain.p$starting.richness <- ifelse(sgain.p$r.rich >= 1 & sgain.p$r.rich <= 5, '1-5 species',
                                    ifelse(sgain.p$r.rich >=6 & sgain.p$r.rich <=10, '6-10',
                                           ifelse(sgain.p$r.rich >=11 & sgain.p$r.rich <=15, '11-15',    
                                                  ifelse(sgain.p$r.rich >=16 & sgain.p$r.rich <=20, '16-20',
                                                         ifelse(sgain.p$r.rich >=21 & sgain.p$r.rich <=25, '21-25',
                                                                ifelse(sgain.p$r.rich >=26, '>26', 'other'))))))

sgain.p$starting.richness <- factor(sgain.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sgain.p$anthropogenic<-as.factor(sgain.p$anthropogenic)
sgain.p$grazed<-as.factor(as.character(sgain.p$grazed))
sgain.p$managed<-as.factor(as.character(sgain.p$managed))
sgain.p$burned<-as.factor(as.character(sgain.p$burned))

sgain.p$site_rich_range <- ifelse(sgain.p$site_richness >= 2 & sgain.p$site_richness <= 44, '2-40 species',
                                  ifelse(sgain.p$site_richness >=45 & sgain.p$site_richness <=69, '45-69',
                                         ifelse(sgain.p$site_richness >=70 & sgain.p$site_richness <=90, '70-90',    
                                                ifelse(sgain.p$site_richness >=91 & sgain.p$site_richness <=119, '90-119',
                                                       ifelse(sgain.p$site_richness >=120 & sgain.p$site_richness <=144, '120-144',
                                                              ifelse(sgain.p$site_richness >=145, '>145', 'other'))))))


write.csv(sgain.p,"~/Dropbox/Projects/NutNet/Data/sgain_posteriors.csv")









cde.p <- read.csv("~/Dropbox/Projects/NutNet/Data/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sg.p <- read.csv("~/Dropbox/Projects/NutNet/Data/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sl.p <- read.csv("~/Dropbox/Projects/NutNet/Data/sl_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sgain.p <- read.csv("~/Dropbox/Projects/NutNet/Data/sgain_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
sloss.p <- read.csv("~/Dropbox/Projects/NutNet/Data/sloss_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


loss<- bind_cols(sl.p,sloss.p)

gains<- bind_cols(sg.p,sgain.p)

head(gains)


ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = cde.p,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = cde.trt + unique(cde.trt_global_slope) ), 
               colour= "purple",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = cde.p,aes(x=0, #persistent
                              y= cde.trt + unique(cde.trt_global_slope), ),
             colour="purple",size=0.1,alpha = .1) +
# geom_errorbar(data = cde.p,aes(x=0,
#                                ymin = cde.trt_lower_slope, ymax = cde.trt_upper_slope), width=0,colour = "purple", size = 0.55,alpha=0.3) 
  geom_segment(data = loss,
               aes(x = 0,
                   xend = (sloss.trt + unique(sloss.trt_global_slope)),
                   y = 0,
                   yend = (sl.trt + unique(sl.trt_global_slope)) ),
               colour= "red",
               size = 0.2,  alpha = .1,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = loss, aes(x= (sloss.trt + unique(sloss.trt_global_slope)), #loss
                                  y=  (sl.trt + unique(sl.trt_global_slope)) ),
             colour="red",size=0.2,alpha = .1)+
  # geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
  #                                    ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "blue", size = 0.35,alpha=0.3) +
  # geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
  #                                     xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "blue", size = 0.35,alpha=0.3) +
geom_segment(data = gains,
             aes(x = 0,
                 xend = sgain.trt + unique(sgain.trt_global_slope),
                 y = 0,
                 yend = sg.trt + unique(sg.trt_global_slope) ),
             colour= "blue",
             size = 0.2,  alpha = .1,
             arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
geom_point(data = gains,aes(x=sgain.trt + unique(sgain.trt_global_slope), #losses
                                y=sg.trt + unique(sg.trt_global_slope) ),
           colour="blue",
           size=0.2,alpha = .1)
# geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
#                                    ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "red", size = 0.35,alpha=0.3) +
# geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
#                                     xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "red", size = 0.35,alpha=0.3) 
# 




