

# Figs 3-4


# FIGURE 3
# DERIVATIVES DELTA OVER TIME
# CAFE BAYES VECTORS

# SP LOSS VECTOR, SP GAIN VECTOR, CDE VECTOR... but what about....transformations?
# data
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


#price partition data
pp <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


head(pp)

#models
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sl.Rdata') # sl.trt.i
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.sg.Rdata') # sg.trt.i
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.cde.Rdata') # p.CDE.trt.i
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.rich.Rdata') # plot.rich.im

rich_fixef <- fixef(plot.rich.im)
sl_fixef <- fixef(sl.trt.i)
sg_fixef <- fixef(sg.trt.i)
cde_fixef <- fixef(p.CDE.trt.i)

rich_fixef<-as.data.frame(rich_fixef)
sl_fixef<-as.data.frame(sl_fixef)
sg_fixef<-as.data.frame(sg_fixef)
cde_fixef<-as.data.frame(cde_fixef)

rich_fixef$Model<-'Richness'
sl_fixef$Estimate<-(sl_fixef$Estimate)*-1
sl_fixef$Model<-'SL'
sg_fixef$Model<-'SG'
cde_fixef$Model<-'CDE'
fixedf_pp<-bind_rows(sl_fixef,sg_fixef,cde_fixef,rich_fixef)
fixedf_pp

ggplot()+
  geom_segment(data = fixedf_pp,
               aes(x = Estimate[4]+Estimate[8],
                   xend = Estimate[4]+Estimate[8],
                   y = Estimate[8],
                   yend = +Estimate[12]),
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp,
               aes(x = Estimate[16],
                   xend = Estimate[16],
                   y = Estimate[4],
                   yend = Estimate[8]),
               colour= "red",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = filter(fixedf_pp, Model=='SL'),
                            aes(x = 0,
                                xend = Estimate[16],
                                y = 0,
                                yend = Estimate[4]),
                            colour= "blue",
                            size = 1.5,
                            arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  labs(x = 'Species Richness',
       y = 'Biomass',
       title= 'CAFE Bayes Vector') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")



ggplot()+
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate[12]),
               colour= "purple",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[8],
                   y = 0,
                   yend = Estimate[8]),
               colour= "red",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = filter(fixedf_pp, Model=='SL'),
               aes(x = 0,
                   xend = Estimate[4],
                   y = 0,
                   yend = Estimate[4]),
               colour= "blue",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")



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



# FIGURE 4
# POSTERIORS ACROSS GROUPS

# SITE DIVERSITY
# CO-LIMITED (STAN'S PAPER--show him coef effects and ask how he would determine) PLOTS WITH EFFECT VS. PLOTS WITH NO EFFECT
# EXOTIC VS. NATIVE DOMINATED
# ANTHROPOGENIC
# HERBIVORY
# BIOGEO / CLIMATE
# N. DEPOSITION

# FACETED AS LOSSES, GAINS, CDE? WITH POSTERIORS GROUPED AS ABOVE IN DIFF COLOURS
# CONTROLS GREY IN THE BACKGROUND





