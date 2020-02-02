

detach("package:ggplot2", unload=TRUE)
detach("package:plyr", unload=TRUE)


library(tidyverse)
library(brms)
library(broom)
library(lme4)
library(loo)
library(sjstats)
library(bayesplot)
library(grid)
library(gridExtra)
library(ggplot2)


# grid arrange shared legend function

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

# plot data
pplot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(plot)
plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$log.rich<-log(plot$rich)
#bm
plot$log.live.mass<-log(plot$live_mass)

par(mfrow=c(2,2))
hist(plot$rich,breaks =30, main="Rich")
hist(plot$live_mass, breaks=30, main="Biomass")
hist(plot$log.rich, breaks=30, main="Log Rich")
hist(plot$log.live.mass,breaks =30, main="Log Biomass")


summary(pplot)

# richness models
# no transform, poisson distribution
#load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.p
# no transform, lognormal distribution
#load('~/Dropbox/Projects/NutNet/Model_fits/rich2.Rdata') # plot.rich.log
# gaussian distribution 
load('~/Dropbox/Projects/NutNet/Model_fits/rich3.Rdata') # plot.rich.g

summary(plot.rich.p)
summary(plot.rich.log)
summary(plot.rich.g)

plot(plot.rich.p) 
plot(plot.rich.log) # catepillars are wonky
plot(plot.rich.g)

color_scheme_set("darkgray")
pr1<-pp_check(plot.rich.p)+ theme_classic()
pr2<-pp_check(plot.rich.log)+ theme_classic()
pr3<-pp_check(plot.rich.g)+ theme_classic()
pr3
grid_arrange_shared_legend(pr1,pr2,pr3,ncol=3) 

m1<-residuals(plot.rich.g)
m1<-as.data.frame(m1)
rr.plot<-cbind(plot,m1$Estimate)
View(rr.plot)

head(rr.plot)
par(mfrow=c(1,1))
with(rr.plot, plot(Experiment, m1$Estimate))
with(rr.plot, plot(site, m1$Estimate))
with(rr.plot, plot(block, m1$Estimate))
with(rr.plot, plot(fyr.trt, m1$Estimate))
plot$seed.rich<-as.factor(plot$seed.rich)
with(rr.plot, plot(seed.rich, m1$Estimate))

#normal model
m1<-residuals(rich.new3)
m1<-as.data.frame(m1)
nrow(m1)
nrow(plot)
rr.plot<-cbind(plot,m1$Estimate)

head(rr.plot)
par(mfrow=c(2,3))
with(rr.plot, plot(Experiment, m1$Estimate))
with(rr.plot, plot(site, m1$Estimate))
with(rr.plot, plot(block, m1$Estimate))
with(rr.plot, plot(fyr.trt, m1$Estimate))
plot$seed.rich<-as.factor(plot$seed.rich)
with(rr.plot, plot(seed.rich, m1$Estimate))


#biomass
# no transform, gaussian distribution
#load('~/Dropbox/Projects/NutNet/Model_fits/biomass.Rdata') # plot.bm
# log transform, gauss distribution
load('~/Dropbox/Projects/NutNet/Model_fits/biomass2.Rdata') # plot.bm.logt
# no transform, lognormal distribution # small ESS
#load('~/Dropbox/Projects/NutNet/Model_fits/biomass3.Rdata') # plot.bm.logd

summary(plot.bm)
summary(plot.bm.logt)
summary(plot.bm.logd)

# compare two models using loo (but should use waic or loo criterion?)
# must have same number of observations
plot.bm.logt <- add_criterion(plot.bm.logt, "waic")
plot.bm.logt <- add_criterion(plot.bm.logt, "loo")
plot.bm.logd <- add_criterion(plot.bm.logd, "waic")
plot.bm.logd <- add_criterion(plot.bm.logd, "loo")
loo_compare(plot.bm.logt, plot.bm.logd, criterion = "waic")
loo_compare(plot.bm.logt, plot.bm.logd, criterion = "loo")

# inspection of chain diagnostics
plot(plot.bm) 
plot(plot.bm.logt) 
plot(plot.bm.logd) 

# predicted values vs observed
pb1<-pp_check(plot.bm)+ theme_classic()
pb2<-pp_check(plot.bm.logt)+ theme_classic()
pb3<-pp_check(plot.bm.logd)+ theme_classic()
grid_arrange_shared_legend(pb1,pb2,pb3,ncol=3) 

#residuals
m2<-residuals(biomass.new)
m2<-as.data.frame(m2)
rb.plot<-cbind(plot,m2$Estimate)
View(rb.plot)

head(rb.plot)
par(mfrow=c(1,1))
with(rb.plot, plot(Experiment, m2$Estimate));abline(h=0, lty=2)
with(rb.plot, plot(site, m2$Estimate));abline(h=0, lty=2)
with(rb.plot, plot(block, m2$Estimate));abline(h=0, lty=2)
with(rb.plot, plot(fyr.trt, m2$Estimate));abline(h=0, lty=2)
with(rb.plot, plot(seed.rich, m2$Estimate));abline(h=0, lty=2)



# price data
price <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

summary(price)
head(price)


par(mfrow=c(2,3))
hist(price$SL.p,breaks =40, main="Loss Func") # biomass change due to species loss (positive)
hist(price$SG, breaks=40, main="Gain Func") # biomass change due to species gains
hist(price$CDE, breaks=40, main="Persistent Func") # biomass change in persistent species
hist(price$s.loss.p,breaks =40, main="Sp Loss")  # species loss (positive)
hist(price$s.gain,breaks =40, main="Gain Sp") # species gains
hist(plot$rich,breaks =40, main="Richness") # richness (compare to loss and gains)

# SL- effect of species loss on biomass
# no transform, hurdle lognormal dist, non convergence
#load('~/Dropbox/Projects/NutNet/Model_fits/sl.Rdata') # sl.trt.h
# no transform, hurdle lognormal dist, 4000 iterations, non convergence
load('~/Dropbox/Projects/NutNet/Model_fits/sl2.Rdata') # sl.trt.h.d
# no transform, hurdle lognormal dist, 6000 iterations
load('~/Dropbox/Projects/NutNet/Model_fits/sl3.Rdata') # sl.trt.h.t

summary(sl.trt.h.d)
summary(sl.trt.h.t)

plot(sl.trt.h.d)
plot(sl.trt.h.t)

# predicted values vs observed
pp_check(sl.trt.h.d)+ theme_classic() + scale_x_continuous(limits = c(0, 700))
pp_check(sl.trt.h.t)+ theme_classic() + scale_x_continuous(limits = c(0, 700))

# SG - effect of species gains on biomass
# no transform, hurdle lognormal dist
#load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.trt.h
# no transform, hurdle lognormal dist, 4000 iterations
load('~/Dropbox/Projects/NutNet/Model_fits/sg2.Rdata') # sg.trt.d

summary(sg.trt.d)


plot(sg.trt.h.)

# predicted values vs observed
pp_check(sg.trt.d)+ theme_classic() + scale_x_continuous(limits = c(0, 700))


# CDE - biomass change in persistent species
# no transform, gaussian, non convergence
#load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # p.CDE.trt.i
# student-t distribution, not converged
load('~/Dropbox/Projects/NutNet/Model_fits/cde2.Rdata') # CDE.s
# student-t distribution, double the iterations (4000), not converged
load('~/Dropbox/Projects/NutNet/Model_fits/cde3.Rdata') # CDE.s.d
# student-t distribution,  triple the iterations (6000)
load('~/Dropbox/Projects/NutNet/Model_fits/cde4.Rdata') # CDE.s.t

summary(p.CDE.trt.i)
summary(CDE.s.d)
summary(CDE.s.t)

plot(p.CDE.trt.i)
plot(CDE.s.d)
plot(CDE.s.t)

# predicted values vs observed
c1<-pp_check(p.CDE.trt.i)+ theme_classic()
pp_check(CDE.s.d)+ theme_classic() + scale_x_continuous(limits = c(-1000, 1000))
pp_check(CDE.s.t)+ theme_classic() + scale_x_continuous(limits = c(-1000, 1000))


grid_arrange_shared_legend(c1,c2,ncol=2) 


# sloss - species loss
# gaussian, not converged
#load('~/Dropbox/Projects/NutNet/Model_fits/sloss.Rdata') # s.loss.i
# hurdle log normal distribution  does not converge
#load('~/Dropbox/Projects/NutNet/Model_fits/sloss2.Rdata') # s.loss.h
# no transform, poission distribution 
#load('~/Dropbox/Projects/NutNet/Model_fits/sloss3.Rdata') # s.loss.p
# no transform, poission distribution , double iterations
load('~/Dropbox/Projects/NutNet/Model_fits/sloss4.Rdata') # s.loss.p.d


summary(s.loss.i)
summary(s.loss.h)
summary(s.loss.p.d)

plot(s.loss.i)
plot(s.loss.h)
plot(s.loss.p)

# predicted values vs observed
sloss1<-pp_check(s.loss.i)+ theme_classic()
sloss2<-pp_check(s.loss.h)+ theme_classic()
sloss3<-pp_check(s.loss.p.d)+ theme_classic()
sloss3
grid_arrange_shared_legend(sloss1,sloss2,sloss3,ncol=3) 


# sgain - species gain
# gaussian, not converged
#load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.i
# no transform, hurdle lognormal distribution  does not converge
#load('~/Dropbox/Projects/NutNet/Model_fits/sgain2.Rdata') # s.gain.h
# poisson, not converged
#load('~/Dropbox/Projects/NutNet/Model_fits/sgain3.Rdata') # s.gain.p
# poisson, double iterations
load('~/Dropbox/Projects/NutNet/Model_fits/sgain4.Rdata') # s.gain.p.d

summary(s.gain.h)
summary(s.gain.i)
summary(s.gain.p.d)


plot(s.gain.i)
plot(s.gain.h)
plot(s.gain.p.d)

# predicted values vs observed
sgain1<-pp_check(s.gain.i)+ theme_classic()
sgain2<-pp_check(s.gain.h)+ theme_classic()
sgain3<-pp_check(s.gain.p.d)+ theme_classic()
sgain3
grid_arrange_shared_legend(sgain1,sgain2,sgain3,ncol=3) 


