

detach("package:ggplot2", unload=TRUE)
detach("package:plyr", unload=TRUE)


library(tidyverse)
library(brms)
library(broom)
library(lme4)
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

hist(plot$rich)
hist(plot$log.rich)
hist(plot$live_mass)
hist(plot$log.live.mass)

summary(pplot)

# richness models
# no transform, poisson distribution, log link, non-convergence
load('~/Dropbox/Projects/NutNet/Model_fits/rich.poisson.Rdata') # plot.rich
# no transform, lognormal distribution
load('~/Dropbox/Projects/NutNet/Model_fits/rich2.Rdata') # plot.rich.log
#lognormal distribution
load('~/Dropbox/Projects/SeedAdd/Model_fits/rich3.Rdata')


summary(plot.rich)
summary(plot.rich.log)
summary(rich.new3)

plot(plot.rich) 
plot(plot.rich.log) # catepillars are wonky
plot(rich.new3)

color_scheme_set("darkgray")
pr1<-pp_check(plot.rich)+ theme_classic()
pr2<-pp_check(plot.rich.log)+ theme_classic()
pr3<-pp_check(rich.new3)+ theme_classic()
grid_arrange_shared_legend(pr1,pr2,ncol=2) 

m1<-residuals(rich.new)
m1<-as.data.frame(m1)
View(m1)
nrow(m1)
nrow(plot)
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
load('~/Dropbox/Projects/NutNet/Model_fits/biomass.local.Rdata') # plot.bm.im
# no transform, lognormal distribution, error because the neg values!!
load('~/Dropbox/Projects/NutNet/Model_fits/biomass1.Rdata') # plot.bm.log
# no transform, lognormal distribution
load('~/Dropbox/Projects/SeedAdd/Model_fits/biomass3.Rdata')

summary(plot.bm.im)
summary(plot.bm.log)
summary(biomass.new3)

# inspection of chain diagnostics
plot(plot.bm.im) 
plot(plot.bm.log) 
plot(biomass.new3) 

# predicted values vs observed
pb1<-pp_check(plot.bm.im)+ theme_classic()
pb2<-pp_check(plot.bm.log)+ theme_classic()
pb3<-pp_check(biomass.new3)+ theme_classic()
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

head(price)

hist(price$SL.p)  # biomass change due to species loss
hist(price$SG) # biomass change due to species gains
hist(price$CDE) # biomass change in persistent species
hist(price$s.loss.p) # species loss
hist(price$s.gain) # species gains


#SL

#SG


# cde
# no transform, gaussian, non convergence
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # p.CDE.trt.i
# studentt distribution
load('~/Dropbox/Projects/NutNet/Model_fits/cde2.Rdata') # CDE.s


summary(p.CDE.trt.i)
summary(CDE.s)



# sloss
# hurdle log normal distribution (original mod)
load('~/Dropbox/Projects/SeedAdd/Model_fits/sloss2.Rdata') # s.loss.h
# gaussian, not converged
load('~/Dropbox/Projects/NutNet/Model_fits/sloss.Rdata') # s.loss.i
# no transform, gaussian distribution 
load('~/Dropbox/Projects/SeedAdd/Model_fits/multi3.Rdata')
# lognormal distribution, richness not transformed, biomass not transformed
load('~/Dropbox/Projects/SeedAdd/Model_fits/multi4.Rdata')

summary(s.loss.h)
summary(s.loss.i)
summary(multi3)
summary(multi4)

plot(multi)
plot(multi2)
plot(multi3)
plot(multi4)


#sgain
# no transform, hurdle lognormal distribution (original mod)
load('~/Dropbox/Projects/NutNet/Model_fits/sgain2.Rdata') # s.gain.h
# gaussian, not converged
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.i


summary(s.gain.h)
summary(s.gain.i)



