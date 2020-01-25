

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
pplot <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

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
# no transform, poisson distribution, non-convergence
load('~/Dropbox/Projects/NutNet/Model_fits/rich.poisson.Rdata') # plot.rich
# no transform, gaussian distribution
load('~/Dropbox/Projects/NutNet/Model_fits/rich2.Rdata') # plot.rich.log
#lognormal distribution
load('~/Dropbox/Projects/SeedAdd/Model_fits/rich3.Rdata')


summary(plot.rich)
summary(plot.rich.log)
summary(rich.new3)

plot(rich.new) 
plot(plot.rich.log)
plot(rich.new3)

color_scheme_set("darkgray")
pr1<-pp_check(rich.new)+ theme_classic()
pr2<-pp_check(plot.rich.log)+ theme_classic()
pr3<-pp_check(rich.new3)+ theme_classic()
grid_arrange_shared_legend(pr1,pr2,pr3,ncol=3) 

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





#price data


# sloss
# gaussian, not converged
load('~/Dropbox/Projects/NutNet/Model_fits/sloss.Rdata') # s.loss.i
# gaussian distribution, richness not transformed, biomass transformed, 
load('~/Dropbox/Projects/SeedAdd/Model_fits/multi2.Rdata')
# no transform, gaussian distribution 
load('~/Dropbox/Projects/SeedAdd/Model_fits/multi3.Rdata')
# lognormal distribution, richness not transformed, biomass not transformed
load('~/Dropbox/Projects/SeedAdd/Model_fits/multi4.Rdata')

summary(s.loss.i)
summary(multi2)
summary(multi3)
summary(multi4)

plot(multi)
plot(multi2)
plot(multi3)
plot(multi4)

pmb<-pp_check(multi, resp = 'lbiomass')+ theme_classic()
pmr<-pp_check(multi, resp = 'lrich')+ theme_classic()
#grid_arrange_shared_legend(pr,pb,nrow=1) 

pmb2<-pp_check(multi2, resp = 'lbiomass')+ theme_classic()
pmr2<-pp_check(multi2, resp = 'richplot')+ theme_classic()

pmb3<-pp_check(multi3, resp = 'biomassplot')+ theme_classic()
pmr3<-pp_check(multi3, resp = 'richplot')+ theme_classic()

pmb4<-pp_check(multi4, resp = 'biomassplot')+ theme_classic()
pmr4<-pp_check(multi4, resp = 'richplot')+ theme_classic()
grid_arrange_shared_legend(pmr,pmr2,pmr3,pmr4,pmb,pmb2,pmb3,pmb4,nrow=2,ncol=4) 

m1<-residuals(multi)
m1
plot <- cbind(plot,
              residual_m1_rich = m1[,,'lrich'][,'Estimate'],
              residual_m1_biomass = m1[,,'lbiomass'][,'Estimate'])

m2<-residuals(multi2)
m2
plot <- cbind(plot,
              residual_m2_rich = m2[,,'richplot'][,'Estimate'],
              residual_m2_biomass = m2[,,'lbiomass'][,'Estimate'])

par(mfrow=c(5,2))
with(plot, plot(Experiment, residual_m1_rich))
with(plot, plot(Experiment, residual_m2_rich))
with(plot, plot(site, residual_m1_rich))
with(plot, plot(site, residual_m2_rich))
with(plot, plot(block, residual_m1_rich))
with(plot, plot(block, residual_m2_rich))
with(plot, plot(fyr.trt, residual_m1_rich))
with(plot, plot(fyr.trt, residual_m2_rich))
with(plot, plot(seed.rich, residual_m1_rich))
with(plot, plot(seed.rich, residual_m2_rich))

par(mfrow=c(5,2))
with(plot, plot(Experiment, residual_m1_biomass))
with(plot, plot(Experiment, residual_m2_biomass))
with(plot, plot(site, residual_m1_biomass))
with(plot, plot(site, residual_m2_biomass))
with(plot, plot(block, residual_m1_biomass))
with(plot, plot(block, residual_m2_biomass))
with(plot, plot(fyr.trt, residual_m1_biomass))
with(plot, plot(fyr.trt, residual_m2_biomass))
with(plot, plot(seed.rich, residual_m1_biomass))
with(plot, plot(seed.rich, residual_m2_biomass))


# Diversity
seed.pie.dat <- read.csv("~/Dropbox/Projects/SeedAdd/Data/seed.pie.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))%>% 
  as_tibble()

seed.pie$fyr.trt<-as.factor(seed.pie$yr.trt)
seed.pie$seed.rich<-as.numeric(as.character(seed.pie$seed.rich))
seed.pie$site<-as.factor(seed.pie$site)
seed.pie$block<-as.factor(seed.pie$block)
seed.pie$seed.rich.m<-seed.pie$seed.rich-mean(seed.pie$seed.rich)
seed.pie$l.b.pie <- log(seed.pie$biomass.pie)

hist(seed.pie$biomass.pie)
hist(seed.pie$l.b.pie)


# seedpie
# log transform variables, gaussian distribution 
load('~/Dropbox/Projects/SeedAdd/Model_fits/seedpie.Rdata')
# gaussian distribution, no transform
load('~/Dropbox/Projects/SeedAdd/Model_fits/seedpie2.Rdata')
# no transform, lognormal distribution 
load('~/Dropbox/Projects/SeedAdd/Model_fits/seedpie3.Rdata')


summary(m.l.seed.pie)
summary(seed.pie2)
summary(seed.pie3)

# inspection of chain diagnostics
plot(m.l.seed.pie) 
plot(seed.pie2) 
plot(seed.pie3) 

# predicted values vs observed
sp1<-pp_check(m.l.seed.pie)+ theme_classic()
sp2<-pp_check(seed.pie2)+ theme_classic()
sp3<-pp_check(seed.pie3)+ theme_classic()
grid_arrange_shared_legend(sp1,sp2,sp3,ncol=3) 

#residuals
m2<-residuals(seed.pie2)
m2<-as.data.frame(m2)
sp.plot2<-cbind(seed.pie.dat,m2$Estimate)

m1<-residuals(m.l.seed.pie)
m1<-as.data.frame(m1)
sp.plot1<-cbind(seed.pie.dat,m1$Estimate)

par(mfrow=c(5,2))
with(sp.plot1, plot(Experiment, m1$Estimate));abline(h=0, lty=2)
with(sp.plot2, plot(Experiment, m2$Estimate));abline(h=0, lty=2)
with(sp.plot1, plot(site, m1$Estimate));abline(h=0, lty=2)
with(sp.plot2, plot(site, m2$Estimate));abline(h=0, lty=2)
with(sp.plot1, plot(block, m1$Estimate));abline(h=0, lty=2)
with(sp.plot2, plot(block, m2$Estimate));abline(h=0, lty=2)
with(sp.plot1, plot(fyr.trt, m1$Estimate));abline(h=0, lty=2)
with(sp.plot2, plot(fyr.trt, m2$Estimate));abline(h=0, lty=2)
with(sp.plot1, plot(seed.rich, m1$Estimate));abline(h=0, lty=2)
with(sp.plot2, plot(seed.rich, m2$Estimate));abline(h=0, lty=2)


#sgain
#gaussian, not converged
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.i

summary(s.gain.i)



