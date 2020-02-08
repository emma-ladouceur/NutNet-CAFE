

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
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )

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

summary(plot)
summa
summary(pplot)

# richness models
# gaussian distribution 
load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.g

summary(plot.rich.g)

plot(plot.rich.g)

color_scheme_set("darkgray")
pp_check(plot.rich.g)+ theme_classic()

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


# biomass
# student t, 6000 iterations, 1000 warmup
load('~/Dropbox/Projects/NutNet/Model_Fits/bm.Rdata') # plot.bm.s

summary(plot.bm.s)

plot(plot.bm.s) 

# predicted values vs observed
pp_check(plot.bm.s)+ theme_classic()+ scale_x_continuous(limits = c(-1000, 2000))


#residuals
m2<-residuals(plot.bm.s)
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


price <- droplevels( price[-which(price$year.y == "0"), ] )

summary(price)
head(price)


par(mfrow=c(2,3))
hist(price$SL.p,breaks =40, main="Loss Func") # biomass change due to species loss (positive)
hist(price$SG, breaks=40, main="Gain Func") # biomass change due to species gains
hist(price$CDE, breaks=40, main="Persistent Func") # biomass change in persistent species
hist(price$s.loss.p,breaks =40, main="Sp Loss")  # species loss (positive)
hist(price$s.gain,breaks =40, main="Gain Sp") # species gains
hist(pplot$rich,breaks =40, main="Richness") # richness (compare to loss and gains)

# SL - effect of species loss on biomass
#  student-t dist, 10,000 iterations, 1000 warm, converged!! :)
load('~/Dropbox/Projects/NutNet/Model_fits/sl.Rdata') # sl.s

summary(sl.s)
# Some Rhats are still quite high
# some ESS are low

plot(sl.s)

# predicted values vs observed
pp_check(sl.s)+ theme_classic() + scale_x_continuous(limits = c(0, 700))


# SG - effect of species gains on biomass
# student, 15,000 iteration, 1000 warmup , delta_adapt .99
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s


summary(sg.s)


plot(sg.s.)


# predicted values vs observed
pp_check(sg.s)+ theme_classic() + scale_x_continuous(limits = c(0, 700))


# CDE - biomass change in persistent species

# student-t distribution,  triple the iterations (6000), 1000 warm up
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s

summary(CDE.s)

plot(CDE.s)

# predicted values vs observed
pp_check(CDE.s)+ theme_classic() + scale_x_continuous(limits = c(-1000, 1000))



# sloss - species loss

load('~/Dropbox/Projects/NutNet/Model_fits/sloss.Rdata') # s.loss.s

summary(s.loss.s)

plot(s.loss.s)


# predicted values vs observed
pp_check(s.loss.s)+ theme_classic()+ scale_x_continuous(limits = c(-50, 50))



# sgain - species gain
# student, 6000 iterations, 1000 warm up-
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s

summary(s.gain.s)

plot(s.gain.s)

# predicted values vs observed
pp_check(s.gain.s)+ theme_classic()+ scale_x_continuous(limits = c(-50, 50))


