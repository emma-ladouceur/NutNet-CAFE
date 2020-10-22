



rm(list=ls())



library(tidyverse)
library(ggplot2)
library(brms)
library(patchwork)

# model compare
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


load('~/Dropbox/Projects/NutNet/Model_fits/3/bm.Rdata') # plot.bm.3
load('~/Dropbox/Projects/NutNet/Model_fits/3/rich.Rdata') # plot.rich.3




load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sgain.Rdata') # s.gain.s

load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/multi.Rdata') # s.gain.s



load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')




summary(plot.rich.3)
summary(plot.bm.3)
summary(sl.3)
summary(sg.3)
summary(CDE.3)
summary(sloss.3)
summary(sgain.rich.3)
summary(nn.multi.3)


rich.p
bm.p
sl.p
sg.p
cde.p
sloss.p
sgain.p





