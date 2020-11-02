



library(tidyverse)
library(brms)
library(ggplot2)
library(bayesplot)
library(patchwork)

plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



# nn.multi <- brm(mvbind(rich, plot.mass) ~ trt * year_trt + (trt * year_trt  | p | site_code), 
#                 data = plot,family=student(),  cores = 4, iter=6000, warmup = 1000,chains = 4)


# load model object
load("~/Dropbox/Projects/NutNet/Data/Model_Fits/3/multi.Rdata") # object name: nn.multi

summary(nn.multi.3)

color_scheme_set("darkgray")
nnb<-pp_check(nn.multi.3, resp = 'plotmass')+ theme_classic()+ scale_x_continuous(limits = c(-1000, 2000))+ labs(x=expression(paste('Biomass (g/',m^2, ')')),
                                                                                                                y = 'Frequency') 
nnr<-pp_check(nn.multi.3, resp = 'alldiv')+ theme_classic()+ scale_x_continuous(limits = c(-10, 50))+ labs(x='Species Richness',
                                                                                                           y = 'Frequency') 
# Figure S3h
(nnr | nnb)




