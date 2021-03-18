



library(tidyverse)
library(brms)
library(ggplot2)
library(bayesplot)
library(patchwork)

plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



# nn.multi <- brm(mvbind(rich, plot.mass) ~ trt * year_trt + (trt * year_trt  | p | site_code), 
#                 data = plot,family=student(),  cores = 4, iter=6000, warmup = 1000,chains = 4)


# load model object
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/multi.Rdata") # object name: nn.multi

summary(nn.multi.3)

color_scheme_set("darkgray")
nnb<-pp_check(nn.multi.3, resp = 'plotmass')+ theme_classic()+ scale_x_continuous(limits = c(-1000, 2000))+ labs(x=expression(paste('Biomass (g/',m^2, ')')),
                                                                                                                y = 'Density') 
nnr<-pp_check(nn.multi.3, resp = 'alldiv')+ theme_classic()+ scale_x_continuous(limits = c(-10, 50))+ labs(x='Species Richness',
                                                                                                           y = 'Density') 
# Figure S3h
(nnr | nnb)



# load model object
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/multi_price.Rdata") # object name: pp.multi



# pp.multi <- brm(mvbind(SL,SG,CDE) ~ trt.y * year.y.m + (trt.y * year.y.m  | p | site_code),
#                 data = p.all,family=student(),  cores = 4, iter=6000, warmup = 1000,chains = 4)


summary(pp.multi)

color_scheme_set("darkgray")
sl<-pp_check(pp.multi, resp = 'SL')+ theme_classic()+ scale_x_continuous(limits = c(-1000, 200))+ labs(x=expression(paste('SL')),
                                                                                                                 y = 'Density') 
sg<-pp_check(pp.multi, resp = 'SG')+ theme_classic()+ scale_x_continuous(limits = c(-200, 1000))+ labs(x='SG',
                                                                                                           y = 'Density') 

cde<-pp_check(pp.multi, resp = 'SG')+ theme_classic()+ scale_x_continuous(limits = c(-1000, 1000))+ labs(x='CDE',
                                                                                                     y = 'Density') 
# Figure S3h
(sl | sg | cde)


# load model object
load("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/multi_sp.Rdata") # object name: nn.multi



# sp.multi <- brm(mvbind(s.loss.n, s.gain) ~ trt.y * year.y.m + (trt.y * year.y.m  | p | site_code),
#                 data = p.all,family=student(),  cores = 4, iter=6000, warmup = 1000,chains = 4)


summary(sp.multi)

color_scheme_set("darkgray")
sloss<-pp_check(sp.multi, resp = 'slossn')+ theme_classic()+ scale_x_continuous(limits = c(-50, 50))+ labs(x=expression(paste('s.loss')),
                                                                                                        y = 'Density') 
sgain<-pp_check(sp.multi, resp = 'sgain')+ theme_classic()+ scale_x_continuous(limits = c(-50, 50))+ labs(x='s.gain',
                                                                                                    y = 'Density') 

# Figure S3h
(sloss | sgain)



