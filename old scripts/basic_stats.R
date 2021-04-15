



rm(list=ls())



library(tidyverse)
library(ggplot2)
library(brms)
library(patchwork)

# model compare
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/bm.Rdata') # plot.bm.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/rich.Rdata') # plot.rich.3

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/cde.Rdata') # CDE.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sloss.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sgain.Rdata') # s.gain.s


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Model_fits/3/multi.Rdata') # s.gain.s



#  SIGMAS
# NO SL
#load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sg_sigma2-7609895.Rdata') # sg.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/cde_sigma2-7609676.Rdata') # CDE.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sloss_sigma2-7609893.Rdata') # s.loss.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sgain_sigma2-7609897.Rdata') # s.gain.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/sloss_sigma2-7609893.Rdata') # s.loss.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/nn_time.bm-7612956.Rdata') # plot.bm.3_sigma

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/nn_time.rich_sigma-7612959.Rdata') # plot.rich.3_sigma


# still not converged
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/nn_timesl-sigma-7612932.Rdata') # sl.3_sigma

summary(plot.rich.3_sigma)

pp_check(plot.rich.3_sigma)

summary(sl.3_sigma)

pp_check(sl.3_sigma)


summary(plot.bm.3_sigma)
summary(plot.bm.3)


summary(sg.3_sigma2)
summary(CDE.3_sigma2)
summary(s.loss.3_sigma2)
summary(s.gain.3_sigma2)

pp_check(plot.bm.3_sigma)




load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/p.effs.Rdata')




summary(plot.rich.3)
summary(plot.bm.3)
summary(sl.3)
summary(sg.3)
summary(CDE.3)
summary(s.loss.3)
summary(s.gain.3)
summary(nn.multi.3)


rich.p
bm.p
sl.p
sg.p
cde.p
sloss.p
sgain.p


rich.p$Model<- "Species Richness"
bm.p$Model<- "Biomass"
sloss.p$Model<- "Species Loss"
sgain.p$Model<- "Species Gain"
sl.p$Model<- "Change Biomass Due to Species Loss"
sg.p$Model<- "Change in Biomass Due to Species Gain"
cde.p$Model<- "Persistent Species Change in Biomass"



p.all <- rich.p %>% bind_rows(bm.p) %>% bind_rows(sloss.p) %>% bind_rows(sgain.p) %>%
  bind_rows(sl.p) %>% bind_rows(sg.p) %>% bind_rows(cde.p) %>%
  mutate(Treatment = response,
         Estimate = eff,
         Upper_Estimate = eff_upper,
         Lower_Estimate = eff_lower) %>%
  select(-response,-eff,-eff_upper,-eff_lower)



write.csv(p.all, '~/Dropbox/Projects/NutNet/Data/Table_S2.csv')


