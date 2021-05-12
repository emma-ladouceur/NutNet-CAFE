


rm(list = ls())


library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)

# plot level data
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot <- plot %>% filter(max.year >= 3)

head(plot)

plot$site_code <- as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$year_trt<-as.factor(plot$year_trt)


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/old/Model_fits/3/sg-sigma.Rdata') # sg.3_sigma2
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/old/Model_fits/3/sl-sigma.Rdata') # sg.3_sigma2

# model objects
#/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/
load('~/Desktop/mods/bm.Rdata') # bm.3
load('~/Desktop/mods/rich.Rdata') # rich.3

# sigma objects = trt + year_trt
load('~/Desktop/mods/bm_sigma.Rdata') # bm.3_sigma
load('~/Desktop/mods/rich_sigma.Rdata') # rich.3_sigma

# sigma objects = trt * year_trt
load('~/Desktop/mods/bm_sigmai.Rdata') # bm.3_sigmai 
load('~/Desktop/mods/rich_sigmai.Rdata') # rich.3_sigmai


# richness
summary(rich.3)
summary(rich.3_sigma)
summary(rich.3_sigmai)


# waic loo
(waic.r3 <- waic(rich.3))
(waic.r3_s <- waic(rich.3_sigma))
(waic.r3_sp <- waic(rich.3_sigmai))

loo_compare(waic.r3,waic.r3_s)
loo_compare(waic.r3_s, waic.r3_sp)


# sigmai  is favoured


# pp check
color_scheme_set("darkgray")
fig_s3a <- pp_check(rich.3) + theme_classic() + 
  labs(x= "Species richness", y = "Density")

fig_s3a_s <- pp_check(rich.3_sigma) + theme_classic() + 
  labs(x= "Species richness", y = "Density")

fig_s3a_sp <- pp_check(rich.3_sigmai) + theme_classic() + 
  labs(x= "Species richness", y = "Density")

(fig_s3a_s + fig_s3a_sp)


# biomass
summary(bm.3)
summary(bm.3_sigma)
summary(bm.3_sigmai)

# waic loo
(waic.b3 <- waic(bm.3))
(waic.b3_s <- waic(bm.3_sigma))
(waic.b3_si <- waic(bm.3_sigmai))

loo_compare( waic.b3,waic.b3_s)
loo_compare( waic.b3,waic.b3_si)
loo_compare( waic.b3_s,waic.b3_si)

#  sigmai is favoured

fig_s3b <- pp_check(bm.3) + theme_classic() + 
  labs( x = expression(paste('Biomass (g/',m^2, ')')) , y = "Density") + 
  scale_x_continuous(limits = c(-1000, 2000))

fig_s3b_s <- pp_check(bm.3_sigma) + theme_classic() + 
  labs( x = expression(paste('Biomass (g/',m^2, ')')) , y = "Density") + 
  scale_x_continuous(limits = c(-1000, 2000))

fig_s3b_si <- pp_check(bm.3_sigmai) + theme_classic() + 
  labs( x = expression(paste('Biomass (g/',m^2, ')')) , y = "Density") + 
  scale_x_continuous(limits = c(-1000, 2000))

fig_s3b_si






# data
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(p.all)
p.all <- p.all %>% group_by(site_code) %>% filter(max.year >= 3)
nrow(p.all)

p.all$site_code <- as.factor(p.all$site_code)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
p.all$year.y<-as.factor(p.all$year.y)


# price mods
rm(list = ls())

# load model objects
load('~/Desktop/mods/sloss.Rdata') # s.loss.3
load('~/Desktop/mods/sgain.Rdata') # s.gain.3
load('~/Desktop/mods/sl.Rdata') # sl.3
load('~/Desktop/mods/sg.Rdata') # sg.3
load('~/Desktop/mods/cde.Rdata') # CDE.3


#  +
load('~/Desktop/mods/sloss_sigma.Rdata') # s.loss.3_sigma2
load('~/Desktop/mods/sgain_sigma.Rdata') # s.gain.3_sigma2
load('~/Desktop/mods/sl_sigma.Rdata') # sl.3_sigma2
load('~/Desktop/mods/sg_sigma.Rdata') # sg.3_sigma2
load('~/Desktop/mods/cde_sigma.Rdata') # CDE.3_sigma2


#  *
load('~/Desktop/mods/sloss_sigmai.Rdata') # s.loss.3_sigma2
load('~/Desktop/mods/sgain_sigmai.Rdata') # s.gain.3_sigma2
load('~/Desktop/mods/sl_sigmai.Rdata') # sl.3_sigma2
load('~/Desktop/mods/sg_sigmai.Rdata') # sg.3_sigma2
load('~/Desktop/mods/cde_sigmai.Rdata') # CDE.3_sigma2


summary(ps.3_sigma)

summary(sl.3_sigma)


# s.loss

sloss.trt_fixef <- fixef(sloss.3)
sloss.trt_fixef_sigma <- fixef(sloss.3_sigma)

sloss.trt_fixef
sloss.trt_fixef_sigma

summary(sloss.3)
summary(sloss.3_sigmai)

# waic loo
(waic.loss3 <- waic(sloss.3))
(waic.loss_s <- waic(sloss.3_sigma))
(waic.loss_si <- waic(sloss.3_sigmai))

loo_compare(waic.loss3,waic.loss_s)
loo_compare(waic.loss_s,waic.loss_si)

#  sigma or sigma i are both great

# pp check
fig_s3c <- pp_check(s.loss.3) + theme_classic() + 
  labs(x= "Species loss (s.loss)", y = "Density") + 
  scale_x_continuous(limits = c(-50, 50))

fig_s3c_s <- pp_check(s.loss.3_sigma2) + theme_classic() + 
  labs(x= "Species loss (s.loss)", y = "Density") + 
  scale_x_continuous(limits = c(-50, 50))


(fig_s3c + fig_s3c_s)


# s.gain
summary(s.gain.3)
summary(sgain.3_sigmai)

# waic loo
(waic.gain3 <- waic(sgain.3))
(waic.gain_s <- waic(sgain.3_sigma))
(waic.gain_si <- waic(sgain.3_sigmai))

loo_compare(waic.gain3,waic.gain_s)
loo_compare(waic.gain_s,waic.gain_si)


#sigma is favoured but v close to sigma i
# pp check
fig_s3d <- pp_check(s.gain.3) + theme_classic() + 
  labs(x= "Species gain (s.gain)", y = "Density") + 
  scale_x_continuous(limits = c(-50, 50))

fig_s3d_s <- pp_check(s.gain.3_sigma2) + theme_classic() + 
  labs(x= "Species gain (s.gain)", y = "Density") + 
  scale_x_continuous(limits = c(-50, 50))

(fig_s3d + fig_s3d_s)



# biomass partitions


# sl
summary(sl.3)
summary(sl.3_sigma)
summary(sl.3_sigmai)

# waic loo
(waic.sl3 <- waic(sl.3))
(waic.sls_s <- waic(sl.3_sigma))
(waic.sls_si <- waic(sl.3_sigmai))

loo_compare(waic.sl3, waic.sls_s)
loo_compare(waic.sls_s, waic.sls_si)


# pp check
fig_s3e <- pp_check(sl.3) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with species sloss (SL)')), y = "Density") + 
  scale_x_continuous(limits = c(-700, 100))


fig_s3e_s <- pp_check(sl.3_sigma2) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with species sloss (SL)')), y = "Density") + 
  scale_x_continuous(limits = c(-700, 100))

(fig_s3e + fig_s3e_s)


# sg
#emmas updated sg mods
summary(sg.3)
summary(sg.3_sigma)


summary(sg.3_sigmai)

#shanes previous sg sigma
summary(sg.3_sigma2)
# Formula: SG ~ trt.y + year.y.m + (trt.y * year.y.m | site_code/block/plot) + trt.y:year.y.m 
# sigma ~ 0 + trt.y + (0 + trt.y | site_code)

# waic loo
(waic.sg3 <- waic(sg.3))
(waic.sg_s <- waic(sg.3_sigma2))

loo_compare(waic.sg3,waic.sg_s)

#  sg_sigmas not converged


# pp check
fig_s3f <- pp_check(sg.3) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with species gain (SG)')), y = "Density") + 
  scale_x_continuous(limits = c(-100, 700))

fig_s3f_s <- pp_check(sg.3_sigma2) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with species gain (SG)')), y = "Density") + 
  scale_x_continuous(limits = c(-100, 700))


(fig_s3f + fig_s3f_s)



# cde
summary(CDE.3)
summary(cde.3_sigmai)

# waic loo
(waic.cde3 <- waic(cde.3))
(waic.cde_s <- waic(cde.3_sigma))
(waic.cde_si <- waic(cde.3_sigmai))

loo_compare(waic.cde3,waic.cde_s)
loo_compare(waic.cde_s, waic.cde_si)
#sigma i is favoured  

# pp check
fig_s3g <- pp_check(CDE.3) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with persistent species (PS)')), y = "Density") + 
  scale_x_continuous(limits = c(-1000, 1000))

fig_s3g_s <- pp_check(cde.3_sigmai) + theme_classic() + 
  labs(x= expression(paste('Biomass change (g/' ,m^2, ') associated with persistent species (PS)')), y = "Density") + 
  scale_x_continuous(limits = c(-1000, 1000))

fig_s3g_s


cde.fixed.p <- posterior_samples(CDE.3, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )

cde.fixed.p_s <- posterior_samples(CDE.3_sigma2, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )



# cde or biomass change associated with persistent species
cde_global_posterior <-  cde.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.global =`b_year.y.m`,
         cde.npk.global=`b_trt.yNPK:year.y.m`,
         cde.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(cde_global_posterior)



cde.p.npk <-  cde_global_posterior %>% 
  mutate( response="NPK", eff = mean(cde.trt.global),
          eff_lower = quantile(cde.trt.global, probs=0.025),
          eff_upper = quantile(cde.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

cde.p.ctl <-  cde_global_posterior %>% 
  mutate( response="Control", eff = mean(cde.ctl.global),
          eff_lower = quantile(cde.ctl.global, probs=0.025),
          eff_upper = quantile(cde.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.cde.p <- bind_rows(cde.p.npk,cde.p.ctl)

global.cde.p


#sigma

cde_global_posterior_s <-  cde.fixed.p_s %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(cde.ctl.global =`b_year.y.m`,
         cde.npk.global=`b_trt.yNPK:year.y.m`,
         cde.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(cde_global_posterior_s)



cde.p.npk_s <-  cde_global_posterior_s %>% 
  mutate( response="NPK", eff = mean(cde.trt.global),
          eff_lower = quantile(cde.trt.global, probs=0.025),
          eff_upper = quantile(cde.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

cde.p.ctl_s <-  cde_global_posterior_s %>% 
  mutate( response="Control", eff = mean(cde.ctl.global),
          eff_lower = quantile(cde.ctl.global, probs=0.025),
          eff_upper = quantile(cde.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.cde.p_s <- bind_rows(cde.p.npk_s,cde.p.ctl_s)

global.cde.p_s


