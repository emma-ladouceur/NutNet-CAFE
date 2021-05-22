

rm(list = ls())


library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)


# Figure S3
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


colnames(sp)

sp_rel <- sp %>% group_by(id) %>%
  summarise( rel_plot_bm = sum(biomass.sp.full)) %>%
  ungroup() %>%
  left_join(sp) %>%
  mutate( rel_cover_plot = max_cover/plot.cover,
          rel_cover_cat = max_cover/cat.cover,
          rel_strip_bm_full = biomass.sp.full/strip.mass,
          rel_strip_bm_cat = biomass.sp.cat/cat.mass,
          rel_strip_bm_plot = biomass.sp.plot/strip.mass,
  ) 



fig_S3a <- ggplot() + geom_point( data = sp_rel, aes(x = rel_cover_cat, y = rel_strip_bm_cat), alpha = 0.2) +
  labs( title = 'a) Functional Group Category',
        x = 'Species percent cover relative to plot',
        y = 'Per species biomass estimate relative to strip')+
  theme_classic()

fig_S3a


fig_S3b <- ggplot() + geom_point( data = sp_rel, aes(x = rel_cover_plot, y = rel_strip_bm_plot), alpha = 0.2) +
  labs( title = 'b) Plot',
        x = 'Species percent cover relative to plot',
        y = 'Per species biomass estimate relative to strip')+
  theme_classic()



fig_S3 <-  (fig_S3a + fig_S3b)

fig_S3




# Figure S4


#plot cover
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/cover/cover_sigma.Rdata') # cover.3_sigmai
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm_sigmai.Rdata') # rich.3_sigmai
load('~/Desktop/nn mods/cde.Rdata') # rich.3_sigmai
load('~/Desktop/nn mods/sl.Rdata') # rich.3_sigmai
load('~/Desktop/nn mods/sg.Rdata') # rich.3_sigmai
# produced in '7_Model_Data_Posteriors.R' (biomass models)
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/global.p.effs.Rdata')

# check cover mods
summary(sl.3_cover)
summary(sg.3_cover)
summary(cde.3_cover)


sl.fixed.p <- posterior_samples(sl.3_cover, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p <- posterior_samples(sg.3_cover, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p <- posterior_samples(cde.3_cover, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )



#  model summary
summary(cover.3_sigmai)
# caterpillar plots
#plot(cover.3_sigmai)
# predicted values vs. observed
color_scheme_set("darkgray")
fig_s3b <- pp_check(cover.3_sigmai) + theme_classic() + 
  labs( x = expression(paste('Biomass (g/',m^2, ')')) , y = "Density") + 
  scale_x_continuous(limits = c(-1000, 2000))

fig_s3b


cover.fixed.p<-posterior_samples(cover.3_sigmai, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 

# biomass
cover_global_posterior <-  cover.fixed.p %>% dplyr::select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(cover.ctl.global =`b_year_trt`,
         cover.npk.global=`b_trtNPK:year_trt`,
         cover.trt.global=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  dplyr::select(-c(`b_year_trt`,`b_trtNPK:year_trt`)) 

head(cover_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
cover.p.npk <-  cover_global_posterior %>% 
  mutate( response="NPK", eff = mean(cover.trt.global),
          eff_lower = quantile(cover.trt.global, probs=0.025),
          eff_upper = quantile(cover.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

cover.p.ctl <-  cover_global_posterior %>% 
  mutate( response="Control", eff = mean(cover.ctl.global),
          eff_lower = quantile(cover.ctl.global, probs=0.025),
          eff_upper = quantile(cover.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.cover.p <- bind_rows(cover.p.npk,cover.p.ctl)

global.cover.p


global.bm.p <- global.bm.p %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Biomass") 

global.plot.p_c <- global.cover.p %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Cover") %>% bind_rows(global.bm.p) %>%
  unite("Response Treatment", Model, Treatment, sep= " ", remove=F)
global.plot.p_c

fig_s4a <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.plot.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.plot.p_c, aes(x = Treatment,ymin = eff_lower,
                                            ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'a) Strip biomass & plot cover change',
        x = '',
        y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_s4a



# SL : biomass change associated with species loss
sl_global_posterior <-  sl.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sl.ctl.global =`b_year.y.m`,
         sl.npk.global=`b_trt.yNPK:year.y.m`,
         sl.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sl_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
sl.p.npk <-  sl_global_posterior %>% 
  mutate( response="NPK", eff = mean(sl.trt.global),
          eff_lower = quantile(sl.trt.global, probs=0.025),
          eff_upper = quantile(sl.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

sl.p.ctl <-  sl_global_posterior %>% 
  mutate( response="Control", eff = mean(sl.ctl.global),
          eff_lower = quantile(sl.ctl.global, probs=0.025),
          eff_upper = quantile(sl.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.sl.p_cover <- bind_rows(sl.p.npk,sl.p.ctl)



global.sl.p <- global.sl.p %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Biomass") 

global.sl.p_c <- global.sl.p_cover %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Cover") %>% bind_rows(global.sl.p) %>%
  unite("Response Treatment", Model, Treatment, sep= " ", remove=F)
global.sl.p_c


fig_s4b <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.sl.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.sl.p_c, aes(x = Treatment,ymin = eff_lower,
                                         ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'b) Biomass & cover change associated \n with species loss (SL)',
        x = '',
        y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_s4b




# SG : biomass change associated with species gains
sg_global_posterior <-  sg.fixed.p %>% dplyr::select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(sg.ctl.global =`b_year.y.m`,
         sg.npk.global=`b_trt.yNPK:year.y.m`,
         sg.trt.global=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  dplyr::select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`)) 

head(sg_global_posterior)


# take the mean quantiles of the fixed effects posteriors for each treatment
sg.p.npk <-  sg_global_posterior %>% 
  mutate( response="NPK", eff = mean(sg.trt.global),
          eff_lower = quantile(sg.trt.global, probs=0.025),
          eff_upper = quantile(sg.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

sg.p.ctl <-  sg_global_posterior %>% 
  mutate( response="Control", eff = mean(sg.ctl.global),
          eff_lower = quantile(sg.ctl.global, probs=0.025),
          eff_upper = quantile(sg.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.sg.p_cover <- bind_rows(sg.p.npk,sg.p.ctl)

global.sg.p_cover


global.sg.p <- global.sg.p %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Biomass") 

global.sg.p_c <- global.sg.p_cover %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Cover") %>% bind_rows(global.sg.p) %>%
  unite("Response Treatment", Model, Treatment, sep= " ", remove=F)
global.sg.p_c


fig_s4c <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.sg.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.sg.p_c, aes(x = Treatment,ymin = eff_lower,
                                         ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'c) Biomass & cover change associated \n with species gain (SG)',
        x = '',
        y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
#  scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_s4c



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
global.cde.p_cover <- bind_rows(cde.p.npk,cde.p.ctl)


global.cde.p <- global.cde.p %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Biomass") 

global.cde.p_c <- global.cde.p_cover %>% rename(Treatment = response) %>%
  mutate(Treatment = factor(Treatment , levels=c("Control", "NPK")),
         Model = "Cover") %>% bind_rows(global.cde.p) %>%
  unite("Response Treatment", Model, Treatment, sep= " ", remove=F)
global.cde.p_c




fig_s4d <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.cde.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.cde.p_c, aes(x =  Treatment,ymin = eff_lower,
                                           ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'd) Biomass & cover change associated \n with persistent species (PS)',
        x = '',
        y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#F98400")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_s4d




(fig_s4a + fig_s4b)/(fig_s4c + fig_s4d)

# New Figure S4

