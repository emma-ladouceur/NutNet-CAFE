

rm(list = ls())


library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)

# plot level data
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot <- plot %>% filter(year_max >= 3) 



head(plot)

plot$site_code <- as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$year_trt<-as.factor(plot$year_trt)


#plot cover
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/cover/cover_sigma.Rdata') # cover.3_sigmai
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm_sigmai.Rdata') # rich.3_sigmai
load('~/Desktop/nn mods/cde.Rdata') # rich.3_sigmai
load('~/Desktop/nn mods/sl.Rdata') # rich.3_sigmai
load('~/Desktop/nn mods/sg.Rdata') # rich.3_sigmai
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/global.p.effs.Rdata')



summary(sl.3_cover)
summary(sg.3_cover)
summary(cde.3_cover)


sl.fixed.p <- posterior_samples(sl.3_cover, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p <- posterior_samples(sg.3_cover, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) ) 
cde.fixed.p <- posterior_samples(cde.3_cover, "^b",subset = floor(runif(n = 1000, 1, max = 2000)) )




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




fig_S7d <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.cde.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.cde.p_c, aes(x =  Treatment,ymin = eff_lower,
                                        ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'd) Biomass change associated \n with persistent species (PS)',
        x = '',
        y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#F98400")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_S7d



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


fig_S7b <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.sl.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.sl.p_c, aes(x = Treatment,ymin = eff_lower,
                                         ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'b) Biomass change associated \n with species loss (SL)',
        x = '',
        y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_S7b




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


fig_S7c <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.sg.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.sg.p_c, aes(x = Treatment,ymin = eff_lower,
                                         ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'c) Biomass change associated \n with species gain (SG)',
        x = '',
        y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
#  scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_S7c


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

fig_S7a <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = global.plot.p_c, aes(x = Treatment, y = eff,color=Treatment, shape = Model),size = 2) +
  geom_errorbar(data = global.plot.p_c, aes(x = Treatment,ymin = eff_lower,
                                        ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs( title = 'a) Plot cover & strip biomass change',
    x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_S7a



(fig_S7a + fig_S7b)/(fig_S7c + fig_S7d)




p.cover <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time_cover.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.bm <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(p.cover)

cover <- p.cover %>% select(unique.id, site_code, block, plot, year.y, SL, SG, CDE) %>%
  mutate(SL_cover = SL,
         SG_cover= SG,
    CDE_cover = CDE
         ) %>% select (-c(SL,SG,CDE))
head(cover)
  

bm_cover <- p.bm %>% select(unique.id, site_code, block, plot, year.y, SL, SG, CDE) %>%
  mutate(SL_bm = SL,
         SG_bm= SG,
         CDE_bm = CDE
  ) %>% select (-c(SL,SG,CDE)) %>% left_join(cover)
head(bm_cover)


  
 ggplot() + geom_point( data = bm_cover, aes(x = SL_cover, y = SL_bm), alpha = 0.2) +
   scale_y_reverse() +
   scale_x_reverse() +
  labs( title = 'a) SL',
        x = 'SL cover',
        y = 'SL bm')+
  theme_classic()

 ggplot() + geom_point( data = bm_cover, aes(x = SG_cover, y = SG_bm), alpha = 0.2) +
   labs( title = 'a) SL',
         x = 'SL cover',
         y = 'SL bm')+
   theme_classic()

 ggplot() + geom_point( data = bm_cover, aes(x = CDE_cover, y = CDE_bm), alpha = 0.2) +
   labs( title = 'a) SL',
         x = 'SL cover',
         y = 'SL bm')+
   theme_classic()





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


 fig_S7a <- ggplot() + geom_point( data = sp_rel, aes(x = rel_cover_plot, y = rel_strip_bm_plot), alpha = 0.2) +
   labs( title = 'a) Plot',
     x = 'Species percent cover relative to plot',
         y = 'Per species biomass estimate relative to strip')+
  theme_classic()
 
 
 fig_S7b <- ggplot() + geom_point( data = sp_rel, aes(x = rel_cover_cat, y = rel_strip_bm_cat), alpha = 0.2) +
   labs( title = 'b) Category',
         x = 'Species percent cover relative to plot',
         y = 'Per species biomass estimate relative to strip')+
   theme_classic()
 
 fig_S7b
 
 
 
  
fig_S7 <-  (fig_S7a + fig_S7b)

fig_S7
