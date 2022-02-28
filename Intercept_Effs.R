
# packages
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(patchwork)


# load modelobjects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.s

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.s



meta <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- meta %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()

colnames(meta)

# Similarly  to '7_Model_Data_Posteriors.R' we 
# Extract 1000 posterior samples from Fixed Effects (Overall/Population/Global Effects) 
# for the price partitions
sloss.fixed.p <- posterior_samples(sloss.3_p, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sgain.fixed.p <- posterior_samples(sgain.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 

cde.fixed.p <- posterior_samples(cde.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) )
sl.fixed.p <- posterior_samples(sl.3_p, "^b" , subset = floor(runif(n = 2000, 1, max = 3000))) 
sg.fixed.p <- posterior_samples(sg.3_p, "^b",subset = floor(runif(n = 2000, 1, max = 3000)) ) 


nrow(sl.fixed.p)


sl.fixed.p2 <- sl.fixed.p %>% 
  mutate(sl.trt.p = `b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sl.ctl.p =`b_Intercept`) %>%
  select(sl.ctl.p,
         sl.trt.p) 


sg.fixed.p2 <- sg.fixed.p %>% 
  mutate(sg.trt.p=`b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sg.ctl.p=`b_Intercept`) %>%
  select(sg.ctl.p,
         sg.trt.p)

cde.fixed.p2 <-cde.fixed.p %>% 
  mutate(cde.trt.p=`b_Intercept` + `b_trt.yNPK`) %>%
  mutate(cde.ctl.p=`b_Intercept`) %>%
  select(cde.ctl.p,
         cde.trt.p) 


sloss.fixed.p2 <-sloss.fixed.p %>% 
  mutate(sloss.trt.p=`b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sloss.ctl.p=`b_Intercept`) %>%
  select(sloss.ctl.p,
         sloss.trt.p) 


sgain.fixed.p2 <-sgain.fixed.p %>% 
  mutate(sgain.trt.p=`b_Intercept` + `b_trt.yNPK`) %>%
  mutate(sgain.ctl.p=`b_Intercept`) %>%
  select(sgain.ctl.p,
         sgain.trt.p)


sl.p.npk <-  sl.fixed.p2 %>% 
  mutate( Treatment="NPK", eff = mean(sl.trt.p),
          eff_lower = quantile(sl.trt.p, probs=0.025),
          eff_upper = quantile(sl.trt.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "SL")

sl.p.ctl <-  sl.fixed.p2 %>% 
  mutate( Treatment="Control", eff = mean(sl.ctl.p),
          eff_lower = quantile(sl.ctl.p, probs=0.025),
          eff_upper = quantile(sl.ctl.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "SL")
# combine them into one data frame
global.sl.p <- bind_rows(sl.p.npk,sl.p.ctl)

global.sl.p



sg.p.npk <-  sg.fixed.p2 %>% 
  mutate( Treatment="NPK", eff = mean(sg.trt.p),
          eff_lower = quantile(sg.trt.p, probs=0.025),
          eff_upper = quantile(sg.trt.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "SG")

sg.p.ctl <-  sg.fixed.p2 %>% 
  mutate( Treatment="Control", eff = mean(sg.ctl.p),
          eff_lower = quantile(sg.ctl.p, probs=0.025),
          eff_upper = quantile(sg.ctl.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "SG")

# combine them into one data frame
global.sg.p <- bind_rows(sg.p.npk,sg.p.ctl)

global.sg.p

cde.p.npk <-  cde.fixed.p2 %>% 
  mutate( Treatment="NPK", eff = mean(cde.trt.p),
          eff_lower = quantile(cde.trt.p, probs=0.025),
          eff_upper = quantile(cde.trt.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "CDE")

cde.p.ctl <-  cde.fixed.p2 %>% 
  mutate( Treatment="Control", eff = mean(cde.ctl.p),
          eff_lower = quantile(cde.ctl.p, probs=0.025),
          eff_upper = quantile(cde.ctl.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "CDE")
# combine them into one data frame
global.cde.p <- bind_rows(cde.p.npk,cde.p.ctl)

global.cde.p


sgain.p.npk <-  sgain.fixed.p2 %>% 
  mutate( Treatment="NPK", eff = mean(sgain.trt.p),
          eff_lower = quantile(sgain.trt.p, probs=0.025),
          eff_upper = quantile(sgain.trt.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "s.gain")

sgain.p.ctl <-  sgain.fixed.p2 %>% 
  mutate( Treatment="Control", eff = mean(sgain.ctl.p),
          eff_lower = quantile(sgain.ctl.p, probs=0.025),
          eff_upper = quantile(sgain.ctl.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "s.gain")
# combine them into one data frame
global.sgain.p <- bind_rows(sgain.p.npk,sgain.p.ctl)

global.sgain.p


sloss.p.npk <-  sloss.fixed.p2 %>% 
  mutate( Treatment="NPK", eff = mean(sloss.trt.p),
          eff_lower = quantile(sloss.trt.p, probs=0.025),
          eff_upper = quantile(sloss.trt.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "s.loss")

sloss.p.ctl <-  sloss.fixed.p2 %>% 
  mutate( Treatment="Control", eff = mean(sloss.ctl.p),
          eff_lower = quantile(sloss.ctl.p, probs=0.025),
          eff_upper = quantile(sloss.ctl.p, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,Treatment)) %>% distinct()   %>%
  mutate( Response = "s.loss")
# combine them into one data frame
global.sloss.p <- bind_rows(sloss.p.npk,sloss.p.ctl)

global.sloss.p


all.effs <- global.sl.p %>% bind_rows(
  global.sg.p, global.cde.p, global.sloss.p,
  global.sgain.p
)


head(all.effs)
nrow(all.effs)



fig_effsa <- ggplot() + 
  facet_wrap(~Response, scales="free_y") +
  geom_point(data = all.effs %>% filter(Response == "SL"), aes(x = Treatment, y = eff, color=Treatment),size = 2) +
  geom_errorbar(data = all.effs%>% filter(Response == "SL"), aes(x = Treatment, ymin = eff_lower,
                                         ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Effect Estimate') +
  geom_hline(yintercept = 0, lty = 2) +
 # scale_y_continuous(breaks=c(-8,0,4,14)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_effsa

fig_effsb <- ggplot() + 
  facet_wrap(~Response, scales="free_y") +
  geom_point(data = all.effs %>% filter(Response == "SG"), aes(x = Treatment, y = eff, color=Treatment),size = 2) +
  geom_errorbar(data = all.effs%>% filter(Response == "SG"), aes(x = Treatment, ymin = eff_lower,
                                                                 ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Effect Estimate') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(-8,0,4,14)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_effsb



fig_effsc <- ggplot() + 
  facet_wrap(~Response, scales="free_y") +
  geom_point(data = all.effs %>% filter(Response == "CDE"), aes(x = Treatment, y = eff, color=Treatment),size = 2) +
  geom_errorbar(data = all.effs%>% filter(Response == "CDE"), aes(x = Treatment, ymin = eff_lower,
                                                                 ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Effect Estimate') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(-8,0,4,14)) +
  scale_color_manual(values = c("#000000","#F98400")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_effsc



fig_effse <- ggplot() + 
  facet_wrap(~Response, scales="free_y") +
  geom_point(data = all.effs %>% filter(Response == "s.gain"), aes(x = Treatment, y = eff, color=Treatment),size = 2) +
  geom_errorbar(data = all.effs%>% filter(Response == "s.gain"), aes(x = Treatment, ymin = eff_lower,
                                                                 ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Effect Estimate') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(-8,0,4,14)) +
  scale_color_manual(values = c("#000000","#B40F20")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_effse

fig_effsd <- ggplot() + 
  facet_wrap(~Response, scales="free_y") +
  geom_point(data = all.effs %>% filter(Response == "s.loss"), aes(x = Treatment, y = eff, color=Treatment),size = 2) +
  geom_errorbar(data = all.effs%>% filter(Response == "s.loss"), aes(x = Treatment, ymin = eff_lower,
                                                                 ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Effect Estimate') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(-8,0,4,14)) +
  scale_color_manual(values = c("#000000","#046C9A")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_effsd


eff_top <- (fig_effsa + fig_effsb + fig_effsc)

eff_bottom <- ( fig_effsd + fig_effse)

effs <- (eff_top / eff_bottom)

effs
