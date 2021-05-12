

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


#plot cover
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/cover/cover_sigma.Rdata') # cover.3_sigmai
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm_sigmai.Rdata') # rich.3_sigmai


#  model summary
summary(cover.3_sigmai)
# caterpillar plots
plot(cover.3_sigmai)
# predicted values vs. observed
color_scheme_set("darkgray")
fig_s3b <- pp_check(cover.3_sigmai) + theme_classic() + 
  labs( x = expression(paste('Biomass (g/',m^2, ')')) , y = "Density") + 
  scale_x_continuous(limits = c(-1000, 2000))

fig_s3b

# residuals (this take a minute)
colnames(plot)
plot.cover <- plot %>% filter(!is.na(plot.mass))
cover.m <- residuals(cover.3_sigmai)
cover.m <- as.data.frame(cover.m)
br.plot <- cbind(plot.cover, cover.m$Estimate)
head(br.plot)

par(mfrow=c(2,2))
with(br.plot, plot(site_code, cover.m$Estimate))
with(br.plot, plot(block, cover.m$Estimate))
with(br.plot, plot(year_trt, cover.m$Estimate))
with(br.plot, plot(plot, cover.m$Estimate))




bm.fixed.p<-posterior_samples(bm.3_sigmai, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 

# biomass
bm_global_posterior <-  bm.fixed.p %>% dplyr::select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(bm.ctl.global =`b_year_trt`,
         bm.npk.global=`b_trtNPK:year_trt`,
         bm.trt.global=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  dplyr::select(-c(`b_year_trt`,`b_trtNPK:year_trt`)) 

head(bm_global_posterior)

# take the mean quantiles of the fixed effects posteriors for each treatment
bm.p.npk <-  bm_global_posterior %>% 
  mutate( response="NPK", eff = mean(bm.trt.global),
          eff_lower = quantile(bm.trt.global, probs=0.025),
          eff_upper = quantile(bm.trt.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  

bm.p.ctl <-  bm_global_posterior %>% 
  mutate( response="Control", eff = mean(bm.ctl.global),
          eff_lower = quantile(bm.ctl.global, probs=0.025),
          eff_upper = quantile(bm.ctl.global, probs=0.975))  %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()  
# combine them into one data frame
global.bm.p <- bind_rows(bm.p.npk,bm.p.ctl)

global.bm.p


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


global.bm.p$Model<-"Biomass"
global.cover.p$Model<-"Cover"
global.bm.p <- global.bm.p %>% rename(Treatment = response) 
global.cover.p <- global.cover.p %>% rename(Treatment = response) 
fitted.plot<-bind_rows(global.bm.p,global.cover.p)

fitted.plot$Treatment <- factor(fitted.plot$Treatment , levels=c("Control", "NPK"))
fitted.plot


fig_S7a <- ggplot() + 
  facet_wrap(~Model)+
  geom_point(data = fitted.plot, aes(x = Treatment, y = eff,color=Treatment),size = 2) +
  geom_errorbar(data = fitted.plot, aes(x = Treatment,ymin = eff_lower,
                                        ymax = eff_upper,color=Treatment),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,30)) +
  scale_color_manual(values = c("#000000","#0B775E")) +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")


fig_S7a

