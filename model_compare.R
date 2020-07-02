

library(tidyverse)
library(ggplot2)
library(brms)
library(viridis)
library(patchwork)

# model compare
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


load('~/Dropbox/Projects/NutNet/Model_fits/full/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Model_fits/3/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/rich.Rdata') # plot.rich.g



load('~/Dropbox/Projects/NutNet/Model_fits/5/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/5/rich.Rdata') # plot.rich.g



load('~/Dropbox/Projects/NutNet/Model_fits/6/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/6/rich.Rdata') # plot.rich.g




plot.rich.im_fixef <- as.data.frame(fixef(plot.rich.g))
plot.bm.im_fixef <- as.data.frame(fixef(plot.bm.s))

plot.rich.im_fixef.3 <- as.data.frame(fixef(plot.rich.3))
plot.bm.im_fixef.3 <- as.data.frame(fixef(plot.bm.3))

plot.rich.im_fixef.5 <- as.data.frame(fixef(plot.rich.5))
plot.bm.im_fixef.5 <- as.data.frame(fixef(plot.bm.5))

plot.rich.im_fixef.6 <- as.data.frame(fixef(plot.rich.6))
plot.bm.im_fixef.6 <- as.data.frame(fixef(plot.bm.6))

rich.f <-bind_cols(
  plot.rich.im_fixef['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

rich.f.3 <-bind_cols(
  plot.rich.im_fixef.3['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef.3['trtNPK:year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

rich.f.5 <-bind_cols(
  plot.rich.im_fixef.5['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef.5['trtNPK:year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

rich.f.6 <-bind_cols(
  plot.rich.im_fixef.6['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef.6['trtNPK:year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


rich.effs <- bind_rows(rich.f,rich.f.3,rich.f.5,rich.f.6)


rich.effs$response <- factor(rich.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))

sp.slope<-ggplot() + 
  geom_point(data =rich.effs, aes(x = response, y = trt_slope, color=response),size = 2) +
  geom_errorbar(data = rich.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope, color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Species / Year ')),
       title='a) Overall NPK Effect on Change in Species Richness / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  ylim(-1.2,0.2) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = c("#F98400", "#B40F20","#3B9AB2")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")


sp.slope


bm.f <-bind_cols(
  plot.bm.im_fixef['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)



bm.f.3 <-bind_cols(
  plot.bm.im_fixef.3['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef.3['trtNPK:year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


bm.f.5 <-bind_cols(
  plot.bm.im_fixef.5['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef.5['trtNPK:year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

bm.f.6 <-bind_cols(
  plot.bm.im_fixef.6['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef.6['trtNPK:year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)



bm.effs <- bind_rows(bm.f,bm.f.3,bm.f.5,bm.f.6)

bm.effs$response <- factor(bm.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


bm.slope<-ggplot() + 
  geom_point(data =bm.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = bm.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year ')),
       title='b) Overall NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")

bm.slope

(sp.slope)/(bm.slope)




# price effects

load('~/Dropbox/Projects/NutNet/Model_fits/full/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Model_fits/full/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/sgain.Rdata') # s.gain.s



load('~/Dropbox/Projects/NutNet/Model_fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/cde.Rdata') # CDE.s

load('~/Dropbox/Projects/NutNet/Model_fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/3/sgain.Rdata') # s.gain.s




sl.trt.i_fixef <- as.data.frame(fixef(sl.s))
sg.trt.i_fixef <- as.data.frame(fixef(sg.s))
cde.trt.i_fixef <- as.data.frame(fixef(CDE.s))
sloss.trt.i_fixef <- as.data.frame(fixef(s.loss.n.s))
sgain.trt.i_fixef <- as.data.frame(fixef(s.gain.s))



sl.trt.i_fixef.3 <- as.data.frame(fixef(sl.3))
sg.trt.i_fixef.3 <- as.data.frame(fixef(sg.3))
cde.trt.i_fixef.3 <- as.data.frame(fixef(CDE.3))
sloss.trt.i_fixef.3 <- as.data.frame(fixef(s.loss.3))
sgain.trt.i_fixef.3 <- as.data.frame(fixef(s.gain.3))


sl.f <-bind_cols(
  sl.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sl.f.3 <-bind_cols(
  sl.trt.i_fixef.3['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)



sl.effs <- bind_rows(sl.f,sl.f.3)

sl.effs$response <- factor(sl.effs$response , levels=c("All years","=>3 years"#,"=>5 years", "=>6 years"))
))

sl.slope<-ggplot() + 
  geom_point(data =sl.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sl.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year ')),
       title='b) Overall NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sl.slope



sg.f <-bind_cols(
  sg.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sg.f.3 <-bind_cols(
  sg.trt.i_fixef.3['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)



sg.effs <- bind_rows(sg.f,sg.f.3)

sg.effs$response <- factor(sg.effs$response , levels=c("All years","=>3 years"#,"=>5 years", "=>6 years"))
))

sg.slope<-ggplot() + 
  geom_point(data =sg.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sg.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year ')),
       title='b) Overall NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sg.slope



cde.f <-bind_cols(
  cde.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
 cde.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

cde.f.3 <-bind_cols(
  cde.trt.i_fixef.3['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  cde.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)



cde.effs <- bind_rows(cde.f,cde.f.3)

cde.effs$response <- factor(cde.effs$response , levels=c("All years","=>3 years"#,"=>5 years", "=>6 years"))
))

cde.slope<-ggplot() + 
  geom_point(data =cde.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = cde.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year ')),
       title='b) Overall NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
cde.slope




sloss.f <-bind_cols(
  sloss.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sloss.f.3 <-bind_cols(
  sl.trt.i_fixef.3['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)



sloss.effs <- bind_rows(sl.f,sl.f.3)

sloss.effs$response <- factor(sloss.effs$response , levels=c("All years","=>3 years"#,"=>5 years", "=>6 years"))
))

sloss.slope<-ggplot() + 
  geom_point(data =sloss.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sloss.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year ')),
       title='b) Overall NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sloss.slope



sgain.f <-bind_cols(
  sg.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sgain.f.3 <-bind_cols(
  sgain.trt.i_fixef.3['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)



sgain.effs <- bind_rows(sg.f,sg.f.3)

sgain.effs$response <- factor(sgain.effs$response , levels=c("All years","=>3 years"#,"=>5 years", "=>6 years"))
))

sgain.slope<-ggplot() + 
  geom_point(data =sgain.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sgain.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year ')),
       title='b) Overall NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sgain.slope

