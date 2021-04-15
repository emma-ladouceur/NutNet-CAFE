



library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)



load('~/Dropbox/Projects/NutNet/Model_fits/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.g

load('~/Dropbox/Projects/NutNet/Model_fits/sl.n.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s


load('~/Dropbox/Projects/NutNet/Model_fits/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s



sl.trt.i_fixef <-  as.data.frame(fixef(sl.s))
sg.trt.i_fixef <- as.data.frame(fixef(sg.s))
CDE.trt.i_fixef <- as.data.frame( fixef(CDE.s))
plot.rich.im_fixef <- as.data.frame(fixef(plot.rich.g))
plot.bm.im_fixef <- as.data.frame(fixef(plot.bm.s))
sloss.trt.i_fixef <- as.data.frame(fixef(s.loss.s))
sgain.trt.i_fixef <-as.data.frame( fixef(s.gain.s))

plot.rich.im_fixef
sgain.trt.i_fixef


rich.f <-bind_cols(
  plot.rich.im_fixef['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='Species Richness',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

bm.f <-bind_cols(
  plot.bm.im_fixef['trtNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='Biomass',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


sl.f <-bind_cols(
  sl.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Species Loss',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sg.f <-bind_cols(
  sg.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Species Gain',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


cde.f <-bind_cols(
  CDE.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  CDE.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Persistent Species',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sloss.f <-bind_cols(
  sloss.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
           trt = Estimate,
           trt_upper = Q97.5,
           trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Species Loss',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  )

sgain.f <-bind_cols(
  sgain.trt.i_fixef['trt.yNPK',] %>% 
    mutate(
      trt = Estimate,
      trt_upper = Q97.5,
      trt_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Species Gain',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sgain.f

sp.effs<-bind_rows(rich.f,sloss.f,sgain.f)

bm.effs <-bind_rows(bm.f,sl.f,sg.f,cde.f)

head(all.effs)

write.csv(sp.effs,"~/Dropbox/Projects/NutNet/Data/sp.effs.csv")
write.csv(bm.effs,"~/Dropbox/Projects/NutNet/Data/bm.effs.csv")


sp.effs<-read.csv("~/Dropbox/Projects/NutNet/Data/sp.effs.csv")
bm.effs<-read.csv("~/Dropbox/Projects/NutNet/Data/bm.effs.csv")


sp.effs$response <- factor(sp.effs$response , levels=c("Species Richness","Species Loss","Species Gain"))

sp.slope<-ggplot() + 
  geom_point(data =sp.effs, aes(x = response, y = trt_slope, color=response),size = 2) +
  geom_errorbar(data = sp.effs, aes(x = response,ymin = trt_lower_slope,
                                          ymax = trt_upper_slope, color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Species / Year ')),
      title='b) NPK Effect on Change in Species / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  ylim(-1.2,0.2) +
  scale_color_manual(values = c("#F98400", "#B40F20","#3B9AB2")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")


sp.slope

# sp rich  "#F98400",
# biomass "#0B775E",
# loss "#B40F20",
# gain "#3B9AB2",
# persistent "#35274A"



bm.effs$response <- factor(bm.effs$response , levels=c("Biomass","Species Loss","Species Gain", "Persistent Species"))

bm.slope<-ggplot() + 
  geom_point(data =bm.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = bm.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year ')),
       title='d) NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")


bm.slope
sp.trt<-ggplot() + 
  geom_point(data =sp.effs, aes(x = response, y = trt,color=response),size = 2) +
  geom_errorbar(data = sp.effs, aes(x = response,ymin = trt_lower,
                                    ymax = trt_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Species')),
       title='a) NPK Effect on Species') +
  geom_hline(yintercept = 0, lty = 2) +
  ylim(-1.2,0.2) +
   scale_color_manual(values = c("#F98400","#B40F20","#3B9AB2")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")

sp.trt

bm.trt<-ggplot() + 
  geom_point(data =bm.effs, aes(x = response, y = trt,color=response),size = 2) +
  geom_errorbar(data = bm.effs, aes(x = response,ymin = trt_lower,
                                    ymax = trt_upper,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Biomass (g/' ,m^2, ') ')),
       title='c) NPK Effect on Biomass ') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,50) +
  scale_color_manual(values = c("#0B775E", "#B40F20","#3B9AB2","#35274A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")



bm.trt

grid.arrange(sp.trt,sp.slope,bm.trt,bm.slope,nrow=2,ncol=2)


