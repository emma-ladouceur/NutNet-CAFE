

# Authors: Emma Ladouceur 
# Title:
# Last Updated April 18, 2021

# 13_Supplementary_Figure_S4.R
# This workflow makes Supplementary Figures S4

# packages
library(tidyverse)
library(ggplot2)
library(brms)
library(wesanderson)
library(patchwork)

# data
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


# model objects
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/full/bm_sigma.Rdata') # plot.bm.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/full/rich_sigma.Rdata') # plot.rich.g


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/bm_sigmai.Rdata') # plot.bm.3
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/3/rich_sigmai.Rdata') # plot.rich.3


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/10/bm_sigma.Rdata') # plot.bm.5
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/10/rich_sigma.Rdata') # plot.rich.5


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/6/bm_sigma.Rdata') # plot.bm.6
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_fits/6/rich_sigma.Rdata') # plot.rich.6


rich.im_fixef <- as.data.frame(fixef(rich.all_sigmai))
bm.im_fixef <- as.data.frame(fixef(bm.all_sigmai))

rich.im_fixef.3 <- as.data.frame(fixef(rich.3_sigmai))
bm.im_fixef.3 <- as.data.frame(fixef(bm.3_sigmai))

rich.im_fixef.10 <- as.data.frame(fixef(rich.10_sigmai))
bm.im_fixef.10 <- as.data.frame(fixef(bm.10_sigmai))

rich.im_fixef.6 <- as.data.frame(fixef(rich.6_sigmai))
bm.im_fixef.6 <- as.data.frame(fixef(bm.6_sigmai))

rich.im_fixef


rich.f <-bind_rows(
  rich.im_fixef['year_trt',] %>% 
    mutate(response='All years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  rich.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='All years',)

rich.f


rich.f.3 <-bind_rows(
  rich.im_fixef.3['year_trt',] %>% 
    mutate(response='=>3 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  rich.im_fixef.3['trtNPK:year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years',)


rich.f.10 <-bind_rows(
  rich.im_fixef.10['year_trt',] %>% 
    mutate(response='=>5 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  rich.im_fixef.10['trtNPK:year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope))%>%
  mutate(response='=>10 years')


rich.f.6 <-bind_rows(
  rich.im_fixef.6['year_trt',] %>% 
    mutate(response='=>6 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  rich.im_fixef.6['trtNPK:year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')

rich.f.6

rich.effs <- bind_rows(rich.f,rich.f.3,rich.f.10,rich.f.6)

rich.effs

write.csv(rich.effs, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich.effs.inclusion.csv')

rich.effs <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


rich.effs$response <- factor(rich.effs$response , levels=c("All years","=>3 years","=>6 years", "=>10 years"))

sp.slope <- ggplot() + 
  geom_point(data =rich.effs, aes(x = response, y = trt_slope, color=response),size = 2) +
  geom_errorbar(data = rich.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y= expression(paste('Change in species richness / year ')),
       title='a) NPK effect on change in species richness / year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")


sp.slope


 bm.f <-bind_rows(
  bm.im_fixef['year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  bm.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
   mutate(response='All years')



bm.f.3 <-bind_rows(
  bm.im_fixef.3['year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  bm.im_fixef.3['trtNPK:year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years')


bm.f.10 <-bind_rows(
  bm.im_fixef.10['year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  bm.im_fixef.10['trtNPK:year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>10 years')

bm.f.6 <-bind_rows(
  bm.im_fixef.6['year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  bm.im_fixef.6['trtNPK:year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')



bm.effs <- bind_rows(bm.f,bm.f.3,bm.f.10,bm.f.6)

bm.effs
write.csv(bm.effs, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/bm.effs.inclusion.csv')

bm.effs <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/bm.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


bm.effs$response <- factor(bm.effs$response , levels=c("All years","=>3 years","=>6 years", "=>10 years"))


bm.slope<-ggplot() + 
  geom_point(data =bm.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = bm.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y= expression(paste('Change in biomass (g/' ,m^2, ') / year ')),
       title='b) NPK effect on change in biomass / year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")

bm.slope

(sp.slope)/(bm.slope)




# price effects

rm(list = ls())

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/full/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/full/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/full/cde_sigma.Rdata') # CDE.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/full/sloss_sigma.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/full/sgain_sigma.Rdata') # s.gain.s



load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde_sigmai.Rdata') # CDE.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss_sigmai.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain_sigmai.Rdata') # s.gain.s

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/10/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/10/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/10/cde_sigma.Rdata') # CDE.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/10/sloss_sigma.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/10/sgain_sigma.Rdata') # s.gain.s


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/6/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/6/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/6/cde_sigma.Rdata') # CDE.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/6/sloss_sigma.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/6/sgain_sigma.Rdata') # s.gain.s

sl.trt.i_fixef <- as.data.frame(fixef(sl.all))
sg.trt.i_fixef <- as.data.frame(fixef(sg.all))
cde.trt.i_fixef <- as.data.frame(fixef(cde.all_sigmai))
sloss.trt.i_fixef <- as.data.frame(fixef(sloss.all_sigmai))
sgain.trt.i_fixef <- as.data.frame(fixef(sgain.all_sigmai))


sl.trt.i_fixef.3 <- as.data.frame(fixef(sl.3))
sg.trt.i_fixef.3 <- as.data.frame(fixef(sg.3))
cde.trt.i_fixef.3 <- as.data.frame(fixef(cde.3_sigmai))
sloss.trt.i_fixef.3 <- as.data.frame(fixef(sloss.3_sigmai))
sgain.trt.i_fixef.3 <- as.data.frame(fixef(sgain.3_sigmai))

sl.trt.i_fixef.10 <- as.data.frame(fixef(sl.10))
sg.trt.i_fixef.10 <- as.data.frame(fixef(sg.10))
cde.trt.i_fixef.10 <- as.data.frame(fixef(cde.10_sigmai))
sloss.trt.i_fixef.10 <- as.data.frame(fixef(sloss.10_sigmai))
sgain.trt.i_fixef.10 <- as.data.frame(fixef(sgain.10_sigmai))

sl.trt.i_fixef.6 <- as.data.frame(fixef(sl.6))
sg.trt.i_fixef.6 <- as.data.frame(fixef(sg.6))
cde.trt.i_fixef.6 <- as.data.frame(fixef(cde.6_sigmai))
sloss.trt.i_fixef.6 <- as.data.frame(fixef(sloss.6_sigmai))
sgain.trt.i_fixef.6 <- as.data.frame(fixef(sgain.6_sigmai))


sl.f <-bind_rows(
  sl.trt.i_fixef['year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='All years',)

sl.f.3 <-bind_rows(
  sl.trt.i_fixef.3['year.y.m',] %>% 
    mutate(response='=>3 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years')

sl.f.10 <-bind_rows(
  sl.trt.i_fixef.10['year.y.m',] %>% 
    mutate(response='=>10 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef.10['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>10 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>10 years')

sl.f.6 <-bind_rows(
  sl.trt.i_fixef.6['year.y.m',] %>% 
    mutate(response='=>6 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef.6['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)  %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')


sl.effs <- bind_rows(sl.f,sl.f.3,sl.f.10,sl.f.6)


write.csv(sl.effs, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl.effs.inclusion.csv')

sl.effs <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sl.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sl.effs$response <- factor(sl.effs$response , levels=c("All years","=>3 years","=>6 years", "=>10 years"))


sl.slope<-ggplot() + 
  geom_point(data =sl.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sl.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y= expression(paste('Biomass change (g/' ,m^2, ') / year ')),
       title='e) NPK effect on biomass change / year \n associated with species loss (SL)') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sl.slope



sg.f <-bind_rows(
  sg.trt.i_fixef['year.y.m',] %>% 
    mutate(response='All years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='All years',)

sg.f.3 <-bind_rows(
  sg.trt.i_fixef.3['year.y.m',] %>% 
    mutate(response='=>3 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years')

sg.f.10 <-bind_rows(
  sg.trt.i_fixef.10['year.y.m',] %>% 
    mutate(response='=>10 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef.10['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>10 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>10 years')

sg.f.6 <-bind_rows(
  sg.trt.i_fixef.6['year.y.m',] %>% 
    mutate(response='=>6 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef.6['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')

sg.effs <- bind_rows(sg.f,sg.f.3,sg.f.10,sg.f.6)

write.csv(sg.effs, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sg.effs.inclusion.csv')

sg.effs <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sg.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sg.effs$response <- factor(sg.effs$response , levels=c("All years","=>3 years","=>6 years", "=>10 years"))


sg.slope<-ggplot() + 
  geom_point(data =sg.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sg.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Biomass change (g/' ,m^2, ') / Year ')),
       title='f) NPK effect on biomass change / year \n associated with species gain (SG)') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sg.slope



cde.f <-bind_rows(
  cde.trt.i_fixef['year.y.m',] %>% 
    mutate(response='All years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
 cde.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='All years',)

cde.f.3 <-bind_rows(
  cde.trt.i_fixef.3['year.y.m',] %>% 
    mutate(response='=>3 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  cde.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years')

cde.f.10 <-bind_rows(
  cde.trt.i_fixef.10['year.y.m',] %>% 
    mutate(response='=>10 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  cde.trt.i_fixef.10['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>10 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>10 years')

cde.f.6 <-bind_rows(
  cde.trt.i_fixef.6['year.y.m',] %>% 
    mutate(response='=>6 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  cde.trt.i_fixef.6['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')

cde.effs <- bind_rows(cde.f,cde.f.3,cde.f.10,cde.f.6)

write.csv(cde.effs, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/cde.effs.inclusion.csv')

cde.effs <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/cde.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


cde.effs$response <- factor(cde.effs$response , levels=c("All years","=>3 years","=>6 years", "=>10 years"))


cde.slope<-ggplot() + 
  geom_point(data =cde.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = cde.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Biomass change (g/' ,m^2, ') / Year ')),
       title='g) NPK effect on biomass change / year \n associated with persistent species (PS)') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
cde.slope




sloss.f <-bind_rows(
  sloss.trt.i_fixef['year.y.m',] %>% 
    mutate(response='All years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='All years',)

sloss.f.3 <-bind_rows(
  sloss.trt.i_fixef.3['year.y.m',] %>% 
    mutate(response='=>3 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years')

sloss.f.10 <-bind_rows(
  sloss.trt.i_fixef.10['year.y.m',] %>% 
    mutate(response='=>10 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef.10['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>10 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>10 years')

sloss.f.6 <-bind_rows(
  sloss.trt.i_fixef.6['year.y.m',] %>% 
    mutate(response='=>6 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef.6['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')

sloss.effs <- bind_rows(sloss.f,sloss.f.3,sloss.f.10,sloss.f.6)

write.csv(sloss.effs, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss.effs.inclusion.csv')

sloss.effs <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sloss.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sloss.effs$response <- factor(sloss.effs$response , levels=c("All years","=>3 years","=>6 years", "=>10 years"))


sloss.slope<-ggplot() + 
  geom_point(data =sloss.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sloss.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Species loss / year ')),
       title='c) NPK effect on species loss  / year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sloss.slope



sgain.f <-bind_rows(
  sgain.trt.i_fixef['year.y.m',] %>% 
    mutate(response='All years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='All years',)

sgain.f.3 <-bind_rows(
  sgain.trt.i_fixef.3['year.y.m',] %>% 
    mutate(response='=>3 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef.3['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years')

sgain.f.10 <-bind_rows(
  sgain.trt.i_fixef.10['year.y.m',] %>% 
    mutate(response='=>10 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef.10['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>10 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>10 years')


sgain.f.6 <-bind_rows(
  sgain.trt.i_fixef.6['year.y.m',] %>% 
    mutate(response='=>6 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef.6['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')

sgain.effs <- bind_rows(sgain.f,sgain.f.3,sgain.f.10,sgain.f.6)

write.csv(sgain.effs, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain.effs.inclusion.csv')

sgain.effs <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/sgain.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sgain.effs$response <- factor(sgain.effs$response , levels=c("All years","=>3 years","=>6 years", "=>10 years"))


sgain.slope<-ggplot() + 
  geom_point(data =sgain.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sgain.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Species gain / year ')),
       title='d) NPK effect on species gain  / year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sgain.slope

# LANDSCAPE 10 X 15
(sp.slope | bm.slope)/(sloss.slope | sgain.slope)/(sl.slope | sg.slope | cde.slope)

