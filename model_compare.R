

rm(list=ls())



library(wesanderson)
library(tidyverse)
library(ggplot2)
library(brms)
library(viridis)
library(patchwork)

# model compare
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


load('~/Dropbox/Projects/NutNet/Model_fits/full/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Model_fits/3/bm.Rdata') # plot.bm.3
load('~/Dropbox/Projects/NutNet/Model_fits/3/rich.Rdata') # plot.rich.3


load('~/Dropbox/Projects/NutNet/Model_fits/5/bm.Rdata') # plot.bm.5
load('~/Dropbox/Projects/NutNet/Model_fits/5/rich.Rdata') # plot.rich.5


load('~/Dropbox/Projects/NutNet/Model_fits/6/bm.Rdata') # plot.bm.6
load('~/Dropbox/Projects/NutNet/Model_fits/6/rich.Rdata') # plot.rich.6



plot.rich.im_fixef <- as.data.frame(fixef(plot.rich.g))
plot.bm.im_fixef <- as.data.frame(fixef(plot.bm.s))

plot.rich.im_fixef.3 <- as.data.frame(fixef(plot.rich.3))
plot.bm.im_fixef.3 <- as.data.frame(fixef(plot.bm.3))

plot.rich.im_fixef.5 <- as.data.frame(fixef(plot.rich.5))
plot.bm.im_fixef.5 <- as.data.frame(fixef(plot.bm.5))

plot.rich.im_fixef.6 <- as.data.frame(fixef(plot.rich.6))
plot.bm.im_fixef.6 <- as.data.frame(fixef(plot.bm.6))

plot.rich.im_fixef




rich.f <-bind_rows(
  plot.rich.im_fixef['year_trt',] %>% 
    mutate(response='All years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='All years',)

rich.f


rich.f.3 <-bind_rows(
  plot.rich.im_fixef.3['year_trt',] %>% 
    mutate(response='=>3 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef.3['trtNPK:year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years',)


rich.f.5 <-bind_rows(
  plot.rich.im_fixef.5['year_trt',] %>% 
    mutate(response='=>5 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef.5['trtNPK:year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope))%>%
  mutate(response='=>5 years')


rich.f.6 <-bind_rows(
  plot.rich.im_fixef.6['year_trt',] %>% 
    mutate(response='=>6 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef.6['trtNPK:year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')

rich.f.6

rich.effs <- bind_rows(rich.f,rich.f.3,rich.f.5,rich.f.6)

rich.effs

write.csv(rich.effs, '~/Dropbox/Projects/NutNet/Data/rich.effs.inclusion.csv')

rich.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/rich.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


rich.effs$response <- factor(rich.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))

sp.slope<-ggplot() + 
  geom_point(data =rich.effs, aes(x = response, y = trt_slope, color=response),size = 2) +
  geom_errorbar(data = rich.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope, color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Change in Species / Year ')),
       title='a) NPK Effect on Change in Species Richness / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-1.2,0.2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")


sp.slope


 bm.f <-bind_rows(
  plot.bm.im_fixef['year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='All years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
   mutate(response='All years')



bm.f.3 <-bind_rows(
  plot.bm.im_fixef.3['year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef.3['trtNPK:year_trt',] %>% 
    mutate(response='=>3 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>3 years')


bm.f.5 <-bind_rows(
  plot.bm.im_fixef.5['year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef.5['trtNPK:year_trt',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>5 years')

bm.f.6 <-bind_rows(
  plot.bm.im_fixef.6['year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef.6['trtNPK:year_trt',] %>% 
    mutate(response='=>6 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>6 years')



bm.effs <- bind_rows(bm.f,bm.f.3,bm.f.5,bm.f.6)

bm.effs
write.csv(bm.effs, '~/Dropbox/Projects/NutNet/Data/bm.effs.inclusion.csv')

bm.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/bm.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


bm.effs$response <- factor(bm.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


bm.slope<-ggplot() + 
  geom_point(data =bm.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = bm.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Change in Biomass (g/' ,m^2, ') / Year ')),
       title='b) NPK Effect on Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
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

load('~/Dropbox/Projects/NutNet/Model_fits/5/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/5/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/5/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Model_fits/5/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/5/sgain.Rdata') # s.gain.s


load('~/Dropbox/Projects/NutNet/Model_fits/6/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/6/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/6/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Model_fits/6/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/6/sgain.Rdata') # s.gain.s

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

sl.trt.i_fixef.5 <- as.data.frame(fixef(sl.5))
sg.trt.i_fixef.5 <- as.data.frame(fixef(sg.5))
cde.trt.i_fixef.5 <- as.data.frame(fixef(CDE.5))
sloss.trt.i_fixef.5 <- as.data.frame(fixef(s.loss.5))
sgain.trt.i_fixef.5 <- as.data.frame(fixef(s.gain.5))

sl.trt.i_fixef.6 <- as.data.frame(fixef(sl.6))
sg.trt.i_fixef.6 <- as.data.frame(fixef(sg.6))
cde.trt.i_fixef.6 <- as.data.frame(fixef(CDE.6))
sloss.trt.i_fixef.6 <- as.data.frame(fixef(s.loss.6))
sgain.trt.i_fixef.6 <- as.data.frame(fixef(s.gain.6))


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

sl.f.5 <-bind_rows(
  sl.trt.i_fixef.5['year.y.m',] %>% 
    mutate(response='=>5 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef.5['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>5 years')

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


sl.effs <- bind_rows(sl.f,sl.f.3,sl.f.5,sl.f.6)


write.csv(sl.effs, '~/Dropbox/Projects/NutNet/Data/sl.effs.inclusion.csv')

sl.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sl.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sl.effs$response <- factor(sl.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


sl.slope<-ggplot() + 
  geom_point(data =sl.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sl.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Change in Biomass (g/' ,m^2, ') / Year ')),
       title='e) NPK Effect on Change Biomass / Year Due to Species Loss') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
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

sg.f.5 <-bind_rows(
  sg.trt.i_fixef.5['year.y.m',] %>% 
    mutate(response='=>5 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef.5['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>5 years')

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

sg.effs <- bind_rows(sg.f,sg.f.3,sg.f.5,sg.f.6)

write.csv(sg.effs, '~/Dropbox/Projects/NutNet/Data/sg.effs.inclusion.csv')

sg.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sg.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sg.effs$response <- factor(sg.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


sg.slope<-ggplot() + 
  geom_point(data =sg.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sg.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Change in Biomass (g/' ,m^2, ') / Year ')),
       title='f) NPK Effect on Change in  Biomass / Year Due to Species Gain') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
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

cde.f.5 <-bind_rows(
  cde.trt.i_fixef.5['year.y.m',] %>% 
    mutate(response='=>5 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  cde.trt.i_fixef.5['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>5 years')

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

cde.effs <- bind_rows(cde.f,cde.f.3,cde.f.5,cde.f.6)

write.csv(cde.effs, '~/Dropbox/Projects/NutNet/Data/cde.effs.inclusion.csv')

cde.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/cde.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


cde.effs$response <- factor(cde.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


cde.slope<-ggplot() + 
  geom_point(data =cde.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = cde.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Change in Biomass (g/' ,m^2, ') / Year ')),
       title='g) NPK Effect on Persistent Species Change in Biomass / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
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

sloss.f.5 <-bind_rows(
  sloss.trt.i_fixef.5['year.y.m',] %>% 
    mutate(response='=>5 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef.5['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>5 years')

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

sloss.effs <- bind_rows(sloss.f,sloss.f.3,sloss.f.5,sloss.f.6)

write.csv(sloss.effs, '~/Dropbox/Projects/NutNet/Data/sloss.effs.inclusion.csv')

sloss.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sloss.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sloss.effs$response <- factor(sloss.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


sloss.slope<-ggplot() + 
  geom_point(data =sloss.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sloss.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Species Loss / Year ')),
       title='c) NPK Effect on Species Loss  / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
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

sgain.f.5 <-bind_rows(
  sgain.trt.i_fixef.5['year.y.m',] %>% 
    mutate(response='=>5 years',
      trt_slope = Estimate,
      trt_upper_slope = Q97.5,
      trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef.5['trt.yNPK:year.y.m',] %>% 
    mutate(response='=>5 years',
           trt_slope = Estimate,
           trt_upper_slope = Q97.5,
           trt_lower_slope = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
) %>% summarise(trt_slope=sum(trt_slope),trt_upper_slope=sum(trt_upper_slope),trt_lower_slope=sum(trt_lower_slope)) %>%
  mutate(response='=>5 years')


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

sgain.effs <- bind_rows(sgain.f,sgain.f.3,sgain.f.5,sgain.f.6)

write.csv(sgain.effs, '~/Dropbox/Projects/NutNet/Data/sgain.effs.inclusion.csv')

sgain.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sgain.effs.inclusion.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sgain.effs$response <- factor(sgain.effs$response , levels=c("All years","=>3 years","=>5 years", "=>6 years"))


sgain.slope<-ggplot() + 
  geom_point(data =sgain.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sgain.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Species Gain / Year ')),
       title='d) NPK Effect on Species Gain  / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  #scale_color_viridis(discrete = TRUE, option = "C")+
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sgain.slope


(sp.slope | bm.slope)/(sloss.slope | sgain.slope)/(sl.slope | sg.slope | cde.slope)

