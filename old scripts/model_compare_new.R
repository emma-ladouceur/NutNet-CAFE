




rm(list=ls())



library(wesanderson)
library(tidyverse)
library(ggplot2)
library(brms)
library(viridis)
library(patchwork)

# model compare
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


load('~/Dropbox/Projects/NutNet/Data/Model_fits/full/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/full/rich.Rdata') # plot.rich.g


load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/bm.Rdata') # plot.bm.3
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/rich.Rdata') # plot.rich.3


load('~/Dropbox/Projects/NutNet/Data/Model_fits/5/bm.Rdata') # plot.bm.5
load('~/Dropbox/Projects/NutNet/Data/Model_fits/5/rich.Rdata') # plot.rich.5


load('~/Dropbox/Projects/NutNet/Data/Model_fits/6/bm.Rdata') # plot.bm.6
load('~/Dropbox/Projects/NutNet/Data/Model_fits/6/rich.Rdata') # plot.rich.6





rich.fixed.p<-posterior_samples(plot.rich.g, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
rich.fixed.p.3<-posterior_samples(plot.rich.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
rich.fixed.p.5<-posterior_samples(plot.rich.5, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
rich.fixed.p.6<-posterior_samples(plot.rich.6, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 



rich.p <-  rich.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='All years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

rich.p.3 <-  rich.fixed.p.3 %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='=>3 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

rich.p.5 <-  rich.fixed.p.5 %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='=>5 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

rich.p.6 <-  rich.fixed.p.6 %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='=>6 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()  

rich.effs <- bind_rows(rich.p,rich.p.3,rich.p.5,rich.p.6)

rich.effs

write.csv(rich.effs, '~/Dropbox/Projects/NutNet/Data/rich.effs.compare.csv')

rich.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/rich.effs.compare.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


rich.effs$response <- factor(rich.effs$response , levels=c("=>3 years","=>5 years", "=>6 years","All years"))


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





bm.fixed.p<-posterior_samples(plot.bm.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p.3<-posterior_samples(plot.bm.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p.5<-posterior_samples(plot.bm.5, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p.6<-posterior_samples(plot.bm.6, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 



bm.p <-  bm.fixed.p %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='All years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

bm.p.3 <-  bm.fixed.p.3 %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='=>3 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

bm.p.5 <-  bm.fixed.p.5 %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='=>5 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

bm.p.6 <-  bm.fixed.p.6 %>% select(`b_year_trt`,`b_trtNPK:year_trt`) %>%
  mutate(ctl.slope =`b_year_trt`,
         npk.slope=`b_trtNPK:year_trt`,
         trt.slope=(`b_year_trt`+`b_trtNPK:year_trt`)) %>%
  select(-c(`b_year_trt`,`b_trtNPK:year_trt`,npk.slope,ctl.slope)) %>%
  mutate( response='=>6 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()  

bm.effs <- bind_rows(bm.p,bm.p.3,bm.p.5,bm.p.6)

bm.effs

write.csv(bm.effs, '~/Dropbox/Projects/NutNet/Data/bm.effs.compare.csv')

bm.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/bm.effs.compare.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



bm.effs$response <- factor(bm.effs$response , levels=c("=>3 years","=>5 years", "=>6 years","All years"))


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




load('~/Desktop/full/sl.Rdata') # sl.s
load('~/Desktop/full/sg.Rdata') # sg.s
load('~/Desktop/full/cde.Rdata') # CDE.s
load('~/Desktop/full/sloss.Rdata') # s.loss.s
load('~/Desktop/full/sgain.Rdata') # s.gain.s


load('~/Desktop/6/sl.Rdata') # sl.s
load('~/Desktop/6/sg.Rdata') # sg.s
load('~/Desktop/6/cde.Rdata') # CDE.s
load('~/Desktop/6/sloss.Rdata') # s.loss.s
load('~/Desktop/6/sgain.Rdata') # s.gain.s


# load('~/Dropbox/Projects/NutNet/Data/Model_fits/full/sl.Rdata') # sl.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/full/sg.Rdata') # sg.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/full/cde.Rdata') # CDE.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/full/sloss.n.Rdata') # s.loss.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/full/sgain.Rdata') # s.gain.s

load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/3/sgain.Rdata') # s.gain.s

load('~/Dropbox/Projects/NutNet/Data/Model_fits/5/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/5/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/5/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/5/sloss.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Data/Model_fits/5/sgain.Rdata') # s.gain.s


# load('~/Dropbox/Projects/NutNet/Data/Model_fits/6/sl.Rdata') # sl.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/6/sg.Rdata') # sg.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/6/cde.Rdata') # CDE.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/6/sloss.Rdata') # s.loss.s
# load('~/Dropbox/Projects/NutNet/Data/Model_fits/6/sgain.Rdata') # s.gain.s




sl.fixed.p<-posterior_samples(sl.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sl.fixed.p.3<-posterior_samples(sl.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sl.fixed.p.5<-posterior_samples(sl.5, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sl.fixed.p.6<-posterior_samples(sl.6, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 



sg.fixed.p<-posterior_samples(sg.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p.3<-posterior_samples(sg.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p.5<-posterior_samples(sg.5, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sg.fixed.p.6<-posterior_samples(sg.6, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 


cde.fixed.p<-posterior_samples(CDE.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
cde.fixed.p.3<-posterior_samples(CDE.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
cde.fixed.p.5<-posterior_samples(CDE.5, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
cde.fixed.p.6<-posterior_samples(CDE.6, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 



sloss.fixed.p<-posterior_samples(s.loss.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sloss.fixed.p.3<-posterior_samples(s.loss.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sloss.fixed.p.5<-posterior_samples(s.loss.5, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sloss.fixed.p.6<-posterior_samples(s.loss.6, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))


sgain.fixed.p<-posterior_samples(s.gain.s, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p.3<-posterior_samples(s.gain.3, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p.5<-posterior_samples(s.gain.5, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 
sgain.fixed.p.6<-posterior_samples(s.gain.6, "^b" , subset = floor(runif(n = 1000, 1, max = 2000))) 


#'year.y.m'
#''trt.yNPK:year.y.m'
head(sl.fixed.p)

sl.p <-  sl.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='All years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sl.p.3 <-  sl.fixed.p.3 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>3 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sl.p.5 <-  sl.fixed.p.5 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>5 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sl.p.6 <-  sl.fixed.p.6 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>6 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()  

sl.effs <- bind_rows(sl.p,sl.p.3,sl.p.5,sl.p.6)

sl.effs

write.csv(sl.effs, '~/Dropbox/Projects/NutNet/Data/sl.effs.compare.csv')

sl.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sl.effs.compare.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sl.effs$response <- factor(sl.effs$response , levels=c("=>3 years","=>5 years", "=>6 years","All years"))


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


head(sg.fixed.p)

sg.p <-  sg.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='All years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sg.p.3 <-  sg.fixed.p.3 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>3 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sg.p.5 <-  sg.fixed.p.5 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>5 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sg.p.6 <-  sg.fixed.p.6 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>6 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()  

sg.effs <- bind_rows(sg.p,sg.p.3,sg.p.5,sg.p.6)

sg.effs

write.csv(sg.effs, '~/Dropbox/Projects/NutNet/Data/sg.effs.compare.csv')

sg.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sg.effs.compare.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sg.effs$response <- factor(sg.effs$response , levels=c("=>3 years","=>5 years", "=>6 years","All years"))


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




head(cde.fixed.p)

cde.p <-  cde.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='All years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

cde.p.3 <-  cde.fixed.p.3 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>3 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

cde.p.5 <-  cde.fixed.p.5 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>5 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

cde.p.6 <-  cde.fixed.p.6 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>6 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()  

cde.effs <- bind_rows(cde.p,cde.p.3,cde.p.5,cde.p.6)

cde.effs

write.csv(cde.effs, '~/Dropbox/Projects/NutNet/Data/cde.effs.compare.csv')

cde.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/cde.effs.compare.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


cde.effs$response <- factor(cde.effs$response , levels=c("=>3 years","=>5 years", "=>6 years","All years"))


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



head(sloss.fixed.p)

sloss.p <-  sloss.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='All years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sloss.p.3 <-  sloss.fixed.p.3 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>3 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sloss.p.5 <-  sloss.fixed.p.5 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>5 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sloss.p.6 <-  sloss.fixed.p.6 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>6 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()  

sloss.effs <- bind_rows(sloss.p,sloss.p.3,sloss.p.5,sloss.p.6)

sloss.effs

write.csv(sloss.effs, '~/Dropbox/Projects/NutNet/Data/sloss.effs.compare.csv')

sloss.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sloss.effs.compare.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sloss.effs$response <- factor(sloss.effs$response , levels=c("=>3 years","=>5 years", "=>6 years","All years"))


sloss.slope<-ggplot() + 
  geom_point(data =sloss.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sloss.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Change in Biomass (g/' ,m^2, ') / Year ')),
       title='c) NPK Effect on Species Loss  / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sloss.slope




head(sgain.fixed.p)

sgain.p <-  sgain.fixed.p %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='All years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sgain.p.3 <-  sgain.fixed.p.3 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>3 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sgain.p.5 <-  sgain.fixed.p.5 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>5 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()   

sgain.p.6 <-  sgain.fixed.p.6 %>% select(`b_year.y.m`,`b_trt.yNPK:year.y.m`) %>%
  mutate(ctl.slope =`b_year.y.m`,
         npk.slope=`b_trt.yNPK:year.y.m`,
         trt.slope=(`b_year.y.m`+`b_trt.yNPK:year.y.m`)) %>%
  select(-c(`b_year.y.m`,`b_trt.yNPK:year.y.m`,npk.slope,ctl.slope)) %>%
  mutate( response='=>6 years', trt_slope = mean(trt.slope),
          trt_lower_slope = quantile(trt.slope, probs=0.025),
          trt_upper_slope = quantile(trt.slope, probs=0.975) )  %>%
  select(-c(trt.slope)) %>% distinct()  

sgain.effs <- bind_rows(sgain.p,sgain.p.3,sgain.p.5,sgain.p.6)

sgain.effs

write.csv(sgain.effs, '~/Dropbox/Projects/NutNet/Data/sgain.effs.compare.csv')

sgain.effs <- read.csv("~/Dropbox/Projects/NutNet/Data/sgain.effs.compare.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sgain.effs$response <- factor(sgain.effs$response , levels=c("=>3 years","=>5 years", "=>6 years","All years"))


sgain.slope<-ggplot() + 
  geom_point(data =sgain.effs, aes(x = response, y = trt_slope,color=response),size = 2) +
  geom_errorbar(data = sgain.effs, aes(x = response,ymin = trt_lower_slope,
                                    ymax = trt_upper_slope,color=response),
                width = 0, size = 0.7) +
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Change in Biomass (g/' ,m^2, ') / Year ')),
       title='d) NPK Effect on Species Gain  / Year') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")
sgain.slope





(sp.slope | bm.slope)/(sloss.slope | sgain.slope)/(sl.slope | sg.slope | cde.slope)






