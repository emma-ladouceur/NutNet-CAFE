


library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)
library(patchwork)



load('~/Dropbox/Projects/NutNet/Model_fits/full/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/rich.Rdata') # plot.rich.g

load('~/Dropbox/Projects/NutNet/Model_fits/full/sl.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/cde.Rdata') # CDE.s


load('~/Dropbox/Projects/NutNet/Model_fits/full/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/full/sgain.Rdata') # s.gain.s



sl.trt.i_fixef <-  as.data.frame(fixef(sl.s))
sg.trt.i_fixef <- as.data.frame(fixef(sg.s))
CDE.trt.i_fixef <- as.data.frame( fixef(CDE.s))
plot.rich.im_fixef <- as.data.frame(fixef(plot.rich.g))
plot.bm.im_fixef <- as.data.frame(fixef(plot.bm.s))
sloss.trt.i_fixef <- as.data.frame(fixef(s.loss.n.s))
sgain.trt.i_fixef <-as.data.frame( fixef(s.gain.s))

plot.rich.im_fixef
sgain.trt.i_fixef


rich.f <-bind_rows(
  plot.rich.im_fixef['trtNPK',] %>% 
    mutate(response='Treatment Effect',
      eff = Estimate,
      eff_upper = Q97.5,
      eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='Treatment Effect / Year',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


rich.f$Model <- "i) Species Richness"
rich.f

bm.f <-bind_rows(
  plot.bm.im_fixef['trtNPK',] %>% 
    mutate(response='Treatment Effect',
      eff = Estimate,
      eff_upper = Q97.5,
      eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='Treatment Effect / Year',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

bm.f$Model <- "ii) Biomass"

sl.f <-bind_rows(
  sl.trt.i_fixef['trt.yNPK',] %>% 
    mutate(response='Treatment Effect',
      eff = Estimate,
      eff_upper = Q97.5,
      eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Treatment Effect / Year',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


sl.f$Model <- "v) Species Loss Effect on Biomass"

sg.f <-bind_rows(
  sg.trt.i_fixef['trt.yNPK',] %>% 
    mutate(response='Treatment Effect',
      eff = Estimate,
      eff_upper = Q97.5,
      eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Treatment Effect / Year',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sg.f$Model <- "vi) Species Gain Effect on Biomass"
cde.f <-bind_rows(
  CDE.trt.i_fixef['trt.yNPK',] %>% 
    mutate(response='Treatment Effect',
      eff = Estimate,
      eff_upper = Q97.5,
      eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  CDE.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Treatment Effect / Year',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

cde.f$Model <- "vii) Persistent Species Change in Biomass"
sloss.f <-bind_rows(
  sloss.trt.i_fixef['trt.yNPK',] %>% 
    mutate(response='Treatment Effect',
      eff = Estimate,
      eff_upper = Q97.5,
      eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Treatment Effect / Year',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sloss.f$Model <- "iii) Species Loss"

sgain.f <-bind_rows(
  sgain.trt.i_fixef['trt.yNPK',] %>% 
    mutate(response='Treatment Effect',
      eff = Estimate,
      eff_upper = Q97.5,
      eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='Treatment Effect / Year',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sgain.f$Model <- "iv) Species Gain"

r.bm.effs<-bind_rows(rich.f,bm.f)

r.bm.effs
sp.effs<-bind_rows(sloss.f,sgain.f)
sp.effs
bm.effs <-bind_rows(sl.f,sg.f,cde.f)

bm.effs



# sp rich  "#F98400",
# biomass "#0B775E",
# loss "#B40F20",
# gain "#3B9AB2",
# persistent "#35274A"


r.bm.effs$Model<- factor(  r.bm.effs$Model , levels=c("i) Species Richness","ii) Biomass"))

rich.bm.eff<-ggplot() + 
  geom_point(data =r.bm.effs, aes(x = response, y = eff, color=Model),size = 2) +
  geom_errorbar(data = r.bm.effs, aes(x = response,ymin = eff_lower,
                                   ymax = eff_upper, color=Model),
                width = 0, size = 0.7) +
  facet_wrap(~Model, scales="free")+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK')),
       title='b)') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-1.2,0.2) +
  scale_color_manual(values = c("#F98400","#0B775E")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")


rich.bm.eff


sp.effs$Model<- factor( sp.effs$Model , levels=c("iii) Species Loss","iv) Species Gain"))

sp.eff<-ggplot() + 
  geom_point(data =sp.effs, aes(x = response, y = eff, color=Model),size = 2) +
  geom_errorbar(data = sp.effs, aes(x = response,ymin = eff_lower,
                                      ymax = eff_upper, color=Model),
                width = 0, size = 0.7) +
  facet_wrap(~Model, scales="free")+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK'))
      # title='Species Richness'
      ) +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-1.2,0.2) +
  scale_color_manual(values = c("#B40F20","#046C9A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")


sp.eff

bm.effs$Model<-as.factor(as.character(bm.effs$Model))
levels(bm.effs$Model)
bm.effs$Model<- factor( bm.effs$Model , levels=c("v) Species Loss Effect on Biomass","vi) Species Gain Effect on Biomass","vii) Persistent Species Change in Biomass"))

bm.eff<-ggplot() + 
  geom_point(data =bm.effs, aes(x = response, y = eff, color=Model),size = 2) +
  geom_errorbar(data = bm.effs, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper, color=Model),
                width = 0, size = 0.7) +
  facet_wrap(~Model, scales="free")+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK '))
       #title='Species Richness'
       ) +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-1.2,0.2) +
  scale_color_manual(values = c("#B40F20","#046C9A","#816687")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")


bm.eff


grid.arrange(rich.bm.eff,sp.eff,bm.eff,nrow=3,ncol=1)

(rich.bm.eff)/(sp.eff)/(bm.eff)


# SEPERATELY


rich.f <-bind_rows(
  # plot.rich.im_fixef['trtNPK',] %>% 
  #   mutate(response='Species Richness Trt',
  #          eff = Estimate,
  #          eff_upper = Q97.5,
  #          eff_lower = Q2.5) %>%
  #   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.rich.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


rich.f$Model <- "Species Richness"
rich.f

bm.f <-bind_rows(
  #plot.bm.im_fixef['trtNPK',] %>% 
    # mutate(response='Biomass Trt',
    #        eff = Estimate,
    #        eff_upper = Q97.5,
    #        eff_lower = Q2.5) %>%
    # select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  plot.bm.im_fixef['trtNPK:year_trt',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

bm.f$Model <- "Biomass"

sl.f <-bind_rows(
  # sl.trt.i_fixef['trt.yNPK',] %>% 
  #   mutate(response='SL Trt',
  #          eff = Estimate,
  #          eff_upper = Q97.5,
  #          eff_lower = Q2.5) %>%
  #   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sl.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)


sl.f$Model <- "Species Loss Effect on Biomass"

sg.f <-bind_rows(
  # sg.trt.i_fixef['trt.yNPK',] %>% 
  #   mutate(response='SG Trt',
  #          eff = Estimate,
  #          eff_upper = Q97.5,
  #          eff_lower = Q2.5) %>%
  #   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sg.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sg.f$Model <- "Species Gain Effect on Biomass"
cde.f <-bind_rows(
  # CDE.trt.i_fixef['trt.yNPK',] %>% 
  #   mutate(response='PS Trt',
  #          eff = Estimate,
  #          eff_upper = Q97.5,
  #          eff_lower = Q2.5) %>%
  #   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  CDE.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

cde.f$Model <- "Persistent Species Change in Biomass"
sloss.f <-bind_rows(
  # sloss.trt.i_fixef['trt.yNPK',] %>% 
  #   mutate(response='Species Loss Trt',
  #          eff = Estimate,
  #          eff_upper = Q97.5,
  #          eff_lower = Q2.5) %>%
  #   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sloss.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sloss.f$Model <- "Species Loss"

sgain.f <-bind_rows(
  # sgain.trt.i_fixef['trt.yNPK',] %>% 
  #   mutate(response='Species Gain Trt',
  #          eff = Estimate,
  #          eff_upper = Q97.5,
  #          eff_lower = Q2.5) %>%
  #   select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
  sgain.trt.i_fixef['trt.yNPK:year.y.m',] %>% 
    mutate(response='NPK Slope',
           eff = Estimate,
           eff_upper = Q97.5,
           eff_lower = Q2.5) %>%
    select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
)

sgain.f$Model <- "Species Gain"

setwd('~/Dropbox/Projects/NutNet/Data/')
save(rich.f,bm.f,sloss.f,sgain.f,sl.f,sg.f,cde.f,file = 'effs.Rdata')


rich.eff<-ggplot() + 
  geom_point(data =rich.f, aes(x = response, y = eff, color=response),size = 2) +
  geom_errorbar(data = rich.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  facet_wrap(~Model)+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Species Richness')),
       #title='Species Richness'
       ) +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-1.2,0.2) +
  scale_color_manual(values = c("#F98400")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")


rich.eff


bm.eff<-ggplot() + 
  geom_point(data =bm.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = bm.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  facet_wrap(~Model)+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') ')),
       title='Biomass') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = c("#0B775E")) +
                       theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                                        axis.text.y = element_text(size=6),
                                        axis.title.x = element_text(size=8),
                                        title=element_text(size=8),
                                        strip.background = element_blank(),legend.position="none")



sloss.eff<-ggplot() + 
  geom_point(data =sloss.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sloss.f, aes(x = response,ymin = eff_lower,
                                 ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  facet_wrap(~Model)+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Species Loss')),
       title='Species Loss') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = c("#B40F20")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")

sloss.eff

sgain.eff<-ggplot() + 
  geom_point(data =sgain.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sgain.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  facet_wrap(~Model)+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Species Gain')),
       title='Species Gain') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = c("#046C9A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")

sgain.eff



sl.eff<-ggplot() + 
  geom_point(data =sl.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sl.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  facet_wrap(~Model)+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') ')),
       title='Species Loss Effect on Biomass') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = c("#B40F20")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")

sl.eff


#sg.f$response <- factor(sg.f$response , levels=c("SG Trt","SG / Year"))

sg.eff<-ggplot() + 
  geom_point(data =sg.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = sg.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  facet_wrap(~Model)+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') ')),
       title='Species Gain Effect on Biomass') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = c("#046C9A")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")

sg.eff


cde.eff<-ggplot() + 
  geom_point(data =cde.f, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = cde.f, aes(x = response,ymin = eff_lower,
                                    ymax = eff_upper,color=response),
                width = 0, size = 0.7) +
  facet_wrap(~Model)+
  labs(x = 'Site',
       y = 'Slope') +
  labs(x = '',
       y= expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') ')),
       title='Persistent Species Change in Biomass') +
  geom_hline(yintercept = 0, lty = 2) +
  #ylim(-50,140) +
  scale_color_manual(values = c("#816687")) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   #axis.title.x = element_blank(),#axis.text.y = element_blank(),
                   axis.text.y = element_text(size=6),
                   axis.title.x = element_text(size=8),
                   title=element_text(size=8),
                   strip.background = element_blank(),legend.position="none")

cde.eff



(rich.eff | bm.eff)/(sloss.eff | sgain.eff)/(sl.eff | sg.eff | cde.eff)




