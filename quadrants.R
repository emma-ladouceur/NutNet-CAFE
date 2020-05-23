




library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")


load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')


plot.rich_coef3
plot.bm_coef3


View(plot.rich_coef3)
View(plot.bm_coef3)
colnames(plot.rich_coef3)
colnames(plot.bm_coef3)
plot.rich_coef4<-plot.rich_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18)]
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18)]
colnames(plot.rich_coef4)
View(plot.bm_coef4)

names(plot.rich_coef4) <- c("site_code","IR.Slope","IR.Slope_lower","IR.Slope_upper","R.Slope","R.Slope_lower","R.Slope_upper","starting.richness","continent","habitat")
names(plot.bm_coef4) <- c("site_code","IB.Slope","IB.Slope_lower","IB.Slope_upper","B.Slope","B.Slope_lower","B.Slope_upper","starting.richness","continent","habitat")
#plot.bm_coef5<-plot.bm_coef4[complete.cases(plot.bm_coef4$B.Slope),]
delta.coefs<-left_join(plot.rich_coef4,plot.bm_coef4)
View(delta.coefs)

# DELTA QUDRANT PLOT Model
# GROUP VARIATION
levels(delta.coefs$starting.richness)
delta.coefs$starting.richness <- factor(delta.coefs$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

mod.fig<-ggplot(data=delta.coefs, aes(x= R.Slope, y= B.Slope,color=starting.richness)) +
  #facet_wrap(~site_code)+
  geom_point(size=2) +
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  geom_errorbarh(aes(xmin =  R.Slope_lower, xmax = R.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year')),
       title= '', color= "Starting Richness") +
  geom_vline(xintercept = 0,  alpha=0.8) + geom_hline(yintercept = 0 ,  alpha=0.8) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))
mod.fig



# PARTITIONS

load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/sg_dat.Rdata')

load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')

colnames(sl.trt_coef3)
sl.coef<-sl.trt_coef3 %>% rename(SL.Slope=TESlope,
                        SL.Slope_upper=TESlope_upper,
                        SL.Slope_lower=TESlope_lower) %>%
  select(site_code, starting.richness,SL.Slope,SL.Slope_upper,SL.Slope_lower)


sg.coef<-sg.trt_coef3 %>% rename(SG.Slope=TESlope,
                        SG.Slope_upper=TESlope_upper,
                        SG.Slope_lower=TESlope_lower) %>%
  select(site_code, starting.richness,SG.Slope,SG.Slope_upper,SG.Slope_lower)


sgain.coef<-sgain.trt_coef3  %>% rename(Sgain.Slope=TESlope,
                            Sgain.Slope_upper=TESlope_upper,
                            Sgain.Slope_lower=TESlope_lower) %>%
  select(site_code, starting.richness,Sgain.Slope,Sgain.Slope_upper,Sgain.Slope_lower)

sloss.coef<-sloss.trt_coef3  %>% rename(Sloss.Slope=TESlope,
                                        Sloss.Slope_upper=TESlope_upper,
                                        Sloss.Slope_lower=TESlope_lower) %>%
  select(site_code, starting.richness,Sloss.Slope,Sloss.Slope_upper,Sloss.Slope_lower)


cde.coef<-cde_coef3  %>% rename(cde.Slope=TESlope,
                                        cde.Slope_upper=TESlope_upper,
                                        cde.Slope_lower=TESlope_lower) %>%
  select(site_code, starting.richness,cde.Slope,cde.Slope_upper,cde.Slope_lower)


loss.c<- sl.coef %>% bind_cols(sloss.coef)
gains.c<- sg.coef %>% bind_cols(sgain.coef)
loss.c$Vector="Losses"
gains.c$Vector="Gains"
cde.coef$Vector="Persistent Sp."
loss.gain <- loss.c %>% bind_cols(gains.c)

all.effs <- loss.gain %>% bind_cols(cde.coef)


View(all.effs)


price.cloud<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  #facet_wrap(~site_code)+
geom_point(data = all.effs, aes(x= Sloss.Slope, #loss
                                y=  SL.Slope ),
           colour="#B40F20",size=0.2,alpha = 0.4)+

  geom_point(data = all.effs, aes(x= Sgain.Slope , #losses
                                  y= SG.Slope ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4)+

  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  cde.Slope),
             colour="#816687",size=0.1,alpha = 0.4) +
  geom_errorbar(data = all.effs,aes(x=Sgain.Slope,
                                    ymin = cde.Slope_lower, ymax = cde.Slope_upper),width=0,colour = "#816687", size = 0.55,alpha=0.3) +

geom_errorbar(data = all.effs,aes(x=Sloss.Slope,
                                  ymin = SL.Slope_lower, ymax = SL.Slope_upper),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=SL.Slope,
                                     xmin = Sloss.Slope_lower, xmax = Sloss.Slope_upper),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +

geom_errorbar(data = all.effs,aes(x=Sgain.Slope,
                                  ymin = SG.Slope_lower, ymax = SG.Slope_upper),width=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=SG.Slope,
                                     xmin = Sgain.Slope_lower, xmax = Sgain.Slope_upper),height=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  labs(x = 'Effect of NPK on Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= 'a) Rates / Year ') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))

price.cloud

price.cloud.seg <-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  facet_wrap(~site_code)+
  geom_segment(data = all.effs,
               aes(x = Sgain.Slope,
                   xend = Sgain.Slope,
                   y = SG.Slope,
                   yend = cde.Slope),
               colour= "#816687",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = Sloss.Slope,
                   y = 0,
                   yend = SL.Slope ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= Sloss.Slope, #loss
                                  y=  SL.Slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = Sloss.Slope,
                   xend = Sgain.Slope,
                   y = SL.Slope,
                   yend = SG.Slope),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= Sgain.Slope , #losses
                                  y= SG.Slope ) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  geom_point(data = all.effs,aes(x=0, #persistent
                                 y=  cde.Slope),
             colour="#816687",size=0.1,alpha = 0.4) +
  geom_errorbar(data = all.effs,aes(x=Sgain.Slope,
                                    ymin = cde.Slope_lower, ymax = cde.Slope_upper),width=0,colour = "#816687", size = 0.55,alpha=0.3) +
  geom_errorbar(data = all.effs,aes(x=Sloss.Slope,
                                    ymin = SL.Slope_lower, ymax = SL.Slope_upper),width=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=SL.Slope,
                                     xmin = Sloss.Slope_lower, xmax = Sloss.Slope_upper),height=0,colour = "#B40F20", size = 0.55,alpha=0.3) +
  geom_errorbar(data = all.effs,aes(x=Sgain.Slope,
                                    ymin = SG.Slope_lower, ymax = SG.Slope_upper),width=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = all.effs,aes(y=SG.Slope,
                                     xmin = Sgain.Slope_lower, xmax = Sgain.Slope_upper),height=0,colour = "#046C9A", size = 0.55,alpha=0.3) +
  #ylim(-11,35) +
  labs(x = 'Effect of NPK on Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= 'a) Rates / Year ') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))

price.cloud.seg



# rates add



price.cloud.add<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  facet_wrap(~site_code)+
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = Sloss.Slope ,
                   y = 0,
                   yend = SL.Slope  ),
               colour= "#B40F20",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= Sloss.Slope, #loss
                                  y=  SL.Slope ),
             colour="#B40F20",size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = Sloss.Slope,
                   xend = Sloss.Slope+Sgain.Slope ,
                   y = SL.Slope,
                   yend = SL.Slope+SG.Slope ),
               colour= "#046C9A",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs, aes(x= Sloss.Slope+Sgain.Slope, #losses
                                  y= SL.Slope+SG.Slope) ,
             colour="#046C9A",
             size=0.2,alpha = 0.4)+
  geom_segment(data = all.effs,
               aes(x = Sloss.Slope+Sgain.Slope,
                   xend = Sloss.Slope+Sgain.Slope,
                   y = SL.Slope+SG.Slope,
                   yend =SL.Slope+SG.Slope+ cde.Slope ),
               colour= "#816687",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = all.effs,aes(x=Sloss.Slope+Sgain.Slope, #persistent
                                 y= SL.Slope+SG.Slope+cde.Slope ),
             colour="#816687",size=0.1,alpha = 0.4) +
  geom_errorbar(data = all.effs, aes(x= Sloss.Slope,
                                     ymin =  (SL.Slope_lower ),
                                     ymax = (SL.Slope_upper)),
                width=0,colour =  "#B40F20",
                size = 0.55,alpha=0.3
  ) +
  geom_errorbarh(data = all.effs,aes(y=SL.Slope,
                                     xmin =  (Sloss.Slope_lower)  ,
                                     xmax =  (Sloss.Slope_upper) ) ,
                 height=0,colour =  "#B40F20",
                 size = 0.55,alpha=0.3
  ) +
  
  geom_errorbar(data = all.effs, aes(x= Sloss.Slope+Sgain.Slope,
                                     ymin =  (SL.Slope_lower+SG.Slope_lower) ,
                                     ymax = (SL.Slope_upper+SG.Slope_upper)),
                width=0,colour = "#046C9A",
                size = 0.55,alpha=0.3
                ) +
  geom_errorbarh(data = all.effs,aes(y=SL.Slope+SG.Slope,
                                     xmin =  (Sloss.Slope_lower+Sgain.Slope_lower)  ,
                                     xmax =  (Sloss.Slope_upper+Sgain.Slope_upper) ) ,
                 height=0,colour = "#046C9A",
                 size = 0.55,alpha=0.3
                 ) +
  geom_errorbar(data = all.effs,aes(x=Sloss.Slope+Sgain.Slope,
                                    ymin = SL.Slope_lower+SG.Slope_lower+cde.Slope_lower, ymax = SL.Slope_upper+SG.Slope_upper+cde.Slope_upper),width=0,colour = "#816687", size = 0.55,alpha=0.3
                ) +

  # ylim(-11,35) +
  # xlim(-0.65,0)+
  labs(x = 'Effect of NPK on Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= 'b) Rates / Year Added')+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))

price.cloud.add





