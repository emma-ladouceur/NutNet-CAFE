





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

mod.fig<-ggplot(data=delta.coefs, aes(x= R.Slope, y= B.Slope)) +
  #facet_wrap(~site_code)+
  geom_point(colour="black", alpha=0.2,size=2) +
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(aes(xmin =  R.Slope_lower, xmax = R.Slope_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year')),
       title= '', color= "Starting Richness") +
  geom_vline(xintercept = 0,  alpha=0.8) + geom_hline(yintercept = 0 ,  alpha=0.8) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))
mod.fig





# bef cloud


# RICHNESS AND BIOMASS

plot.rich.im_fixef <- fixef(plot.rich.g)
plot.bm.im_fixef <- fixef(plot.bm.s)


rich.fixed.p<-posterior_samples(plot.rich.g, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 
bm.fixed.p<-posterior_samples(plot.bm.s, "^b" ,subset = floor(runif(n = 1000, 1, max = 2000))) 

plot.rich.im_fixef 
plot.bm.im_fixef 

rich.fixed.p2 <-rich.fixed.p %>% 
  filter(`b_trtNPK:year_trt` > quantile(rich.fixed.p$`b_trtNPK:year_trt`, probs=0.025),
         `b_trtNPK:year_trt` < quantile(rich.fixed.p$`b_trtNPK:year_trt`, probs=0.975)) %>%
  mutate(rich.trt.p=`b_trtNPK:year_trt`) %>%
  mutate(response = 'sl',
         rich.trt_global_slope = plot.rich.im_fixef ['trtNPK:year_trt','Estimate'],
         rich.trt_upper_slope = plot.rich.im_fixef ['trtNPK:year_trt','Q97.5'],
         rich.trt_lower_slope = plot.rich.im_fixef ['trtNPK:year_trt','Q2.5'],)  %>%
  select(rich.trt.p,rich.trt_global_slope, rich.trt_upper_slope,rich.trt_lower_slope)

nrow(rich.fixed.p2)

bm.fixed.p2 <-bm.fixed.p %>% 
  filter(`b_trtNPK:year_trt` > quantile(bm.fixed.p$`b_trtNPK:year_trt`, probs=0.025),
         `b_trtNPK:year_trt` < quantile(bm.fixed.p$`b_trtNPK:year_trt`, probs=0.975)) %>%
  mutate(bm.trt.p=`b_trtNPK:year_trt`) %>%
  mutate(response = 'sl',
         bm.trt_global_slope = plot.bm.im_fixef ['trtNPK:year_trt','Estimate'],
         bm.trt_upper_slope = plot.bm.im_fixef ['trtNPK:year_trt','Q97.5'],
         bm.trt_lower_slope = plot.bm.im_fixef ['trtNPK:year_trt','Q2.5'],)  %>%
  select(bm.trt.p,bm.trt_global_slope, bm.trt_upper_slope,bm.trt_lower_slope)

nrow(bm.fixed.p2)


rich.ss <- rich.fixed.p2 %>% sample_n(50) 
rich.ss$Vector="Species Richness"
biomass.ss <- bm.fixed.p2  %>% sample_n(50)
biomass.ss$Vector="Biomass"


r.bm.effs <- biomass.ss%>% bind_cols(rich.ss)
r.bm.effs$Effects<-"Richness + Biomass"

colnames(r.bm.effs)


mod.fig<-ggplot(data=delta.coefs, aes(x= R.Slope, y= B.Slope)) +
  #facet_wrap(~site_code)+
  geom_point(colour="black", alpha=0.2,size=2) +
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(aes(xmin =  R.Slope_lower, xmax = R.Slope_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year')),
       title= '', color= "Starting Richness") +
  geom_vline(xintercept = 0,  alpha=0.8) + geom_hline(yintercept = 0 ,  alpha=0.8) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))
mod.fig


r.bm.effs<-r.bm.effs %>% select(rich.trt_global_slope,bm.trt_global_slope,bm.trt_lower_slope,bm.trt_upper_slope,rich.trt_lower_slope,rich.trt_upper_slope) %>%
  distinct()
  
bef.cloud<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_point(data=delta.coefs, aes(x= R.Slope, y= B.Slope),colour="black", alpha=0.2,size=2) +
  geom_errorbar(data=delta.coefs,aes(x= R.Slope, y= B.Slope,ymin = B.Slope_lower, ymax = B.Slope_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(data=delta.coefs,aes(x= R.Slope, y= B.Slope,xmin =  R.Slope_lower, xmax = R.Slope_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_point(data = distinct(r.bm.effs), aes(x= rich.trt_global_slope, #loss
                                   y=  bm.trt_global_slope ),
             fill="#0B775E",color="#F98400",size=3)+
  geom_errorbar(data = distinct(r.bm.effs),aes(x=rich.trt_global_slope,
                                     ymin = bm.trt_lower_slope, ymax = bm.trt_upper_slope),width=0,colour = "#0B775E", size = 2,alpha=0.7) +
  geom_errorbarh(data = distinct(r.bm.effs),aes(y=bm.trt_global_slope,
                                      xmin = rich.trt_lower_slope, xmax = rich.trt_upper_slope),height=0,colour = "#F98400", size = 2, alpha=0.7) +
  # ylim(-11,35) +
  # xlim(-0.65,0)+
   #scale_color_manual(name='Overall Effects',values=c("black")) +
  labs(x = 'Effect of NPK on Species Richness / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= '')

bef.cloud


(bm | bef.cloud | rich)

( rich|bm)/(rlegend)/( bef.cloud ) + plot_layout(heights = c(10,0.75,10))


#"#F98400","#0B775E"
bef.leg<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = r.bm.effs,
               aes(x = 0,
                   xend = rich.trt.p ,
                   y = 0,
                   yend = bm.trt.p ),
               colour="#0B775E",
               size = 0.2,  alpha = 0.4,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = r.bm.effs, aes(x= rich.trt.p, #loss
                                   y= bm.trt.p  ),
             colour="#0B775E",size=0.2,alpha = 0.4)+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = r.bm.effs,
               aes(x = 0,
                   xend = rich.trt_global_slope,
                   y = 0,
                   yend = bm.trt_global_slope, colour=Effects),
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = r.bm.effs, aes(x= rich.trt_global_slope, #loss
                                   y=  bm.trt_global_slope ),
             colour="#0B775E",size=0.2,alpha = 0.4)+
  geom_errorbar(data = r.bm.effs,aes(x=rich.trt_global_slope,
                                     ymin = bm.trt_lower_slope, ymax = bm.trt_upper_slope),width=0,colour = "#0B775E", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = r.bm.effs,aes(y=bm.trt_global_slope,
                                      xmin = rich.trt_lower_slope, xmax = rich.trt_upper_slope),height=0,colour = "#0B775E", size = 0.55,alpha=0.3) +
  ylim(-11,30) +
  scale_color_manual(name='Overall Effects',values=c("#0B775E")) +
  labs(x = 'Effect of NPK on Species Richness / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= 'a) Richness & Biomass')




u.leg<-ggplot()+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = r.bm.effs,
               aes(x = 0,
                   xend = rich.trt.p ,
                   y = 0,
                   yend = bm.trt.p ,colour=Effects),
               size = 0.2,  alpha = 0.6,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = r.bm.effs, aes(x= rich.trt.p, #loss
                                   y= bm.trt.p  ),
             colour="#0B775E",size=0.2,alpha = 0.4)+
  # Fiexed effects section
  # black thick arrow background so we can see the arrows
  geom_segment(data = r.bm.effs,
               aes(x = 0,
                   xend = rich.trt_global_slope,
                   y = 0,
                   yend = bm.trt_global_slope),
               colour="#0B775E",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_point(data = r.bm.effs, aes(x= rich.trt_global_slope, #loss
                                   y=  bm.trt_global_slope ),
             colour="#0B775E",size=0.2,alpha = 0.4)+
  geom_errorbar(data = r.bm.effs,aes(x=rich.trt_global_slope,
                                     ymin = bm.trt_lower_slope, ymax = bm.trt_upper_slope),width=0,colour = "#0B775E", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = r.bm.effs,aes(y=bm.trt_global_slope,
                                      xmin = rich.trt_lower_slope, xmax = rich.trt_upper_slope),height=0,colour = "#0B775E", size = 0.55,alpha=0.3) +
  ylim(-11,30) +
  scale_color_manual(name='Uncertainty',values=c("#0B775E")) +
  labs(x = 'Effect of NPK on Species Richness / Year',
       y = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ')/ Year')),
       title= ' Richness & Biomass')




#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



bef.legend<-g_legend(bef.leg)
u.legend<-g_legend(u.leg)

a<-(bef.cloud)/(bef.legend)/(u.legend)+
  plot_layout(heights = c(12,0.5,0.5))

a



