
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(wesanderson)
library(gridExtra)
library(grid)

# models
load('~/Dropbox/Projects/NutNet/Model_fits/sl.n.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s
load('~/Dropbox/Projects/NutNet/Model_fits/sloss.n.Rdata') # s.loss.s
load('~/Dropbox/Projects/NutNet/Model_fits/sgain.Rdata') # s.gain.s


# coeffiecients for experiment level (random) effects, fitted values, fixed effects
#extracted and saved to object in 'cumulatiove_models.R'
load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sg_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')



sloss.trt_coef4<-sloss.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
sl.trt_coef4<-sl.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]

colnames(sloss.trt_coef4)
colnames(sl.trt_coef4)

names(sloss.trt_coef4) <- c("site_code","IX.Slope","IX.Slope_lower","IX.Slope_upper","X.Slope","X.Slope_lower","X.Slope_upper")
names(sl.trt_coef4) <- c("site_code","IY.Slope","IY.Slope_lower","IY.Slope_upper","Y.Slope","Y.Slope_lower","Y.Slope_upper")

loss.coefs<-left_join(sloss.trt_coef4,sl.trt_coef4)
loss.coefs$group<-"Losses"

head(loss.coefs)

#gains
sgain.trt_coef4<-sgain.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
sg.trt_coef4<-sg.trt_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]

colnames(sgain.trt_coef4)
colnames(sg.trt_coef4)

names(sgain.trt_coef4) <- c("site_code","IX.Slope","IX.Slope_lower","IX.Slope_upper","X.Slope","X.Slope_lower","X.Slope_upper")
names(sg.trt_coef4) <- c("site_code","IY.Slope","IY.Slope_lower","IY.Slope_upper","Y.Slope","Y.Slope_lower","Y.Slope_upper")

gain.coefs<-left_join(sgain.trt_coef4,sg.trt_coef4)
gain.coefs$group<-"Gains"

# persistent sp.
colnames(cde_coef3)
cde_coef4<-cde_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
colnames(cde_coef4)
names(cde_coef4) <- c("site_code","IY.Slope","IY.Slope_lower","IY.Slope_upper","Y.Slope","Y.Slope_lower","Y.Slope_upper")
cde_coef4$group<-"Persistent Sp."
cde_coef4$IX.Slope<-0
cde_coef4$IX.Slope_lower<-0
  cde_coef4$IX.Slope_upper<-0
  cde_coef4$X.Slope<-0
  cde_coef4$X.Slope_lower<-0
  cde_coef4$X.Slope_upper<-0
  head(cde_coef4)
  
m.s.coefs<-bind_rows(loss.coefs,gain.coefs,cde_coef4)
head(m.s.coefs)
View(m.s.coefs)

# Simplest Version
# all going from 0
vec.slope.fig<-ggplot(data= m.s.coefs, aes(x= X.Slope, y= Y.Slope,color=group)) +
  geom_point(alpha=0.3,size=2) +
  geom_errorbar(data= m.s.coefs, aes(ymin = Y.Slope_lower, ymax = Y.Slope_upper, color=group), width = 0, size = 0.45,alpha=0.3) +
  geom_errorbarh(data= m.s.coefs, aes(xmin = X.Slope_lower, xmax = X.Slope_upper,color=group), width = 0, size = 0.45,alpha=0.3) +
  geom_segment(data=. %>% filter(group == "Losses"),aes(x=0,
                                                        xend=X.Slope,
                                                        y=0,
                                                        yend=Y.Slope,
                                                        colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data=. %>% filter(group == "Gains"),aes(x= 0,
                                                       xend=X.Slope,
                                                       y=0,
                                                       yend=Y.Slope,
                                                       colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data=. %>% filter(group == "Persistent Sp."),aes(x= 0,
                                                                xend=X.Slope,
                                                                y=0,
                                                                yend=Y.Slope,
                                                                colour=group), alpha=0.3,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  scale_color_manual(values=c("#3B9AB2","#B40F20","#35274A"))+
  
  labs(x = 'Species Change Slope',
       y = 'Biomass Change Slope',
       title= 'Vector 4.0') +
  ylim(-50,50)+
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

vec.slope.fig

# Next version....  a bit more complicated

#reload
#coeffiecients for experiment level (random) effects, fitted values, fixed effects
#extracted and saved to object in 'cumulatiove_models.R'
load('~/Dropbox/Projects/NutNet/Data/sl.n.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sg_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/cde.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')


colnames(sloss.trt_coef3)
colnames(sl.trt_coef3)

sloss.trt_coef4<-sloss.trt_coef3[,c(-1,-2,-3,-5,-6,-7,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
sl.trt_coef4<-sl.trt_coef3[,c(-1,-2,-3,-5,-6,-7,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]

colnames(sloss.trt_coef4)
colnames(sl.trt_coef4)

names(sl.trt_coef4) <- c("site_code","BL.Slope","BL.Slope_lower","BL.Slope_upper")
names(sloss.trt_coef4) <- c("site_code","L.Slope","L.Slope_lower","L.Slope_upper")

loss.coefs<-left_join(sloss.trt_coef4,sl.trt_coef4)
#loss.coefs$group<-"Losses"

head(loss.coefs)
summary(loss.coefs)

#gains
sgain.trt_coef4<-sgain.trt_coef3[,c(-1,-2,-3,-5,-6,-7,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
sg.trt_coef4<-sg.trt_coef3[,c(-1,-2,-3,-5,-6,-7,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]

colnames(sgain.trt_coef4)
colnames(sg.trt_coef4)

names(sg.trt_coef4) <- c("site_code","BG.Slope","BG.Slope_lower","BG.Slope_upper")
names(sgain.trt_coef4) <- c("site_code","G.Slope","G.Slope_lower","G.Slope_upper")

gain.coefs<-left_join(sgain.trt_coef4,sg.trt_coef4)
#gain.coefs$group<-"Gains"

head(gain.coefs)

# persistent sp.
colnames(cde_coef3)
cde_coef4<-cde_coef3[,c(-1,-2,-3,-5,-6,-7,-8,-9,-10,-14,-15,-16,-17,-18,-19,-20,-21,-22)]
colnames(cde_coef4)
names(cde_coef4) <- c("site_code","P.Slope","P.Slope_lower","P.Slope_upper")
#cde_coef4$group<-"Persistent Sp."
head(cde_coef4)
summary(cde_coef4)

lg.coefs<-left_join(loss.coefs,gain.coefs, by="site_code")
m.coefs<-left_join(lg.coefs,cde_coef4)
head(m.coefs)
colnames(m.coefs)
View(m.coefs)


# Fixed effects
load('~/Dropbox/Projects/NutNet/Data/fixedef.Rdata')


sgain_fixef$names <- rownames(sgain_fixef)
sloss_fixef$names <- rownames(sloss_fixef)
sl_fixef$names <- rownames(sl_fixef)
sg_fixef$names <- rownames(sg_fixef)
cde_fixef$names <- rownames(cde_fixef)
sgain_fixef
sloss_fixef
sl_fixef



sgain_fixef$Model<-'Sgain'
sloss_fixef$Model<-'Sloss'
sl_fixef$Model<-'SL'
sg_fixef$Model<-'SG'
cde_fixef$Model<-'CDE'
fixedf_pp<-bind_rows(sl_fixef,sg_fixef,cde_fixef,sloss_fixef,sgain_fixef)
View(fixedf_pp)


# plus fixed effects, also starting from 0
vec.cloud<-ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  ylim(-50,50)+
  geom_point(data=m.coefs,aes(x= L.Slope, y=BL.Slope),alpha=0.3,size=2, colour="#B40F20") +
  geom_errorbar(data=m.coefs,aes(x=L.Slope, ymin = BL.Slope_lower, ymax = BL.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#B40F20") +
  geom_errorbarh(data= m.coefs, aes(y=BL.Slope,xmin = L.Slope_lower, xmax = L.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#B40F20") +
  geom_segment(data=m.coefs,aes(x=0,
                                xend= L.Slope,
                                y=0,
                                yend= BL.Slope), alpha=0.3,colour="#B40F20",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
 geom_point(data=m.coefs, aes(x= G.Slope, y=BG.Slope),alpha=0.3,size=2,colour="#3B9AB2") +
 geom_errorbar(data= m.coefs, aes(x=G.Slope,ymin = BG.Slope_lower, ymax = BG.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#3B9AB2") +
 geom_errorbarh(data= m.coefs, aes(y=BG.Slope,xmin = G.Slope_lower, xmax = G.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#3B9AB2") +
 geom_segment(data=m.coefs,aes(x= 0,
                                                      xend=G.Slope,
                                                      y=0,
                                                      yend=BG.Slope), alpha=0.3,colour="#3B9AB2",
              arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
 geom_point(data=m.coefs, aes(x= 0, y=P.Slope),alpha=0.3,size=2, colour="#35274A") +
  geom_errorbar(data= m.coefs, aes(x=0,ymin = P.Slope_lower, ymax = P.Slope_upper), width = 0, size = 0.45,alpha=0.3) +
  geom_segment(data=m.coefs,aes(x= 0,
                                                                xend=0,
                                                                y=0,
                                                                yend=P.Slope), alpha=0.3,colour="#35274A",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  # Fixed effects
  geom_point(data = fixedf_pp,aes(x=0, #persistent
                                  y=Estimate[12]),
             colour="#35274A",size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=0,
                                     ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbar(data = fixedf_pp,aes(x=0,
                                     ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "#35274A", size = 0.35,alpha=0.3) +
  geom_point(data = fixedf_pp,aes(x=Estimate[20], #gains
                                  y= Estimate[8]),
             colour="#3B9AB2",size=0.7)+
  geom_segment(data = fixedf_pp, 
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate[12]),
               colour= "black",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = 0,
                   y = 0,
                   yend = Estimate[12]),
               colour= "#35274A",
               size = 0.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                     ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                                      xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                     ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                                      xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[20],
                   y = 0,
                   yend = Estimate[8]),
               colour= "black",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[20],
                   y = 0,
                   yend = Estimate[8]),
               colour= "#3B9AB2",
               size = 0.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data = fixedf_pp,aes(x=Estimate[16], #losses
                                  y=Estimate[4]),
             colour="#B40F20",
             size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                                     ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                                      xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                                     ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                                      xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[16],
                   y = 0,
                   yend = Estimate[4]),
               colour= "black",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp,
               aes(x = 0,
                   xend = Estimate[16],
                   y = 0,
                   yend = Estimate[4]),
               colour= "#B40F20",
               size = 0.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  labs(x = 'Species Change Slope',
       y = 'Biomass Change Slope',
       title= 'Vector 4.1') 
 
  


View(fixedf_pp)

# Adding them together will be a bit more complicated


# plus fixed effects, connected CAFE style vector plot
cafe.vec.cloud<-ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  ylim(-50,50)+
  geom_point(data=m.coefs,aes(x= L.Slope, y=BL.Slope),alpha=0.3,size=2, colour="#B40F20") +
  geom_errorbar(data=m.coefs,aes(x=L.Slope, ymin = BL.Slope_lower, ymax = BL.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#B40F20") +
  geom_errorbarh(data= m.coefs, aes(y=BL.Slope,xmin = L.Slope_lower, xmax = L.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#B40F20") +
  geom_segment(data=m.coefs,aes(x=0,
                                xend= L.Slope,
                                y=0,
                                yend= BL.Slope), alpha=0.3,colour="#B40F20",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data=m.coefs, aes(x= G.Slope, y=BG.Slope),alpha=0.3,size=2,colour="#3B9AB2") +
  geom_errorbar(data= m.coefs, aes(x=G.Slope,ymin = BG.Slope_lower, ymax = BG.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#3B9AB2") +
  geom_errorbarh(data= m.coefs, aes(y=BG.Slope,xmin = G.Slope_lower, xmax = G.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#3B9AB2") +
  geom_segment(data=m.coefs,aes(x= L.Slope,
                                xend=G.Slope,
                                y=BL.Slope,
                                yend=BG.Slope), alpha=0.3,colour="#3B9AB2",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data=m.coefs, aes(x= G.Slope, y=P.Slope),alpha=0.3,size=2, colour="#35274A") +
  geom_errorbar(data= m.coefs, aes(x=G.Slope,ymin = P.Slope_lower, ymax = P.Slope_upper), width = 0, size = 0.45,alpha=0.3) +
  geom_segment(data=m.coefs,aes(x= G.Slope,
                                xend=G.Slope,
                                y=BG.Slope,
                                yend=P.Slope), alpha=0.3,colour="#35274A",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  # Fixed effects
  geom_point(data = fixedf_pp,aes(x=Estimate[16], # start fixed effects
                                  y=Estimate[4]),
             colour="#B40F20",
             size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                                     ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                                      xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[16],
                                     ymin = Q2.5[4], ymax = Q97.5[4]),width=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[4],
                                      xmin = Q2.5[16], xmax = Q97.5[16]),height=0,colour = "#B40F20", size = 0.35,alpha=0.3) +
  geom_point(data = fixedf_pp,aes(x=Estimate[20], #gains
                                  y= Estimate[8]),
             colour="#3B9AB2",size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                     ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                                      xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                     ymin = Q2.5[8], ymax = Q97.5[8]),width=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
  geom_errorbarh(data = fixedf_pp,aes(y=Estimate[8],
                                      xmin = Q2.5[20], xmax = Q97.5[20]), height=0, colour = "#3B9AB2", size = 0.35,alpha=0.3) +
  geom_point(data = fixedf_pp,aes(x=Estimate[20], #persistent
                                  y=Estimate[12]),
             colour="#35274A",size=0.7)+
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                     ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "black", size = 0.55,alpha=0.3) +
  geom_errorbar(data = fixedf_pp,aes(x=Estimate[20],
                                     ymin = Q2.5[12], ymax = Q97.5[12]),width=0,colour = "#35274A", size = 0.35,alpha=0.3) +
  #geom_errorbarh(aes(y=Estimate[8]+Estimate[12],
  #                xmin = Q2.5[16]+Q2.5[20], xmax = Q97.5[16]+Q97.5[20]), height=0, colour = "purple", size = 0.35,alpha=0.3) +
  geom_segment(data = fixedf_pp, # cde
               aes(x = Estimate[20], #gains
                   xend = Estimate[20], # gains
                   y = Estimate[8],   # effect of  sg on bm
                   yend = Estimate[12]), # effect of cde on biomass
               colour= "black",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # cde
               aes(x = Estimate[20], #gains
                   xend = Estimate[20], # gains
                   y = Estimate[8],   # effect of  sg on bm
                   yend = Estimate[12]), # effect of cde on biomass
               colour= "#35274A",
               size = 0.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # gains
               aes(x = Estimate[16], # start at losses
                   xend = Estimate[20], # species gains effect
                   y = Estimate[4],    # effect of sl on biomass
                   yend = Estimate[8]),  # effect of sl + effect of sg on bm
               colour= "black",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # gains
               aes(x = Estimate[16], # start at losses
                   xend = Estimate[20], # species gains effect
                   y = Estimate[4],    # effect of sl on biomass
                   yend = Estimate[8]),  # effect of sl + effect of sg on bm
               colour= "#3B9AB2",
               size = 0.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # losses
               aes(x = 0,
                   xend = Estimate[16], # species losses
                   y = 0,
                   yend = Estimate[4]), # effect of sl on biomass
               colour= "black",
               size = 1.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_segment(data = fixedf_pp, # losses
               aes(x = 0,
                   xend = Estimate[16], # species losses
                   y = 0,
                   yend = Estimate[4]), # effect of sl on biomass
               colour= "#B40F20",
               size = 0.5,
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  labs(x = 'Species Change Slope',
       y = 'Biomass Change Slope',
       title= 'Vector 4.2') 


cafe.vec.cloud2<-ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  ylim(-50,50)+
  geom_point(data=m.coefs,aes(x= L.Slope, y=BL.Slope),alpha=0.3,size=2, colour="#B40F20") +
  geom_errorbar(data=m.coefs,aes(x=L.Slope, ymin = BL.Slope_lower, ymax = BL.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#B40F20") +
  geom_errorbarh(data= m.coefs, aes(y=BL.Slope,xmin = L.Slope_lower, xmax = L.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#B40F20") +
  geom_segment(data=m.coefs,aes(x=0,
                                xend= L.Slope,
                                y=0,
                                yend= BL.Slope), alpha=0.3,colour="#B40F20",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data=m.coefs, aes(x= G.Slope, y=BG.Slope),alpha=0.3,size=2,colour="#3B9AB2") +
  geom_errorbar(data= m.coefs, aes(x=G.Slope,ymin = BG.Slope_lower, ymax = BG.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#3B9AB2") +
  geom_errorbarh(data= m.coefs, aes(y=BG.Slope,xmin = G.Slope_lower, xmax = G.Slope_upper), width = 0, size = 0.45,alpha=0.3,colour="#3B9AB2") +
  geom_segment(data=m.coefs,aes(x= L.Slope,
                                xend=G.Slope,
                                y=BL.Slope,
                                yend=BG.Slope), alpha=0.3,colour="#3B9AB2",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +
  geom_point(data=m.coefs, aes(x= G.Slope, y=P.Slope),alpha=0.3,size=2, colour="#35274A") +
  geom_errorbar(data= m.coefs, aes(x=G.Slope,ymin = P.Slope_lower, ymax = P.Slope_upper), width = 0, size = 0.45,alpha=0.3) +
  geom_segment(data=m.coefs,aes(x= G.Slope,
                                xend=G.Slope,
                                y=BG.Slope,
                                yend=P.Slope), alpha=0.3,colour="#35274A",
               arrow=arrow(type="closed",length=unit(0.2,"cm"))) +

  labs(x = 'Species Change Slope',
       y = 'Biomass Change Slope',
       title= 'Vector 4.2') 


grid_arrange_shared_legend(vec.slope.fig,cafe.vec.cloud2,vec.cloud,cafe.vec.cloud,ncol=2,nrow=2)

