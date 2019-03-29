library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(priceTools)
sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



#plot<-p[p$trt %in% c('NPK'),]

colnames(plot)
plot$year_trt<-as.factor(as.character(plot$year_trt))
plot$continent<-as.factor(plot$continent)
plot$habitat<-as.factor(plot$habitat)
plot$site<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

#native<-sp[sp$local_provenance %in% c('NAT'),]
#introduced<-sp[sp$local_provenance %in% c('INT'),]

Control<-plot[plot$trt %in% c('Control'),]
NPK<-plot[plot$trt %in% c('NPK'),]
Control.three<-Control[Control$year_trt %in% c('3'),]
NPK.three<-NPK[NPK$year_trt %in% c('3'),]
Control.zero<-Control[Control$year_trt %in% c('0'),]
NPK.zero<-NPK[NPK$year_trt %in% c('0'),]

View(Control)
Control.three$treat<-'No NPK'
NPK.three$treat<-'NPK'
Control.zero$treat<-'Control'
NPK.zero$treat<-'Control'

tplot<-bind_rows(Control.three,NPK.three,Control.zero,NPK.zero)
summary(tplot)
tplot$treat<-as.factor(as.character(tplot$treat))
levels(tplot$treat)
View(tplot)

p2 <- plot[!(is.na(p$live_mass)),]
View(p2)
#s<-distinct(p2,site_code,continent, habitat,trt)
#View(s)
#site.r.bm<- p2 %>% group_by(site_code,year_trt) %>% dplyr::summarise(site.rich=sum(rich),site.bm=sum(live_mass))
#site<-full_join(s,site.r.bm, by="site_code")
#View(site)

colnames(plot)
View(year.three)
#plot all
plot.rich.trt.m <- brm(rich ~  trt + (trt | site_code/block/plot), 
                   data = year.three, cores = 4, chains = 4)

plot.bm.trt.m <- brm(live_mass ~  trt + (trt | site_code/block/plot), 
                 data =year.three, cores = 4, chains = 4)

#with time

tplot.rich.trt.m <- brm(rich ~  treat + (treat | site_code/block/plot), 
                       data = tplot, cores = 4, chains = 4)

tplot.bm.trt.m <- brm(live_mass ~  treat + (treat | site_code/block/plot), 
                        data = tplot, cores = 4, chains = 4)

setwd('~/Dropbox/Projects/NutNet/Model_fits/')
save(plot.rich.trt.m,plot.bm.trt.m,tplot.rich.trt.m,tplot.bm.trt.m,file = 'plot.nutnet.trt.models.Rdata')
load('/Users/el50nico/Desktop/Academic/R code/NutNet/plot.nutnet.trt.models.Rdata')


summary(plot.rich.trt.m)


# inspection of chain diagnostic
plot(plot.rich.trt.m)# looks ok


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(plot.rich.trt.m)

#residuals
rtrtm1<-residuals(plot.rich.trt.m)
rtrtm1<-as.data.frame(rtrtm1)
View(rtrtm1)
nrow(rtrtm1)
nrow(plot)
rtr.plot<-cbind(plot,rtrtm1$Estimate)
View(rtr.plot)

head(rtr.plot)
par(mfrow=c(3,2))
with(rtr.plot, plot(continent, rtrtm1$Estimate))
with(rtr.plot, plot(habitat, rtrtm1$Estimate))
with(rtr.plot, plot(site_code, rtrtm1$Estimate))
with(rtr.plot, plot(block, rtrtm1$Estimate))
with(rtr.plot, plot(plot, rtrtm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
plot.rich.trt_fitted <- cbind(plot.rich.trt.m$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(plot.rich.trt.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.rich.trt_fixef <- fixef(plot.rich.trt.m)
plot.rich.trt_fixef<-as.data.frame(plot.rich.trt_fixef)
View(plot.rich.trt_fixef)

# coefficients for experiment-level (random) effects
plot.rich.trt_coef <- coef(plot.rich.trt.m)
plot.rich.trt_coef<-as.data.frame(plot.rich.trt_coef$site_code)
View(plot.rich.trt_coef)
plot.rich.trt_coef2 <-  bind_cols(plot.rich.trt_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(plot.rich.trt_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
plot.rich.trt_coef$site_code[,,'trtNPK'] %>% 
  as_tibble() %>% 
  mutate(Slope = Estimate,
         Slope_lower = Q2.5,
         Slope_upper = Q97.5) %>% 
  select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) 


View(plot.rich.trt_coef2)

dat<-distinct(plot, site_code, continent,habitat)
plot.rich.trt_coef3<-full_join(plot.rich.trt_coef2,dat)
plot.rich.trt_coef3$Model<-'_Richness'

rp3<-ggplot() + 
  geom_point(data = plot.rich.trt_coef3, aes(x = reorder(site_code, Slope), y = Slope,colour = continent),size = 2) +
  geom_errorbar(data = plot.rich.trt_coef3, aes(x = site_code,ymin = Slope_lower,
                                            ymax = Slope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(plot.rich.trt_fixef,),
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(plot.rich.trt_fixef, ),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  labs(x = 'site_code',
       y = 'Intercept') +
  scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", 
                                "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+
  coord_flip() + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")




#TIME
summary(tplot.rich.trt.m)


# inspection of chain diagnostic
plot(tplot.rich.trt.m)# looks ok


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(tplot.rich.trt.m)

#residuals
rtrtm1<-residuals(tplot.rich.trt.m)
rtrtm1<-as.data.frame(rtrtm1)
View(rtrtm1)
nrow(rtrtm1)
nrow(plot)
rtr.plot<-cbind(plot,rtrtm1$Estimate)
View(rtr.plot)

head(rtr.plot)
par(mfrow=c(3,2))
with(rtr.plot, plot(continent, rtrtm1$Estimate))
with(rtr.plot, plot(habitat, rtrtm1$Estimate))
with(rtr.plot, plot(site_code, rtrtm1$Estimate))
with(rtr.plot, plot(block, rtrtm1$Estimate))
with(rtr.plot, plot(plot, rtrtm1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
tplot.rich.trt_fitted <- cbind(tplot.rich.trt.m$data,
                              # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                              fitted(tplot.rich.trt.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
tplot.rich.trt_fixef <- fixef(tplot.rich.trt.m)
tplot.rich.trt_fixef<-as.data.frame(tplot.rich.trt_fixef)
View(tplot.rich.trt_fixef)

tplot.rich_coef <- coef(tplot.rich.m)
plot.rich_coef2 <-  bind_cols(tplot.rich_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(plot.rich_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5))
                              
plot.rich_coef3 <-  bind_cols(tplot.rich_coef$site_code[,,'treatNPK'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(tplot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code')

plot.rich_coef4<-bind_cols(plot.rich_coef2,plot.rich_coef3)


View(tplot.rich.trt_fixef2)
library(data.table)
tplot.rich.trt_fixef2<-setDT(tplot.rich.trt_fixef, keep.rownames = TRUE)[]
tplot.rich.trt_fixef3<-tplot.rich.trt_fixef2[-c(1), ] 
tplot.rich.trt_fixef3$Model<-"_Richness"


tr1<-ggplot() + 
  geom_point(data = tplot.rich.trt_fixef3, aes(x = rn, y = Estimate),size = 2) +
  geom_errorbar(data = tplot.rich.trt_fixef3, aes(x = rn,ymin = Q2.5,
                                                 ymax = Q97.5),
                width = 0, size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_discrete(labels=c("Control","NPK"))+
  labs(x = 'Treatment',
       y = 'Estimate', title= 'a) Plot Richness') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")



tplot.bm.trt_fitted <- cbind(tplot.bm.trt.m$data,
                               # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                               fitted(tplot.bm.trt.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
tplot.bm.trt_fixef <- fixef(tplot.bm.trt.m)
tplot.bm.trt_fixef<-as.data.frame(tplot.bm.trt_fixef)
View(tplot.bm.trt_fixef)

View(tplot.bm.trt_fixef2)
library(data.table)
tplot.bm.trt_fixef2<-setDT(tplot.bm.trt_fixef, keep.rownames = TRUE)[]
tplot.bm.trt_fixef3<-tplot.bm.trt_fixef2[-c(1), ] 
tplot.bm.trt_fixef3$Model<-"Biomass"


tb1<-ggplot() + 
  geom_point(data = tplot.bm.trt_fixef3, aes(x = rn, y = Estimate),size = 2) +
  geom_errorbar(data = tplot.bm.trt_fixef3, aes(x = rn,ymin = Q2.5,
                                                  ymax = Q97.5),
                width = 0, size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_discrete(labels=c("Control","NPK"))+
  labs(x = 'Treatment',
       y = 'Estimate', title= 'a) Plot Biomass') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")



tplot.bm.trt_fitted <- cbind(tplot.bm.trt.m$data,
                             # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                             fitted(tplot.bm.trt.m, re_formula = NA)) %>% 
  as_tibble() 



grid.arrange(tr1,tb1,nrow=1)



p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all2<-p.all[p.all$trt.x %in% c('NPK'),]
p.all3<-p.all2[p.all2$trt.y %in% c('NPK'),]
levels(p.all3$trt_year)
View(p.all3)
p.all4<-separate(p.all3,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all4$f.year.y<-as.factor(p.all4$year.y)
p.all4$plot<-as.factor(p.all4$plot)
p.all4$site_code<-as.factor(p.all4$site_code)
p.all2<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)
list1<-distinct(p,site_code,site_name)
p.all3<-inner_join(p.all2,list1,by="site_code")

View(p.all3)
colnames(p.all3)



p.all4<-p.all3 %>% filter(year.y != "0" )

p.all4<-p.all4[p.all4$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]

#take max value of year.y??

#max values only
npk4<-npk3 %>% group_by(site_code) %>% top_n(1, year.y)
View(npk4)




#price

p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all$trt_year<-as.factor(as.character(p.all$trt_year))

p.all2 <- p.all[p.all$trt_year %in% c('Control_0 NPK_0', 'NPK_0 NPK_3', 'Control_0 Control_3'),]

Control.three<-p.all2[p.all2$trt_year %in% c('Control_0 Control_3'),]
NPK.three<-p.all2[p.all2$trt_year %in% c('NPK_0 NPK_3'),]
Control.zero<-p.all2[p.all2$trt_year %in% c('Control_0 NPK_0'),]

Control.three$treat<-'No NPK'
NPK.three$treat<-'NPK'
Control.zero$treat<-'Control'


price.tplot<-bind_rows(Control.three,NPK.three,Control.zero)
colnames(price.tplot)

#models
#multi
multi_sg.sl <- brm(cbind(SL, SG) ~  treat + (treat | p | site.year.id), 
               data = price.tplot, cores = 4, chains = 4)

#univariate
SL.trt.m <- brm(SL ~   treat + (treat | site.year.id/block/plot/site.year.id.y), 
                data = price.tplot, cores = 4, chains = 4)

SG.trt.m <- brm(SG ~   treat + (treat | site.year.id/block/plot/site.year.id.y), 
                 data = price.tplot, cores = 4, chains = 4)

CDE.trt.m <- brm(CDE ~   treat + (treat | site.year.id/block/plot/site.year.id.y), 
                       data = price.tplot, cores = 4, chains = 4)

setwd('~/Dropbox/Projects/NutNet/Model_fits/')
save(multi_sg.sl,CDE.trt.m,file = 'price.trt.year.three.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price.trt.year.three.Rdata')

NPK.three<-NPK.three[complete.cases(NPK.three), ]
View(NPK.three)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
leap.zig(NPK.three,type='cafe',xlim=c(0,15),ylim=c(0,700),standardize = FALSE,raw.points = FALSE)+ 
  annotate("text", x = mean(NPK.three$x.rich), y = mean(NPK.three$x.func), 
           label = "*",size=8)+ggtitle('NPK 0 vs NPK 3')+theme_classic()


summary(CDE.trt.m)


# inspection of chain diagnostic
plot(CDE.trt.m)# looks ok


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(CDE.trt.m)

#residuals
cdem1<-residuals(CDE.trt.m)
cdem1<-as.data.frame(cdem1)
nrow(cdem1)
nrow(price.tplot)
cde.plot<-cbind(price.tplot,cdem1$Estimate)
View(cde.plot)

head(rtr.plot)
par(mfrow=c(1,3))
#with(rtr.plot, plot(continent, rtrtm1$Estimate))
#with(rtr.plot, plot(habitat, rtrtm1$Estimate))
with(cde.plot, plot(site.year.id, cdem1$Estimate))
with(cde.plot, plot(block, cdem1$Estimate))
with(cde.plot, plot(plot, cdem1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
CDE.trt.m_fitted <- cbind(CDE.trt.m$data,
                               # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                               fitted(CDE.trt.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
CDE.trt.m_fixef <- fixef(CDE.trt.m)
CDE.trt.m_fixef<-as.data.frame(CDE.trt.m_fixef)
View(CDE.trt.m_fixef)

CDE.trt.m_coef <- coef(CDE.trt.m)
plot.rich_coef2 <-  bind_cols(tplot.rich_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(plot.rich_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5))

plot.rich_coef3 <-  bind_cols(tplot.rich_coef$site_code[,,'treatNPK'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(tplot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code')

plot.rich_coef4<-bind_cols(plot.rich_coef2,plot.rich_coef3)


View(CDE.trt.m_fixef)
library(data.table)
CDE.trt.m_fixef2<-setDT(CDE.trt.m_fixef, keep.rownames = TRUE)[]
View(CDE.trt.m_fixef3)
CDE.trt.m_fixef3<-CDE.trt.m_fixef2[-c(1), ] 
CDE.trt.m_fixef3$Model<-"CDE"


cde<-ggplot() + 
  geom_point(data = CDE.trt.m_fixef3, aes(x = rn, y = Estimate),size = 2) +
  geom_errorbar(data = CDE.trt.m_fixef3, aes(x = rn,ymin = Q2.5,
                                                  ymax = Q97.5),
                width = 0, size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_discrete(labels=c("Control","NPK"))+
  labs(x = 'Treatment',
       y = 'Estimate', title= 'c) Change in Function of Persistent Species') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")




#multi

m1<-residuals(multi_sg.sl)
m1
nrow(m1)
nrow(price.tplot)
price.tplot2<-price.tplot[complete.cases(price.tplot), ]
nrow(price.tplot2)
plot <- cbind(price.tplot2,
              residual_m1_SL = m1[,,'SL'][,'Estimate'],
              residual_m1_SG = m1[,,'SG'][,'Estimate'])

colnames(plot)
par(mfrow=c(1,3))
with(plot, plot(site.year.id, residual_m1_SL))
with(plot, plot(block, residual_m1_SL))
with(plot, plot(plot, residual_m1_SL))
#with(plot, plot(fyr.trt, residual_m1_SL))

par(mfrow=c(1,3))
with(plot, plot(site.year.id, residual_m1_SG))
with(plot, plot(block, residual_m1_SG))
with(plot, plot(plot, residual_m1_SG))


mm_fitted <- cbind(multi_sg.sl$data,
                   # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                   fitted(multi_sg.sl, re_formula = NA)) %>% 
  as_tibble() 
  # get the seed.rich values for plotting
  #inner_join(plot %>% distinct(Experiment, seed.rich, seed.rich.m, biomass.plot),
        #     by = c('Experiment', 'seed.rich.m')) 

# fixed effect coefficients (I want these for the coefficient plot)
mm_fixef <- fixef(multi_sg.sl)
View(mm_fixef)


mm_fixef2<-as.data.frame(mm_fixef)
mm_fixef3<-setDT(mm_fixef2, keep.rownames = TRUE)[]
View(mm_fixef3)

mm_SL_fixef<- mm_fixef3[mm_fixef3$rn %in% c('SL_Intercept', 'SL_treatNoNPK', 'SL_treatNPK'),]
mm_SL_fixef$Model<-"SL"

mm_SG_fixef<- mm_fixef3[mm_fixef3$rn %in% c('SG_Intercept', 'SG_treatNoNPK', 'SG_treatNPK'),]
mm_SG_fixef$Model<-"SG"

mm_SL_fixef2<-mm_SL_fixef[-c(1), ] 
mm_SG_fixef2<-mm_SG_fixef[-c(1), ] 


sl<-ggplot() + 
  geom_point(data = mm_SL_fixef2, aes(x = rn, y = Estimate),size = 2) +
  geom_errorbar(data = mm_SL_fixef2, aes(x = rn,ymin = Q2.5,
                                             ymax = Q97.5),
                width = 0, size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_discrete(labels=c("Control","NPK"))+
  labs(x = 'Treatment',
       y = 'Estimate', title= 'a) EF as affected by SL') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")



sg<-ggplot() + 
  geom_point(data = mm_SG_fixef2, aes(x = rn, y = Estimate),size = 2) +
  geom_errorbar(data = mm_SG_fixef2, aes(x = rn,ymin = Q2.5,
                                         ymax = Q97.5),
                width = 0, size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_discrete(labels=c("Control","NPK"))+
  labs(x = 'Treatment',
       y = 'Estimate', title= 'b) EF as affected by SG') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")

grid.arrange(tr1,tb1,sl,sg,cde,nrow=2)




