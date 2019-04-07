
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)

sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

dat2<-distinct(p, continent, site_code, year_trt)
View(dat2)

#plot<-p[p$trt %in% c('NPK'),]
#or
plot<-p[p$trt %in% c('NPK', 'Control'),]
nrow(plot)

colnames(plot)
plot$year_trt<-as.numeric(as.character(plot$year_trt))
plot$continent<-as.factor(plot$continent)
plot$habitat<-as.factor(plot$habitat)
plot$site<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)
plot$f.year_trt<-as.factor(as.character(plot$year_trt))

#log
plot$log.rich<-log(plot$rich)
plot$log.live.mass<-log(plot$live_mass)
plot$log.year.trt<-log(plot$year_trt + 1)


par(mfrow=c(2,2))
hist(plot$rich,breaks =40, main="rich", xlab= "rich")
hist(plot$live_mass, breaks=40, main="bm", xlab= "bm")

hist(plot$log.rich,breaks =40, main="rich", xlab= "rich")
hist(plot$log.live.mass, breaks=40, main="bm", xlab= "bm")

#native<-sp[sp$local_provenance %in% c('NAT'),]
#introduced<-sp[sp$local_provenance %in% c('INT'),]

View(plot)
View(p)
#site
#plot2 <- plot[!(is.na(plot$log.live.mass)),]
#View(p2)


head(plot2)

#plot all
plot.rich.m <- brm(rich ~  year_trt + (year_trt | site_code), 
                   data = plot, family = poisson() ,  cores = 4, chains = 4)

plot.rich.im <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
                   data = plot,   cores = 4, chains = 4)


plot.bm.m <- brm(live_mass ~ year_trt + (year_trt | site_code), 
                 data = plot, family = lognormal() , cores = 4, chains = 4)

plot.bm.im <- brm(log.live.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                   data = plot , cores = 4, chains = 4)



setwd('~/Dropbox/Projects/NutNet/Model_fits/')
save(plot.rich.m,plot.bm.m,file = 'plot.nutnet.models.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/plot.nutnet.models.Rdata')

save(plot.rich.im,plot.bm.im,file = 'plot.nutnet.i.models.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/plot.nutnet.i.models.Rdata')

summary(plot.rich.im)


# inspection of chain diagnostic
plot(plot.rich.im)


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
color_scheme_set("purple")
pp_check(plot.rich.im)



#residuals
m1<-residuals(plot.rich.im)
m1<-as.data.frame(m1)
nrow(m1)
nrow(plot)
rr.plot<-cbind(plot,m1$Estimate)
View(rr.plot)

head(rr.plot)
par(mfrow=c(3,2))
with(rr.plot, plot(continent, m1$Estimate))
with(rr.plot, plot(habitat, m1$Estimate))
with(rr.plot, plot(site_code, m1$Estimate))
with(rr.plot, plot(block, m1$Estimate))
with(rr.plot, plot(plot, m1$Estimate))
with(rr.plot, plot(f.year_trt, m1$Estimate))


# #------plot richness model all sp----------------
# fixed effects

plot.rich_fitted <- cbind(plot.rich.im$data,
                        # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                        fitted(plot.rich.im, re_formula = NA)) %>% 
  as_tibble() 
#%>% 
  # get the seed.rich values for plotting
 # inner_join(plot2 %>%
            #   distinct(site_code, rich ,log.rich, continent, habitat),
             #by = c('site_code', 'log.rich'))

View(plot.rich_fitted)
# fixed effect coefficients (I want these for the coefficient plot)
plot.rich_fixef <- fixef(plot.rich.im)

# coefficients for experiment-level (random) effects
plot.rich_coeff <- coef(plot.rich.im)

plot.rich_coef<-as.data.frame(plot.rich_coeff$site_code)
#names(plot.rich_coef) <- gsub(":", ".", names(plot.rich_coef), fixed = TRUE)
plot.rich_coef %>% head

plot.rich_coef2 <-  bind_cols(plot.rich_coef %>% 
                                as_tibble() %>% 
                                mutate(
                                 # Estimate.trtNPK.year_trt = "Estimate.trtNPK:year_trt",
                                  #trtNPK.year_trt_lower = "Q2.5.trtNPK:year_trt",
                                  #trtNPK.year_trt_upper = "Q97.5.trtNPK:year_trt",
                                   #    Slope = Estimate.year_trt ,
                                    #   Slope_lower =Q2.5.year_trt ,
                                     #  Slope_upper = Q97.5.year_trt,
                                      site_code = rownames(plot.rich_coef)) %>% 
                                as_tibble() %>%
                          # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code'))

colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"




View(plot.rich_coef3)

dat<-distinct(plot, site_code, continent,habitat)
plot.rich_coef3<-full_join(plot.rich_coef2,dat)

View(plot.rich_fitted)
plot.rich_fitted2<-full_join(plot.rich_fitted,dat)
plot.rich_fitted.npk<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('NPK'),]
plot.rich_fitted.ctl<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('Control'),]
View(plot.rich_fitted2)

colnames(plot.rich_coef3)
View(plot.rich_fitted2)

plot.rich_fixef
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
r1<-ggplot() +
  # data
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = rich,
                 colour = continent, alpha=0.5),
             size = 1.2) +
  geom_jitter(data=plot.rich_fitted.npk,
           aes(x = year_trt, y = rich,
             colour = continent), height=0.25,width = 0.25)+
  # experiment (random) effects
  geom_segment(data = plot.rich_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Estimate.Intercept + Estimate.trtNPK + (Estimate.year_trt + Estimate.trtNPK.year_trt) * xmin),
                   yend = exp(Estimate.Intercept + Estimate.trtNPK + (Estimate.year_trt + Estimate.trtNPK.year_trt) * xmax),
                   group = site_code,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = plot.rich_fitted.npk,
            aes(x = year_trt, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = plot.rich_fitted.ctl,
            aes(x = year_trt, y = Estimate),
            size = 1.5,linetype= "dashed") +
  #scale_y_continuous(trans = 'log10') +
  # # sanity check
  # geom_abline(data = plot.rich_fixef2,
  #             aes(intercept = plot.rich_fixef2['Intercept','Estimate'],
  #                 slope = plot.rich_fixef2['year_trt', 'Estimate']),
  #             colour = 'pink') +
  labs(x = 'Years',
       y = 'Species richness', title= 'a) Plot Richness') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()#+ theme(legend.position="bottom")
r1


#plot biomass


summary(plot.bm.im)


# inspection of chain diagnostic
plot(plot.bm.im)  


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(plot.bm.im)


#residuals
bm1<-residuals(plot.bm.im)
bm1<-as.data.frame(bm1)
nrow(bm1)
nrow(plot)
plot2 <- plot[!(is.na(plot$live_mass)),]
rb.plot<-cbind(plot2,bm1$Estimate)
View(rb.plot)

head(rb.plot)
par(mfrow=c(3,2))
with(rb.plot, plot(continent, bm1$Estimate))
with(rb.plot, plot(habitat, bm1$Estimate))
with(rb.plot, plot(site_code, bm1$Estimate))
with(rb.plot, plot(block, bm1$Estimate))
with(rb.plot, plot(plot, bm1$Estimate))
with(rb.plot, plot(f.year_trt, bm1$Estimate))

# #------plot richness model all sp----------------
# fixed effects
plot.bm_fitted <- cbind(plot.bm.im$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(plot.bm.im, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
plot.bm_fixef <- fixef(plot.bm.im)

# coefficients for experiment-level (random) effects
plot.bm_coef <- coef(plot.bm.im)
plot.bm_coef 

plot.bm_coef2 <-  bind_cols(plot.bm_coef$site_code[,,'Intercept'] %>% 
                             as_tibble() %>% 
                             mutate(Intercept = Estimate,
                                    Intercept_lower = Q2.5,
                                    Intercept_upper = Q97.5,
                                    site_code = rownames(plot.bm_coef$site_code[,,'Intercept'])) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           plot.bm_coef$site_code[,,'year_trt'] %>% 
                             as_tibble() %>% 
                             mutate(ISlope = Estimate,
                                    ISlope_lower = Q2.5,
                                    ISlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           plot.bm_coef$site_code[,,'trtNPK'] %>% 
                             as_tibble() %>% 
                             mutate(TE = Estimate,
                                    TE_lower = Q2.5,
                                    TE_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                           plot.bm_coef$site_code[,,'trtNPK:year_trt'] %>% 
                             as_tibble() %>% 
                             mutate(TESlope = Estimate,
                                    TESlope_lower = Q2.5,
                                    TESlope_upper = Q97.5) %>% 
                             select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(plot %>% 
               group_by(site_code) %>% 
               summarise(xmin = min(year_trt),
                         xmax = max(year_trt)),
             by = 'site_code')


View(plot.bm_coef2)



#dat<-distinct(plot, site_code, continent,habitat)

plot.bm_coef3<-full_join(plot.bm_coef2,dat)

View(plot.bm_coef3)
View(plot.bm_fitted)
plot.bm_fitted2<-full_join(plot.bm_fitted,dat2)
View(plot.bm_fitted2)
View(plot)
dat2<-distinct(plot,habitat, continent,site_code, year_trt,block, plot,log.live.mass,live_mass)

plot.bm_fitted.npk<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('NPK'),]
plot.bm_fitted.ctl<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('Control'),]

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
b1<-ggplot() +
  # data
  geom_point(data = plot.bm_fitted.npk,
             aes(x = year_trt, y = live_mass,
                 colour = continent),
             size = 1.2,alpha=0.5) +
  geom_jitter(data=plot.bm_fitted.npk,
              aes(x = year_trt, y = live_mass,
                colour = continent), height=0.25,width = 0.25)+
   #experiment (random) effects
  geom_segment(data = plot.bm_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + ISlope + (TE+TESlope) * xmin),
                   yend = exp(Intercept + ISlope + (TE+TESlope) * xmax),
                   group = site_code,
                   colour = continent),
               size = 0.7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.bm_fitted.npk,
              aes(x = year_trt, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = plot.bm_fitted.npk,
            aes(x = year_trt, y = exp(Estimate)),
            size = 1.5) +
    geom_ribbon(data = plot.bm_fitted.ctl,
                aes(x = year_trt, ymin = exp(Q2.5), ymax = exp(Q97.5)),
                alpha = 0.3) +
    # fixed effect
    geom_line(data = plot.bm_fitted.ctl,
              aes(x = year_trt, y = exp(Estimate)),
              size = 1.5,linetype= "dashed") +
  scale_y_continuous(trans = 'log', breaks = c(8, 64, 512, 1024, 2048, 4096)) +
  labs(x = 'Years',
       y = expression(paste('Biomass (g/',m^2, ')')), title= 'b) Plot Biomass') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  theme_bw()#+ theme(legend.position="bottom")
  
b1


grid_arrange_shared_legend(r1,b1,nrow=1)
#scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+





#coefficients

View(plot.bm_coef2)
View(plot.rich_coef2)
colnames(plot.rich_coef2)
colnames(plot.bm_coef2)
#biomass_exp_coef3<-biomass_exp_coef2[,c(-10,-11)]
plot.rich_coef2$Model<-'_Richness'
plot.bm_coef2$Model<-'Biomass'
coef.all<-bind_rows(plot.rich_coef3,plot.bm_coef3)
coef.all<-inner_join(coef.all,dat2)
View(coef.all)

#fixed
fixef_r<-as.data.frame(plot.rich_fixef)
fixef_b<-as.data.frame(plot.bm_fixef)
fixef_r$Model<-'_Richness'
fixef_b$Model<-'Biomass'
fixedf_df<-bind_rows(fixef_r,fixef_b)
View(fixedf_df)

coef.all$site_code<-as.factor(coef.all$site_code)

View(coef.all)
write.csv(coef.all,"~/Dropbox/Projects/SeedAdd/Data/mm_coef.all.csv")



#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot() + 
  geom_point(data = coef.all, aes(x = reorder(site_code, Slope), y = Slope,colour = site_code),size = 4) +
  geom_errorbar(data = coef.all, aes(x = site_code,ymin = Slope_lower,
                                     ymax = Slope_upper,colour = site_code),
                width = 0, size = 1.5) + facet_grid(~Model,scales="free")+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixedf_df, Model=='_Richness'),
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixedf_df, Model=='_Richness'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  geom_hline(data = filter(fixedf_df, Model=='Biomass'),
             aes(yintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixedf_df, Model=='Biomass'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[2], ymax = Q97.5[2]),
            alpha = 0.3) +
  labs(x = 'site_code',
       y = 'Slope') +
  #scale_colour_manual(values = c("#FA6B09FF", "#EE0011FF" , "#EC579AFF", "#15983DFF", "#149BEDFF", 
                                # "#A1C720FF","#0C5BB0FF", "#8F2F8BFF", "#F9B90AFF", "#16A08CFF" ,"#6A7F93FF","#9A703EFF" ))+
  #scale_x_discrete(limits = rev(levels(coef.all$site_code)))+
  coord_flip() + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")




plot.rich_coef3<-full_join(plot.rich_coef2,dat)
plot.rich_fixef2<-as.data.frame(plot.rich_fixef)
#plot.rich_coef3$Slope<-as.numeric(plot.rich_coef3$Slope)
is.numeric(plot.rich_coef3$Slope)

View(plot.rich_fixef2)
View(plot.rich_coef3)
colnames(plot.rich_coef3)

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
r2<-ggplot() + 
  geom_point(data = plot.rich_coef3, aes(x = reorder(site_code,Estimate.trtNPK.year_trt), y = Estimate.trtNPK.year_trt,colour = continent),size = 2) +
  geom_errorbar(data = plot.rich_coef3, aes(x = reorder(site_code,Estimate.trtNPK.year_trt),ymin = Q2.5.trtNPK.year_trt,
                                      ymax = Q97.5.trtNPK.year_trt,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = plot.rich_fixef2,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = plot.rich_fixef2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  ylim(-0.3, 0.3) +
  labs(x = 'Site',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'a) Plot Richness') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

r2

View(plot)

plot.bm_coef3<-full_join(plot.bm_coef2,dat)
View(plot.bm_coef3)
plot.rich_coef3$slope.rich<-plot.rich_coef3$Estimate.trtNPK.year_trt
View(plot.rich_coef3)
rich.slope<-select(plot.rich_coef3,site_code,slope.rich)
plot.bm_coef4<-inner_join(plot.bm_coef3,rich.slope)
is.numeric(plot.bm_coef3$slope.rich)
plot.bm_coef3$site_code <- reorder(plot.bm_coef3$site_code, plot.bm_coef3$slope.rich)

View(fixef_b)
View(plot.bm_coef4)
colnames(plot.bm_coef4)

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
b2<-ggplot() + 
  geom_point(data = plot.bm_coef4, aes(x = reorder(site_code, slope.rich), y = TESlope, colour = continent),size = 2) +
  geom_errorbar(data = plot.bm_coef4, aes(x = reorder(site_code, slope.rich),ymin = TESlope_lower,
                                            ymax = TESlope_upper,colour = continent),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixef_b,),
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = filter(fixef_b, ),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  ylim(-0.3, 0.3) +
  labs(x = 'Site',
       y = 'Slope') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF"))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'b) Plot Biomass') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

b2

grid_arrange_shared_legend(r2,b2,nrow=1)

grid_arrange_shared_legend(r1,b1,r2,b2,nrow=2,ncol=2)


#delta

View(plot.rich_coef3)
View(plot.bm_coef3)
colnames(plot.rich_coef3)
colnames(plot.bm_coef3)
plot.rich_coef4<-plot.rich_coef3[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-16,-17,-18,-20)]
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9,-10,-12)]
colnames(plot.rich_coef4)
colnames(plot.bm_coef4)
names(plot.rich_coef4) <- c("R.Slope","R.Slope_lower","R.Slope_upper","site_code","continent")
names(plot.bm_coef4) <- c("site_code","B.Slope","B.Slope_lower","B.Slope_upper","continent")
delta.coefs<-bind_cols(plot.rich_coef4,plot.bm_coef4)
View(delta.coefs)

#By Experiment
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(data=delta.coefs, aes(x=R.Slope, y=B.Slope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = R.Slope_lower, xmax = R.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'Richness Slope',
       y = 'Biomass Slope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")


ggplot(data=delta.coefs, aes(x=R.Slope, y=B.Slope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  #facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = R.Slope_lower, xmax = R.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'Richness Slope',
       y = 'Biomass Slope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")






#biomass by CDE delta plot

View(cde_coef3)
View(plot.bm_coef3)
colnames(cde_coef3)
colnames(plot.bm_coef3)
cde_coef4<-cde_coef3[,c(-1,-2,-3,-8,-9,-10,-11)]
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9)]
colnames(cde_coef4)
colnames(plot.bm_coef4)
names(cde_coef4) <- c("site_code","C.Slope","C.Slope_lower","C.Slope_upper","continent","habitat")
names(plot.bm_coef4) <- c("site_code","B.Slope","B.Slope_lower","B.Slope_upper","continent","habitat")
delta.coefs<-bind_cols(cde_coef4,plot.bm_coef4)
View(delta.coefs)

#By Experiment
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(data=delta.coefs, aes(x=C.Slope, y=B.Slope,color=continent)) +
  geom_point(size=2) +
  #geom_jitter(height=0.45,width = 0.45)+
  facet_grid(continent~., scales= 'free', space='free')+
  geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  geom_errorbarh(aes(xmin = C.Slope_lower, xmax = C.Slope_upper,colour = continent), width = 0, size = 0.75,alpha=0.5) +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  labs(x = 'Biomass Change Slope',
       y = 'Overall Biomass Slope') +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")





