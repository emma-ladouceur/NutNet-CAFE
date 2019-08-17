


library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(sjstats)
library(bayesplot)

load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.bm.Rdata') # plot.bm.im 
load('~/Dropbox/Projects/NutNet/Model_fits/nn_time.rich.Rdata') # plot.rich.im

summary(plot.bm.im )
pp_check(plot.bm.im)

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

View(plot.rich_fitted)
# fixed effect coefficients (I want these for the coefficient plot)
plot.rich_fixef <- fixef(plot.rich.im)

# coefficients for experiment-level (random) effects
plot.rich_coeff <- coef(plot.rich.im)

plot.rich_coef<-as.data.frame(plot.rich_coeff$site_code)
#names(plot.rich_coef) <- gsub(":", ".", names(plot.rich_coef), fixed = TRUE)


startrich<-plot[plot$year_trt %in% c('0'),]
# View(startrich)

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
                                           by = 'site_code') %>%
                                inner_join(startrich %>%
                                             group_by(site_code) %>%
                                             summarise(m.rich = mean(rich),
                                                       r.rich = round(m.rich)),
                                           by = 'site_code'))



plot.rich_coef2$starting.richness <- ifelse(plot.rich_coef2$r.rich >= 1 & plot.rich_coef2$r.rich <= 5, '1-5 species',
                                            ifelse(plot.rich_coef2$r.rich >=6 & plot.rich_coef2$r.rich <=10, '6-10',
                                                   ifelse(plot.rich_coef2$r.rich >=11 & plot.rich_coef2$r.rich <=15, '11-15',    
                                                          ifelse(plot.rich_coef2$r.rich >=16 & plot.rich_coef2$r.rich <=20, '16-20',
                                                                 ifelse(plot.rich_coef2$r.rich >=21 & plot.rich_coef2$r.rich <=25, '21-25',
                                                                        ifelse(plot.rich_coef2$r.rich >=26, '>26', 'other'))))))
#View(plot.rich_coef2)                       

colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"


View(plot.rich_coef2)

dat<-distinct(plot, site_code, continent,habitat)
plot.rich_coef3<-full_join(plot.rich_coef2,dat)

View(plot.rich_fitted)
summary(plot.rich_fitted)
plot.rich_fitted$starting.richness <- ifelse(plot.rich_fitted$rich >= 1 & plot.rich_fitted$rich <= 5, '1-5 species',
                                             ifelse(plot.rich_fitted$rich >=6 & plot.rich_fitted$rich <=10, '6-10',
                                                    ifelse(plot.rich_fitted$rich >=11 & plot.rich_fitted$rich <=15, '11-15',    
                                                           ifelse(plot.rich_fitted$rich >=16 & plot.rich_fitted$rich <=20, '16-20',
                                                                  ifelse(plot.rich_fitted$rich >=21 & plot.rich_fitted$rich <=25, '21-25',
                                                                         ifelse(plot.rich_fitted$rich >=26, '>26', 'other'))))))
plot.rich_fitted2<-full_join(plot.rich_fitted,dat)
plot.rich_fitted.npk<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('NPK'),]
plot.rich_fitted.ctl<-plot.rich_fitted2[plot.rich_fitted2$trt %in% c('Control'),]
# View(plot.rich_fitted2)

# View(plot.rich_coef3)
View(plot.rich_fitted.npk)

# plot.rich_fixef
# 
# levels(plot.rich_fitted.npk$rich.cat)
# levels(plot.rich_coef3$rich.cat)

plot.rich_fitted.npk<-plot.rich_fitted.npk[complete.cases(plot.rich_fitted.npk$starting.richness), ]
plot.rich_coef3<-plot.rich_coef3[complete.cases(plot.rich_coef3$starting.richness), ]

plot.rich_fitted.npk$starting.richness <- factor(plot.rich_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.rich_coef3$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


r1<-ggplot() +
  # data
  geom_point(data = plot.rich_fitted.npk,
             aes(x = year_trt, y = rich,
                 colour = starting.richness, alpha=0.1),
             size = 1.3, position = position_jitter(width = 0.45, height = 0.45)) +
  # geom_jitter(data=plot.rich_fitted.npk,
  #          aes(x = year_trt, y = rich,
  #            colour = starting.richness), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = plot.rich_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = (Estimate.Intercept + Estimate.trtNPK + (Estimate.year_trt + Estimate.trtNPK.year_trt) * xmin),
                   yend = (Estimate.Intercept + Estimate.trtNPK + (Estimate.year_trt + Estimate.trtNPK.year_trt) * xmax),
                   group = site_code,
                   colour = starting.richness),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = plot.rich_fitted.npk,
            aes(x = year_trt, y = Estimate),
            size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
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
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")
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
             by = 'site_code') %>%
  inner_join(startrich %>%
               group_by(site_code) %>%
               summarise(m.rich = mean(rich),
                         r.rich = round(m.rich)),
             by = 'site_code')


plot.bm_coef2$starting.richness <- ifelse(plot.bm_coef2$r.rich >= 1 & plot.bm_coef2$r.rich <= 5, '1-5 species',
                                          ifelse(plot.bm_coef2$r.rich >=6 & plot.bm_coef2$r.rich <=10, '6-10',
                                                 ifelse(plot.bm_coef2$r.rich >=11 & plot.bm_coef2$r.rich <=15, '11-15',    
                                                        ifelse(plot.bm_coef2$r.rich >=16 & plot.bm_coef2$r.rich <=20, '16-20',
                                                               ifelse(plot.bm_coef2$r.rich >=21 & plot.bm_coef2$r.rich <=25, '21-25',
                                                                      ifelse(plot.bm_coef2$r.rich >=26, '>26', 'other'))))))
# View(plot.bm_coef2)



#dat<-distinct(plot, site_code, continent,habitat)

plot.bm_coef3<-full_join(plot.bm_coef2,dat)

# View(plot.bm_coef3)
# View(plot.bm_fitted)
dat2$block<-as.numeric(dat2$block)
plot.bm_fitted$block<-as.numeric(plot.bm_fitted$block)
dat2$plot<-as.numeric(dat2$plot)
plot.bm_fitted$plot<-as.numeric(plot.bm_fitted$plot)
dat2<-distinct(plot,habitat, continent,site_code, year_trt,block, plot,log.live.mass,live_mass,rich)
plot.bm_fitted2<-full_join(plot.bm_fitted,dat2)
# View(plot.bm_fitted)
# View(dat2)


plot.bm_fitted2$starting.richness <- ifelse(plot.bm_fitted2$rich >= 1 & plot.bm_fitted2$rich <= 5, '1-5 species',
                                            ifelse(plot.bm_fitted2$rich >=6 & plot.bm_fitted2$rich <=10, '6-10',
                                                   ifelse(plot.bm_fitted2$rich >=11 & plot.bm_fitted2$rich <=15, '11-15',    
                                                          ifelse(plot.bm_fitted2$rich >=16 & plot.bm_fitted2$rich <=20, '16-20',
                                                                 ifelse(plot.bm_fitted2$rich >=21 & plot.bm_fitted2$rich <=25, '21-25',
                                                                        ifelse(plot.bm_fitted2$rich >=26, '>26', 'other'))))))

plot.bm_fitted.npk<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('NPK'),]
plot.bm_fitted.ctl<-plot.bm_fitted2[plot.bm_fitted2$trt %in% c('Control'),]




plot.bm_fitted.npk<-plot.bm_fitted.npk[complete.cases(plot.bm_fitted.npk$starting.richness), ]
plot.bm_coef3<-plot.bm_coef3[complete.cases(plot.bm_coef3$starting.richness), ]


plot.bm_fitted.npk$starting.richness <- factor(plot.bm_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.bm_coef3$starting.richness <- factor(plot.bm_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))



b1<-ggplot() +
  # data
  geom_point(data = plot.bm_fitted.npk,
             aes(x = year_trt, y = live_mass,
                 colour = starting.richness, alpha=0.1),
             size = .7, position = position_jitter(width = 0.45, height = 0.45)) +
  #experiment (random) effects
  geom_segment(data = plot.bm_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = exp(Intercept + TE  + (ISlope+TESlope) * xmin),
                   yend = exp(Intercept + TE + (ISlope+TESlope) * xmax),
                   group = site_code,
                   colour = factor(starting.richness)),
               size = 0.7) +
  # uncertainy in fixed effect
  geom_ribbon(data = plot.bm_fitted.npk,
              aes(x = year_trt, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = plot.bm_fitted.npk,
            aes(x = year_trt, y = exp(Estimate)),
            size = 1.5) +
  geom_ribbon(data = plot.bm_fitted.ctl,
              aes(x = year_trt, ymin = exp(Q2.5), ymax = exp(Q97.5)),
              alpha = 0.5) +
  # fixed effect
  geom_line(data = plot.bm_fitted.ctl,
            aes(x = year_trt, y = exp(Estimate)),
            size = 1.5,linetype= "dashed") +
  scale_y_continuous(trans = 'log10', #breaks = c(8, 64, 512, 1024, 2048, 4096)
  ) +
  labs(x = 'Years',
       y = expression(paste('Biomass (g/',m^2, ')')), title= 'b) Plot Biomass') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"))
#+ theme(legend.position="bottom")

b1


grid_arrange_shared_legend(r1,b1,nrow=1)



#coefficients

View(plot.bm_coef3)
View(plot.rich_coef3)
colnames(plot.rich_coef3)
colnames(plot.bm_coef3)
#biomass_exp_coef3<-biomass_exp_coef2[,c(-10,-11)]
plot.rich_coef3$Model<-'_Richness'
plot.bm_coef3$Model<-'Biomass'

plot.rich_coef4<-plot.rich_coef3[,c(-1,-2,-3,-4,-5,-6,-7,-8,-10,-14,-18,-19,-20,-21)]
colnames(plot.rich_coef4)[colnames(plot.rich_coef4)=="Estimate.trtNPK.year_trt"] <-  "TESlope"
colnames(plot.rich_coef4)[colnames(plot.rich_coef4)=="Q2.5.trtNPK.year_trt"] <-  "TESlope_lower"
colnames(plot.rich_coef4)[colnames(plot.rich_coef4)=="Q97.5.trtNPK.year_trt"] <-  "TESlope_upper"

colnames(plot.rich_coef4)[colnames(plot.rich_coef4)=="Estimate.year_trt"] <-  "ISlope"
colnames(plot.rich_coef4)[colnames(plot.rich_coef4)=="Q2.5.year_trt"] <-  "ISlope_lower"
colnames(plot.rich_coef4)[colnames(plot.rich_coef4)=="Q97.5.year_trt"] <-  "ISlope_upper"
colnames(plot.rich_coef4)
colnames(plot.bm_coef3)
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17)]
colnames(plot.bm_coef4)

coef.all<-bind_rows(plot.rich_coef4,plot.bm_coef4)
c#oef.all<-inner_join(coef.all,dat2)
View(coef.all)

#fixed
View(plot.bm_fitted.npk)
fixef_r<-as.data.frame(plot.rich_fixef)
fixef_b<-as.data.frame(plot.bm_fixef)
View(fixef_r)
View(fixef_b)
fixef_r$Model<-'_Richness'
fixef_b$Model<-'Biomass'
fixedf_df<-bind_rows(fixef_r,fixef_b)
View(fixedf_df)

coef.all$site_code<-as.factor(coef.all$site_code)

View(coef.all)
colnames(coef.all)
write.csv(coef.all,"~/Dropbox/Projects/NutNet/Data/coef.all.csv")

coef.all<-coef.all[complete.cases(coef.all$TESlope),]

coef.all$starting.richness <- factor(coef.all$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

ggplot() + 
  geom_point(data = coef.all, aes(x = reorder(site_code, ISlope+TESlope), y = ISlope+TESlope,colour = starting.richness),size = 4) +
  geom_errorbar(data = coef.all, aes(x = site_code,ymin = ISlope_lower+TESlope_lower,
                                     ymax = ISlope_upper+TESlope_upper,colour = starting.richness),
                width = 0, size = 1.5) + facet_grid(~Model,scales="free")+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixedf_df, Model=='_Richness'),
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = filter(fixedf_df, Model=='_Richness'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  geom_hline(data = filter(fixedf_df, Model=='Biomass'),
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = filter(fixedf_df, Model=='Biomass'),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  labs(x = 'site_code',
       y = 'Slope') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  coord_flip() + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="none")




plot.rich_coef3<-full_join(plot.rich_coef2,dat)
plot.rich_fixef2<-as.data.frame(plot.rich_fixef)
#plot.rich_coef3$Slope<-as.numeric(plot.rich_coef3$Slope)
is.numeric(plot.rich_coef3$Slope)

# View(plot.rich_fixef2)
# View(plot.rich_coef3)
colnames(plot.rich_coef4)
plot.rich_coef4$starting.richness <- factor(plot.rich_coef4$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
plot.rich_coef4<-plot.rich_coef4[complete.cases(plot.rich_coef4$TESlope),]

r2<-ggplot() + 
  geom_point(data = plot.rich_coef4, aes(x = reorder(site_code,ISlope+TESlope), y = ISlope+TESlope, colour= starting.richness),size = 2) +
  geom_errorbar(data = plot.rich_coef4, aes(x = reorder(site_code,ISlope+TESlope),ymin = ISlope_lower+TESlope_lower,
                                            ymax = ISlope_upper+TESlope_upper,colour= starting.richness),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  #facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = fixef_r,
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = fixef_r,
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  #ylim(-0.3, 0.3) +
  labs(x = 'Site',
       y = 'Slope') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'a) Plot Richness') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

r2



summary(plot.bm_coef4)
colnames(plot.bm_coef4)

plot.bm_coef4<-plot.bm_coef4[complete.cases(plot.bm_coef4$TESlope),]
plot.bm_coef4$starting.richness <- factor(plot.bm_coef4$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
b2<-ggplot() + 
  geom_point(data = plot.bm_coef4, aes(x = reorder(site_code, ISlope + TESlope), y = ISlope+ TESlope, colour = starting.richness),size = 2) +
  geom_errorbar(data = plot.bm_coef4, aes(x = reorder(site_code, ISlope+TESlope),ymin = ISlope_lower+TESlope_lower,
                                          ymax = ISlope_upper + TESlope_upper,colour = starting.richness),
                width = 0, size = 0.7) + 
  facet_wrap(Model~.)+
  #facet_grid(continent~., scales= 'free', space='free')+
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(data = filter(fixef_b,),
             aes(yintercept = Estimate[4]), size = 1.2) +
  geom_rect(data = filter(fixef_b, ),
            aes(xmin = -Inf, xmax = Inf,
                ymin = Q2.5[4], ymax = Q97.5[4]),
            alpha = 0.3) +
  #ylim(-0.3, 0.3) +
  labs(x = 'Site',
       y = 'Slope') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  coord_flip() + 
  labs(x = 'Site',
       y = 'Slope', title= 'b) Plot Biomass') +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   axis.title.y = element_blank(),#axis.text.y = element_blank(),
                   strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")

b2

grid_arrange_shared_legend(r2,b2,nrow=1)

#grid_arrange_shared_legend(r1,b1,r2,b2,nrow=2,ncol=2)




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





