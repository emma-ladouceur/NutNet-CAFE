


plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

start.rich <-read.csv("~/Dropbox/Projects/NutNet/Data/start.rich.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

View(start.rich)

load('~/Dropbox/Projects/NutNet/Model_fits/full/rich.Rdata') # plot.rich.g

load('~/Dropbox/Projects/NutNet/Model_fits/full/rich.log.Rdata') # plot.rich.l



summary(plot.rich.l)


color_scheme_set("gray")
pp_check(plot.rich.l) + theme_classic()



plot.rich_coef <- coef(plot.rich.g)

plot.rich_coef2 <-  bind_cols(plot.rich_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(plot.rich_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'year_trt'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK:year_trt'] %>% 
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
             by = 'site_code') %>% left_join(start.rich, by="site_code")



colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.rich_coef2)[colnames(plot.rich_coef2)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"


dat<-distinct(plot, site_code, continent,habitat)
plot.rich_coef3<-full_join(plot.rich_coef2,dat)

View(plot.rich_coef3)



levels(delta.coefs$starting.richness)
plot.rich_coef3$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sat.fig<-ggplot(data=plot.rich_coef3, aes(x= TESlope, y= starting.richness,color=starting.richness)) +
  #facet_wrap(~site_code)+
  geom_point(size=2) +
  #geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  geom_errorbarh(aes(xmin =  TESlope_lower, xmax = TESlope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  labs(x = 'Effect of NPK on Change in Species / Year',
       y = 'Starting Richness',
       title= '', color= "Starting Richness") +
  geom_vline(xintercept = 0,  alpha=0.8) + geom_hline(yintercept = 0 ,  alpha=0.8) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))
sat.fig


#LOG


plot$log.rich<-log(plot$rich)

View(plot)

plot.rich_coef.log <- coef(plot.rich.l)

plot.rich_coef2.log <-  bind_cols(plot.rich_coef$site_code[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site_code = rownames(plot.rich_coef$site_code[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'year_trt'] %>% 
                                as_tibble() %>% 
                                mutate(ISlope = Estimate,
                                       ISlope_lower = Q2.5,
                                       ISlope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK'] %>% 
                                as_tibble() %>% 
                                mutate(TE = Estimate,
                                       TE_lower = Q2.5,
                                       TE_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              plot.rich_coef$site_code[,,'trtNPK:year_trt'] %>% 
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
             by = 'site_code') %>% left_join(start.rich, by="site_code")



colnames(plot.rich_coef2.log)[colnames(plot.rich_coef2.log)=="Estimate.trtNPK:year_trt"] <- "Estimate.trtNPK.year_trt"

colnames(plot.rich_coef2.log)[colnames(plot.rich_coef2.log)=="Q2.5.trtNPK:year_trt"] <- "Q2.5.trtNPK.year_trt"
colnames(plot.rich_coef2.log)[colnames(plot.rich_coef2.log)=="Q97.5.trtNPK:year_trt"] <- "Q97.5.trtNPK.year_trt"


dat<-distinct(plot, site_code, continent,habitat)
plot.rich_coef3.log<-full_join(plot.rich_coef2.log,dat)

View(plot.rich_coef3.log)



levels(delta.coefs.log$starting.richness)
plot.rich_coef3.log$starting.richness <- factor(plot.rich_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sat.log.fig<-ggplot(data=plot.rich_coef3.log, aes(x= TESlope, y= starting.richness,color=starting.richness)) +
  #facet_wrap(~site_code)+
  geom_point(size=2) +
  #geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  geom_errorbarh(aes(xmin =  TESlope_lower, xmax = TESlope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  labs(x = 'Effect of NPK on Change in [log(Species)] / Year',
       y = 'Starting Richness',
       title= '', color= "Starting Richness") +
  geom_vline(xintercept = 0,  alpha=0.8) + geom_hline(yintercept = 0 ,  alpha=0.8) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))
sat.log.fig

(sat.fig + sat.log.fig)








load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')


plot.rich_coef3
plot.bm_coef3


View(plot.rich_coef3)
View(plot.bm_coef3)
colnames(plot.rich_coef3)
colnames(plot.bm_coef3)
plot.rich_coef4<-plot.rich_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17)]
plot.bm_coef4<-plot.bm_coef3[,c(-1,-2,-3,-8,-9,-10,-14,-15,-16,-17)]
colnames(plot.rich_coef4)
View(plot.bm_coef4)

names(plot.rich_coef4) <- c("site_code","IR.Slope","IR.Slope_lower","IR.Slope_upper","R.Slope","R.Slope_lower","R.Slope_upper","average richness","starting.richness","continent","habitat")
names(plot.bm_coef4) <- c("site_code","IB.Slope","IB.Slope_lower","IB.Slope_upper","B.Slope","B.Slope_lower","B.Slope_upper","average richness","starting.richness","continent","habitat")
#plot.bm_coef5<-plot.bm_coef4[complete.cases(plot.bm_coef4$B.Slope),]
delta.coefs<-left_join(plot.rich_coef4,plot.bm_coef4)
View(delta.coefs)


plot.rich_fixef<-as.data.frame(plot.rich_fixef)
plot.bm_fixef<-as.data.frame(plot.bm_fixef)
plot.rich_fixef$Model<-'Richness'
plot.bm_fixef$Model<-'Biomass'
fixef.all<-bind_rows(plot.rich_fixef,plot.bm_fixef)
View(fixef.all)


# DELTA QUDRANT PLOT Model
# GROUP VARIATION
levels(delta.coefs$starting.richness)
delta.coefs$starting.richness <- factor(delta.coefs$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

rich.fig<-ggplot() +
  #facet_wrap(~site_code)+
  geom_point(data=delta.coefs, aes(x= R.Slope, y= starting.richness,color=starting.richness),size=2) +
  geom_errorbarh(data=delta.coefs, aes( y= starting.richness, xmin =  R.Slope_lower, xmax = R.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(data = filter(fixef.all, Model=='Richness'),
             aes(xintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixef.all, Model=='Richness'),
            aes(ymin = -Inf, ymax = Inf,
                xmin = Q2.5[2], xmax = Q97.5[2]),
            alpha = 0.2) +
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
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.title = element_text(size=12))
rich.fig


b.fig<-ggplot() +
  #facet_wrap(~site_code)+
  geom_point(data=delta.coefs, aes(x= B.Slope, y= starting.richness,color=starting.richness),size=2) +
  #geom_errorbar(aes(ymin = B.Slope_lower, ymax = B.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  geom_errorbarh(data=delta.coefs,aes(y= starting.richness,xmin =  B.Slope_lower, xmax = B.Slope_upper,colour = starting.richness), width = 0, size = 0.75,alpha=0.3) +
  geom_vline(data = filter(fixef.all, Model=='Biomass'),
             aes(xintercept = Estimate[2]), size = 1.2) +
  geom_rect(data = filter(fixef.all, Model=='Biomass'),
            aes(ymin = -Inf, ymax = Inf,
                xmin = Q2.5[2], xmax = Q97.5[2]),
            alpha = 0.2) +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  labs(x = expression(paste('Effect of NPK on Change in Biomass (g/' ,m^2, ') / Year')),
       y = 'Starting Richness',
       title= '', color= "Starting Richness") +
  geom_vline(xintercept = 0,  alpha=0.8) + geom_hline(yintercept = 0 ,  alpha=0.8) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))
b.fig

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

rlegend<-g_legend(b.fig)

(rich.fig + b.fig + theme(legend.position="none"))/(rlegend) +
  plot_layout(heights = c(10,3.5))



# overall affect line from coefficient plots



