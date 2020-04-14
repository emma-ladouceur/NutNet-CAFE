# Nutrient Network Descriptive Statistics for website and general use
# Author: Emma Ladouceur
# Email: emmala@gmail.com


library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(priceTools)


nn <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

nn <- droplevels( nn[-which(nn$year.zero.only == "1"), ] )
nn <- droplevels( nn[-which(nn$no.year.zero == "1"), ] )

# SOME DATA WRANGLING FIRST
# create a new dataset of some basics from comb-by-plot

# get a list of unique sites, continent, habitat, year_trt year and experiment type
colnames(nn)
unique.sites <- unique(nn[c("site_code","country","continent","habitat","year_trt","year","experiment_type")])
# add a column to count the number of sites
unique.sites$site_n<-"1"
#write to a new file
View(unique.sites)
write.csv(unique.sites, "~/Dropbox/Projects/NutNet/Data/basics_1.csv", row.names=F)

# and a second dataset that summarizes sites, by only their max year
nn_agg <- read.csv("~/Dropbox/Projects/NutNet/Data/basics_1.csv", sep=",",header=T, strip.white=T)
# take only the max year of every site
nn_agg_2<-nn_agg %>% group_by(site_code) %>% top_n(1, year_trt)
View(nn_agg_2)
??group_by
write.csv(nn_agg_2, "~/Dropbox/Projects/NutNet/Data/basics_2.csv", row.names=F)



nnr <- read.csv("~/Dropbox/Projects/NutNet/Data/basics_2.csv", sep=",",header=T, strip.white=T)
nnr2 <- droplevels(subset(nnr, experiment_type == "Experimental (Full Factorial)"| experiment_type == "Experimental (Nutrients Only)"))

colnames(nnr2)
nnr2$year_trt<-as.factor(nnr2$year_trt)
nnr3<- nnr2[nnr2$year_trt != "0" ,]
nnr4<- nnr3[nnr3$year_trt != "1" ,]
levels(nnr3$year_trt)
View(nnr4)

write.csv(nnr4, "~/Dropbox/Projects/NutNet/Data/basics_3.csv", row.names=F)






#I LIKE THIS ONE

nnr <- read.csv("~/Dropbox/Projects/NutNet/Data/basics_2.csv", sep=",",header=T, strip.white=T)
nnr2 <- droplevels(subset(nnr, experiment_type == "Experimental (Full Factorial)" | experiment_type == "Experimental (Nutrients Only)"))

ggplot(data = nnr2, aes(x = continent, y = site_n, color=habitat)) +scale_x_discrete(limits = rev(levels(nnr2$continent)))+
  geom_col(fill = "white") +
  geom_text(aes(label = year_trt),
            position = position_stack(vjust = .5))+coord_flip()+
  labs(x = 'Continents',
       y = 'Number of Sites', title= 'The Number of Experimental Sites per Continent in Nutrient Network') +
  #facet_grid(experiment_type ~ .)
  theme_classic()+ theme(legend.position="bottom")


colnames(nnr2)
#and this
nnr2$year_trt<-as.factor(as.character(nnr2$year_trt))
nnr2$year_trt<- factor(nnr2$year_trt, levels=c("0","1","2","3","4","5","6","7","8","9","10","11"))

ggplot(data = nnr2, aes(x = year_trt, y = site_n, color=continent)) +
  geom_col(fill = "white") +
   geom_text(aes(label = site_code),
             position = position_stack(vjust = .5))+ 
  labs(x = 'Number of Years',
       y = 'Number of Sites', title= 'The Number of Experimental Sites ') +
  #facet_grid(experiment_type ~ .)
  theme_classic()+ theme(legend.position="bottom")



# price plots raw
price <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only3.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



summary(price)
head(price)

price<-price[complete.cases(price$SG),]
price<-price[complete.cases(price$SL),]
dat1<-price[price$trt.xy %in% c('Control_Control'),] # control
dat2<-price[price$trt.xy %in% c('NPK_NPK'),] # npk treatments

# heres a quick lil plot
p1 <- leap.zig(dat1,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Control')+theme_classic()
p2 <- leap.zig(dat2,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('NPK')+theme_classic()
p3 <- leap.zig(dat1,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = T)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Control')+theme_classic()
p4 <- leap.zig(dat2,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = T)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('NPK')+theme_classic()

grid_arrange_shared_legend(p1,p2,p3,p4,ncol=2,nrow=2)

View(price)
# Heres colins summary statistics func
test.partitions(price,type='cafe',treat.var = 'trt.xy',control = 'Control_Control',print=F,plot=T)
# this  uses raw species gains and losses as a metric

# Here's species gains and losses are calculated
# p.dat2$s.loss <- -1*(p.dat2$x.rich - p.dat2$c.rich)
# p.dat2$s.gain <- p.dat2$y.rich - p.dat2$c.rich
# p.dat2$s.change <- p.dat2$y.rich - p.dat2$x.rich

# some raw plots
s.loss<-ggplot(price, aes(x=trt.y, y=s.loss, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  geom_point(aes(color=trt.y), alpha=0.1,size = .7, position = position_jitter(width = 0.2))+
  labs(title= 'c) Species Loss',
       y = 'Species Loss',
       x="",color="Treatment"
       ) +
  ylim(-10,2)+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",  plot.title = element_text(size=12)) 
s.loss

s.gain<-ggplot(price, aes(x=trt.y, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  geom_point(aes(color=trt.y), alpha=0.1,size = .7, position = position_jitter(width = 0.2))+
  labs(title= 'd) Species Gains',
       y = 'Species Gains',
       x="",color="Treatment") +
  ylim(-2,10)+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",  plot.title = element_text(size=12)) 
s.gain

SL<-ggplot(price, aes(x=trt.y, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  geom_point(aes(color=trt.y), alpha=0.1,size = .7, position = position_jitter(width = 0.2))+
  labs(title= 'e) Effect of Species Loss on Biomass',
       y = expression(paste('Biomass Change (g/',m^2, ')')),
       x="",color="Treatment") +
  ylim(-300,10)+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",   plot.title = element_text(size=12)) 
SL

SG<-ggplot(price, aes(x=trt.y, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  geom_point(aes(color=trt.y), alpha=0.1,size = .7, position = position_jitter(width = 0.2))+
  labs( title= 'f) Effect of Species Gains on Biomass',
        y = expression(paste('Biomass Change (g/',m^2, ')')),
        x="",color="Treatment") +
  ylim(-10,300)+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="bottom",  plot.title = element_text(size=12)) 
SG
CDE<-ggplot(price, aes(x=trt.y, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  geom_point(aes(color=trt.y), alpha=0.1,size = .7, position = position_jitter(width = 0.2))+
  labs( title= 'g) Persistent Species Change in Biomass',
        y = expression(paste('Biomass Change (g/',m^2, ')')),
        x="",color="Treatment") +
  ylim(-300,1000)+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",   plot.title = element_text(size=12)) 
CDE

rich<-ggplot(price, aes(x=trt.y, y=y.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  geom_point(aes(color=trt.y), alpha=0.1,size = .7, position = position_jitter(width = 0.2))+
  labs(title= 'a) Species Richness',
       y='Species Richness', x='',color="Treatment") +
  ylim(0,40)+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none", plot.title = element_text(size=12)) 
rich
bm<-ggplot(price, aes(x=trt.y, y=y.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  geom_point(aes(color=trt.y), alpha=0.1,size = .7, position = position_jitter(width = 0.2))+
  labs( title= 'b) Live Biomass',
        y = expression(paste('Biomass (g/',m^2, ')')),
        x="",color="Treatment") +
  ylim(0,2000)+
  scale_color_manual(values = c("#00AFBB","#E7B800"))+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",     plot.title = element_text(size=12)) 
bm

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,nrow=3,ncol=3)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

rlegend<-g_legend(SG)



# patchwork solution
library(patchwork)

(rich | bm)/(s.loss|s.gain)/(SL|SG+ theme(legend.position="none")|CDE )/(rlegend) +
  plot_layout(heights = c(10, 10,10,3.5))


#raw plots across years for npk only
dat2$f.year_trt<-as.factor(as.character(dat2$year.y))
levels(dat2$f.year_trt)
dat2$f.year_trt <- factor(dat2$f.year_trt, levels = c("0","1","2","3","4","5","6","7","8","9","10","11"))


View(dat2)
rich<-ggplot(dat2, aes(x=f.year_trt, y=y.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'a) Richness',
       y = 'Species Richness',
       x="",color="Year") +
  ylim(0,40)+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",     plot.title = element_text(size=12)) 


bm<-ggplot(dat2, aes(x=f.year_trt, y=y.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'b) Biomass',
        y = expression(paste('Biomass (g/',m^2, ')')),
        x="",color="Year") +
  ylim(0,2000)+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",     plot.title = element_text(size=12)) 


s.loss<-ggplot(dat2, aes(x=f.year_trt, y=s.loss, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'c) Species Loss',
        y = 'Species Loss',
        x="",color="Year") +
  ylim(-10,2)+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",     plot.title = element_text(size=12)) 
s.loss

s.gain<-ggplot(dat2, aes(x=f.year_trt, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'd) Species Gains',
        y = 'Species Gain',
        x="",color="Year") +
  ylim(-2,10)+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",     plot.title = element_text(size=12)) 

SL<-ggplot(dat2, aes(x=f.year_trt, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'e) Effect of Species Loss on Biomass',
       y = expression(paste('Biomass Change (g/',m^2, ')')),
       x="",color="Year") +
  ylim(-300,10)+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",     plot.title = element_text(size=12)) 

SG<-ggplot(dat2, aes(x=f.year_trt, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'f) Effect of Species Gains on Biomass',
       y = expression(paste('Biomass Change (g/',m^2, ')')),
       x="",color="Year") +
  ylim(-10,300)+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="bottom",     plot.title = element_text(size=12)) 


CDE<-ggplot(dat2, aes(x=f.year_trt, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'g) Biomass Change in Persistent Species',
        y = expression(paste('Biomass Change (g/',m^2, ')')),
        x="",color="Year") +
  ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),
                     legend.position="none",     plot.title = element_text(size=12)) 



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

rlegend<-g_legend(SG)


(rich | bm)/(s.loss|s.gain)/(SL|SG+ theme(legend.position="none")|CDE )/(rlegend) +
  plot_layout(heights = c(10, 10,10,3.5))

