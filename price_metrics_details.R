
library(priceTools)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

# price data from Nut Net
# In this price pairwise comparison every plot is compared to itself through time
# so plot 1 is compared to itself from year 0 to year 1, year 0 to year 2, year 0 to year 3...etc
# i refer to this as cumulative time
# the 'baseline' plot is the starting point always referred to as x, and comparioson plot is y
# so x.func is year 0 biomass, and y.func is year 1 biomass etc, all columns labelled as such
price <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

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
  labs(title= 'c) Species Loss') +
  ylim(-10,2)+
  theme_bw() + theme(axis.text.x=element_blank()) 
s.loss

s.gain<-ggplot(price, aes(x=trt.y, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(title= 'd) Species Gains') +
  ylim(-2,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 
s.gain

SL<-ggplot(price, aes(x=trt.y, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(title= 'e) EF : Species Loss') +
  ylim(-300,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 
SL
SG<-ggplot(price, aes(x=trt.y, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'f) EF: Species Gains') +
  ylim(-10,300)+
  theme_bw() + theme(axis.text.x=element_blank()) 
SG
CDE<-ggplot(price, aes(x=trt.y, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'g) Context Dependent Effect') +
  ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 
CDE

rich<-ggplot(price, aes(x=trt.y, y=y.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(title= 'a) Richness') +
  ylim(0,40)+
  theme_bw() + theme(axis.text.x=element_blank()) 
rich
bm<-ggplot(price, aes(x=trt.y, y=y.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'b) Live Biomass') +
  ylim(0,2000)+
  theme_bw() + theme(axis.text.x=element_blank()) 
bm

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,nrow=3,ncol=3)


#raw plots across years for npk only
dat2$f.year_trt<-as.factor(as.character(dat2$year.y))
levels(dat2$f.year_trt)
dat2$f.year_trt <- factor(dat2$f.year_trt, levels = c("0","1","2","3","4","5","6","7","8","9","10","11"))

rich<-ggplot(dat2, aes(x=f.year_trt, y=y.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'a) Richness') +
  ylim(0,40)+
  theme_bw() + theme(axis.text.x=element_blank()) 

bm<-ggplot(dat2, aes(x=f.year_trt, y=y.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'b) Live Biomass') +
  ylim(0,2000)+
  theme_bw() + theme(axis.text.x=element_blank()) 


s.loss<-ggplot(dat2, aes(x=f.year_trt, y=s.loss, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'c) Species Loss') +
  ylim(-10,2)+
  theme_bw() + theme(axis.text.x=element_blank()) 
s.loss

s.gain<-ggplot(dat2, aes(x=f.year_trt, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'd) Species Gains') +
  ylim(-2,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SL<-ggplot(dat2, aes(x=f.year_trt, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'e) EF : Species Loss') +
  ylim(-300,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SG<-ggplot(dat2, aes(x=f.year_trt, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'f) EF: Species Gains') +
  ylim(-10,300)+
  theme_bw() + theme(axis.text.x=element_blank()) 

c.rich<-ggplot(dat2, aes(x=f.year_trt, y=c.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'g) Persistent Species') +
  # ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

CDE<-ggplot(dat2, aes(x=f.year_trt, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'g) Biomass Change in Persistent Species') +
  ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 


grid_arrange_shared_legend(rich,bm,nrow=1,ncol=2)

grid_arrange_shared_legend(s.loss,s.gain,SL,SG,CDE,nrow=2,ncol=3)

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,nrow=3,ncol=3)



# Below is the code for Colin's CAFE vector plots
# CAFE component    richness    function
# base = c(x.rich,x.func)
# SL = c(c.rich,SL)
# SG = c(y.rich,SL+SG)
# CDE = c(y.rich,y.func)

# cols <- c(group.vars,'x.func','SL','SG','y.func','x.rich','c.rich','y.rich')
# p2 <- reshape2::melt(data[,cols],id.vars=c(group.vars,'x.rich','c.rich','y.rich'))
# 
# # add richness column:
# p2$rich <- ifelse(p2$variable == "x.func", p2$x.rich,
#                   ifelse(p2$variable == "SL", p2$c.rich,
#                          ifelse(p2$variable == "SG", p2$y.rich,
#                                 ifelse(p2$variable == "y.func", p2$y.rich, NA))))
# 
# # summarize raw points:
# if(!is.null(group.vars)){
#   p3b <- p2 %>% group_by_(.dots=c(group.vars,'variable')) %>% summarise(mean.y=mean(value),
#                                                                         y.qt.lw=quantile(value, probs=0.025),
#                                                                         y.qt.up=quantile(value, probs=0.975),
#                                                                         mean.x=mean(rich),
#                                                                         x.qt.lw=quantile(rich, probs=0.025),
#                                                                         x.qt.up=quantile(rich, probs=0.975))
# }else{
#   p3b <- p2 %>% group_by(variable) %>% summarise(mean.y=mean(value),
#                                                  y.qt.lw=quantile(value, probs=0.025),
#                                                  y.qt.up=quantile(value, probs=0.975),
#                                                  mean.x=mean(rich),
#                                                  x.qt.lw=quantile(rich, probs=0.025),
#                                                  x.qt.up=quantile(rich, probs=0.975))
# }
# 
# p4 <- p3b
# 
# 
# # Organize factor levels for plotting:
# p2$variable <- factor(p2$variable,levels=c("x.func","SL","SG","y.func"),
#                       labels=c("baseline","SL","SG","comparison"))
# p3b$variable <- factor(p3b$variable,levels=c("x.func","SL","SG","y.func"),
#                        labels=c("baseline","SL","SG","comparison"))
# 
# # updated code:
# p4$variable <- as.character(p4$variable)
# p4$variable <- ifelse(p4$variable=="y.func","SG",p4$variable)
# p4$variable <- factor(p4$variable,levels=c("x.func","SL","SG"),
#                       labels=c("SL vector","SG vector","CDE vector"))
# 
# p2$variable <- factor(p2$variable, levels=c("baseline","SL","SG","comparison",
#                                             "SL vector","SG vector","CDE vector"))
# p3b$variable <- factor(p3b$variable, levels=c("baseline","SL","SG","comparison",
#                                               "SL vector","SG vector","CDE vector"))
# p4$variable <- factor(p4$variable, levels=c("baseline","SL","SG","comparison",
#                                             "SL vector","SG vector","CDE vector"))
# 
# p3b <- p3b[p3b$variable != "baseline",]
# 
# return(list(p2, p3b, p4))
# }

