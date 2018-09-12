library(gridExtra)
library(grid)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyr)

library(priceTools)
library(dplyr)



# TIDYVERSE SHARED LEGEND
#https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

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


#GLOBAL WORLDWIDE OVERALL
setwd("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/")
#PLOT LEVEL
pp3 <- read.csv("delta_plot_bind_year3_2.csv", sep=",",header=T, strip.white=T)
#SITE LEVEL
pp3 <- read.csv("site_bind_year3_2.csv", sep=",",header=T, strip.white=T)
levels(pp3$trt_year)

dat1<-pp3[pp3$trt_year %in% c('NPK_0 NPK_3'),]
dat2<-pp3[pp3$trt_year %in% c('Control_3 NPK_3'),]
dat3<-pp3[pp3$trt_year %in% c('Control_0 Control_3'),]

#all sp
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('NPK 0 NPK 3')+theme_classic()
p2 <- leap.zig(dat2,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Control 3 NPK 3')+theme_classic()
p3 <- leap.zig(dat3,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Control 0 Control 3')+theme_classic()

grid_arrange_shared_legend(p1,p2,p3,ncol=3,nrow=1)


test.partitions(pp3,type='price',treat.var = 'trt_year',control = 'Control_0 Control_3',print=F,plot=T)



#SITE VS PLOT
setwd("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/")
#PLOT LEVEL
plot <- read.csv("delta_plot_bind_year3_2.csv", sep=",",header=T, strip.white=T)
#SITE LEVEL
site <- read.csv("site_bind_year3_2.csv", sep=",",header=T, strip.white=T)
levels(plot$trt_year)

p<-plot[plot$trt_year %in% c('NPK_0 NPK_3'),]
s<-site[site$trt_year %in% c('NPK_0 NPK_3'),]


#all sp by site vs plot
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(p,type='cafe',xlim=c(0,22),ylim=c(0,1700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(p$x.rich), y = mean(p$x.func), 
           label = "*",size=8)+ggtitle('Plot: NPK 0 NPK 3')+theme_classic()
p2 <- leap.zig(s,type='cafe',xlim=c(0,22),ylim=c(0,1700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(s$x.rich), y = mean(s$x.func), 
           label = "*",size=8)+ggtitle('Site:NPK 0 NPK 3')+theme_classic()

grid_arrange_shared_legend(p1,p2,ncol=2,nrow=1)

pp<-plot[plot$trt_year %in% c('NPK_0 NPK_3','Control_0 Control_3','Control_0 NPK_3'),]
ss<-site[site$trt_year %in% c('NPK_0 NPK_3','Control_0 Control_3','Control_0 NPK_3'),]
colnames(pp)
pp %>% distinct(site_name,trt_year)


test.partitions(plot,type='price',treat.var = 'trt_year',control = 'Control_0 Control_3',print=F,plot=T)

test.partitions(site,type='price',treat.var = 'trt_year',control = 'Control_0 Control_3',print=F,plot=T)


# species richness and biomass plot level
p<-plot[plot$trt_year %in% c('Control_0 Control_3','NPK_0 NPK_3'),]
s<-site[site$trt_year %in% c('Control_0 Control_3', 'NPK_0 NPK_3'),]

View(p)
View(s)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
pr <- ggplot(p, aes(x = trt_year, y = y.rich))+ 
  geom_boxplot(aes(fill = trt_year))+theme_classic()
pp

sr <- ggplot(s, aes(x = trt_year, y = y.rich))+
  geom_boxplot(aes(fill = trt_year))+theme_classic()
ss

pb <- ggplot(p, aes(x = trt_year, y = y.func))+ 
  geom_boxplot(aes(fill = trt_year))+theme_classic()
pb

sb <- ggplot(s, aes(x = trt_year, y = y.func))+
  geom_boxplot(aes(fill = trt_year))+theme_classic()
sb

grid_arrange_shared_legend(sr,sb,ncol=2,nrow=1)



# MATRIX OF EVERY SITE
#PLOT LEVEL
pp1 <- read.csv("delta_plot_bind_year3_2.csv", sep=",",row.names=1,header=T, strip.white=T)
#SITE LEVEL
pp1 <- read.csv("site_bind_year3_2.csv", sep=",",header=T, strip.white=T)

#SUBSETS
dat1<-pp1[pp1$trt_year %in% c('Control_3 NPK_3'),]
#NOT ALL SITES HAVE A YEAR 0, SO RUN 3-3 FIRST, AND THOSE ONES WILL FILL GAPS OF MISSING YEAR 0
dat1<-pp1[pp1$trt_year %in% c('NPK_0 NPK_3'),]
dat1<-pp1[pp1$trt_year %in% c('NPK_0 NPK_3','Control_3 NPK_3'),]

dat2<-dat1[dat1$price %in% c('Bogong Bogong'),]
dat3<-dat1[dat1$price %in% c('Boulder South Campus Boulder South Campus'),]
dat4<-dat1[dat1$price %in% c('Bunchgrass (Andrews LTER) Bunchgrass (Andrews LTER)'),]
dat5<-dat1[dat1$price %in% c('Burrawan Burrawan'),]
dat6<-dat1[dat1$price %in% c('Cedar Creek LTER Cedar Creek LTER'),]
dat7<-dat1[dat1$price %in% c('Cedar Point Biological Station Cedar Point Biological Station'),]
dat8<-dat1[dat1$price %in% c('CEREEP - Ecotron IDF CEREEP - Ecotron IDF'),]
dat9<-dat1[dat1$price %in% c('Chichaqua Bottoms Chichaqua Bottoms'),]
dat10<-dat1[dat1$price %in% c('Companhia das Lezirias Companhia das Lezirias'),]
dat11<-dat1[dat1$price %in% c('Cowichan Cowichan'),]
dat12<-dat1[dat1$price %in% c('Duke Forest Duke Forest'),]
dat13<-dat1[dat1$price %in% c('Elliott Chaparral Elliott Chaparral'),]
dat14<-dat1[dat1$price %in% c('Fruebuel Fruebuel'),]
dat15<-dat1[dat1$price %in% c('Halls Prairie Halls Prairie'),]
dat16<-dat1[dat1$price %in% c('Hanover Hanover'),]
dat17<-dat1[dat1$price %in% c('Hart Mountain Hart Mountain'),]
dat18<-dat1[dat1$price %in% c('Heronsbrook (Silwood Park) Heronsbrook (Silwood Park)'),]
dat19<-dat1[dat1$price %in% c('Hopland REC Hopland REC'),]
dat20<-dat1[dat1$price %in% c('Kibber (Spiti) Kibber (Spiti)'),]
dat21<-dat1[dat1$price %in% c('Kinypanial Kinypanial'),]
dat22<-dat1[dat1$price %in% c('Koffler Scientific Reserve at Jokers Hill Koffler Scientific Reserve at Jokers Hill'),]
dat23<-dat1[dat1$price %in% c('Konza LTER Konza LTER'),]
dat24<-dat1[dat1$price %in% c('Lancaster Lancaster'),]
dat25<-dat1[dat1$price %in% c('Las Chilcas Las Chilcas'),]
dat26<-dat1[dat1$price %in% c('Lookout (Andrews LTER) Lookout (Andrews LTER)'),]
dat27<-dat1[dat1$price %in% c('Mar Chiquita Mar Chiquita'),]
dat28<-dat1[dat1$price %in% c('Mclaughlin UCNRS Mclaughlin UCNRS'),]
dat29<-dat1[dat1$price %in% c('Mt Gilboa Mt Gilboa'),]
dat30<-dat1[dat1$price %in% c('Mt. Caroline Mt. Caroline'),]
dat31<-dat1[dat1$price %in% c('Papenburg Papenburg'),]
dat32<-dat1[dat1$price %in% c('Pingelly Paddock Pingelly Paddock'),]
dat33<-dat1[dat1$price %in% c('Pinjarra Hills Pinjarra Hills'),]
dat34<-dat1[dat1$price %in% c('Rookery (Silwood Park) Rookery (Silwood Park)'),]
dat35<-dat1[dat1$price %in% c('Saana Saana'),]
dat36<-dat1[dat1$price %in% c('Sagehen Creek UCNRS Sagehen Creek UCNRS'),]
dat37<-dat1[dat1$price %in% c('Saline Experimental Range Saline Experimental Range'),]
dat38<-dat1[dat1$price %in% c('Savannah River Savannah River'),]
dat39<-dat1[dat1$price %in% c('Sedgwick Reserve UCNRS Sedgwick Reserve UCNRS'),]
dat40<-dat1[dat1$price %in% c('Serengeti Serengeti'),]
dat41<-dat1[dat1$price %in% c('Sevilleta LTER Sevilleta LTER'),]
dat42<-dat1[dat1$price %in% c('Sheep Experimental Station Sheep Experimental Station'),]
dat43<-dat1[dat1$price %in% c('Shortgrass Steppe LTER Shortgrass Steppe LTER'),]
dat44<-dat1[dat1$price %in% c('Sierra Foothills REC Sierra Foothills REC'),]
dat45<-dat1[dat1$price %in% c('Smith Prairie Smith Prairie'),]
dat46<-dat1[dat1$price %in% c('Spindletop Spindletop'),]
dat47<-dat1[dat1$price %in% c('Summerveld Summerveld'),]
dat48<-dat1[dat1$price %in% c('Temple Temple'),]
dat49<-dat1[dat1$price %in% c('Trelease Trelease'),]
dat50<-dat1[dat1$price %in% c('Ukulinga Ukulinga'),]
dat51<-dat1[dat1$price %in% c('Val Mustair Val Mustair'),]

#YEAR 3

theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat2,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p2 <- leap.zig(dat3,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Boulder South Campus')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p3 <- leap.zig(dat4,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Bunchgrass (Andrews LTER)')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p4 <- leap.zig(dat5,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p5 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Cedar Creek LTER')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p6 <- leap.zig(dat7,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat7$x.rich), y = mean(dat7$x.func), 
           label = "*",size=8)+ggtitle('Cedar Point Biological Station')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p7 <- leap.zig(dat8,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat8$x.rich), y = mean(dat8$x.func), 
           label = "*",size=8)+ggtitle('CEREEP - Ecotron IDF')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p8 <- leap.zig(dat9,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat9$x.rich), y = mean(dat9$x.func), 
           label = "*",size=8)+ggtitle('Chichaqua Bottoms')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p9 <- leap.zig(dat10,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat10$x.rich), y = mean(dat10$x.func), 
           label = "*",size=8)+ggtitle('Companhia das Lezirias')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p10 <- leap.zig(dat11,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat11$x.rich), y = mean(dat11$x.func), 
           label = "*",size=8)+ggtitle('Cowichan')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())

p11 <- leap.zig(dat12,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat12$x.rich), y = mean(dat12$x.func), 
           label = "*",size=8)+ggtitle('Duke Forest')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p12 <- leap.zig(dat13,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat13$x.rich), y = mean(dat13$x.func), 
           label = "*",size=8)+ggtitle('*Elliott Chaparral')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p13 <- leap.zig(dat14,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat14$x.rich), y = mean(dat14$x.func), 
           label = "*",size=8)+ggtitle('Fruebuel')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p14 <- leap.zig(dat15,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat15$x.rich), y = mean(dat15$x.func), 
           label = "*",size=8)+ggtitle('Halls Prairie')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p15 <- leap.zig(dat16,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat16$x.rich), y = mean(dat16$x.func), 
           label = "*",size=8)+ggtitle('Hanover')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p16 <- leap.zig(dat17,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat17$x.rich), y = mean(dat17$x.func), 
           label = "*",size=8)+ggtitle('Hart Mountain')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p17 <- leap.zig(dat18,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat18$x.rich), y = mean(dat18$x.func), 
           label = "*",size=8)+ggtitle('*Heronsbrook (Silwood Park)')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p18 <- leap.zig(dat19,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat19$x.rich), y = mean(dat19$x.func), 
           label = "*",size=8)+ggtitle('Hopland REC')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p19 <- leap.zig(dat20,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat20$x.rich), y = mean(dat20$x.func), 
           label = "*",size=8)+ggtitle('Kibber (Spiti)')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p20 <- leap.zig(dat21,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat21$x.rich), y = mean(dat21$x.func), 
           label = "*",size=8)+ggtitle('*Kinypanial')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())

p21 <- leap.zig(dat22,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat22$x.rich), y = mean(dat22$x.func), 
           label = "*",size=8)+ggtitle('Koffler Scientific Reserve at Jokers Hill')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p22 <- leap.zig(dat23,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat23$x.rich), y = mean(dat23$x.func), 
           label = "*",size=8)+ggtitle('Konza LTER')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p23 <- leap.zig(dat24,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat24$x.rich), y = mean(dat24$x.func), 
           label = "*",size=8)+ggtitle('Lancaster')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p24 <- leap.zig(dat25,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat25$x.rich), y = mean(dat25$x.func), 
           label = "*",size=8)+ggtitle('Las Chilcas')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p25 <- leap.zig(dat26,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat26$x.rich), y = mean(dat26$x.func), 
           label = "*",size=8)+ggtitle('Lookout (Andrews LTER)')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p26 <- leap.zig(dat27,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat27$x.rich), y = mean(dat27$x.func), 
           label = "*",size=8)+ggtitle('Mar Chiquita')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p27 <- leap.zig(dat28,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat28$x.rich), y = mean(dat28$x.func), 
           label = "*",size=8)+ggtitle('Mclaughlin UCNRS')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p28 <- leap.zig(dat29,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat29$x.rich), y = mean(dat29$x.func), 
           label = "*",size=8)+ggtitle('Mt Gilboa')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p29 <- leap.zig(dat30,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat30$x.rich), y = mean(dat30$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p30 <- leap.zig(dat31,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat31$x.rich), y = mean(dat31$x.func), 
           label = "*",size=8)+ggtitle('Papenburg')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())

p31 <- leap.zig(dat32,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat32$x.rich), y = mean(dat32$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p32 <- leap.zig(dat33,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat33$x.rich), y = mean(dat33$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p33 <- leap.zig(dat34,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat34$x.rich), y = mean(dat34$x.func), 
           label = "*",size=8)+ggtitle('*Rookery (Silwood Park)')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p34 <- leap.zig(dat35,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat35$x.rich), y = mean(dat35$x.func), 
           label = "*",size=8)+ggtitle('*Saana')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p35 <- leap.zig(dat36,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat36$x.rich), y = mean(dat36$x.func), 
           label = "*",size=8)+ggtitle('Sagehen Creek UCNRS')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p36 <- leap.zig(dat37,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat37$x.rich), y = mean(dat37$x.func), 
           label = "*",size=8)+ggtitle('*Saline Experimental Range')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p37 <- leap.zig(dat38,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat38$x.rich), y = mean(dat38$x.func), 
           label = "*",size=8)+ggtitle('Savannah River')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p38 <- leap.zig(dat39,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat39$x.rich), y = mean(dat39$x.func), 
           label = "*",size=8)+ggtitle('Sedgwick Reserve UCNRS')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p39 <- leap.zig(dat40,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat40$x.rich), y = mean(dat40$x.func), 
           label = "*",size=8)+ggtitle('Serengeti')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p40 <- leap.zig(dat41,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat41$x.rich), y = mean(dat41$x.func), 
           label = "*",size=8)+ggtitle('Sevilleta LTER')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())

p41 <- leap.zig(dat42,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat42$x.rich), y = mean(dat42$x.func), 
           label = "*",size=8)+ggtitle('Sheep Experimental Station')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p42 <- leap.zig(dat43,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat43$x.rich), y = mean(dat43$x.func), 
           label = "*",size=8)+ggtitle('Shortgrass Steppe LTER')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p43 <- leap.zig(dat44,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat44$x.rich), y = mean(dat44$x.func), 
           label = "*",size=8)+ggtitle('Sierra Foothills REC')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p44 <- leap.zig(dat45,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat45$x.rich), y = mean(dat45$x.func), 
           label = "*",size=8)+ggtitle('Smith Prairie')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p45 <- leap.zig(dat46,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat46$x.rich), y = mean(dat46$x.func), 
           label = "*",size=8)+ggtitle('Spindletop')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p46 <- leap.zig(dat47,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat47$x.rich), y = mean(dat47$x.func), 
           label = "*",size=8)+ggtitle('Summerveld')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p47 <- leap.zig(dat48,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat48$x.rich), y = mean(dat48$x.func), 
           label = "*",size=8)+ggtitle('Temple')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p48 <- leap.zig(dat49,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat49$x.rich), y = mean(dat49$x.func), 
           label = "*",size=8)+ggtitle('*Trelease')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p49 <- leap.zig(dat50,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat50$x.rich), y = mean(dat50$x.func), 
           label = "*",size=8)+ggtitle('Ukulinga')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())
p50 <- leap.zig(dat51,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat51$x.rich), y = mean(dat51$x.func), 
           label = "*",size=8)+ggtitle('Val Mustair')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank())

#PLOT USING GRID.ARRANGE WITH TIDYVERSE LEGEND FUNCTION
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,ncol=10,nrow=5)



??leap.zig


#  DELTA PLOT
pp1<- read.csv("delta_plot_bind_year3_2.csv", sep=",",header=T, strip.white=T)

pp1<-pp1[pp1$trt_year %in% c('NPK_0 NPK_3'),]
pp1<-pp1[pp1$trt_year %in% c('Control_3 NPK_3'),]
colnames(pp1)
View(pp1)


theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(pp1, aes(x=delta.rich, y=delta.func,color=continent)) +
  geom_point() +
  lims(x=c(-25,13),y=c(-1500,2000)) +
  theme_minimal() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +theme_classic()




# DELTA SITE
pp1<- read.csv("site_bind_year3_2.csv", sep=",",header=T, strip.white=T)
pp1<-pp1[pp1$trt_year %in% c('NPK_0 NPK_3'),]
pp1<-pp1[pp1$trt_year %in% c('Control_3 NPK_3'),]
colnames(pp1)
View(pp1)


theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ggplot(pp1, aes(x=delta.rich, y=delta.func,color=continent,label=site_name)) +
  geom_text() + 
  lims(x=c(-25,13),y=c(-1500,2000)) +
  theme_minimal() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +theme_classic()







#SCALING 101??? ASK PETR
library(scales)


pp1 <- read.csv("delta_plot_bind_year3_2.csv", sep=",",row.names=1,header=T, strip.white=T)

pp1<-pp1[pp1$trt_year %in% c('Control_3 NPK_3'),]
pp1<-pp1[pp1$continent %in% c('Australia'),]
dat1<-pp1[pp1$site_name %in% c('Bogong'),]
dat2<-pp1[pp1$site_name %in% c('Burrawan'),]
dat3<-pp1[pp1$site_name %in% c('Kinypanial'),]
dat4<-pp1[pp1$site_name %in% c('Mt. Caroline'),]
dat5<-pp1[pp1$site_name %in% c('Pingelly Paddock'),]
dat6<-pp1[pp1$site_name %in% c('Pinjarra Hills'),]
View(dat6)

#LOG10 
# Error in seq.default(min, max, by = by) : 'from' must be a finite number 
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_log10(limits = c(0, 30))+
  scale_y_log10(limits = c(0, 2200))+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_log10(limits = c(0, 30))+
  scale_y_log10(limits = c(0, 2200))+
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_log10(limits = c(0, 30))+
  scale_y_log10(limits = c(0, 2200))+
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_log10(limits = c(0, 30))+
  scale_y_log10(limits = c(0, 2200))+
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_log10(limits = c(0, 30))+
  scale_y_log10(limits = c(0, 2200))+
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  scale_x_log10(limits = c(0, 30))+
  scale_y_log10(limits = c(0, 2200))+
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()

grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

#LOG10 IN ANOTHER METHOD
# Error in seq.default(min, max, by = by) : 'from' must be a finite number 
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

#LOG10 IN ANOTHER METHOD WITH LIMITS
# Error in seq.default(min, max, by = by) : 'from' must be a finite number 
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log10")+
  scale_y_continuous(limits = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log10")+
  scale_y_continuous(limits = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log10")+
  scale_y_continuous(limits = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log10")+
  scale_y_continuous(limits = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log10")+
  scale_y_continuous(limits = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log10")+
  scale_y_continuous(limits = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

#LOG10 IN ANOTHER METHOD WITH BREAKS?
#  Error in seq.default(dots[[1L]][[1L]], dots[[2L]][[1L]], length.out = dots[[3L]][[1L]]) :'from' must be a finite number
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0, 30),trans="log10")+
  scale_y_continuous(breaks = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0, 30),trans="log10")+
  scale_y_continuous(breaks = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0, 30),trans="log10")+
  scale_y_continuous(breaks = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0, 30),trans="log10")+
  scale_y_continuous(breaks = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0, 30),trans="log10")+
  scale_y_continuous(breaks = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0, 30),trans="log10")+
  scale_y_continuous(breaks = c(0, 2200),trans="log10")+
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

#LOG 1P NORMAL
#ITS OK....BUT NOT SURE HOW ITS DIFFERENT FROM UNTRANSFORMED DATA
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous( trans="log1p")+
  scale_y_continuous( trans="log1p")+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous( trans="log1p")+
  scale_y_continuous( trans="log1p")+
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log1p")+
  scale_y_continuous(trans="log1p")+
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous( trans="log1p")+
  scale_y_continuous(trans="log1p")+
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(trans="log1p")+
  scale_y_continuous(trans="log1p")+
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous( trans="log1p")+
  scale_y_continuous(trans="log1p")+
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

#LOG 1P WITH LIMITS
#PLOTS BUT DOESNT STICK TO LIMITS
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30), trans="log1p")+
  scale_y_continuous(limits = c(0, 2200), trans="log1p")+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30), trans="log1p")+
  scale_y_continuous(limits = c(0, 2200), trans="log1p")+
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log1p")+
  scale_y_continuous(limits = c(0, 2200),trans="log1p")+
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30), trans="log1p")+
  scale_y_continuous(limits = c(0, 2200),trans="log1p")+
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30),trans="log1p")+
  scale_y_continuous(limits = c(0, 2200),trans="log1p")+
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(limits = c(0, 30), trans="log1p")+
  scale_y_continuous(limits = c(0, 2200),trans="log1p")+
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

#LOG 1P WITH BREAKS?
#WORKS BUT WEEEEIIIRD
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0,15, 30), trans="log1p")+
  scale_y_continuous(breaks = c(0,500,1200, 2200), trans="log1p")+
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0,15, 30), trans="log1p")+
  scale_y_continuous(breaks = c(0,500,1200, 2200), trans="log1p")+
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0,15, 30),trans="log1p")+
  scale_y_continuous(breaks = c(0,500,1200, 2200),trans="log1p")+
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0,15, 30), trans="log1p")+
  scale_y_continuous(breaks = c(0,500,1200,2200),trans="log1p")+
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0,15, 30),trans="log1p")+
  scale_y_continuous(breaks = c(0, 500,1200,2200),trans="log1p")+
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  scale_x_continuous(breaks = c(0,15, 30), trans="log1p")+
  scale_y_continuous(breaks = c(0,500,1200, 2200),trans="log1p")+
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)

#NORMAL UNSTANDARDIZED
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Bogong')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Burrawan')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Kinypanial')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Mt. Caroline')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Pingelly Paddock')+theme_classic()
p6 <- leap.zig(dat6,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Pinjarra Hills')+theme_classic()

grid_arrange_shared_legend(p1,p2,p3,p4,p5,p6,ncol=3,nrow=2)





