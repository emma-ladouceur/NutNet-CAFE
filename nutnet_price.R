
library(tidyr)
library(dplyr)

library(gridExtra)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyr)

library(priceTools)


# determine a nice age to maximise sites and age
# remove years in between
# remove NA's
# check with Stan: total biomass, or live biomass, or how did he do it?
# remove all non NPK treats
# run 0 + cpntrol -> NPK max year overall
# ask stan about yann's stability suggestion
#run it all by jenn




setwd("/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/")

nn <- read.csv("first_biomass_conversion2.csv", sep=",",row.names=1,header=T, strip.white=T)
colnames(nn)

#how many years since treatment?
nn$year_trt<-as.factor(nn$year_trt)
levels(nn$year_trt)
nn$year_trt<-as.numeric(nn$year_trt)


summary(nn)

nn1 <- nn[ !(nn$experiment_type %in% c("Observational")), ]
levels(nn1$trt)
nn2 <- nn1[ !(nn1$trt %in% c("Fence","K","N","NK","NP", "NPK+Fence","P","PK")), ]
View(nn2)


write.csv(nn2,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/reduced.csv")

nnr <- read.csv("yearssites.csv", sep=",",header=T, strip.white=T)


nnr$years<-as.factor(nnr$years)
nnr$sites<-as.numeric(nnr$sites)
f <- ggplot(nnr, aes(x = years, y = sites))
# Change bar plot line colors by groups
f + geom_bar(aes(color = years),
             stat="identity", fill="white")+theme_classic()

nrow(nnr)
summary(nnr)
nnr$year_trt<-as.factor(nnr$year_trt)
levels(nnr$year_trt)


nn3 <- read.csv("year3.csv", sep=",",row.names=1,header=T, strip.white=T)
nn5 <- read.csv("year5.csv", sep=",",row.names=1,header=T, strip.white=T)
colnames(nn3)

nn5_native<-nn5[nn5$local_provenance %in% c('NAT'),]
nn5_int<-nn5[nn5$local_provenance %in% c('INT'),]

group.vars <- c('site_name')
group.vars1 <- c('block')
group.vars2 <- c('plot')
treat.vars<-c('trt')
grouped.data1 <- nn3 %>% group_by_(.dots=c(group.vars,group.vars1,group.vars2,treat.vars))
View(grouped.data1)

#takes a long time
res1 <- pairwise.price(grouped.data1, species="Taxon", func="Biomass_CalcSp")
View(data.frame(res1))

#Visually
# Create a single column keeping track of the paired set of enrichment treatments & other grouping variables:
pp1<-res1
pp1<-group.columns(pp1,gps=c(group.vars,group.vars1,group.vars2,treat.vars),drop=T)
View(pp1)

# Subset pairwise results:
pp1<-pp1[pp1$trt %in% c('Control NPK'),]


View(pp1)

levels(pp1$trt)

pp1 <- read.csv("pp1_site_block_plot_treat_year3_all_sp.csv", sep=",",row.names=1,header=T, strip.white=T)

# Update factor labeling for Disturbance treatment (helps later with plotting)
pp1$trt<-factor(as.character(pp1$trt),levels=c('Control NPK'))
head(as.data.frame(pp1))

dat1<-pp1[pp1$trt %in% c('Control NPK'),]
nat5<-pp1[pp1$trt %in% c('Control NPK'),]
int5<-pp1[pp1$trt %in% c('Control NPK'),]

all3<-pp1[pp1$trt %in% c('Control NPK'),]
nat3<-pp1[pp1$trt %in% c('Control NPK'),]
int3<-pp1[pp1$trt %in% c('Control NPK'),]

??leap.zig

theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
allsp5 <- leap.zig(dat1,type='cafe',xlim=c(0,45),ylim=c(0,4500),standardize = FALSE,raw.points = FALSE)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('All Sp')+theme_classic()
allsp5
natives5 <- leap.zig(nat5,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(nat5$x.rich), y = mean(nat5$x.func), 
           label = "*",size=8)+ggtitle('Natives')+theme_classic()
introduced5 <- leap.zig(int5,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(int5$x.rich), y = mean(int5$x.func), 
           label = "*",size=8)+ggtitle('Introduced')+theme_classic()

allsp3 <- leap.zig(all3,type='cafe',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(all3$x.rich), y = mean(all3$x.func), 
           label = "*",size=8)+ggtitle('All Sp')+theme_classic()
allsp3
natives3 <- leap.zig(nat3,type='price',standardize = FALSE,raw.points = F)+ 
               annotate("text", x = mean(nat3$x.rich), y = mean(nat3$x.func), 
                        label = "*",size=8)+ggtitle('Natives')+theme_classic()
introduced3 <- leap.zig(int3,type='price',standardize = FALSE,raw.points = F)+ 
               annotate("text", x = mean(int3$x.rich), y = mean(int3$x.func), 
                        label = "*",size=8)+ggtitle('Introduced')+theme_classic()

allsp3          
grid.arrange(allsp5,natives5,introduced5,nrow=1)
grid.arrange(allsp5,natives5,int5,allsp3,natives3,int3,nrow=2)

View(dat1)

write.csv(pp1, "/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/pp1_site_block_plot_treat_year3_all_sp.csv")

pp1 <- read.csv("pp1_habitat_treat_subset.csv", sep=",",row.names=1,header=T, strip.white=T)
View(pp1)

#mfacet by treatment, page by habitat

theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Control Fence')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('Control K')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('Control N')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('Control NK')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('Control NP')+theme_classic()
p6 <- leap.zig(dat6,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('Control NPK')+theme_classic()
p7 <- leap.zig(dat7,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat7$x.rich), y = mean(dat7$x.func), 
           label = "*",size=8)+ggtitle('Control NPK+Fence')+theme_classic()
p8 <- leap.zig(dat8,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat8$x.rich), y = mean(dat8$x.func), 
           label = "*",size=8)+ggtitle('Control P')+theme_classic()
p9 <- leap.zig(dat9,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat9$x.rich), y = mean(dat9$x.func), 
           label = "*",size=8)+ggtitle('Control PK')+theme_classic()


grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow=3)


cf<-pp1[pp1$trt %in% c('Control Fence'),]
ck<-pp1[pp1$trt %in% c('Control K'),]
cn<-pp1[pp1$trt %in% c('Control N'),]
cnk<-pp1[pp1$trt %in% c('Control NK'),]
cnp<-pp1[pp1$trt %in% c('Control NP'),]
cnpk<-pp1[pp1$trt %in% c('Control NPK'),]
cnpkf<-pp1[pp1$trt %in% c('Control NPK+Fence'),]
cp<-pp1[pp1$trt %in% c('Control P'),]
cpk<-pp1[pp1$trt %in% c('Control PK'),]
View(cnpk)

dat1<-cnpk[cnpk$habitat %in% c('alpine grassland'),]
dat2<-cnpk[cnpk$habitat %in% c('calcareous grassland'),]
dat3<-cnpk[cnpk$habitat %in% c('desert grassland'),]
dat4<-cnpk[cnpk$habitat %in% c('grassland'),]
dat5<-cnpk[cnpk$habitat %in% c('mesic grassland'),]
dat6<-cnpk[cnpk$habitat %in% c('mixedgrass prairie'),]
dat7<-cnpk[cnpk$habitat %in% c('montane grassland'),]
dat8<-cnpk[cnpk$habitat %in% c('old field'),]
dat9<-cnpk[cnpk$habitat %in% c('pasture'),]
dat10<-cnpk[cnpk$habitat %in% c('savanna'),]
dat11<-cnpk[cnpk$habitat %in% c('semiarid grassland'),]
dat12<-cnpk[cnpk$habitat %in% c('shortgrass prairie'),]
dat13<-cnpk[cnpk$habitat %in% c('shrub steppe'),]
dat14<-cnpk[cnpk$habitat %in% c('tallgrass prairie'),]
dat15<-cnpk[cnpk$habitat %in% c('tundra grassland'),]


levels(pp1$habitat)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',legend=F,standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('alpine grassland')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('calcareous grassland')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('desert grassland')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('grassland')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('mesic grassland')+theme_classic()
p6 <- leap.zig(dat6,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('mixedgrass prarie')+theme_classic()
p7 <- leap.zig(dat7,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat7$x.rich), y = mean(dat7$x.func), 
           label = "*",size=8)+ggtitle('montane grassland')+theme_classic()
p8 <- leap.zig(dat8,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat8$x.rich), y = mean(dat8$x.func), 
           label = "*",size=8)+ggtitle('old field')+theme_classic()
p9 <- leap.zig(dat9,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat9$x.rich), y = mean(dat9$x.func), 
           label = "*",size=8)+ggtitle('pasture')+theme_classic()
p10 <- leap.zig(dat10,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat10$x.rich), y = mean(dat10$x.func), 
           label = "*",size=8)+ggtitle('savanna')+theme_classic()
p11 <- leap.zig(dat11,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat11$x.rich), y = mean(dat11$x.func), 
           label = "*",size=8)+ggtitle('semiarid grassland')+theme_classic()
p12 <- leap.zig(dat12,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat12$x.rich), y = mean(dat12$x.func), 
           label = "*",size=8)+ggtitle('shortgrass prarie')+theme_classic()
p13 <- leap.zig(dat13,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat13$x.rich), y = mean(dat13$x.func), 
           label = "*",size=8)+ggtitle('shrub steppe')+theme_classic()
p14 <- leap.zig(dat14,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat14$x.rich), y = mean(dat14$x.func), 
           label = "*",size=8)+ggtitle('tallgrass prarie')+theme_classic()
p15 <- leap.zig(dat15,type='price',standardize = FALSE,raw.points = F,ylim=c(0,450000),xlim=c(0,30))+ 
  annotate("text", x = mean(dat15$x.rich), y = mean(dat15$x.func), 
           label = "*",size=8)+ggtitle('tundra grassland')+theme_classic()
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,nrow=4)



theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='price',legend=F,standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('alpine grassland')+theme_classic()
p2 <- leap.zig(dat2,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('calcareous grassland')+theme_classic()
p3 <- leap.zig(dat3,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('desert grassland')+theme_classic()
p4 <- leap.zig(dat4,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('grassland')+theme_classic()
p5 <- leap.zig(dat5,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('mesic grassland')+theme_classic()
p6 <- leap.zig(dat6,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('mixedgrass prarie')+theme_classic()
p7 <- leap.zig(dat7,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat7$x.rich), y = mean(dat7$x.func), 
           label = "*",size=8)+ggtitle('montane grassland')+theme_classic()
p8 <- leap.zig(dat8,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat8$x.rich), y = mean(dat8$x.func), 
           label = "*",size=8)+ggtitle('old field')+theme_classic()
p9 <- leap.zig(dat9,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat9$x.rich), y = mean(dat9$x.func), 
           label = "*",size=8)+ggtitle('pasture')+theme_classic()
p10 <- leap.zig(dat10,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat10$x.rich), y = mean(dat10$x.func), 
           label = "*",size=8)+ggtitle('savanna')+theme_classic()
p11 <- leap.zig(dat11,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat11$x.rich), y = mean(dat11$x.func), 
           label = "*",size=8)+ggtitle('semiarid grassland')+theme_classic()
p12 <- leap.zig(dat12,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat12$x.rich), y = mean(dat12$x.func), 
           label = "*",size=8)+ggtitle('shortgrass prarie')+theme_classic()
p13 <- leap.zig(dat13,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat13$x.rich), y = mean(dat13$x.func), 
           label = "*",size=8)+ggtitle('shrub steppe')+theme_classic()
p14 <- leap.zig(dat14,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat14$x.rich), y = mean(dat14$x.func), 
           label = "*",size=8)+ggtitle('tallgrass prarie')+theme_classic()
p15 <- leap.zig(dat15,type='price',standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat15$x.rich), y = mean(dat15$x.func), 
           label = "*",size=8)+ggtitle('tundra grassland')+theme_classic()
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,nrow=4)

??gridExtra

View(pp1)
s1 <- leap.zig(dat1,type='price',
               error.bars=T,
               vectors=T,raw.points = T,standardize = T,legend=F)+theme_classic()
s1

levels(pp1$habitat)
View(dg)
install.packages("yarrr")  # Install package from CRAN
library("yarrr") # Load the package!
colnames(pp1)

pp1 <- read.csv("bind_price_year3_2.csv", sep=",",header=T, strip.white=T)
pp5 <- read.csv("bind_price_year5_2.csv", sep=",",row.names=1,header=T, strip.white=T)

View(pp1)

pp1<-pp1[pp1$site_name %in% c('Cedar Creek LTER'),]
par(mfrow = c(4, 4), mar=c(4,4.5,2,2))

pirateplot(formula = SRE.L ~ trt_year ,
           data = pp1,
           main = "SRE.L",
           xlab="",
           ylab="",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")
pirateplot(formula = SRE.G ~ trt_year ,
           data = pp1,
           main = "SRE.G",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")
pirateplot(formula = SIE.L ~ trt_year ,
           data = pp1,
           main = "SIE.L",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")
pirateplot(formula = SIE.G ~ trt_year ,
           data = pp1,main = "SIE.G",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")

pirateplot(formula = CDE ~ trt_year ,
           data = pp1,main = "CDE",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")
pirateplot(formula = SL ~ trt_year ,
           data = pp1,main = "SL",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")
pirateplot(formula = SG ~ trt_year ,
           data = pp1,main = "SG",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")
pirateplot(formula = SR ~ trt_year ,
           data = pp1,main = "SR",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")

pirateplot(formula = CE ~ trt_year ,
           data = pp1,main = "CE",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")

pirateplot(formula = x.func ~ trt_year ,
           data = pp1,main = "x.func",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")

pirateplot(formula = y.func ~ trt_year ,
           data = pp1,main = "y.func",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")

pirateplot(formula = x.rich ~ trt_year ,
           data = pp1,main = "x.rich",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")

pirateplot(formula = y.rich ~ trt_year ,
           data = pp1,main = "y.rich",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")
pirateplot(formula = c.rich ~ trt_year ,
           data = pp1,main = "c.rich",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "xmen",
           gl.col = "gray")




setwd("/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/")


comb <- read.csv("year3.csv", sep=",",header=T, strip.white=T, na.strings=c("NULL", "NA"))

View(comb)
colnames(comb)
piratepal(palette = "all", plot.result = TRUE)

par(mfrow = c(2, 1), mar=c(4,4.5,2,2))


pirateplot(formula = live_mass ~ trt_year ,
  data = comb,main = "Live Mass",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")

pirateplot(formula = dead_mass ~ trt_year ,
           data = comb,main = "Dead Mass",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")


pirateplot(formula = Biomass_CalcSp ~ functional_group + trt_year ,
           data = comb,main = " Biomass",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")

pirateplot(formula = rich ~ functional_group + trt_year ,
           data = comb,main = "Plot richness",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")


par(mfrow = c(2, 2), mar=c(4,4.5,2,2))


pirateplot(formula = rich ~ trt_year ,
           data = comb,main = "Total plot richness",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")

pirateplot(formula = NAT_rich ~ trt_year ,
           data = comb,main = "Native plot richness",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")


pirateplot(formula = INT_rich ~ trt_year ,
           data = comb,main = "Introduced plot richness",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")

pirateplot(formula = site_richness ~ trt_year ,
           data = comb,main = "Total Site richness",
           theme=0,bean.f.o = .4, # Bean fill
           point.o = .4, # Points
           inf.f.o = .4, # Inference fill
           inf.b.o = .8, # Inference border
           avg.line.o = 0.8, # Average line
           point.cex = .5, # Points
           quant = c(.1, .9),
           pal = "basel",
           gl.col = "gray")


??priceTools



