
library(grid)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyr)
library("yarrr")

library(priceTools)
library(readr)
library(raster)

library(tidyverse)
library(data.table)
library(foreach)

p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input_new/", pattern = ".rds$", recursive = TRUE, full.names = TRUE)
#price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input_new/", pattern = ".rds$")


price.all <- foreach (file = price.list,.combine=rbind) %do% {
  price.file<-readRDS(file)
  price.file$data
}

View(price.all)

write.csv(price.all,"~/Desktop/Academic/Data/NutNet/nutnet_price_combine_new.csv")

price.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_combine_new.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(price.all)
price.all2<-separate(price.all,trt_year,into=c("trt_year.x","trt_year.y"),sep = " ", remove=FALSE)
price.all3<-separate(price.all2,trt_year.y,into=c("trt.y","year.y"),sep = "_", remove=FALSE)
price.all4<-separate(price.all3,trt_year.x,into=c("trt.x","year.x"),sep = "_", remove=FALSE)
price.all5<-separate(price.all4,site.year.id,into=c("site.year.id.x","site.year.id.y"),sep = " ", remove=FALSE)

View(price.all5)
nrow(price.all5)
#88,982 rows


price.all5$unique.id<-as.character(with(price.all5, paste(site.year.id,trt_year,block,plot, sep=".")))


price.r<-price.all5 %>% distinct(unique.id, .keep_all = TRUE)

price.r$year.y<-as.numeric(price.r$year.y)
is.numeric(price.r$year.y)
is.numeric(price.r$SL)

write.csv(price.r,"~/Desktop/Academic/Data/NutNet/nutnet_price_all.csv")


#
price.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(price.all)
price.all2<-separate(price.all,trt_year,into=c("trt_year.x","trt_year.y"),sep = " ", remove=FALSE)
price.all3<-separate(price.all2,trt_year.y,into=c("trt.y","year.y"),sep = "_", remove=FALSE)
price.all4<-separate(price.all3,trt_year.x,into=c("trt.x","year.x"),sep = "_", remove=FALSE)
price.all5<-separate(price.all4,site.year.id,into=c("site.year.id.x","site.year.id.y"),sep = " ", remove=FALSE)
price.all6<-unite_(price.all5, "trt.xy", c("trt.x","trt.y"), remove=FALSE)
price.all7<-unite_(price.all6, "year.xy", c("year.x","year.y"), remove=FALSE)

View(price.all7)
nrow(price.all7)
#84,546 rows

price.reduced<-price.all7[price.all7$trt.xy %in% c('Control_Control','NPK_NPK'),]
price.reduced2<-price.reduced[price.reduced$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]
price.reduced2$year.xy<-as.factor(price.reduced2$year.xy)
View(price.reduced2)

levels(price.reduced2$year.xy)
price.reduced3<-price.reduced2[price.reduced2$year.xy %in% c('0_0','0_1','0_2','0_3','0_4','0_5','0_6','0_7','0_8','0_9','0_10','0_11'),]
View(price.reduced3)
nrow(price.reduced3)
#4787

View(price.reduced3)
price.reduced3$unique.id<-as.character(with(price.reduced3, paste(site.year.id,trt_year,block,plot, sep=".")))


write.csv(price.reduced3,"~/Desktop/Academic/Data/NutNet/nutnet_cumulative_time_only.csv")


# remove all this??
p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_cumulative_time_only.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
View(p.all)

p.all<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all$f.year.y<-as.factor(p.all$year.y)
p.all$plot<-as.factor(p.all$plot)
p.all$site_code<-as.factor(p.all$site_code)

dat<-distinct(plot, site_code, continent,habitat)
p.dat2<-inner_join(p.all,dat)

p.dat2$year.y.m<-p.dat2$year.y-mean(p.dat2$year.y)

p.dat2$SL.p<-abs(p.dat2$SL)
p.dat2$SL.p.log1<-log1p(p.dat2$SL.p)

is.numeric(p.dat2$SG)

p.dat2$SG.log1<-log1p(p.dat2$SG)
p.dat2$CDE.log<-log1p(p.dat2$CDE)


p.dat2$s.loss <- -1*(p.dat2$x.rich - p.dat2$c.rich)
p.dat2$s.gain <- p.dat2$y.rich - p.dat2$c.rich
p.dat2$s.change <- p.dat2$y.rich - p.dat2$x.rich

p.dat2$s.loss.p<-abs(p.dat2$s.loss)
p.dat2$s.loss.p.log <- log1p(abs(p.dat2$s.loss.p))
p.dat2$s.gain.log<-log1p(p.dat2$s.gain)


#histograms of sl & sg
par(mfrow=c(2,3))
hist(p.dat2$SL,breaks =40, main="Species Loss", xlab= "Species Loss")
hist(p.dat2$SG, breaks=40, main="Species Gains", xlab= "Species Gains")
hist(p.dat2$CDE, breaks=40, main="CDE", xlab= "CDE")

hist(p.dat2$SL.p,breaks=40, main="Positive Species Loss")
hist(p.dat2$SG, breaks=40, main="Species Gains")
hist(p.dat2$CDE, breaks=40, main="CDE ")



par(mfrow=c(2,3))
hist(p.dat2$s.loss.p,breaks =10, main="Species Loss", xlab= "Species Loss")
hist(p.dat2$s.gain, breaks=10, main="Species Gains", xlab= "Species Gains")
hist(p.dat2$s.change, breaks=10, main="Species Change", xlab= "Species Change")
hist(p.dat2$s.loss.p.log,breaks =10, main="Log Species Loss", xlab= "Log Species Loss")
hist(p.dat2$s.gain.log, breaks=10, main="Log Species Gains", xlab= "Log Species Gains")
hist(p.dat2$s.change.log, breaks=10, main="Log Species Change", xlab= "Log Species Change")


View(p.dat2)
write.csv(p.dat2,"~/Dropbox/Projects/NutNet/Data/cumulative_time_only2.csv")




