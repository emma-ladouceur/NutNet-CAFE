


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

p <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input_new/", pattern = ".rds$", recursive = TRUE, full.names = TRUE)
#price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input_new/", pattern = ".rds$")


price.all <- foreach (file = price.list,.combine=rbind) %do% {
  price.file<-readRDS(file)
  price.file$data
}

View(price.all)

write.csv(price.all,"~/Dropbox/Projects/NutNet/Data/nutnet_price_all.csv")

price.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_price_all.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(price.all)
price.all2<-separate(price.all,trt_year,into=c("trt_year.x","trt_year.y"),sep = " ", remove=FALSE)
price.all3<-separate(price.all2,trt_year.y,into=c("trt.y","year.y"),sep = "_", remove=FALSE)
price.all4<-separate(price.all3,trt_year.x,into=c("trt.x","year.x"),sep = "_", remove=FALSE)
price.all5<-separate(price.all4,site.year.id,into=c("site.year.id.x","site.year.id.y"),sep = " ", remove=FALSE)

View(price.all5)
nrow(price.all5)
#88,982 rows


price.all5$unique.id<-as.character(with(price.all5, paste(site.year.id,trt_year,block,plot, sep=".")))



price.all2<-separate(price.all5,trt_year,into=c("trt_year.x","trt_year.y"),sep = " ", remove=FALSE)
price.all3<-separate(price.all2,trt_year.y,into=c("trt.y","year.y"),sep = "_", remove=FALSE)
price.all4<-separate(price.all3,trt_year.x,into=c("trt.x","year.x"),sep = "_", remove=FALSE)
price.all5<-separate(price.all4,site.year.id,into=c("site.year.id.x","site.year.id.y"),sep = " ", remove=FALSE)
price.all6<-unite_(price.all5, "trt.xy", c("trt.x","trt.y"), remove=FALSE)
price.all7<-unite_(price.all6, "year.xy", c("year.x","year.y"), remove=FALSE)

View(price.all7)
nrow(price.all7)
#97,296 rows

price.reduced<-price.all7[price.all7$trt.xy %in% c('Control_Control','NPK_NPK'),]
price.reduced2<-price.reduced[price.reduced$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]
price.reduced2$year.xy<-as.factor(price.reduced2$year.xy)
nrow(price.reduced2)



levels(price.reduced2$year.xy)
nrow(price.reduced2)
price.reduced3<-price.reduced2[price.reduced2$year.xy %in% c('0_1','0_2','0_3','0_4','0_5','0_6','0_7','0_8','0_9','0_10','0_11','0_12'),]

levels(price.reduced3$plot)
price.reduced4<-separate(price.reduced3,plot,into=c("plot.x","plot.y"),sep = " ", remove=FALSE)
price.reduced4$plot.x<-as.factor(as.character(price.reduced4$plot.x))
levels(price.reduced4$plot.x)
#price.all6<-unite_(price.all5, "trt.xy", c("trt.x","trt.y"), remove=FALSE)
colnames(price.reduced4)
View(price.reduced4)

price.reduced5<-separate(price.reduced4,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)
#9633
price.reduced6<-price.reduced5[price.reduced5$plot %in% c('1 1','2 2','3 3','4 4','5 5','6 6','7 7','8 8','9 9','10 10','11 11','12 12','13 13','14 14','15 15','16 16','17 17','18 18','19 19','20 20','21 21','22 22','23 23','24 24','25 25','26 26','27 27','28 28','29 29','30 30','31 31','32 32','33 33','34 34','35 35','36 36','37 37', '38 38','39 39','40 40','41 41','42 42','43 43','44 44','45 45','46 46','47 47','48 48','49 49','50 50','51 51','52 52','53 53','54 54','55 55','56 56','57 57','58 58','59 59','60 60','61 61','62 62'),]

View(price.reduced6)
price.reduced6$unique.id<-as.character(with(price.reduced6, paste(site.year.id,trt_year,block,plot, sep=".")))


price.reduced6$year.y<- as.numeric(price.reduced6$year.y)
price.reduced6$year.y.m<-price.reduced6$year.y-mean(price.reduced6$year.y)
price.reduced6$s.loss <- price.reduced6$x.rich - price.reduced6$c.rich
price.reduced6$s.loss.n<-(price.reduced6$s.loss*-1)
price.reduced6$s.gain <- price.reduced6$y.rich - price.reduced6$c.rich

nrow(price.reduced6)
sites<-price.reduced6 %>% distinct(site_code)

View(sites)


write.csv(price.reduced6,"~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv")
