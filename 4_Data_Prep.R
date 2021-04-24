

# Author: Emma Ladouceur
# Title:
# Last Updated April 17, 2021

# 4 Data Prep
# This workflow uses the data produced by the script 'Price_Pairs.R", which is submitted to the cluster
# by wrapper submit script, "Price_Pairs_Wrapper.sh" and nested Submit Script "Price_Pairs_Submit.sh" a
# In this script we prune the pairwise comparisons down to only meaningful temporal pairings

# packagaes
library(tidyverse)
library(foreach)


# plot data
p <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# run cluster -based price script, then load the new input
price.list <- list.files(path = "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/price pairs data/", pattern = ".rds$", recursive = TRUE, full.names = TRUE)

price.all <- foreach (file = price.list,.combine=rbind) %do% {
  price.file<-readRDS(file)
  price.file$data
}

head(price.all)

# write data
write.csv(price.all,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/nutnet_price_all.csv")

# load data frame
price.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/nutnet_price_all.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# the function compares everything possible so we now need to prune the data back to only meaningful comparisons
head(price.all)
nrow(price.all)
# 94,828

# clean and prune the data
price.pairs <- price.all %>% separate(trt_year,into=c("trt_year.x","trt_year.y"),sep = " ", remove=FALSE) %>%
  # separate columns for filtering and analysis
  separate(trt_year.y, into=c("trt.y","year.y"),sep = "_", remove=FALSE) %>%
  separate(trt_year.x, into=c("trt.x","year.x"),sep = "_", remove=FALSE) %>%
  separate(site.year.id, into=c("site.year.id.x","site.year.id.y"),sep = " ", remove=FALSE) %>% 
  separate(plot, into=c("plot.x","plot.y"),sep = " ", remove=FALSE) %>%
  separate(site.year.id.x, into=c("site_code","year.x"),sep = "_", remove=FALSE) %>%
  # unite separated columns for filtering
  unite("trt.xy", c("trt.x","trt.y"), sep="_", remove=FALSE) %>%
  unite("year.xy", c("year.x","year.y"), sep="_", remove=FALSE) %>%
  # create unique id
  unite("unique.id", c("site.year.id","trt_year","block","plot"), sep="_", remove=FALSE) %>%
  # filter for meaningful comparisons
  filter(trt.xy %in% c('Control_Control','NPK_NPK'), # filter treatment comparisons
         block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'), # filter within block comparisons
         year.xy %in% c('0_1','0_2','0_3','0_4','0_5','0_6','0_7','0_8','0_9','0_10','0_11','0_12'), # filter year comparisons
         plot %in% c('1 1','2 2','3 3','4 4','5 5','6 6','7 7','8 8','9 9','10 10','11 11','12 12','13 13','14 14','15 15', # filter plot comparisons
                     '16 16','17 17','18 18','19 19','20 20','21 21','22 22','23 23','24 24','25 25','26 26','27 27','28 28',
                     '29 29','30 30','31 31','32 32','33 33','34 34','35 35','36 36','37 37', '38 38','39 39','40 40','41 41',
                     '42 42','43 43','44 44','45 45','46 46','47 47','48 48','49 49','50 50','51 51','52 52','53 53','54 54','55 55',
                     '56 56','57 57','58 58','59 59','60 60','61 61','62 62')) %>%
  group_by(site_code) %>%
  summarise(year_min = min(year.x), # min year and  max year for each site
            year_max = max(year.y)) %>% 
  left_join(price.pairs) %>%
  mutate(s.loss = (x.rich - c.rich), # calculate species loss
         s.gain = (y.rich - c.rich), # calculate species gain
         s.loss.n = (s.loss * - 1) , # convert species loss to negative
         year.y.m = (year.y - mean(year.y) ) ) # center year


head(price.pairs)
nrow(price.pairs)
# 2,966

price.pairs$year.xy <-as.factor(price.pairs$year.xy)
price.pairs$plot.x <-as.factor(as.character(price.pairs$plot.x))
price.pairs$year.y <- as.numeric(price.pairs$year.y)
price.pairs$year_max <- as.numeric(price.pairs$year_max)

#look at what sites will be included in main analysis
sites <- price.pairs %>% distinct(site_code, year_max) %>% filter(year_max >= 3)

View(sites)

write.csv(price.pairs,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/nutnet_cumulative_time.csv")


