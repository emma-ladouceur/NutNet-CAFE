

# Author: Emma Ladouceur
# Title: Post-Price equation data clean
# Last Updated  May 2, 2021

#  Produces dataset: 'nutnet_price_all.csv' & 'nutnet_cumulative_tims.csv'
# This workflow uses the data produced by the script 'Price_Pairs.R", which is submitted to the cluster
# by wrapper submit script, "Price_Pairs_Wrapper.sh" and nested Submit Script "Price_Pairs_Submit.sh" 
# In this script we prune the pairwise comparisons down to only meaningful temporal pairings
# which we refer to as 'cumulative time' this compares every plot at 'year 0' or year before fertilization
# to itself at every time of measurement since fertilization treatment
# this gives us a temporally cumulative time series of pairwise partitioned responses
# to turn this into a rate over time, we then model the dataset with  time as a continuous predictor
# but the first step, done here, is prune comparisons down only to the meaningful temporal comparisons we want
# to prepare for modeling

# packaaes
library(tidyverse)
library(foreach)


# plot data
p <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
# run cluster -based price script, then load the new input
price.list <- list.files(path = "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/price pairs data/", pattern = ".rds$", recursive = TRUE, full.names = TRUE)

# bring in all 428 pairwise price datasets
price.all <- foreach (file = price.list,.combine=rbind) %do% {
  price.file<-readRDS(file)
  price.file$data
}

head(price.all)

# we keep this unpruned version just in case we need to check or change something
write.csv(price.all,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_price_all.csv")

# load unpruned data frame
price.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_price_all.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# the function compares everything possible so we now need to prune the data back to only meaningful comparisons
head(price.all)
nrow(price.all)
# 94,828 rows, most of them unnecessary and meaningless


# separate and unite the columns to improve ease of sorting, filtering and pruning the dataset
price.sort <- price.all %>% 
  mutate( site.year.id = str_replace( site.year.id, "tejon_", "tejon"),) %>% # reformat tejon_foot.us  and tejon_south.us to match other site code formats
  separate(trt_year,into=c("trt_year.x","trt_year.y"),sep = " ", remove=FALSE) %>%
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
  unite("unique.id", c("site.year.id","trt_year","block","plot"), sep="_", remove=FALSE)

# have a look at all the levels that we need to filter
price.sort$trt.xy<- as.factor(as.character(price.sort$trt.xy))
levels(price.sort$trt.xy)
price.sort$block<- as.factor(as.character(price.sort$block))
levels(price.sort$block)
price.sort$year.xy<- as.factor(as.character(price.sort$year.xy))
levels(price.sort$year.xy)
price.sort$plot<- as.factor(as.character(price.sort$plot))
levels(price.sort$plot)


# filter data down to only the comparisons we want
price.pairs <-  price.sort  %>%
  # filter for meaningful comparisons
  filter(trt.xy %in% c('Control_Control','NPK_NPK'), # filter treatment comparisons
         block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'), # filter within block comparisons
         year.xy %in% c('0_1','0_2','0_3','0_4','0_5','0_6','0_7','0_8','0_9','0_10','0_11','0_12', '0_13'), # filter year comparisons
         plot %in% c('1 1','2 2','3 3','4 4','5 5','6 6','7 7','8 8','9 9','10 10','11 11','12 12','13 13','14 14','15 15', # filter plot comparisons
                     '16 16','17 17','18 18','19 19','20 20','21 21','22 22','23 23','24 24','25 25','26 26','27 27','28 28',
                     '29 29','30 30','31 31','32 32','33 33','34 34','35 35','36 36','37 37', '38 38','39 39','40 40','41 41',
                     '42 42','43 43','44 44','45 45','46 46','47 47','48 48','49 49','50 50','51 51','52 52','53 53','54 54','55 55',
                     '56 56','57 57','58 58','59 59','60 60','61 61','62 62')) 


price.pairs$plot<- as.factor(as.character(price.pairs$plot))
levels(price.pairs$plot)
# calculate some of the extra metrics we need for modeling

price.pairs$year.y <- as.numeric(price.pairs$year.y)
 
price.pairs.calc <-  price.pairs %>% group_by(site_code) %>%
  summarise(year_min = min(year.x), # min year and  max year for each site
            year_max = max(year.y)) %>% 
  left_join(price.pairs) %>%
  mutate(s.loss = (x.rich - c.rich), # calculate species loss
         s.gain = (y.rich - c.rich), # calculate species gain
         s.loss.n = (s.loss * - 1) , # convert species loss to negative
         year.y.m = (year.y - mean(year.y) ) ) # center year


head(price.pairs.calc)
nrow(price.pairs.calc)
# 2,966

price.pairs.calc$year.xy <- as.factor(price.pairs.calc$year.xy)
price.pairs.calc$plot.x <- as.factor(as.character(price.pairs.calc$plot.x))
price.pairs.calc$year.y <- as.numeric(price.pairs.calc$year.y)
price.pairs.calc$year_max <- as.numeric(price.pairs.calc$year_max)



# look at what sites will be included in main analysis
sites <- price.pairs.calc %>% distinct(site_code, year_max) %>% filter(year_max >= 3)

View(sites) # looks good!!

write.csv(price.pairs.calc,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv")


