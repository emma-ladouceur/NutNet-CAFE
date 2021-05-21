
# Authors: Emma Ladouceur & Adam T. Clark
# Title: Price equation preparation: site level subsets
# Last Updated May 2, 2021

# Produces datasets: in folder pairs prep data; 428 datasets subset by site and year_trt
# 3 preps data for price equation comparisons, which just pairs data into temporal subsets for meaningful comparisons
# and outputs subsets of the data in pairs

# packages
library(tidyverse)
library(priceTools)

# data
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

head(sp)
sp$year_trt <- as.factor(sp$year_trt)
sp$site_code <- as.factor(sp$site_code)
levels(sp$year_trt)
levels(sp$site_code)

pairs_prep <- sp %>% group_by(site_code, year_trt) %>% 
  unite("site.year.id", c("site_code","year_trt"), remove = FALSE) %>%
  unite("trt_year", c("trt","year_trt"), remove = FALSE) %>% 
  unite( "site_trt_year", c("site_code","trt","year_trt"), remove = FALSE)

pairs_prep$year_trt <- as.numeric(as.character(pairs_prep$year_trt))
pairs_prep$site.year.id <- as.character(pairs_prep$site.year.id)

pairs_prep %>% filter(site_code == "jena.de") %>%
  distinct(site_code, year_trt)

# create index's
index <- paste(pairs_prep$site_name, pairs_prep$site.year.id)
sindex <- as.character(pairs_prep$site_code)
yindex <- as.character(pairs_prep$year_trt)
uindex <- sort(unique(index))
usindex <- sort(unique(sindex))
uyindex <- sort(unique(yindex))

# cumulative time approach
# we pair site codes in time to reduce computation time or price partition comparisons
pairs_lst <- NULL
n <- 1
for (i in 1:length(usindex)){
  subs <- which(sindex==usindex[i])
  uindex_small<-sort(unique(pairs_prep[subs,]$year_trt))
  
  for(j in 2:length(uindex_small)) {
    subs2<-which(yindex[subs]%in%c(uindex_small[j], "0"))
    
    pairs_lst[[n]]<-pairs_prep[subs[subs2],]
    names(pairs_lst)[n]<-paste(usindex[i], uindex_small[j], sep="_")
    n<-n+1
  }
  print(i/length(usindex))
}

folder = "pairs prep data"
# input RDS files for cluster, price analysis
# if this doesnt work close R and try again only happens because wd has changed
mapply(saveRDS, pairs_lst, version =2 , file=paste0(folder, "/",names(pairs_lst), '.rds'))


# following this, the scripts starting with 'Price Pairs' are used to send these dat to the cluster
# this can be run on a local machine, but will take time, and code needs to be adapted
# a test script is below
# if written in a loop, for each data subset we have prepared, it wont take long


# test it out, test the code for priceTools we will use on the cluster next
samp <- readRDS("pairs prep data/arch.us_2.rds")

colnames(samp)
head(samp)

group.vars <- c('site.year.id','plot','block')
treat.vars <- c('trt_year')

grouped.data <- samp %>% group_by(.dots=c(group.vars,treat.vars))

#takes a long time if data is big
res <- pairwise.price(grouped.data, species= "Taxon", func = "biomass.sp.full")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp <- res
pp <- group.columns(pp,gps=c(group.vars,treat.vars), drop=T)


head(pp)
