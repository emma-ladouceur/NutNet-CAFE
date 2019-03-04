
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

price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input/", pattern = ".rds$", recursive = TRUE, full.names = TRUE)
View(price.list)


price.all <- foreach (file = price.list,.combine=rbind) %do% {
  price.file<-readRDS(file)
  price.file$data
}

View(price.all)
levels(price.all$site_code)







