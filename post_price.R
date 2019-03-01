
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


price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input/", pattern = ".rds$", recursive = TRUE, full.names = TRUE)
View(price.list)

setwd('~/Desktop/Academic/R Code/NutNet/input/')

table1 <- readRDS("output-1.rds")
table2 <- readRDS("output-2.rds")
table3 <- readRDS("output-3.rds")
table4 <- readRDS("output-4.rds")
table5 <- readRDS("output-5.rds")
table6 <- readRDS("output-6.rds")
table7 <- readRDS("output-7.rds")
table8 <- readRDS("output-8.rds")
table9 <- readRDS("output-9.rds")
table10 <- readRDS("output-10.rds")


table1<-as.data.frame(table1)
table2<-as.data.frame(table2)
table3<-as.data.frame(table3)
table4<-as.data.frame(table4)
table5<-as.data.frame(table5)
table6<-as.data.frame(table6)
table7<-as.data.frame(table7)
table8<-as.data.frame(table8)
table9<-as.data.frame(table9)
table10<-as.data.frame(table10)

View(table3)
View(table4)
View(table5)
View(table6)
View(table7)
View(table8)
View(table9)
View(table10)
levels(table1$data.trt_year)
levels(table2$data.trt_year)
levels(table3$data.trt_year)
levels(table4$data.trt_year)
levels(table5$data.trt_year)

dat<-bind_rows(table1,table2,table3,table4,table5,table6,table7,table8,table9,table10)

