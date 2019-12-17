

rm(list=ls())


library(gridExtra)
library(ggplot2)
library(reshape2) 
library(MCMCglmm)
library(tidyr)

library(priceTools)
library(tidyverse)


sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(sp)
head(sp)

sp$year_trt<-as.factor(sp$year_trt)
levels(sp$year_trt)
levels(sp$site_code)
sp2<-group_by(sp, site_code,year_trt)
head(sp2)

all<-unite_(sp2, "site.year.id", c("site_code","year_trt"), remove=FALSE)
all<-unite_(all, "trt_year", c("trt","year_trt"), remove=FALSE)
all<-unite_(all, "site_trt_year", c("site_code","trt","year_trt"), remove=FALSE)
# View(all)
levels(all$site_code)
colnames(all)
native<-all[all$local_provenance %in% c('NAT'),]
introduced<-all[all$local_provenance %in% c('INT'),]


levels(all$year_trt)
all$year_trt<-as.numeric(as.character(all$year_trt))
all$site.year.id<-as.character(all$site.year.id)


#create index's
index<-paste(all$site_name, all$site.year.id)
sindex<-as.character(all$site_code)
yindex<-as.character(all$year_trt)
uindex<-sort(unique(index))
usindex<-sort(unique(sindex))

uyindex<-sort(unique(yindex))

#cumulative
all_lst<-NULL
n<-1
for (i in 1:length(usindex)){
  subs<-which(sindex==usindex[i])
  uindex_small<-sort(unique(all[subs,]$year_trt))
  
  for(j in 2:length(uindex_small)) {
    subs2<-which(yindex[subs]%in%c(uindex_small[j], "0"))
    
    all_lst[[n]]<-all[subs[subs2],]
    names(all_lst)[n]<-paste(usindex[i], uindex_small[j], sep="_")
    n<-n+1
  }
  print(i/length(usindex))
}


#progressive
# all_lst<-NULL
# n<-1
# for (i in 1:length(usindex)){
#   #select all instances from a site
#   subs<-which(sindex==usindex[i])
#   uindex_small<-sort(unique(all[subs,]$year_trt))
# 
#   for(j in 2:length(uindex_small)) {
#     subs2<-which(as.numeric(as.character(yindex[subs]))%in%
#                    (uindex_small[j]-c(0,1)))
# 
#     #table(all[subs[subs2],]$site.year.id)
#     if(length(unique(all[subs[subs2],]$site.year.id))==2) {
#       all_lst[[n]]<-all[subs[subs2],]
#       names(all_lst)[n]<-paste(usindex[i], uindex_small[j], sep="_")
#       n<-n+1
#     }
#   }
#   print(i/length(usindex))
# }


summary(arch.us)
summary(arch.us_2)
View(all_lst)
folder = "output_new"
#input RDS files for cluster, price analysis
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#if this doesnt work close R and try again only happens because wd has changed
mapply(saveRDS, all_lst, file=paste0(folder, "/",names(all_lst), '.rds', version=3.4))

#test
samp <- readRDS("arch.us_2.rds")
colnames(samp)

group.vars <- c('site.year.id','plot','block')
treat.vars<-c('trt_year')

grouped.data <- samp %>% group_by(.dots=c(group.vars,treat.vars))

#takes a long time
res <- pairwise.price(grouped.data, species="Taxon", func="biomass.sp")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp<-res
pp<-group.columns(pp,gps=c(group.vars,treat.vars), drop=T)

View(pp)
?pairwise.price

#NATIVE
nindex<-paste(native$site_name, native$site.year.id)
nsindex<-as.character(native$site_code)
nyindex<-as.character(native$year_trt)
nuindex<-sort(unique(index))
nusindex<-sort(unique(sindex))
nuyindex<-sort(unique(yindex))


colnames(native)

n_lst<-NULL
n<-1
for (i in 1:length(nusindex)){
  nsubs<-which(nsindex==nusindex[i])
  nuindex_small<-sort(unique(native[nsubs,]$year_trt))
  
  for(j in 2:length(nuindex_small)) {
    nsubs2<-which(nyindex[nsubs]%in%c(nuindex_small[j], "0"))
    
    n_lst[[n]]<-native[nsubs[nsubs2],]
    names(n_lst)[n]<-paste(nusindex[i], nuindex_small[j], sep="_")
    n<-n+1
  }
  print(i/length(nusindex))
}


View(n_lst)

folder = "output_native"
#input RDS files for cluster, price analysis
mapply(saveRDS, n_lst, file=paste0(folder, "/",names(n_lst), '.rds'))


#INTRODUCED
iindex<-paste(introduced$site_name, introduced$site.year.id)
isindex<-as.character(introduced$site_code)
iyindex<-as.character(introduced$year_trt)
iuindex<-sort(unique(index))
iusindex<-sort(unique(sindex))
iuyindex<-sort(unique(yindex))


i_lst<-NULL
n<-1
for (i in 1:length(iusindex)){
  isubs<-which(isindex==iusindex[i])
  iuindex_small<-sort(unique(introduced[isubs,]$year_trt))
  
  for(j in 2:length(iuindex_small)) {
    isubs2<-which(iyindex[isubs]%in%c(iuindex_small[j], "0"))
    
    i_lst[[n]]<-introduced[isubs[isubs2],]
    names(i_lst)[n]<-paste(iusindex[i], iuindex_small[j], sep="_")
    n<-n+1
  }
  print(i/length(iusindex))
}



View(i_lst)

folder = "output_introduced"
#input RDS files for cluster, price analysis
mapply(saveRDS, i_lst, file=paste0(folder, "/",names(i_lst), '.rds'))




#test cluster code on small subset
View(all_lst)
list2env(all_lst, envir= .GlobalEnv) #split the list into separate dataframes
nn.dist<-distinct(all,site.year.id)
# View(nn.dist)
azi.cn_2$site.year.id<-as.factor(as.character(azi.cn_2$site.year.id))
levels(azi.cn_2$site.year.id)
View(azi.cn_2)
colnames(azi.cn_2)

group.vars <- c('site.year.id','plot','block')
treat.vars<-c('trt_year')

grouped.data <- azi.cn_5 %>% group_by(.dots=c(group.vars,treat.vars))

#takes a long time
res <- pairwise.price(grouped.data, species="Taxon", func="Biomass_CalcSp")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp<-res
pp<-group.columns(pp,gps=c(group.vars,treat.vars), drop=T)

View(pp)






