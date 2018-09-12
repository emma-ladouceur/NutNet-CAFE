


library(gridExtra)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyr)

library(priceTools)


sp <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sp$year_trt<-as.factor(sp$year_trt)
levels(sp$year_trt)
sp2<-group_by(sp, site_code,year_trt)
controls<-filter(sp2, year_trt=="0")
years<-filter(sp2, !year_trt=="0")

controls$subset<-"controls"
years$subset<-"years"

colnames(controls)
years<-unite_(years, "site.year.id", c("site_code","year_trt"), remove=FALSE)
controls<-unite_(controls, "site.year.id", c("site_code","year_trt"), remove=FALSE)
all<-unite_(sp2, "site.year.id", c("site_code","year_trt"), remove=FALSE)
View(all)


#try all
uniq.c <- distinct(controls,site_code, year_trt,site.year.id)
uniq.y <- distinct(years,site_code, year_trt,site.year.id)
uniq.a <- distinct(all,site_code, year_trt,site.year.id)



levels(all$year_trt)
uniq.a$year_trt<-as.numeric(as.character(uniq.a$year_trt))
all$year_trt<-as.numeric(as.character(all$year_trt))
is.numeric(all$year_trt)
is.numeric(uniq.a$year_trt)
uniq.a$site.year.id<-as.character(uniq.a$site.year.id)
all$site.year.id<-as.character(all$site.year.id)


#########FUCKING MAGIC WHAT THE ACTUAL FUCK
for (i in 1:length(uniq.a)){
  assign(paste0(uniq.a$site.year.id[i]),
        all %>% subset(site_code==uniq.a$site_code[i] & year_trt
                        %in% c(0, year_trt=uniq.a$year_trt[i])),
         envir = .GlobalEnv)
}

View(a1)
a1$site.year.id<-factor(a1$site.year.id)
levels(a1$site.year.id)
View(uniq.a)


for (i in 1:length(all)){
  assign(paste0(all$site.year.id[i]),
         all %>% subset(all$site_code[i] & year_trt
                        %in% c(0, year_trt=all$year_trt[i])),
         envir = .GlobalEnv)
}

View(badlau.de_1)


for (i in 1:length(uniq.a)){
  assign(paste0(uniq.a$site.year.id[i]),
         all[(all$site_code==uniq.a$site_code[i] & all$year_trt %in%c(0, year_trt=uniq.a$year_trt[i])), ],
         envir = .GlobalEnv)
}

uniq.a$site.year.id<-as.factor(uniq.a$site.year.id)
levels(uniq.a$site.year.id)
View(badlau.de_1)

uniq.a$site.year.id<-as.factor(uniq.a$site.year.id)
levels(uniq.a$site.year.id)


View(azi.cn_2)
View(azi.cn_1)
azi.cn_1$site.year.id<-as.factor(azi.cn_1$site.year.id)
levels(azi.cn_1$site.year.id)


#next try to remove 0 levels
#write to list
#write list to rds files



for (i in 1:length(uniq.y)){
  assign(paste0(uniq.y$site.year.id[i]),
        all %>% subset(site_code==uniq.y$site_code[i] & all$year_trt
                        %in% c(0, year_trt=uniq.c$year_trt[i])),
         envir = .GlobalEnv)
}
View(badlau.de_2)

View(l1)






for (i in 1:length(uniq.a)){
  assign(paste0(uniq.a$site.year.id[i]),
         all[(uniq.a$site_code[i] & all$year_trt %in%c(all$year_trt=="0", all$year_trt=uniq.a$year_trt[i])), ],
         envir = .GlobalEnv)
}



##########
for (i in 1:length(uniq.a)){
  assign(paste0(uniq.a$site.year.id[i]),
         all %>% subset(uniq.a$site_code[i] & uniq.a$year_trt[i]),
         envir = .GlobalEnv)
}
View(azi.cn_2)
#########################


#FUCKING WINNER CHIECKEN DINNER
#works
ss2<-all[(all$site_code == "azi.cn" & all$year_trt %in%c(0, 1)), ]
View(ss2)
#works
subs<-all %>% which(all$site_code=="azi.cn" & all$year_trt %in%c(0, 1))
View(subs)
ss <- subset(all,(site_code=="azi.cn" & all$year_trt %in% c(0, 1)))
View(ss)
ss$site.year.id<-as.factor(ss$site.year.id)
levels(ss$site.year.id)


###################################################################
for (i in 1:length(uniq.y)) {
  assign(paste0(uniq.y$site.year.id[i]),
         all %>% subset(site_code==uniq.y$site_code[i] 
                        & year_trt==uniq.y$year_trt[i]))
         }

for (i in 1:length(uniq.c)) {
  assign(paste0(uniq.c$site.year.id[i]),
         all %>% subset(site_code==uniq.c$site_code[i] 
                        & year_trt==0))
}

for (i in 1:length(uniq.y)) {
  assign(paste0(uniq.y$site.year.id[i]),
         years %>% subset(site_code==uniq.y$site_code[i] 
                        & year_trt==uniq.y$year_trt[i]),
         controls %>% subset(site_code==uniq.c$site_code[i] 
                        & year_trt==0))
          do.call(rbind, foo)
}

View(y)

test<-bind_rows(azi.cn_0, azi.cn_1)
View(test)


  #subset across years  
for (i in 1:length(uniq.a)) {
    assign(paste0(uniq.a$site.year.id[i]),
           all %>% subset(site_code==uniq.a$site_code[i] 
                            & year_trt==uniq.a$year_trt[i]),
           envir = .GlobalEnv)}
View(azi.cn_1)

  
#subsets?

nn_pw <- function(x){
#subset across years  
for (i in 1:length(uniq.y)) {
  assign(paste0(uniq.y$site.year.id[i]),
  years %>% subset(site_code==uniq.y$site_code[i] 
                   & year_trt==uniq.y$year_trt[i]),
  envir = .GlobalEnv)
}

#subset comntrols
for (i in 1:length(uniq.c)){
  assign(paste0(uniq.c$site.year.id[i]),
  controls %>% subset(site_code==uniq.c$site_code[i] 
                   & year_trt==0),
  envir = .GlobalEnv)
}
View(badlau.de_1)


for (i in 1:length(uniq.c)){
  assign(paste0(uniq.c$site.year.id[i]),
         controls %>% subset(site_code==uniq.c$site_code[i] 
                             & year_trt==0),
         envir = .GlobalEnv)
}

?which
for (i in 1:length(uniq.a)){
  assign(paste0(uniq.a$site.year.id[i]),
         which(all$site_code==uniq.a$site_code[i] & all$year_trt
                       %in% c(0, year_trt==uniq.y$year_trt[i])),
         envir = .GlobalEnv)
}







#subset<-which(site_code==uniq.a$site_code[i] & year_trt %in%c(0, year_trt==uniq.y$year_trt[i]))

#subs<-which(site==1 & year %in%c(0, 1))
#or back to lists

ss<-subset(all, value %in% (all$site_code=="azi.cn" & all$year_trt ==0))




all$index<-paste(all$site_code, all$block, all$plot, sep="_")
all$index<-as.factor(all$index)
summary(all)
subs1<-all[which(site_code==1 & year_trt==1),]
subs2<-all[which(site_code==1 & year_trt==0),]

site.list.years <- split(years, years$site.year.id, drop=TRUE) 
site.list.controls <- split(controls, controls$site.year.id, drop=TRUE) 

[match(index[which(site_code==1 & year_trt==1)], index[which(site_code==1 & year_trt==0)])]


for (i in 1:length(uniq.a)){
  assign(paste0(uniq.a$site.year.id[i]),
         all %>% subset(uniq.a$site_code[i] & all$year_trt
                        %in% c(0, year_trt=uniq.a$year_trt[i])),
         envir = .GlobalEnv)
}


for (i in 1:length(uniq.y)){
  newdat<-lapply(site.list.years, function(x){
    out <- do.call(rbind, uniq.y[i])
    out$from <- gsub(".", "", rownames(out))
    rownames(out) <- NULL
    return(out)
  })
}





uniq.c <- distinct(controls,site_code, year_trt,site.year.id)
uniq.y <- distinct(years,site_code, year_trt,site.year.id)

site.list.years <- split(years, years$site.year.id, drop=TRUE) 

site.list.controls <- split(controls, controls$site.year.id, drop=TRUE) 

out<-mapply(rbind, site.list.years, site.list.controls, SIMPLIFY=F)


newlist<-bind_rows(site.list.years,site.list.controls)

View(newlist)

View(site.list.controls)
View(site.list.years)

df_list <- mget(uniq.c$site_code)
View(df_list)


View(bb)
out<-mapply(rbind, site.list.years, site.list.controls, SIMPLIFY=F)
View(out)
#do.call(rbind, lapply(l, function(x) x[match(names(l[[1]]), names(x))]))



index<-paste(site, block, plot)

subs1<-data[which(site==1 & year==1),]
subs2<-data[which(site==1 & year==0),]





View(ndat)
?lapply
for (i in 1:length(uniq.y)){
  newdat<-lapply(site.list.years, function(x){
  out <- do.call(rbind, uniq.y[i])
  out$from <- gsub(".", "", rownames(out))
  rownames(out) <- NULL
  return(out)
})
}

View(newdat)






uniq.y$site_code<-as.character(uniq.y$site.year.id)
match.y <- mget(uniq.y$site.year.id)
View(match.y)
uniq.c$site_code<-as.character(uniq.c$site_code)
match.c <- mget(uniq.c$site_code)
View(match.c)






??lapply



View(site.list.years)
View(site.list.controls)


View(azi.cn_0)
View(azi.cn_1)
??bind_rows
test<-bind_rows(azi.cn_0,azi.cn_1)
View(test)
test$site.year.id<-as.factor(test$site.year.id)
levels(test$site.year.id)

uniq.y$site_code<-as.character(uniq.y$site_code)
match.y <- mget(uniq.y$site_code)
uniq.c$site_code<-as.character(uniq.c$site_code)
match.c <- mget(uniq.c$site_code)
View(match.c)





uniq.c <- distinct(controls,site_code, year_trt,site.year.id)
uniq.y <- distinct(years,site_code, year_trt,site.year.id)

#split dataframe by study site
site.list.controls <- split(controls, controls$site_code, drop=TRUE) 
site.list.years <- split(years, years$site_code, drop=TRUE) 
View(site.list.years)


fdf <- lapply(funiq.y, function(x){
  out <- do.call(rbind, new_fruit_df_list[x])
  out$from <- gsub("(\\..*)", "", rownames(out))
  rownames(out) <- NULL
  return(out)
})




nestedlist <- lapply(site.list.years, function(x) split(x, x[['site.year.id']], drop = TRUE))



View(nestedlist)
list2env(nestedlist, envir= .GlobalEnv) 
list2env(site.list.controls, envir= .GlobalEnv) 


#????????
site.list.years <- split(years, years$site.year.id, drop=TRUE) 
list2env(site.list.years, envir= .GlobalEnv) 
list2env(site.list.controls, envir= .GlobalEnv) 

eapply(.GlobalEnv,is.data.frame)



#OR??? keep all controls and years together...but then how to bind??
sp3<-unite_(sp2, "site.year.id", c("site_code","year_trt"), remove=FALSE)
site.list.years <- split(sp3, sp3$site_code, drop=TRUE) 
#create a nested list
#https://stackoverflow.com/questions/46407320/data-frame-to-nested-list
nestedlist2 <- lapply(site.list.years, function(x) split(x, x[['year_trt']], drop = TRUE))
View(nestedlist2)

View(sp2)
View(nestedlist2)
View(nestedlist)
View(site.list.controls)



list2env(site.list, envir= .GlobalEnv) 
View(site.list.controls)
View(site.list.years)



levels(years$site.year.id)


write.csv(arch.us,"/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/subset_test.csv")


sp$site.yr<-as.character(with(sp, paste(site_code, year_trt, sep="_")))


sp$input<-sp$site.yr 
head(sp)
sp$input <- sp %>% group_indices(input) 
sp %>% distinct(site.yr,input)
sp$inputt<-"input-"
sp$input<-as.character(with(sp, paste(inputt,input, sep="")))
head(sp)
colnames((sp))
sp<-sp[,c(-66)]
spsplit <- split(sp, sp$input)
View(spsplit)
new_names <- as.character(unique(sp$input))
View(new_names)

View(input-5)

mapply(saveRDS, spsplit, file=paste0(names(spsplit), '.rds'))




native<-sp[sp$local_provenance %in% c('NAT'),]
introduced<-sp[sp$local_provenance %in% c('INT'),]