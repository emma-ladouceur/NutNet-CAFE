#install price tools
devtools::install_github("ctkremer/priceTools")

library(gridExtra)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyverse)


comb <- read.csv("~/Dropbox/NutNet data/comb-by-plot-01-November-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
cover<- read.csv("~/Dropbox/NutNet data/full-cover-01-November-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
biomass <- read.csv("~/Dropbox/NutNet data/full-biomass-01-November-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


#There are 0's when biomass wasnt collected.
#actually they are NA's for some reason 'view' shows them as 0's incorrectly
#comb %>%
#  mutate(live_mass = ifelse(rich > 0 & live_mass == 0, NA,comb$live_mass))


View(comb)
par(mfrow=c(1,2))
hist(comb$rich)
hist(comb$live_mass)

datnn2<- cover %>% group_by(year,site_name,site_code,block,plot,subplot,trt)

# remove NA's in 'Family'
# this removes litter, rocks, bryophytes with no species name....etc.
datnn3<-datnn2[complete.cases(datnn2$Family),]
datnn3$max_cover<-as.numeric(datnn3$max_cover)

# reduce to only relevant functional groups.....
levels(datnn3$functional_group)
levels(biomass$category)

#filter?
#cover.r<-datnn3 %>% filter(functional_group %in% c("FERN",'FORB',"GRAMINOID","GRASS","LEGUME","WOODY")) 

#biomass.r<-biomass %>%  filter(category %in% c("ANNUAL","FORB","GRAMINOID","LEGUME","LIVE","PERENNIAL","PTERIDOPHYTE","VASCULAR", "WOODY")) 

head(biomass)

head(datnn3)

# or... just match

cov.bm<- 
# recalculate live mass....

head(biomass.r)
datnn_bm<- biomass.r%>% group_by(year,site_name,site_code,block,plot,trt,year_trt) %>% summarise(live.cover=sum(max_cover))

datnn_cover<- datnn3 %>% group_by(year,site_name,site_code,block,plot,trt,year_trt) %>% summarise(live.cover=sum(max_cover))
View(datnn_cover)

View(comb)
comb$live_mass<-as.numeric(comb$live_mass)
datnn_cover2<-unite_(datnn_cover, "id", c("year","site_name","site_code","block","plot","trt","year_trt"), remove=FALSE)
View(datnn_cover2)
comb2<-unite_(comb, "id", c("year","site_name","site_code","block","plot","trt","year_trt"), remove=FALSE)


#plot level cover and plot env. data cobmined
datnn<-full_join(comb2,datnn_cover2, by= "id")
head(datnn)

View(datnn)
#make id label
datnn4<-unite_(datnn3, "id", c("year","site_name","site_code","block","plot","trt","year_trt"), remove=FALSE)
#join species level cover with plot data
datnn5<-full_join(datnn,datnn4, by= "id")

#biomass per species
datnn5$biomass.sp <- datnn5$max_cover/datnn5$live.cover * datnn5$live_mass

# subset NAT, INT, UNK
# calculate plot level biomass for each
nat <- droplevels(subset(datnn5, local_provenance == "NAT"))
int <- droplevels(subset(datnn5, local_provenance == "INT"))
unk <- droplevels(subset(datnn5, local_provenance == "UNK"))
nat2<- nat %>% group_by(id) %>% summarise(nat_biomass=sum(biomass.sp))
int2<- int %>% group_by(id) %>% summarise(int_biomass=sum(biomass.sp))
unk2<- unk %>% group_by(id) %>% summarise(unk_biomass=sum(biomass.sp))
head(unk2)
p.biomass<-full_join(nat2,int2,by="id")
View(p.biomass2)
p.biomass2<-full_join(p.biomass,unk2,by="id")
datnn6<-full_join(datnn5,p.biomass2,by="id")
head(datnn6)

write.csv(datnn6,"/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc.csv")

# clean it up, reduce it down
biomassc <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

bce <- droplevels(subset(biomassc, experiment_type == "Experimental (Full Factorial)"| experiment_type == "Experimental (Nutrients Only)"))
bce2 <- droplevels(subset(bce, trt=="NPK"|trt=="Control"))


bce2[131:161,]

colnames(bce2)
bce3 <- bce2[,c(-48,-49,-50,-51,-52,-53,-54,-56,-57,-58,-59,-60,-61,-62,-63)]
colnames(bce3)


colnames(bce3)
colnames(bce3)[colnames(bce3)=="site_name.x"] <- "site_name"
colnames(bce3)[colnames(bce3)=="site_code.x"] <- "site_code"
colnames(bce3)[colnames(bce3)=="block.x"] <- "block"
colnames(bce3)[colnames(bce3)=="plot.x"] <- "plot"
colnames(bce3)[colnames(bce3)=="year_trt.x"] <- "year_trt"
colnames(bce3)[colnames(bce3)=="trt.x"] <- "trt"
colnames(bce3)[colnames(bce3)=="year.x"] <- "year"
colnames(bce3)


nndistinct<-distinct(bce3,continent, country,site_code,year_trt,year)
# View(nndistinct)
levels(nndistinct$site_code)



# seperate NA's from 0's

# colnames(bce3)
# head(bce3)
# bce3$nat_biomass[is.na(bce3$nat_biomass)] <- 0
# bce3$int_biomass[is.na(bce3$int_biomass)] <- 0
# bce3$unk_biomass[is.na(bce3$unk_biomass)] <- 0
# summary(bce3)
#!!!!!!!!!!!!!!!!!
# filter manually, where live_mass is NA, NA belongs in biomass columns

#write.csv(bce3,"/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv")
write.csv(bce3,"/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv")

# PLOT LEVEL
biomassc <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

biomassc$rich<-as.numeric(biomassc$rich)
nrow(biomassc)
colnames(biomassc)
head(biomassc)
# drop sp columns
biomassc2<-biomassc[ , -which(names(biomassc) %in% c("X.1","X","Family","Taxon","local_provenance","local_lifeform","local_lifespan","functional_group","N_fixer","ps_path","max_cover","biomass.sp"))]
colnames(biomassc2)
biomassc3 <- unique(biomassc2)
head(biomassc3)
nrow(biomassc3)


par(mfrow=c(1,2))
hist(biomassc3$rich)
hist(biomassc3$live_mass)

#quick look
ggplot() +
  geom_boxplot(data = biomassc3,
               aes(x = trt, y = rich)) +
  #scale_y_continuous(trans = 'log2') +
  theme_bw()

ggplot() +
  geom_boxplot(data = biomassc3,
               aes(x = trt, y = live_mass)) +
  theme_bw()

View(biomassc3)


write.csv(biomassc3,"/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv")





#NEXT
library(gridExtra)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyr)
library(dplyr)

library(priceTools)

#First
#Remove unlive material
#DO BIOMASS CALC
#Manually reduce main nut net file/ or write code for this eventually (pls)
#Only year 0, 3, 4 (just in case) & 5
#Only Control + NPK
#only experimental
#combine year_trt and trt to trt_year
#reduce and bind pariwsise comparisons below
#then seperate year 3 and year 5 (edit subsets below to inclkude year 3)

setwd("/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/")
nn5 <- read.csv("year3_live.csv", sep=",",header=T, strip.white=T)
levels(nn5$trt_year)

group.vars <- c('site_name')
#group.vars1 <- c('block')
#group.vars2 <- c('plot')
treat.vars<-c('trt_year')
grouped.data1 <- nn5 %>% group_by_(.dots=c(group.vars,treat.vars))

#takes a long time
res1 <- pairwise.price(grouped.data1, species="Taxon", func="Biomass_CalcSp")
pp1<-res1
pp1<-group.columns(pp1,gps=c(group.vars,treat.vars),drop=T)
View(pp1)
levels(pp1$trt_year)
dat1<-pp1[pp1$trt_year %in% c('Control_0 Control_0','Control_3 Control_3','Control_0 NPK_3','Control_3 NPK_3','NPK_0 NPK_3', 'Control_0 Control_3'),]
dat1<-pp1[pp1$trt_year %in% c('Control_0 Control_0','Control_5 Control_5','Control_0 NPK_5','Control_5 NPK_5','NPK_0 NPK_5', 'Control_0 Control_5'),]

write.csv(dat1,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/pp1_site_treat_year3_live_sp.csv")



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#REMOVE APOSTREPHES '''''''''''''''''
dat1 <- read.csv("pp1_site__plot_treat_year3_live_sp_2.csv", sep=",",row.names=1,header=T, strip.white=T)
dat1 <- read.csv("pp1_site_block_plot_treat_year3_all_sp.csv", sep=",",row.names=1,header=T, strip.white=T)
dat1 <- read.csv("pp1_site_treat_year3_live_sp.csv", sep=",",header=T, strip.white=T)


View(dat1)
levels(dat2$trt_year)
dat2<-dat1[dat1$site_name %in% c('Bogong Bogong'),]
dat3<-dat1[dat1$site_name %in% c('Boulder South Campus Boulder South Campus'),]
dat4<-dat1[dat1$site_name %in% c('Bunchgrass (Andrews LTER) Bunchgrass (Andrews LTER)'),]
dat5<-dat1[dat1$site_name %in% c('Burrawan Burrawan'),]
dat6<-dat1[dat1$site_name %in% c('Cedar Creek LTER Cedar Creek LTER'),]
dat7<-dat1[dat1$site_name %in% c('Cedar Point Biological Station Cedar Point Biological Station'),]
dat8<-dat1[dat1$site_name %in% c('CEREEP - Ecotron IDF CEREEP - Ecotron IDF'),]
dat9<-dat1[dat1$site_name %in% c('Chichaqua Bottoms Chichaqua Bottoms'),]
dat10<-dat1[dat1$site_name %in% c('Cowichan Cowichan'),]
dat11<-dat1[dat1$site_name %in% c('Elliott Chaparral Elliott Chaparral'),]
dat12<-dat1[dat1$site_name %in% c('Fruebuel Fruebuel'),]
dat13<-dat1[dat1$site_name %in% c('Halls Prairie Halls Prairie'),]
dat14<-dat1[dat1$site_name %in% c('Hart Mountain Hart Mountain'),]
dat15<-dat1[dat1$site_name %in% c('Heronsbrook (Silwood Park) Heronsbrook (Silwood Park)'),]
dat16<-dat1[dat1$site_name %in% c('Hopland REC Hopland REC'),]
dat17<-dat1[dat1$site_name %in% c('Kinypanial Kinypanial'),]
dat18<-dat1[dat1$site_name %in% c('Koffler Scientific Reserve at Jokers Hill Koffler Scientific Reserve at Jokers Hill'),]
dat19<-dat1[dat1$site_name %in% c('Konza LTER Konza LTER'),]
dat20<-dat1[dat1$site_name %in% c('Lancaster Lancaster'),]
dat21<-dat1[dat1$site_name %in% c('Lookout (Andrews LTER) Lookout (Andrews LTER)'),]
dat22<-dat1[dat1$site_name %in% c('Mar Chiquita Mar Chiquita'),]
dat23<-dat1[dat1$site_name %in% c('Mclaughlin UCNRS Mclaughlin UCNRS'),]
dat24<-dat1[dat1$site_name %in% c('Mt. Caroline Mt. Caroline'),]
dat25<-dat1[dat1$site_name %in% c('Papenburg Papenburg'),]
dat26<-dat1[dat1$site_name %in% c('Rookery (Silwood Park) Rookery (Silwood Park)'),]
dat27<-dat1[dat1$site_name %in% c('Sagehen Creek UCNRS Sagehen Creek UCNRS'),]
dat28<-dat1[dat1$site_name %in% c('Saline Experimental Range Saline Experimental Range'),]
dat29<-dat1[dat1$site_name %in% c('Savannah River Savannah River'),]
dat30<-dat1[dat1$site_name %in% c('Sedgwick Reserve UCNRS Sedgwick Reserve UCNRS'),]
dat31<-dat1[dat1$site_name %in% c('Sevilleta LTER Sevilleta LTER'),]
dat32<-dat1[dat1$site_name %in% c('Sheep Experimental Station Sheep Experimental Station'),]
dat33<-dat1[dat1$site_name %in% c('Shortgrass Steppe LTER Shortgrass Steppe LTER'),]
dat34<-dat1[dat1$site_name %in% c('Sierra Foothills REC Sierra Foothills REC'),]
dat35<-dat1[dat1$site_name %in% c('Smith Prairie Smith Prairie'),]
dat36<-dat1[dat1$site_name %in% c('Spindletop Spindletop'),]
dat37<-dat1[dat1$site_name %in% c('Temple Temple'),]
dat38<-dat1[dat1$site_name %in% c('Trelease Trelease'),]
dat39<-dat1[dat1$site_name %in% c('Ukulinga Ukulinga'),]
dat40<-dat1[dat1$site_name %in% c('Val Mustair Val Mustair'),]

#YEAR 3
dat41<-dat1[dat1$site_name %in% c('Companhia das Lezirias Companhia das Lezirias'),]
dat42<-dat1[dat1$site_name %in% c('Duke Forest Duke Forest'),]
dat43<-dat1[dat1$site_name %in% c('Hanover Hanover'),]
dat44<-dat1[dat1$site_name %in% c('Kibber (Spiti) Kibber (Spiti)'),]
dat45<-dat1[dat1$site_name %in% c('Las Chilcas Las Chilcas'),]
dat46<-dat1[dat1$site_name %in% c('Mt Gilboa Mt Gilboa'),]
dat47<-dat1[dat1$site_name %in% c('Pingelly Paddock Pingelly Paddock'),]
dat48<-dat1[dat1$site_name %in% c('Pinjarra Hills Pinjarra Hills'),]
dat49<-dat1[dat1$site_name %in% c('Saana Saana'),]
dat50<-dat1[dat1$site_name %in% c('Serengeti Serengeti'),]
dat51<-dat1[dat1$site_name %in% c('Summerveld Summerveld'),]


??bind_rows
#year5
dat<-bind_rows(dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12,dat13,dat14,dat15,dat16,dat17,dat18,dat19,dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,dat31,dat32,dat33,dat34,dat35,dat36,dat37,dat38,dat39,dat40)
#year3
dat<-bind_rows(dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9,dat10,dat11,dat12,dat13,dat14,dat15,dat16,dat17,dat18,dat19,dat20,dat21,dat22,dat23,dat24,dat25,dat26,dat27,dat28,dat29,dat30,dat31,dat32,dat33,dat34,dat35,dat36,dat37,dat38,dat39,dat40,dat41,dat42,dat43,dat44,dat45,dat46,dat47,dat48,dat49,dat50,dat51)

#PLOT
binddata_all<-dat[dat$plot %in% c('1 1','2 2','3 3','4 4','5 5','6 6','7 7','8 8','9 9','10 10','11 11','12 12','13 13','14 14','15 15','16 16','17 17','18 18','19 19','20 20','21 21','22 22','23 23','24 24','25 25','26 26','27 27','28 28','29 29','30 30','31 31','32 32','33 33','34 34','35 35','36 36','37 37','38 38','39 39','40 40','41 41','42 42','43 43','44 44','45 45','46 46','47 47','48 48','49 49','50 50','51 51','52 52','53 53','54 54','55 55','56 56','57 57','58 58','59 59','60 60','61 61','62 62'),]



levels(binddata_all$trt_year)
write.csv(binddata_all,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/bind_price_year5.csv")
write.csv(binddata_all,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/bind_price_year3.csv")
write.csv(dat,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/site_bind_year3.csv")

nn5p <- read.csv("delta_plot_bind_year3.csv", sep=",",header=T, strip.white=T)
nn5 <- read.csv("year3_env.csv", sep=",",header=T, strip.white=T)
nn5p <- read.csv("bind_price_year3.csv", sep=",",header=T, strip.white=T)
nn5p <- read.csv("site_bind_year3.csv", sep=",",header=T, strip.white=T)
levels(nn5p$trt_year)
pricemerge<- merge(nn5, nn5p, by = c("price"))

View(pricemerge)
write.csv(pricemerge,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/bind_price_year5_2.csv")
write.csv(pricemerge,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/bind_price_continent_year3_2.csv")
write.csv(pricemerge,"/Users/el50nico/Desktop/Academic/Manuscripts/2018_6 _NutNet_biomass/delta_plot_bind_year3_2.csv")

