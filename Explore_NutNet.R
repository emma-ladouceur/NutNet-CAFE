# Nutrient Network Descriptive Statistics for website and general use
# Author: Emma Ladouceur
# Email: emmala@gmail.com

library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)


#just use the here package to locate data
# you can update this script with new comb-by-plot versions just by changing this line
nn <- read.csv("~/Dropbox/NutNet data/comb-by-plot-25-January-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

# SOME DATA WRANGLING FIRST
# create a new dataset of some basics from comb-by-plot

# get a list of unique sites, continent, habitat, year_trt year and experiment type
colnames(nn)
unique.sites <- unique(nn[c("site_name","country","continent","habitat","year_trt","year","experiment_type")])
# add a column to count the number of sites
unique.sites$site_n<-"1"
#write to a new file
View(unique.sites)
write.csv(unique.sites, "/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_1.csv", row.names=F)

# and a second dataset that summarizes sites, by only their max year
nn_agg <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_1.csv", sep=",",header=T, strip.white=T)
# take only the max year of every site
nn_agg_2<-nn_agg %>% group_by(site_name) %>% top_n(1, year_trt)
View(nn_agg_2)
??group_by
write.csv(nn_agg_2, "/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_2.csv", row.names=F)



nnr <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_2.csv", sep=",",header=T, strip.white=T)
nnr2 <- droplevels(subset(nnr, experiment_type == "Experimental (Full Factorial)"| experiment_type == "Experimental (Nutrients Only)"))

colnames(nnr2)
nnr2$year_trt<-as.factor(nnr2$year_trt)
nnr3<- nnr2[nnr2$year_trt != "0" ,]
nnr4<- nnr3[nnr3$year_trt != "1" ,]
levels(nnr3$year_trt)
View(nnr4)

write.csv(nnr4, "/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_3.csv", row.names=F)





# Okay now ready to plot descriptive statistics
# PLOT 1: YEAR X SITE
nn_agg <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_1.csv", sep=",",header=T, strip.white=T)

# !!!!!!! ATTENTION: Optional to remove observational sites, 
# if you want to exclude them just remove the # and run this line
#nn_agg <- nnr[nnr$experiment_type != "Observational",]
#aggregate data
nn_agg$year_trt<-as.factor(nn_agg$year_trt)
nngroup <- group_by(nn_agg, year_trt)
year_site <- summarize(nngroup, site_n = n())

# plot total number of sites, and years 
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
year_site$year_trt<-as.factor(year_site$year_trt)
year_site$site_n<-as.numeric(year_site$site_n)
YearSiteFig <- ggplot(year_site, aes(x = year_trt, y = site_n))+ geom_bar(aes(color = year_trt),
                                                                          stat="identity", fill="white",show.legend=F)+theme_classic()
YearSiteFig


# PLOT 2: EXPERIMENT TYPE
nn_agg <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_1.csv", sep=",",header=T, strip.white=T)
# Again, below is optional to remove observational sites, or whatever you want...
nn_agg2 <- nn_agg[nn_agg$experiment_type != "Observational",]
#aggregate data
nn_agg2$year_trt<-as.factor(nn_agg2$year_trt)
nngroup2 <- group_by(nn_agg2, year_trt, experiment_type)
year_site_s <- summarize(nngroup2, site_n = n())

# stack & color by experimental type
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
year_site_s$year_trt<-as.factor(year_site_s$year_trt)
year_site_s$site_n<-as.numeric(year_site_s$site_n)
YearSiteStackFig <- ggplot(year_site_s, aes(x = year_trt, y = site_n, fill=experiment_type))+geom_bar(aes(color = experiment_type),
                                                                                                      stat="identity", fill="white")+theme_classic()+ theme(legend.position="bottom")
YearSiteStackFig

#vertical
grid.arrange(YearSiteFig,YearSiteStackFig,nrow=2)
#horizontal
grid.arrange(YearSiteFig,YearSiteStackFig,nrow=1)

# PLOT 3: 
# continent x experimental type x habitat x number of years
nnr <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_2.csv", sep=",",header=T, strip.white=T)

# no numbers, faceted by experiment type
#you can facet anything you want, like year
nnr$year_trt<-as.factor(nnr$year_trt)
nnr$site_n<-as.numeric(nnr$site_n)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
ConHab <- ggplot(nnr, aes(x = continent, y = site_n))+scale_x_discrete(limits = rev(levels(nnr$continent)))+ geom_bar(aes(color = habitat),
                                                                                                                      stat="identity", fill="white")+coord_flip() + facet_grid(experiment_type ~ .)+theme_classic()+ theme(legend.position="bottom")
ConHab

#PLOT 4:
# same same but all new now with numbers
nnr <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_2.csv", sep=",",header=T, strip.white=T)

ggplot(data = nnr, aes(x = continent, y = site_n, color=habitat)) +scale_x_discrete(limits = rev(levels(nnr$continent)))+
  geom_col(fill = "white") +
  geom_text(aes(label = year_trt),
            position = position_stack(vjust = .5))+coord_flip()+facet_grid(experiment_type ~ .)+theme_classic()+ theme(legend.position="bottom")

#PLOT 5:
# subset just  the full factorial experimental sites

nnr2 <- droplevels(subset(nnr, experiment_type == "Experimental (Full Factorial)"))

View(nnr2)

nnr2$year_trt<-as.factor(nnr2$year_trt)
nnr2$site_n<-as.numeric(nnr2$site_n)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
f <- ggplot(nnr2, aes(x = continent, y = site_n))+scale_x_discrete(limits = rev(levels(nnr$continent)))+ggtitle("Experimental (Full Factorial) Plots") 
# Change bar plot line colors by groups
f + geom_bar(aes(color = habitat),
             stat="identity", fill="white")+coord_flip() +theme_classic()+ theme(legend.position="bottom")




#PLOT 6:
# change y axis to habitats and colors to continents
nnr <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_2.csv", sep=",",header=T, strip.white=T)

ggplot(data = nnr, aes(x = habitat, y = site_n, color=continent)) +scale_x_discrete(limits = rev(levels(nnr$habitat)))+
  geom_col(fill = "white") +
  geom_text(aes(label = year_trt),
            position = position_stack(vjust = .5))+coord_flip()+facet_grid(experiment_type ~ .)+theme_classic()+ theme(legend.position="bottom")

#PLOT 7:
# drop observational and look at all experimental sites together
nnr <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_2.csv", sep=",",header=T, strip.white=T)
nnr <- nnr[nnr$experiment_type != "Observational",]

ggplot(data=nnr, aes(x=habitat, y=site_n, color=continent)) +scale_x_discrete(limits = rev(levels(nnr$habitat)))+
  geom_col(fill = "white") +
  geom_text(aes(label = year_trt),
            position = position_stack(vjust = .5))+coord_flip()+theme_classic()+ theme(legend.position="bottom")




#montane
nn <- read.csv("~/Dropbox/NutNet data/comb-by-plot-31-August-2018.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
colnames(nn)
levels(nn$habitat)
nnm <- droplevels(subset(nn, habitat=="montane grassland"))
View(nnm)

unique.sites <- unique(nnm[c("site_name","country","continent","habitat","year_trt","year","experiment_type","elevation","longitude","latitude")])
# add a column to count the number of sites
unique.sites$site_n<-"1"
View(unique.sites)

nnm2<-unique.sites %>% group_by(site_name) %>% top_n(1, year_trt)
nnm2$year_trt<-as.factor(nnm2$year_trt)

ggplot(data = nnm2, aes(x = site_name, y = elevation, color=year_trt)) +scale_x_discrete(limits = rev(levels(nnr$csite_name)))+
  geom_col(fill = "white") +
  geom_text(aes(label = year_trt),
            position = position_stack(vjust = .5))+coord_flip()+theme_classic()+ theme(legend.position="bottom")


nnm.dat <- droplevels(subset(nnm, trt=="Control"))
View(nnm.dat)



#practical plot, how many sites will i have for specific years

nnr <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/basics_2.csv", sep=",",header=T, strip.white=T)

nnr2 <- droplevels(subset(nnr, experiment_type == "Experimental (Full Factorial)"| experiment_type == "Experimental (Nutrients Only)"))

nnr2$year_trt<-as.factor(nnr2$year_trt)
nnr3<- nnr2[nnr2$year_trt != "0" ,]
nnr4<- nnr3[nnr3$year_trt != "1" ,]
nnr5<- nnr4[nnr4$year_trt != "2" ,]
nnr5<- nnr5[nnr5$year_trt != "3" ,]
nnr5<- nnr5[nnr5$year_trt != "4" ,]
View(nnr5)

nnr5$year_trt<-as.factor(nnr5$year_trt)
nnr5$site_n<-as.numeric(nnr5$site_n)
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
f <- ggplot(nnr5, aes(x = continent, y = site_n))+scale_x_discrete(limits = rev(levels(nnr$continent)))+ggtitle("Experimental  Plots (Full Factorial & Nutrients Only)") 
# Change bar plot line colors by groups
f + geom_bar(aes(color = habitat),
             stat="identity", fill="white")+coord_flip() +theme_classic()+ theme(legend.position="bottom")

nrow(nnr5)
# 3 years = 62 sites
# 4 years =51
# 5 years =45


write.csv(nnr5, "/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/sites_3.csv", row.names=F)

