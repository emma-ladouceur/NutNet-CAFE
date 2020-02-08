



clim <- read.csv("~/Dropbox/Projects/NutNet/Data/site-worldclim-2-August-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


View(clim)


clim$Realm <- ifelse(clim$latitude.p > 23.5 & clim$latitude.p < 60, 'Temperate',
                                 ifelse(clim$latitude.p >23.5 , 'Tropical',
                                        ifelse(clim$latitude.p  < 60, 'Polar', 'other')))



clim$NDep.cats <- ifelse(clim$NDep >= 30.01 & clim$NDep <= 35.91, '30.01-35.91',
                     ifelse(clim$NDep >= 25.01 & clim$NDep <= 30.00, '25.01-30.00',
                            ifelse(clim$NDep >= 20.01 & clim$NDep <= 25.00, '20.01-25.00',
                                   ifelse(clim$NDep >= 15.01 & clim$NDep <= 20.00, '15.01-20.00',
                                          ifelse(clim$NDep >= 10.01 & clim$NDep <= 15.00, '10.01-15.00',
                                                 ifelse(clim$NDep >= 5.01 & clim$NDep <= 10.00, '5.01-10.00',
                                                        ifelse(clim$NDep >= 2.51 & clim$NDep <= 5.00, '2.51-5.00',
                                                               ifelse(clim$NDep >= 1.00 & clim$NDep <= 2.50, '1.00-2.50',
                                                                      ifelse(clim$NDep < 1.0 , '< 1', 'other')))))))))
View(clim)

clim$NDep.cats<-as.factor(as.character(clim$NDep.cats))

summary(clim)



#plot dat
plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot <- droplevels( plot[-which(plot$year.zero.only == "1"), ] )
plot <- droplevels( plot[-which(plot$no.year.zero == "1"), ] )

pdeets<-distinct(plot,site_code,block,plot,trt,year_trt,rich,NAT_rich,INT_rich,UNK_rich,live_mass)
p.x<-pdeets[pdeets$year_trt %in% c('0'),]
p.y<-pdeets[pdeets$year_trt != "0",]
colnames(p.x)

colnames(p.x)[5] <-"year_trt.x"
colnames(p.x)[6] <-"rich.x"
colnames(p.x)[7] <-"NAT_rich.x"
colnames(p.x)[8] <-"INT_rich.x"
colnames(p.x)[9] <-"UNK_rich.x"
colnames(p.x)[10] <-"live_mass.x"

colnames(p.y)[5] <-"year_trt.y"
colnames(p.y)[6] <-"rich.y"
colnames(p.y)[7] <-"NAT_rich.y"
colnames(p.y)[8] <-"INT_rich.y"
colnames(p.y)[9] <-"UNK_rich.y"
colnames(p.y)[10] <-"live_mass.y"


p.xy<-left_join(p.x,p.y)
View(p.xy)

View(plot)
p2<-distinct(plot,elevation,latitude.p,latitude,longitude,continent,country,site_code,region,grazed,burned,managed,anthropogenic,habitat,site_richness,site_native_richness,site_introduced_richness)

p2$site_percent_native_rich<- (p2$site_native_richness/p2$site_richness) * 100
p2$site_percent_int_rich<- (p2$site_introduced_richness/p2$site_richness) * 100

p2$site_dom <- ifelse(p2$site_percent_native_rich >= p2$site_percent_int_rich , 'native dominated',
                      ifelse(p2$site_percent_int_rich  >= p2$site_percent_native_rich, 'introduced dominated','other'))

View(p.xy)
rr<-p.xy %>% group_by(site_code) %>%
  summarise(s.rich = mean(rich.x),
            r.rich = round(s.rich))

View(p2)
meta<-left_join(p2,rr)

View(meta)


# climate data longitude has some different decimal points than
# also a couple latitudes
# nut net site data (suspect rounded differently)
# so dont match by longitude or latitude

plot_clim <- left_join(meta,clim, by="site_code")


View(plot_clim)

write.csv(plot_clim,"~/Dropbox/Projects/NutNet/Data/plot_clim.csv" )



