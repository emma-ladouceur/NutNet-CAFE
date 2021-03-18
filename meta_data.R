

library(tidyverse)




# Fig 2

library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)



load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/bm.Rdata') # plot.bm.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/rich.Rdata') # plot.rich.g


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/bm.mod.dat.Rdata')



load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/p.effs.Rdata')

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/study.p.effs.Rdata')

rich.p2 <-rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) %>%
  filter(response=="NPK")

bm.p2<-bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) %>%
  filter(response=="NPK")

effs.p <- rich.p2 %>% left_join(bm.p2)
 
effs.p

study.rich.p2 <-study.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) 

study.bm.p2<-study.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) 

study.effs.p <- study.rich.p2 %>% left_join(study.bm.p2) %>% filter(response == "NPK")

colnames(study.effs.p)

study.effs.p$Quadrant <- ifelse(study.effs.p$r.eff < 0 & study.effs.p$b.eff > 0, '-rich +biomass',
                                ifelse(study.effs.p$r.eff < 0 & study.effs.p$b.eff < 0,  '-rich -biomass',
                                       ifelse(study.effs.p$r.eff  > 0 & study.effs.p$b.eff > 0,  '+rich +biomass',
                                              ifelse(study.effs.p$r.eff > 0 & study.effs.p$b.eff < 0, '+rich -biomass','other'))))
View(study.effs.p)

study.effs.p$Quadrant <- factor(study.effs.p$Quadrant, levels= c("-rich +biomass",  "+rich +biomass", "-rich -biomass", "+rich -biomass"))


bef.cloud <- ggplot()+
  facet_wrap(~Quadrant) +
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_point(data=study.effs.p, aes(x= r.eff , y= b.eff), colour="black",alpha=0.2,size=2) +
  geom_errorbar(data=study.effs.p,aes(x= r.eff, y= b.eff,ymin = b.eff_lower, ymax = b.eff_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(data=study.effs.p,aes(x= r.eff, y= b.eff,xmin =  r.eff_lower, xmax =r.eff_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
 # geom_text(data=study.effs.p, aes(x= r.eff , y= b.eff, label=site_code))+
  # geom_point(data = effs.p, aes(x= r.eff, #loss
  #                               y=  b.eff ),
  #            fill="#0B775E",color="#0B775E",size=8, alpha=0.6)+
  # geom_errorbar(data = effs.p,aes(x=r.eff,
  #                                 ymin = b.eff_lower, ymax = b.eff_upper),width=0,colour = "#0B775E", size = 2,alpha=0.9) +
  # geom_errorbarh(data = effs.p,aes(y=b.eff,
  #                                  xmin = r.eff_lower, xmax = r.eff_upper),height=0,colour = "#0B775E", size = 2, alpha=0.9) +
  scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  labs(x = 'Rate of change in species richness (species/year)',
       y = expression(paste('Rate of change in plot biomass (g/' ,m^2, '/year)')),
       # title= 'Control Slope + NPK Slope'
       title = ' ')+ theme_classic(base_size=14) + theme(strip.text = element_text(size=14))

bef.cloud



Quads <- study.effs.p %>% select(site_code, Quadrant)


View(Quads)


write.csv(Quads,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv" )



# meta data for fig 5
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
biogeo <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biogeographic_realms.csv", stringsAsFactors = FALSE)
site.include <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_S1.csv", stringsAsFactors = FALSE)
quads <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv", stringsAsFactors = FALSE)

colnames(plot)


plot %>% distinct(site_code, max.year)

plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()


colnames(plot)
pdeets<-distinct(plot,site_code,block,plot,trt,year_trt,all.div,nat.div,int.div,unk.div,plot.mass)
p.x<-pdeets[pdeets$year_trt %in% c('0'),]
p.y<-pdeets[pdeets$year_trt != "0",]
colnames(p.x)

colnames(p.x)[2] <-"year_trt.x"
colnames(p.x)[7] <-"all.div.x"
colnames(p.x)[9] <-"nat.div.x"
colnames(p.x)[8] <-"int.div.x"
colnames(p.x)[10] <-"unk.div.x"
colnames(p.x)[5] <-"plot.mass.x"

colnames(p.y)[2] <-"year_trt.y"
colnames(p.y)[7] <-"all.div.y"
colnames(p.y)[9] <-"nat.div.y"
colnames(p.y)[8] <-"int.div.y"
colnames(p.y)[10] <-"unk.div.y"
colnames(p.y)[5] <-"plot.mass.y"


p.xy<-left_join(p.x,p.y)
View(p.xy)

View(plot)
p2<-distinct(plot,elevation,latitude,longitude,continent,country,site_code,region,grazed,burned,managed,anthropogenic,habitat,site_richness,site_native_richness,site_introduced_richness)

p2$site_percent_native_rich<- (p2$site_native_richness/p2$site_richness) * 100
p2$site_percent_int_rich<- (p2$site_introduced_richness/p2$site_richness) * 100

p2<-p2 %>% mutate(site_percent_int_rich= ifelse(is.na(site_percent_int_rich), 0, site_percent_int_rich),
                  site_percent_native_rich= ifelse(is.na(site_percent_native_rich), 0, site_percent_native_rich) )

p2$site_dom <- ifelse(p2$site_percent_native_rich >= p2$site_percent_int_rich , 'native dominated',
                      ifelse(p2$site_percent_int_rich  >= p2$site_percent_native_rich, 'introduced dominated','other'))

View(p.xy)
rr<-p.xy %>% group_by(site_code) %>%
  summarise(s.rich = mean(all.div.x),
            r.rich = round(s.rich))
  #           max.year = max(year_trt.y)) %>%
  # filter(max.year >= 3)  %>%
  # filter(!is.na(max.year)) %>% droplevels()

View(p2)
meta<-left_join(rr,p2) %>% select(-country)

View(meta)
 meta.include <- site.include %>% left_join(meta, by="site_code") %>% left_join(biogeo,by="country") %>% left_join(quads,by="site_code") %>%
   select(-X.x,-X.y)

 
 View(meta.include)

write.csv(meta.include,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/meta.csv" )


sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sp$pres <- 1

colnames(sp)

explore <- sp %>% select(site_code,trt,year_trt,Taxon,category.mod,cat.cover,local_lifeform,local_lifespan,local_provenance)


View(explore)
