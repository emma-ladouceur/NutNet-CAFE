


# calculate life form dominance

# calculate native vs exotic domionance


cov<- read.csv("~/Dropbox/NutNet data/full-cover-01-November-2019.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

meta <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

cov<-cov[complete.cases(cov$Family),]

cov$sp.pres<-"1"
cov$sp.pres<-as.numeric(cov$sp.pres)


dom.sum<-cov %>% distinct(site_code,year_trt,Taxon,functional_group,sp.pres) %>%
  group_by(site_code,functional_group,year_trt) %>%
  summarise(func.rich=sum(sp.pres)) %>%
  arrange(site_code,year_trt)

head(dom.sum,n=20)
dom.sum.wide<-dom.sum %>% group_by(site_code,year_trt)%>%
  spread(functional_group,func.rich) %>%
  select(FORB,GRAMINOID,GRASS,LEGUME)

head(dom.sum.wide,n=20)


dom.sum.check<-meta %>% distinct(site_code,year_trt,site_year_rich)%>%left_join(dom.sum.wide) %>%
  select(site_code, year_trt, site_year_rich, FORB,GRAMINOID,GRASS,LEGUME)

head(dom.sum.check,n=20)

dom.sum.check$percent_forb<- (dom.sum.check$FORB/dom.sum.check$site_year_rich) * 100
dom.sum.check$percent_gram<- (dom.sum.check$GRAMINOID/dom.sum.check$site_year_rich) * 100
dom.sum.check$percent_grass<- (dom.sum.check$GRASS/dom.sum.check$site_year_rich) * 100
dom.sum.check$percent_legume<- (dom.sum.check$LEGUME/dom.sum.check$site_year_rich) * 100



dom.sum.check$site_dom_func <- ifelse(dom.sum.check$percent_forb >= dom.sum.check$percent_grass , 'forb sp. dominated',
                      ifelse(dom.sum.check$percent_grass  >= dom.sum.check$percent_forb, 'grass sp. dominated','other'))


head(dom.sum.check,n=20)


dom.sums<-dom.sum.check %>% select(site_code,year_trt,FORB,GRAMINOID,GRASS,percent_forb,percent_grass,percent_gram,site_dom_func) %>%
  distinct(site_code,year_trt,site_dom_func)

head(dom.sums)

View(dom.sums)

meta.clim <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


head(meta.clim)







