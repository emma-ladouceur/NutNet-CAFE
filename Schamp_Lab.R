


library(tidyverse)

p <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

biomass <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

#sp
colnames(biomass)

biomass.clean <- biomass %>% dplyr::select(site_code,year, year_trt, block, plot, trt, Taxon,local_lifeform,local_lifespan,local_provenance,
                                    max_cover, biomass.sp.full) %>%
  mutate(biomass.sp=biomass.sp.full) %>% dplyr::select(-biomass.sp.full)

head(biomass.clean)


write.csv(biomass.clean, "~/Dropbox/Projects/NutNet Co-oc/Schamp/biomass_sp.csv")


#plot
colnames(p)

p.clean <- p %>% dplyr::select(site_code,year,year_trt,block,plot,trt,plot.cover,plot.mass,all.div,int.div,nat.div,sum_INT_cover,sum_NAT_cover,sum_UNK_cover,
                        site_richness,site_native_richness,site_introduced_richness) %>%
  mutate(plot.rich=all.div,
         plot.int.rich=int.div,
         plot.nat.rich=nat.div) %>% dplyr::select(-c(all.div,int.div,nat.div))

head(p.clean)

write.csv(p.clean, "~/Dropbox/Projects/NutNet Co-oc/Schamp/plot.csv")


#pairwise
head(p.all)
price.clean <- p.all %>% dplyr::select(-c(X.1,X,SRE.L, SRE.G, SIE.L,SIE.G,CE,SR,site.year.id,site.year.id.x,site.year.id.y,
                                          trt_year,trt_year.x,trt_year.y,trt.x,trt.y,year.y,year.x,plot.x,plot.y,unique.id,year.y.m, SR)) 


View(price.clean)

write.csv(price.clean, "~/Dropbox/Projects/NutNet Co-oc/Schamp/pairwise_plots.csv")
