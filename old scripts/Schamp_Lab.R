


library(tidyverse)





p <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

biomass <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

bce <- droplevels(subset(biomass, experiment_type == "Experimental (Full Factorial)"| experiment_type == "Experimental (Nutrients Only)"))

# this site is categorized as full factorial but only has controls
bce2 <- droplevels(subset(bce, !site_code=="nilla.au"))

bce3<-bce2 %>% filter(!is.na(biomass.sp.full)) %>% droplevels()

check<-bce3 %>% distinct(site_code,year_trt)
View(check)

# remove sites that dont have a year 0 or only have year 0
bce4 <- bce3 %>% group_by(site_code) %>%
  summarise(year_min=min(year_trt),
            year_max=max(year_trt)) %>%
  mutate(year.zero = ifelse(year_min <= 0 & year_max > 0, "keep",'remove')) %>%
  left_join(bce3)

#sp
colnames(bce4)



check<-bce4 %>% distinct(site_code,year_trt,year.zero)
View(check)

bce5 <- droplevels( bce4[-which(bce4$year.zero == "remove"), ] )
colnames(bce5)

bsites<- bce5 %>% distinct(site_code)
View(bsites)


bce6<- bce5 %>% select(id,site_code,experiment_type,year,year_trt,trt,block,plot,plot.cover,orig.bm.cat,category.mod,cat.cover,subplot.cov,subplot.bm,local_lifeform,local_lifespan,functional_group,Taxon,max_cover,category,mass,cat.mass,plot.mass,biomass.sp.plot,biomass.sp.cat,biomass.sp.full,biomass.m.full)

View(bce6)

site.inclusion<-bce6 %>% distinct(site_code)


cover<- read.csv("~/Dropbox/Projects/NutNet/Data/full-cover-24-September-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
cover2<-unite_(cover, "id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE)

species.nn <- bce6 %>%  left_join(cover2)


biomass.clean <- species.nn %>% dplyr::select(site_code,experiment_type,year, year_trt, block, plot, trt, Taxon,local_lifeform,local_lifespan,local_provenance,
                                    max_cover, biomass.sp.full) %>%
  mutate(biomass.sp=biomass.sp.full) %>% dplyr::select(-biomass.sp.full)

head(biomass.clean)

View(biomass.clean)

write.csv(biomass.clean, "~/Dropbox/Projects/NutNet Co-oc/Schamp/biomass_sp.csv")


#plot
colnames(p)

comb <- read.csv("~/Dropbox/NutNet data/comb-by-plot-31-August-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

comb2<-unite_(comb, "id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE)
#select only columns of interest
colnames(comb2)
comb3<- select(comb2, id,site_code, continent,country, region, managed, burned, grazed, anthropogenic,habitat,elevation, latitude,longitude,experiment_type,block,plot,year_trt,trt,first_nutrient_year,year)


# re calc the metrics 

species.nn<-unite_(species.nn, "ids", c("site_code","year","year_trt","trt","block","plot","subplot.cov"), remove=FALSE)


View(species.nn)


# make div dataframe
plot.div <- data.frame(id=species.nn$id,ids=species.nn$ids,
                       site_code=species.nn$site_code,	year_trt=species.nn$year_trt,
                       block=species.nn$block,
                       plot=species.nn$plot, subplot.cov=species.nn$subplot.cov, local_provenance=species.nn$local_provenance)


View(plot.div)


all.div <- plot.div %>% distinct (ids)

int.div <- plot.div %>% filter(local_provenance=="INT") %>% distinct (ids)

nat.div <- plot.div %>% filter(local_provenance=="NAT") %>% distinct (ids)

unk.div <- plot.div %>% filter(local_provenance=="UNK") %>% distinct (ids)

View(unk.div)



View(species.nn)
rich <- species.nn %>%  group_by(ids,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise(
    all.div= n_distinct(Taxon))  %>% ungroup()


View(rich)

INT_rich <- species.nn %>% filter(local_provenance=="INT") %>%  group_by(ids,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise(
    int.div = n_distinct(Taxon)) %>% ungroup()

NAT_rich <- species.nn %>% filter(local_provenance=="NAT")  %>%  group_by(ids,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise(
    nat.div = n_distinct(Taxon)) %>% ungroup()

UNK_rich <- species.nn %>% filter(local_provenance=="UNK")  %>%  group_by(ids,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise(
    unk.div = n_distinct(Taxon)) %>% ungroup()

# site metrics
View(species.nn)

site.year.div <- species.nn %>% select(site_code,year_trt, Taxon) %>% distinct() %>% 
  group_by(site_code, year_trt) %>%
  summarise(
    site_year_rich = n_distinct(Taxon)) %>% ungroup()


site.all.div <- species.nn %>% select(site_code,Taxon) %>% distinct() %>% 
  group_by(site_code) %>%
  summarise(
    site_richness = n_distinct(Taxon)) %>% ungroup()


site.nat.div <- species.nn %>% filter(local_provenance=="NAT") %>% 
  select(site_code,Taxon) %>% distinct() %>% 
  group_by(site_code) %>%
  summarise(
    site_native_richness = n_distinct(Taxon)) %>% ungroup()


site.int.div <- species.nn %>% filter(local_provenance=="INT")  %>%  
  select(site_code,Taxon) %>% distinct() %>% 
  group_by(site_code) %>%
  summarise(
    site_introduced_richness = n_distinct(Taxon)) %>% ungroup()


# sum_NAT_cover
nat.cov <- species.nn %>% group_by(ids) %>% 
  filter(local_provenance=="NAT") %>%
  summarise(sum_NAT_cover=sum(max_cover))


# sum_INT_cover
int.cov <- species.nn %>% group_by(ids) %>% 
  filter(local_provenance=="INT") %>%
  summarise(sum_INT_cover=sum(max_cover))

# sum_UNK_cover
unk.cov <- species.nn %>% group_by(ids) %>% 
  filter(local_provenance=="UNK") %>%
  summarise(sum_UNK_cover=sum(max_cover))

# join plot level mass and cover with clean comb
species.nn<-unite_(species.nn, "site_year", c("site_code","year_trt"), remove=FALSE)
colnames(species.nn)
colnames(comb3)

sp <- species.nn %>% distinct(id,ids,site_code,site_year,block,plot,year_trt,Taxon,max_cover,local_provenance,orig.bm.cat,category.mod,cat.cover,subplot.bm,local_lifeform,local_lifespan,functional_group,category,mass,cat.mass,biomass.sp.cat,biomass.sp.plot,biomass.sp.full,biomass.m.full,plot.mass,plot.cover) %>% left_join(comb3) %>%
  left_join(rich) %>% left_join(INT_rich) %>% left_join(NAT_rich) %>% left_join(UNK_rich) %>% 
  left_join(site.all.div) %>% left_join(site.int.div) %>% left_join(site.nat.div) %>% left_join(site.year.div) %>%
  left_join(int.cov) %>% left_join(nat.cov) %>% left_join(unk.cov) %>% 
  arrange(ids)

# perfection
View(sp)


sp.d<- sp %>% distinct(site_code)
View(sp.d)


# join comb with all metrics above.
plot <- sp %>% select(-c(Taxon, max_cover,local_provenance,category.mod,cat.cover,subplot.bm,local_lifeform,local_lifespan,functional_group,category,mass,cat.mass,biomass.sp.cat,biomass.sp.plot,biomass.sp.full,biomass.m.full)) %>%
  distinct(ids, .keep_all = T)

colnames(plot)

site.inclusion<-plot %>% distinct(site_code)
View(site.inclusion)



p.clean <- plot %>% dplyr::select(site_code,year,year_trt,block,plot,trt,plot.cover,plot.mass,all.div,int.div,nat.div,sum_INT_cover,sum_NAT_cover,sum_UNK_cover,
                        site_richness,site_native_richness,site_introduced_richness) %>%
  mutate(plot.rich=all.div,
         plot.int.rich=int.div,
         plot.nat.rich=nat.div) %>% dplyr::select(-c(all.div,int.div,nat.div))

head(p.clean)
View(p.clean)

write.csv(p.clean, "~/Dropbox/Projects/NutNet Co-oc/Schamp/plot.csv")


# pairwise
head(p.all)
price.clean <- p.all %>% dplyr::select(-c(X.1,X,SRE.L, SRE.G, SIE.L,SIE.G,CE,SR,site.year.id,site.year.id.x,site.year.id.y,
                                          trt_year,trt_year.x,trt_year.y,trt.x,trt.y,year.y,year.x,plot.x,plot.y,unique.id,year.y.m, SR)) 


View(price.clean)

write.csv(price.clean, "~/Dropbox/Projects/NutNet Co-oc/Schamp/pairwise_plots.csv")
