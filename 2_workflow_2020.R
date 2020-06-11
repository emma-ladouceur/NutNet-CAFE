
# workflow_2020
# step 2

library(tidyverse)
library(vegan)
library(priceTools)

biomass <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


bce <- droplevels(subset(biomass, experiment_type == "Experimental (Full Factorial)"| experiment_type == "Experimental (Nutrients Only)"))
bce2 <- droplevels(subset(bce, trt=="NPK"|trt=="Control"))

# this site is categorized as full factorial but only has controls
bce2 <- droplevels(subset(bce2, !site_code=="nilla.au"))

bce3<-bce2 %>% filter(!is.na(biomass.sp.full)) %>% droplevels()

bce3 %>% distinct(site_code,year_trt)

# ethamc.au_2019_6_NPK_3_26
# has negative biomass for graminoids. i assume this should be 0.37 and not -0.37 (desert grassland)
# mention to NutNet HQ
bce3$biomass.sp.full<-abs(bce3$biomass.sp.full)

# remove sites that dont have a year 0 or only have year 0
bce4 <- bce3 %>% group_by(site_code) %>%
  summarise(year_min=min(year_trt),
            year_max=max(year_trt)) %>%
  mutate(year.zero = ifelse(year_min <= 0 & year_max > 0, "keep",'remove')) %>%
  left_join(bce3)


check<-bce4 %>% distinct(site_code,year_trt,year.zero)
View(check)

bce5 <- droplevels( bce4[-which(bce4$year.zero == "remove"), ] )
colnames(bce5)

bce6<- bce5 %>% select(id,site_code,year,year_trt,trt,block,plot,plot.cover,orig.bm.cat,category.mod,cat.cover,subplot.cov,subplot.bm,local_lifeform,local_lifespan,functional_group,Taxon,max_cover,category,mass,cat.mass,plot.mass,biomass.sp.plot,biomass.sp.cat,biomass.sp.full,biomass.m.full)
  
View(bce6)
site.inclusion<-bce5 %>% distinct(site_code)

View(site.inclusion)

cover<- read.csv("~/Dropbox/NutNet data/full-cover-03-June-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
cover2<-unite_(cover, "id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE)

species.nn <- bce6 %>%  left_join(cover2)


View(species.nn)

# plot level data
comb <- read.csv("~/Dropbox/NutNet data/comb-by-plot-03-June-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

comb2<-unite_(comb, "id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE)
#select only columns of interest
colnames(comb2)
comb3<- select(comb2, id,site_code, continent,country, region, managed, burned, grazed, anthropogenic,habitat,elevation, latitude,longitude,experiment_type,block,plot,year_trt,trt,first_nutrient_year,year)


# re calc the metrics 

species.nn<-unite_(species.nn, "ids", c("site_code","year","year_trt","trt","block","plot","subplot.cov"), remove=FALSE)

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

# all species
species.wide <- species.nn %>% select(ids, Taxon, max_cover) %>%
  spread(Taxon, max_cover) %>%
  remove_rownames %>% column_to_rownames(var="ids") %>%
  replace(is.na(.), 0)

#introduced
int.wide <- species.nn %>% filter(local_provenance=="INT") %>% droplevels() %>%
  select(ids, Taxon, max_cover) %>%
  spread(Taxon, max_cover) %>%
  remove_rownames %>% column_to_rownames(var="ids") %>%
  replace(is.na(.), 0)
# native
nat.wide <- species.nn %>% filter(local_provenance=="NAT") %>% droplevels() %>%
  select(ids, Taxon, max_cover) %>%
  spread(Taxon, max_cover) %>%
  remove_rownames %>% column_to_rownames(var="ids") %>%
  replace(is.na(.), 0)
# unknown
unk.wide <- species.nn %>% filter(local_provenance=="UNK") %>% droplevels() %>%
  select(ids, Taxon, max_cover) %>%
  spread(Taxon, max_cover) %>%
  remove_rownames %>% column_to_rownames(var="ids") %>%
  replace(is.na(.), 0)

View(unk.wide)
# rich
all.div$rich<-specnumber(species.wide)

View(all.div)
# INT_rich
int.div$INT_rich<-specnumber(int.wide)

View(int.div)
# NAT_rich
nat.div$NAT_rich<-specnumber(nat.wide)

# UNK_rich
unk.div$UNK_rich<-specnumber(unk.wide)

View(unk.div)
# site level

#site dataframes
# make div dataframe
site.div <- data.frame(site_code=species.nn$site_code,local_provenance=species.nn$local_provenance)

site.all.div <- site.div %>% distinct (site_code)

site.int.div <- site.div %>% filter(local_provenance=="INT") %>% distinct (site_code)

site.nat.div <- site.div %>% filter(local_provenance=="NAT") %>% distinct (site_code)

site.unk.div <- site.div %>% filter(local_provenance=="UNK") %>% distinct (site_code)


species.nn$pres<-1
species.wide.site <- species.nn %>% select(site_code, Taxon, pres) %>%
  distinct(site_code,Taxon, .keep_all = T) %>%
  spread(Taxon, pres) %>%
  remove_rownames %>% column_to_rownames(var="site_code") %>%
  replace(is.na(.), 0)

#introduced
int.wide.site <- species.nn %>% filter(local_provenance=="INT") %>% droplevels() %>%
  select(site_code, Taxon, pres) %>%
  distinct(site_code,Taxon, .keep_all = T) %>%
  spread(Taxon, pres) %>%
  remove_rownames %>% column_to_rownames(var="site_code") %>%
  replace(is.na(.), 0)
# native
nat.wide.site <- species.nn %>% filter(local_provenance=="NAT") %>% droplevels() %>%
  select(site_code, Taxon, pres) %>%
  distinct(site_code,Taxon, .keep_all = T) %>%
  spread(Taxon,pres) %>%
  remove_rownames %>% column_to_rownames(var="site_code") %>%
  replace(is.na(.), 0)


# site.year

site.year.long<-unite_(species.nn, "site_year", c("site_code","year_trt"), remove=FALSE)

site.year.wide <- site.year.long %>% 
  select(site_year, Taxon, pres) %>%
  distinct(site_year,Taxon, .keep_all = T) %>%
  spread(Taxon, pres) %>%
  remove_rownames %>% column_to_rownames(var="site_year") %>%
  replace(is.na(.), 0) 

site.year.div <- site.year.long %>% distinct (site_year)

# site_year_rich
site.year.div$site_year_rich<-specnumber(site.year.wide)

View(site.year.div)
# site_richness
site.all.div$site_richness<-specnumber(species.wide.site)

View(site.all.div)
# site_native_richness
site.nat.div$site_native_richness<-specnumber(nat.wide.site)

View(site.nat.div)
# site_introduced_richness
site.int.div$site_introduced_richness<-specnumber(int.wide.site)

View(site.int.div)
# cover sums
# sum_NAT_cover
nat.cov <- species.nn %>% group_by(ids) %>% 
  filter(local_provenance=="NAT") %>%
  summarise(sum_NAT_cover=sum(max_cover))
  
View(nat.cov)
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
  left_join(all.div) %>% left_join(int.div) %>% left_join(nat.div) %>% left_join(unk.div) %>% 
  left_join(site.all.div) %>% left_join(site.int.div) %>% left_join(site.nat.div) %>% left_join(site.year.div) %>%
  left_join(int.cov) %>% left_join(nat.cov) %>% left_join(unk.cov) %>% 
  arrange(ids)

# perfection
  View(sp)

# join comb with all metrics above.

# species biomass data for NPK and Control plots for appropriate sites to match price calcs
write.csv(sp, "~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv")
  
plot <- sp %>% select(-c(Taxon, max_cover,local_provenance,category.mod,cat.cover,subplot.bm,local_lifeform,local_lifespan,functional_group,category,mass,cat.mass,biomass.sp.cat,biomass.sp.plot,biomass.sp.full,biomass.m.full)) %>%
  distinct(ids, .keep_all = T)

colnames(plot)

site.inclusion<-plot %>% distinct(site_code)
View(site.inclusion)

write.csv(plot, "~/Dropbox/Projects/NutNet/Data/plot.csv")

colnames(sp)
colnames(plot)

# have a look at differences in estimates- looks pretty good
bce5$biomass.sp.diff<-  abs(bce5$biomass.sp.plot - bce5$biomass.sp.cat) 

samp <- bce5 %>% top_frac(.5) %>% arrange(desc(biomass.sp.diff)) %>%
  select(id,Taxon,plot.mass,category, mass,biomass.sp.plot,biomass.sp.cat,biomass.sp.diff) 



# get max year for filtering

plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



plot<-plot %>% group_by(site_code) %>% 
                      summarise(min.year = min(year_trt),
                                max.year = max(year_trt))

site.inclusion<-plot %>% distinct(site_code,min.year,max.year)
site.inclusion


p.all<-p.all %>% group_by(site_code) %>% 
  summarise(min.year = min(year.x),
            max.year = max(year.y))

price.inclusion<-p.all %>% distinct(site_code,min.year,max.year)
price.inclusion

write.csv(plot, "~/Dropbox/Projects/NutNet/Data/plot.csv")
write.csv(p.all, "~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv")




# prep for price

bce5$year_trt<-as.factor(bce5$year_trt)
levels(bce5$year_trt)
levels(bce5$site_code)
bce6<-group_by(bce5, site_code,year_trt)
head(bce6)

all<-unite_(bce6, "site.year.id", c("site_code","year_trt"), remove=FALSE)
all<-unite_(all, "trt_year", c("trt","year_trt"), remove=FALSE)
all<-unite_(all, "site_trt_year", c("site_code","trt","year_trt"), remove=FALSE)

all$year_trt<-as.numeric(as.character(all$year_trt))
all$site.year.id<-as.character(all$site.year.id)
View(all)
#create index's
index<-paste(all$site_name, all$site.year.id)
sindex<-as.character(all$site_code)
yindex<-as.character(all$year_trt)
uindex<-sort(unique(index))
usindex<-sort(unique(sindex))

uyindex<-sort(unique(yindex))

# cumulative
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



folder = "output_new"
# input RDS files for cluster, price analysis
# if this doesnt work close R and try again only happens because wd has changed
mapply(saveRDS, all_lst, version=2, file=paste0(folder, "/",names(all_lst), '.rds'))


# test it out, test the code
samp <- readRDS("output_new/arch.us_2.rds")
colnames(samp)

View(samp)

group.vars <- c('site.year.id','plot','block')
treat.vars<-c('trt_year')

grouped.data <- samp %>% group_by(.dots=c(group.vars,treat.vars))

#takes a long time
res <- pairwise.price(grouped.data, species="Taxon", func="biomass.sp.full")

# Create a single column keeping track of the paired set of seeding treatments & other grouping variables:
pp<-res
pp<-group.columns(pp,gps=c(group.vars,treat.vars), drop=T)


View(pp)


