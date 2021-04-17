
# Author: Emma Ladouceur
# Title:
# Last Updated April 15, 2021

# 2 Data Prep
# This workflow takes cleaned, filter data (woody non-vasculars removed) per species biomass estimate data and re-calculates measures of species richness
# this script has 3 main parts
# 1 produces the final biomass/ species dataset 
# 2 the plot dataset with measures of species richness across plots and sites
# 3 preps data for price equation comparisons, which just pairs data into temporal subsets for meaningful comparisons
# and outputs subsets of the data in paris

# packages
library(tidyverse)
library(vegan)
library(priceTools)

# data
biomass <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/biomass_sp.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


colnames(biomass)
biomass %>% distinct(site_code, experiment_type, trt)

# clean  data
biomass_exp <- biomass %>% filter( experiment_type %in% c("Experimental (Full Factorial)", # experiment types we are interested in
                                              "Experimental (Nutrients Only)"),
                    trt %in% c("NPK", "Control"), # treatments we are interested in
                    !site_code %in% c("nilla.au"), # this site is categorized as full factorial but only has controls
                    !is.na(biomass.sp.full) )  # remove rows with NA under main biomass/species column

head(biomass_exp)
colnames(biomass_exp)

biomass_exp %>% distinct(site_code, year_trt)


# remove sites that dont have a year 0 or only have year 0
species_nn <- biomass_exp  %>% 
  group_by(site_code) %>% # group by site
  summarise(year_min = min(year_trt),  # get minimum year for each study
            year_max = max(year_trt)) %>% # get max year for each study
  mutate(year.zero = ifelse(year_min <= 0 & year_max > 0, "keep",'remove')) %>% # label everything with no year zero as remove
  left_join(biomass_exp) %>% # rejoin with data
  filter(!year.zero %in% "remove") %>%  # remove studies with no year 0
  select(-year_min,  -year.zero) %>% # remove excess columns, keep year_max because we want to know how old sites are
  select(id,site_code,year,year_trt,year_max,trt,block,plot,plot.cover,orig.bm.cat,category.mod,cat.cover,subplot.cov,subplot.bm,local_lifeform,local_lifespan,local_provenance,functional_group,Taxon,max_cover,category,orig.mass,strip.mass,cat.mass,biomass.sp.plot,biomass.sp.cat,biomass.sp.full,biomass.m.full)

colnames(species_nn)

# plot richness

rich_plot <- species_nn %>%  group_by(id,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise( rich = n_distinct(Taxon))  %>% ungroup()

INT_rich_plot <- species_nn %>% filter(local_provenance=="INT") %>%  group_by(id,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise(INT_rich = n_distinct(Taxon)) %>% ungroup()

NAT_rich_plot <- species_nn %>% filter(local_provenance=="NAT")  %>%  group_by(id,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise(NAT_rich = n_distinct(Taxon)) %>% ungroup()

UNK_rich_plot <- species_nn %>% filter(local_provenance=="UNK")  %>%  group_by(id,site_code, year_trt,block,plot,subplot.cov) %>%
  summarise( UNK_rich = n_distinct(Taxon)) %>% ungroup()

# site richness

# site_year
site_year_rich_dat <- species_nn %>% select(site_code, year_trt, Taxon) %>% distinct() %>% 
  group_by(site_code, year_trt) %>%
  summarise(site_year_rich = n_distinct(Taxon)) %>% ungroup()

#site
site_richness_dat <- species_nn %>% select(site_code,Taxon) %>% distinct() %>% 
  group_by(site_code) %>%
  summarise( site_richness = n_distinct(Taxon)) %>% ungroup()


site_nat_dat <- species_nn %>% filter(local_provenance=="NAT") %>% 
  select(site_code,Taxon) %>% distinct() %>% 
  group_by(site_code) %>%
  summarise(  site_native_richness = n_distinct(Taxon)) %>% ungroup()


site_int_div <- species_nn %>% filter(local_provenance=="INT")  %>%  
  select(site_code,Taxon) %>% distinct() %>% 
  group_by(site_code) %>%
  summarise( site_introduced_richness = n_distinct(Taxon)) %>% ungroup()


# sum_NAT_cover
nat_cov <- species_nn %>% group_by(id) %>% 
  filter(local_provenance=="NAT") %>%
  summarise( sum_NAT_cover=sum(max_cover))


# sum_INT_cover
int_cov <- species_nn %>% group_by(id) %>% 
  filter(local_provenance=="INT") %>%
  summarise( sum_INT_cover=sum(max_cover))

# sum_UNK_cover
unk_cov <- species_nn %>% group_by(id) %>% 
  filter(local_provenance=="UNK") %>%
  summarise(sum_UNK_cover=sum(max_cover))


sp <- species_nn %>% distinct(id,site_code,block,plot,year_trt,year_max,trt,Taxon,max_cover,local_provenance,
                              orig.bm.cat,category.mod,cat.cover,subplot.bm,local_lifeform,local_lifespan,functional_group,
                              category,cat.mass,biomass.sp.cat,biomass.sp.plot,biomass.sp.full,biomass.m.full,orig.mass,strip.mass,plot.cover) %>%
  left_join(rich_plot) %>% left_join(INT_rich_plot) %>% left_join(NAT_rich_plot) %>% left_join(UNK_rich_plot) %>% 
  left_join(site_year_rich_dat) %>% left_join(site_richness_dat) %>% left_join(site_nat_dat) %>% left_join(site_int_div) %>%
  left_join(nat_cov) %>% left_join(int_cov) %>% left_join(unk_cov) %>% 
  arrange(id)

View(sp)

#  now we revisit the biggest biomass values 
biggest.bm.values <- sp %>%  filter(year_max >= 3) %>% # out analysis is based on sites 3 years or older
  # but we keep all sites because we want to see if answers change accoridng to this age site inclusion (Figure S4)
  select(id, site_code, year_trt, trt, Taxon, category, category.mod, orig.bm.cat,
                                          orig.mass, strip.mass, orig.bm.cat, biomass.sp.full, biomass.m.full) %>%
  top_frac(.5) %>%  arrange(desc(biomass.sp.full)) 
# need help thinking if any of these need to be cleaned/ remove, fixed or whats gone on with strange values
View(biggest.bm.values)

# 1 output species data
# species biomass data for NPK and Control plots for appropriate sites to match price calcs
write.csv(sp, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/biomass_sp_CAFE.csv")

# 2 plot  and site data 
plot <- sp %>% select(-c(Taxon, max_cover,local_provenance,category.mod,cat.cover,subplot.bm,local_lifeform,local_lifespan,functional_group,category,strip.mass,cat.mass,biomass.sp.cat,biomass.sp.plot,biomass.sp.full,biomass.m.full)) %>%
  distinct(id, .keep_all = T)

View(plot)

site.inclusion<-plot %>% distinct(site_code,year_max) %>% filter(year_max >= 3)
# 58 sites will be included in our analyses
head(site.inclusion)

write.csv(plot, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/plot.csv")


# 3 Data prep for price equations
# Authors: Emma Ladouceur & Adam T. Clark

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



# get max year for filtering

plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(plot)
View(p.all)

plot<-plot %>% group_by(site_code) %>% 
  summarise(min.year = min(year_trt),
            max.year = max(year_trt)) %>% left_join(plot)

View(plot)

site.inclusion<-plot %>% distinct(site_code,min.year,max.year)
site.inclusion


p.all<-p.all %>% group_by(site_code) %>% 
  summarise(min.year = min(year.x),
            max.year = max(year.y)) %>% left_join(p.all)


View(p.all)
price.inclusion<-p.all %>% distinct(site_code,min.year,max.year)
price.inclusion

write.csv(plot, "~/Dropbox/Projects/NutNet/Data/plot.csv")
write.csv(p.all, "~/Dropbox/Projects/NutNet/Data/nutnet_cumulative_time.csv")



