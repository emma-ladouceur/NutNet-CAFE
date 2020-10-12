# Emma Ladouceur
# The Effect of Multiple Limiting Resources on Community Assembly and the Functioning of Ecosystems
# Last Updated May 6 th 2020
#  This script cleans data to living herbaceous species only and calculates per species biomass according to total plot biomass or
# life form group (graminoid, forb, legume)


library(tidyverse)

#previous
comb <- read.csv("~/Dropbox/NutNet data/comb-by-plot-30-April-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
cover<- read.csv("~/Dropbox/NutNet data/full-cover-01-May-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
biomass <- read.csv("~/Dropbox/NutNet data/full-biomass-30-April-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))


# update
comb <- read.csv("~/Dropbox/NutNet data/comb-by-plot-31-August-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
cover<- read.csv("~/Dropbox/NutNet data/full-cover-24-September-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
biomass <- read.csv("~/Dropbox/NutNet data/full-biomass-24-September-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))



# COVER  CLEAN UP
head(cover)

# Drop non-living taxa
cover1 <- cover[cover$live == 1,]

# reduce to only relevant functional groups.....
# first for cover
levels(cover1$functional_group)

# Drop mosses, lichens, fungi, woody
cover2 <- cover1 %>% filter(!functional_group %in% c("BRYOPHYTE", "LICHEN", "CLUBMOSS", "LIVERWORT", "NON-LIVE", "WOODY") ) %>%
  droplevels()

# Drop records not assigned to Taxonomic family (litter etc are NA)
cover3<-cover2[complete.cases(cover2$Family),]

# make a unique id for every plot in cover dataset
cover4<-unite_(cover3, "id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE)

head(cover4)

# BIOMASS CLEAN UP
head(biomass)

#make a unique id for biomass
biomass1<-unite_(biomass, "id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE)

# Subset biomass data that just measured total
# not categorized as live so we subset before
# biomass.total <- biomass1 %>% select(id,site_code,year,year_trt,trt,block,plot,subplot,category,mass) %>%
#   filter(category == "TOTAL") %>% droplevels()

# Drop non-living taxa
biomass2 <- biomass1[biomass1$live == 1,]

# Drop mosses, lichens, fungi, litter, woody
biomass2$category<-as.factor(as.character(biomass2$category))
biomass3 <- biomass2 %>% filter(!category %in% c("BRYOPHYTE", "DOWNED WOODY DEBRIS","FUNGUS","LICHEN","LITTER","NON-VASCULAR","STANDING DEAD", "WOODY" )) %>%
  droplevels()

# calculate plot level cover and plot level biomass from cleaned datasets
dat_cover <- cover4 %>% group_by(id,site_code,year,year_trt,trt,block,plot,subplot) %>% 
  summarise(plot.cover=sum(max_cover)) %>% # summarise by subplot because  at least one site measured 2 subplots in a year (but think its an observational site)
  left_join(cover4) %>%
  rename(subplot.cov=subplot) %>% arrange(id,Taxon)

# dat_bm <- biomass3 %>% group_by(id,site_code,year,year_trt,trt,block,plot,subplot) %>% 
#   summarise(plot.mass=sum(mass)) %>%
#   left_join(biomass3) 

dat_bm_step <- biomass3 %>% select(id,site_code,year,year_trt,trt,block,plot,subplot,category,mass) %>%
  distinct() %>%
  group_by(id,site_code,year,year_trt,trt,block,plot,subplot) %>%
  spread(category,mass) %>%
  mutate(LIVE = case_when(GRAMINOID >= 0 | FORB >= 0 | LIVE >= 0 ~ "NA")) %>% # remove live measurements that also measured by group
  gather(category, mass,`ANNUAL`:`<NA>`) %>%
  filter(!is.na(mass),
         !mass == "NA") %>% droplevels() %>% ungroup() %>% arrange(id,site_code,year,year_trt,trt,block,plot,subplot,category)

dat_bm_step$mass<-as.numeric(dat_bm_step$mass)

dat_bm <- dat_bm_step %>%
  group_by(id,site_code,year,year_trt,trt,block,plot,subplot) %>%
  summarise(plot.mass=sum(mass)) %>%
  left_join(dat_bm_test ) %>% 
  ungroup() %>% arrange(id,site_code,year,year_trt,trt,block,plot,subplot,category)

View(dat_bm)

# if else statement to seperate total, vascular, live mass (all total values) from those that seperated biomass by group
# sites that measured total
biomass.total   <- dat_bm %>% ungroup() %>% select(id,site_code,year,year_trt,trt,block,plot,subplot,category,mass) %>%
  filter(category %in% c("TOTAL","VASCULAR", "LIVE")) %>% droplevels() %>%
  filter(!site_code=="ethamc.au") %>% droplevels() %>%
  rename(orig.bm.cat=category,
         subplot.bm=subplot)

biomass.total$plot.mass<-biomass.total$mass

View(biomass.total)
# sites that seperated biomass - remove the totals
sep_dat <- dat_bm %>% ungroup() %>% filter(!category %in% c("TOTAL","VASCULAR", "LIVE")) %>% droplevels() %>%
  filter(!site_code=="ethamc.au") %>% droplevels() 



# SPECIAL CASES
# 1. biomass per species for sites that measured total biomass only
total.dat <- biomass.total %>% left_join(dat_cover, by= c("id", "site_code", "year", "year_trt", "trt", "block", "plot" ) ) %>%
  select(-c(site_name, Family,live,local_provenance,N_fixer,ps_path)) %>% arrange(id, subplot.cov)

colnames(total.dat)

total.dat$biomass.sp.full <- total.dat$max_cover/total.dat$plot.cover * total.dat$plot.mass

head(total.dat)


total.dat$biomass.sp.plot <- total.dat$biomass.sp.full
total.dat$biomass.m.full <- "total"


total.dat2<-total.dat %>% distinct(id,Taxon,mass,orig.bm.cat,biomass.sp.full, .keep_all=T)

View(total.dat2)
# any duplicates? 
dup.summary.total <- total.dat2 %>% group_by(id, Taxon,biomass.sp.full, biomass.m.full) %>% filter(n()>1) %>% summarize(n=n()) %>%
  select(id,Taxon, biomass.sp.full, biomass.m.full,n)
# nope!
head(dup.summary.total)
# next calculate by functional group

# SPECIAL CASES
# 2. studies using annual & perennial categories (only one - shps.us)
ap.bm<-sep_dat %>% filter(category %in% c("ANNUAL", "PERENNIAL") ) %>% droplevels() %>%
  select(id, site_code, year, year_trt, trt, block, plot,subplot,category,mass) %>%
  rename(
         subplot.bm=subplot)

ap.bm$cat.mass<-ap.bm$mass
ap.bm$orig.bm.cat<-ap.bm$category
dat_cover$category <- dat_cover$local_lifespan


ap.cov<-dat_cover %>% filter(site_code=="shps.us" ) %>% droplevels() %>% # look at onyl site in question
  select(id, site_code, year, year_trt, trt, block, plot,subplot.cov,local_lifespan,category,Taxon,max_cover) %>%
  mutate(category = as.character(category)) %>%
  mutate(category = if_else(Taxon == "ANTENNARIA SP.", # categorize antennaria as perennial
                            "PERENNIAL",
                            category))

ap.dat <- ap.cov %>% left_join(ap.bm, by= c("id", "site_code", "year", "year_trt", "trt", "block", "plot", "category" ) ) 


ap.dat2 <- ap.dat %>% group_by(id,site_code,year,year_trt,trt,block,plot,category) %>% 
  summarise(cat.cover=sum(max_cover)) %>% # calculate category cover
  left_join(ap.dat) 


ap.dat3 <- ap.dat2 %>% group_by(id,site_code,year,year_trt,trt,block,plot) %>%
  summarise(plot.cover=sum(cat.cover), # sum plant cover by plot
            plot.mass=sum(cat.mass)) %>% # sum biomass by plot
  left_join(ap.dat2) %>% ungroup()

# calculate per species biomass by sorted category
ap.dat3$biomass.sp.cat <- ap.dat3$max_cover/ap.dat3$cat.cover * ap.dat3$cat.mass
# calculate per species biomass by plot for comparison
ap.dat3$biomass.sp.plot <- ap.dat3$max_cover/ap.dat3$plot.cover * ap.dat3$plot.mass

# rename columns to match down the road
ap.dat3$biomass.sp.full <- ap.dat3$biomass.sp.cat
ap.dat3$biomass.m.full <- "ap category"


View(ap.dat3)

#  ethamc.au
View(ethamc)
eth.bm<- dat_bm %>% ungroup() %>% filter(site_code=="ethamc.au") %>% droplevels() 

eth.bm.live<- eth.bm %>% ungroup() %>% filter(category=="LIVE") %>% droplevels() %>%
  rename(subplot.bm=subplot) %>% select(-live)

View()
eth.bm.sort<- eth.bm %>% ungroup() %>% filter(!category=="LIVE") %>% droplevels() %>%
  rename(
    subplot.bm.sort=subplot,
    mass.sort=mass,
    plot.mass.sort=plot.mass) %>% select(-live) 



eth.bm.sort$category.mod<-eth.bm.sort$category

eth.cov<-dat_cover %>% ungroup() %>% filter(site_code=="ethamc.au" ) %>% droplevels() %>%
  mutate( category.mod = fct_recode(functional_group, c("GRAMINOID"="GRASS"))) %>%
  select(-category,-live)
View(eth.cov)
  
#  many plots where species and cover were recorded but biomass is recorded as 0
# remove all these rows and leave only plots with true 0's
eth.live<-eth.bm.live %>% left_join(eth.cov) %>% filter(is.na(Taxon)) %>%
   select(-site_name)

  View(eth.live)
  
eth.live$biomass.sp.full <- eth.live$max_cover/eth.live$plot.cover * eth.live$plot.mass

eth.live$biomass.sp.plot <- eth.live$biomass.sp.full
eth.live$biomass.m.full <- "total"
  
  
  
eth.sort <- eth.cov %>% left_join(eth.bm.sort, by = c("id", "site_code", "year", "year_trt", "trt", "block", "plot","category.mod")) %>%
  arrange(id)
View(eth.sort)

eth.sort$mass.diff<-eth.sort$plot.mass.sort-eth.sort$mass.sort
eth.sort2 <- eth.sort %>% ungroup() %>%
  filter(!plot.mass.sort == 0 , !mass.sort == 0, !mass.diff == 0 ) %>%
  select(-mass.diff) %>%
  arrange(id) %>%
  rename(plot.mass=plot.mass.sort,
         mass=mass.sort,
         subplot.bm=subplot.bm.sort) %>% select(-site_name.y,-site_name.x) 
View(eth.sort2)

eth.sort3 <- eth.sort2 %>%
   group_by(id,site_code,year,year_trt,trt,block,plot,category) %>% 
  summarise(cat.cover=sum(max_cover),
            cat.mass=sum(mass)) %>% left_join(eth.sort2)

View(eth.sort3)
eth.sort3$biomass.sp.cat <- eth.sort3$max_cover/eth.sort3$cat.cover * eth.sort3$cat.mass
# calculate per species biomass by plot for comparison
eth.sort3$biomass.sp.plot <- eth.sort3$max_cover/eth.sort3$plot.cover * eth.sort3$plot.mass

eth.sort3$biomass.sp.full <- eth.sort3$biomass.sp.cat
eth.sort3$biomass.m.full <- "category"


eth.dat <- eth.sort3 %>% bind_rows(eth.live) %>%
  select(-Family,-N_fixer,-ps_path,-local_provenance) %>%
  rename(orig.bm.cat=category,
         category=category.mod)

View(eth.dat)
colnames(eth.dat)
colnames(special.dat)


# CALCULATE BIOMASS BY STANDARD BIOMASS CATEGORY
# prepcover data
# remove previous site from last step
dat_cover2<-dat_cover %>%  ungroup() %>% filter(!site_code=="shps.us",!site_code=="ethamc.au" ) %>% droplevels() 

test<- dat_cover2%>%distinct(site_code)
View(test)
# put grass into graminoid catgory to match biomass categories
dat_cover3<-dat_cover2 %>% mutate( category.mod = fct_recode(functional_group, c("GRAMINOID"="GRASS"))) %>% 
  select(id, site_code, year, year_trt, trt, block, plot,subplot.cov,local_lifeform,local_lifespan,functional_group,category.mod,Taxon,max_cover) 

dat_cover3$category.mod<-as.character(dat_cover3$category.mod)
# rename cover category to match biomass for phlox diffusa in  bnch.us
dat_cover4 <- dat_cover3 %>% 
  mutate(category.mod = if_else(Taxon == "PHLOX DIFFUSA",   "FORB + PHLOX DIFFUSA",category.mod)) 

# double check phlox diffusa case
bnch<-dat_cover4 %>% filter(site_code=="bnch.us" & year=="2018" & trt == "NPK") 
head(bnch, n=20) # yep all good

# prep biomass data
# remove categories from last step
# this takes a minute - be patient - probably quicker in baser?
sep_dat2<-sep_dat %>% ungroup() %>% filter(!category %in% c("ANNUAL", "PERENNIAL") ) %>% droplevels() 

# do some renaming of lifeform categories to match across biomass and cover datasets
sep_dat2$orig.bm.cat<-sep_dat2$category
sep_dat3 <-sep_dat2 %>% 
  mutate(category.mod= fct_recode(category, c("FERN" = "PTERIDOPHYTE"  )))

# calculate biomass by modified category
sep_dat4 <- sep_dat3 %>% 
  group_by(id,site_code,year,year_trt,trt,block,plot,category.mod) %>% 
  summarise(cat.mass=sum(mass)) %>%
  left_join(sep_dat3) %>%
  select(id, site_code, year, year_trt, trt, block, plot,subplot,category,category.mod,orig.bm.cat,mass,plot.mass,cat.mass) %>%
  rename(subplot.bm=subplot)

bnch<-sep_dat4 %>% filter(site_code=="bnch.us" & year=="2018" & trt == "NPK") 

# unique list of plot id's and plot level  mass
sep_dat.plot<- sep_dat4 %>% ungroup() %>% select(id,plot.mass) %>% distinct(id,plot.mass)

# join cover and cleaned seperated biomas data by category
cat.dat <- dat_cover4 %>% left_join(sep_dat4, by= c("id", "site_code", "year", "year_trt", "trt", "block", "plot", "category.mod" ) ) %>%
  select(-plot.mass)

# summarise cover by the category groups
cat.dat2 <- cat.dat %>% group_by(id,site_code,year,year_trt,trt,block,plot,category.mod) %>% 
  summarise(cat.cover=sum(max_cover)) %>%
  left_join(cat.dat)

# summarise cover by the plot
cat.dat3 <- cat.dat2 %>% group_by(id,site_code,year,year_trt,trt,block,plot) %>% 
  summarise(plot.cover=sum(max_cover)) %>%
  left_join(cat.dat2) %>% ungroup() %>% arrange(id)

# join with plot level biomass data
cat.dat4<- cat.dat3 %>% left_join(sep_dat.plot,by = c("id"))

# calculate per species biomass using plot level values
# call it biomas.sp.plot
cat.dat4$biomass.sp.plot <- cat.dat4$max_cover/cat.dat4$plot.cover * cat.dat4$plot.mass

# calculate per species biomass with seperated categorised biomass
# call it biomass.sp.cat
cat.dat4$biomass.sp.cat <- cat.dat4$max_cover/cat.dat4$cat.cover * cat.dat4$cat.mass
# i keep both of these to compare the values and sanity check


# last steps! almost there!
# some plots have a functional group that was not recorded in biomass but was in cover
# so the biomass for that particular species comes up as a 0
# for these whole plots, we use plot level biomass to calculate per species biomass
# we replace all per species biomass estimates done from catgoeries with plot level, only on a plot case-by-case basis where this happened


# replace Na's with 0's (theres a mixture)
 cat.dat5 <- cat.dat4 %>% 
   select(id,cat.mass,biomass.sp.cat,biomass.sp.plot,Taxon) %>%
   replace(is.na(.), 0)

# filter rows where cat.mass is 0 (because of above mentioned inconsistencies), where biomass.cat is 0,
# but biomas.plot is >0
# get those distinct rows
cat.dat.na <- cat.dat5 %>% ungroup() %>%
  filter(cat.mass == 0 , biomass.sp.cat == 0, biomass.sp.plot >0 ) %>%
  distinct(id) %>%
  arrange(id)

# join this with all rows that match that id to get the whole plot represented
cat.dat.na2 <- cat.dat.na %>% left_join(cat.dat4) 
# rename some stuff
cat.dat.na2$biomass.sp <- cat.dat.na2$biomass.sp.plot
# categorize these rows as being calculated at the 'plot' level
cat.dat.na2$biomass.m.plot <- "plot"
# categorise others as being calculated by category
cat.dat4$biomass.m.cat <- "category"


# join the plot calc'd and the categorized calc lists back together
cat.dat.cat <- cat.dat4 %>%
  left_join(cat.dat.na2, by = c("id", "site_code", "year", "year_trt", "trt", "block", "plot", "plot.cover", "category.mod", "cat.cover", "subplot.cov", "local_lifeform", "local_lifespan", "functional_group", "Taxon", "max_cover", "subplot.bm", "category","orig.bm.cat","mass", "cat.mass", "plot.mass", "biomass.sp.plot", "biomass.sp.cat"))

View(cat.dat.cat)
View(cat.dat.na2)
# create a column called  biomass.sp.full
# we replace rows with 'NA's' in biomass.sp (plot level) with biomass.sp.cat to complete the rows
# biomass.m.full becomes our column that tells us how each row was calculated

cat.dat.cat2 <- cat.dat.cat %>%
  mutate(biomass.sp.full = ifelse(is.na(biomass.sp),
                                  biomass.sp.cat,
                                  biomass.sp),
         biomass.m.full = ifelse(is.na(biomass.m.plot),
                                 biomass.m.cat,
                                 biomass.m.plot) ) %>%
  select(-biomass.sp,-biomass.m.plot,-biomass.m.cat)


View(cat.dat.cat2)
# bind thw rows of the special cases together; total.dat and ap.dat
View(total.dat2)
View(ap.dat3)
special.dat <- total.dat2 %>% bind_rows(ap.dat3) %>%
  arrange(id)

colnames(special.dat)
colnames(eth.dat)
special.dat2 <- special.dat %>% bind_rows(eth.dat)

View(special.dat2)
colnames(special.dat)
colnames(eth.dat)

colnames(cat.dat.cat2)
# any duplicates?
dup.summary.spec <- special.dat2 %>% group_by(id, Taxon,biomass.sp.full, biomass.m.full) %>% filter(n()>1) %>% summarize(n=n()) %>%
  select(id,Taxon, biomass.sp.full, biomass.m.full,n)

head(dup.summary.spec)
# phew

# bind cat.dat and special.dat together
final.dat <- cat.dat.cat2 %>%  bind_rows(special.dat2) %>%
  arrange (id)


View(final.dat)
# we have four categories for how biomas was calculated
final.dat$biomass.m.full<- as.factor(final.dat$biomass.m.full)
levels(final.dat$biomass.m.full)
# total is where total biomass was only measured
# and plot is where biomass was seperated by catgory, but needed to be corrected because biomass did noit match cover samples
# category is where biomass was divided by group and matched with the cover data
# ap category is the same but where categories were annual perennial

# sanity check: any duplicates?
# include subplot.cov, because some sites measured multiple subplots within plots for cover, but only one for biomass
# so these plots appear to be duplicates sometime when in fact, they are not (i think)

dup.summary <- final.dat %>% group_by(id, subplot.cov, Taxon,biomass.sp.full, biomass.m.full) %>% filter(n()>1) %>% summarize(n=n()) %>%
  select(id,subplot.cov,Taxon, biomass.sp.full, biomass.m.full,n)

head(dup.summary)



final.dat$biomass.sp.diff<-  abs(final.dat$biomass.sp.plot - final.dat$biomass.sp.cat) 

colnames(final.dat)
samp <- final.dat %>% top_frac(.5) %>% arrange(desc(biomass.sp.diff)) %>%
  select(id,Taxon,plot.mass,category, mass,biomass.sp.plot,biomass.sp.cat,biomass.sp.diff) 
  

View(samp)

# join with comb_by_plot data
total.dat <- final.dat %>% left_join(comb)

View(total.dat)


sum.m<-total.dat %>% distinct(site_code,biomass.m.full)
View(sum.m)

sites<-total.dat %>% distinct(site_code, year_trt)
View(sites)


write.csv(total.dat, "~/Dropbox/Projects/NutNet/Data/biomass_sp.csv")




library(ggplot2)

colnames(final.dat)

 plot.dat <- final.dat %>% select(id, biomass.sp.plot,  biomass.sp.cat) %>%
gather( key="biomass.m.compare", value="biomass.sp.compare", biomass.sp.plot,biomass.sp.cat)

 
 
ggplot(plot.dat,aes(x=biomass.m.compare, y=biomass.sp.compare)) +
geom_boxplot() + ylim(0,50)





