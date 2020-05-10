# Emma Ladouceur
# The Effect of Multiple Limiting Resources on Community Assembly and the Functioning of Ecosystems
# Last Updated May 6 th 2020
# Calculate per species biomass


library(tidyverse)


comb <- read.csv("~/Dropbox/NutNet data/comb-by-plot-30-April-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
cover<- read.csv("~/Dropbox/NutNet data/full-cover-01-May-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
biomass <- read.csv("~/Dropbox/NutNet data/full-biomass-30-April-2020.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

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

dat_bm <- biomass3 %>% group_by(id,site_code,year,year_trt,trt,block,plot,subplot) %>% 
  summarise(plot.mass=sum(mass)) %>%
  left_join(biomass3)

# if else statement to seperate total, vascular, live mass (all total values) from those that seperated biomass by group
# sites that measured total
biomass.total   <- dat_bm %>% select(id,site_code,year,year_trt,trt,block,plot,subplot,category,mass) %>%
  filter(category %in% c("TOTAL","VASCULAR", "LIVE")) %>% droplevels() %>%
  rename(plot.mass=mass,
         subplot.bm=subplot)
# sites that seperated biomass - remove the totals
sep_dat <- dat_bm %>% filter(!category %in% c("TOTAL","VASCULAR", "LIVE")) %>% droplevels()


# SPECIAL CASES
# 1. biomass per species for sites that measured total biomass only
total.dat <- biomass.total %>% left_join(dat_cover, by= c("id", "site_code", "year", "year_trt", "trt", "block", "plot" ) ) %>%
  select(-c(site_name, Family,live,local_provenance,N_fixer,ps_path)) %>% arrange(id, subplot.cov)

colnames(total.dat)

total.dat$biomass.sp.full <- total.dat$max_cover/total.dat$plot.cover * total.dat$plot.mass

head(total.dat)


total.dat$biomass.sp.plot <- total.dat$biomass.sp.full
total.dat$biomass.m.full <- "total"


total.dat2<-total.dat %>% distinct(id,Taxon,biomass.sp.full, .keep_all=T)

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
  rename(cat.mass=mass,
         subplot.bm=subplot)


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
  left_join(ap.dat2)

# calculate per species biomass by sorted category
ap.dat3$biomass.sp.cat <- ap.dat3$max_cover/ap.dat3$cat.cover * ap.dat3$cat.mass
# calculate per species biomass by plot for comparison
ap.dat3$biomass.sp.plot <- ap.dat3$max_cover/ap.dat3$plot.cover * ap.dat3$plot.mass

# rename columns to match down the road
ap.dat3$biomass.sp.full <- ap.dat3$biomass.sp.cat
ap.dat3$biomass.m.full <- "ap category"


# CALCULATE BIOMASS BY STANDAD BIOMASS CATEGORY
# prepcover data
# remove previous site from last step
dat_cover2<-dat_cover %>% filter(!site_code=="shps.us" ) %>% droplevels() 

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
sep_dat2<-sep_dat %>% filter(!category %in% c("ANNUAL", "PERENNIAL") ) %>% droplevels() 

# do some renaming of lifeform categories to match across biomass and cover datasets
sep_dat3 <-sep_dat2 %>% 
  mutate(category.mod= fct_recode(category, c("FERN" = "PTERIDOPHYTE"  )))

# calculate biomass by modified category
sep_dat4 <- sep_dat3 %>% 
  group_by(id,site_code,year,year_trt,trt,block,plot,category.mod) %>% 
  summarise(cat.mass=sum(mass)) %>%
  left_join(sep_dat3) %>%
  select(id, site_code, year, year_trt, trt, block, plot,subplot,category,category.mod,mass,plot.mass,cat.mass) %>%
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
  left_join(cat.dat.na2, by = c("id", "site_code", "year", "year_trt", "trt", "block", "plot", "plot.cover", "category.mod", "cat.cover", "subplot.cov", "local_lifeform", "local_lifespan", "functional_group", "Taxon", "max_cover", "subplot.bm", "category","mass", "cat.mass", "plot.mass", "biomass.sp.plot", "biomass.sp.cat"))


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


# bind thw rows of the special cases together; total.dat and ap.dat
special.dat <- total.dat2 %>% bind_rows(ap.dat3) %>%
  arrange(id)


# any duplicates?
dup.summary.spec <- special.dat %>% group_by(id, Taxon,biomass.sp.full, biomass.m.full) %>% filter(n()>1) %>% summarize(n=n()) %>%
  select(id,Taxon, biomass.sp.full, biomass.m.full,n)

head(dup.summary.spec)
# phew

# bind cat.dat and special.dat together
final.dat <- cat.dat.cat2 %>%  bind_rows(special.dat) %>%
  arrange (id)

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

# join with comb_by_plot data
total.dat <- comb %>% left_join(final.dat)

summary(total.dat)



write.csv(total.dat, "~/Dropbox/Projects/NutNet/Data/biomass_sp.csv")




