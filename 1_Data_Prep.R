
# Authors: Emma Ladouceur & Adam T. Clark
# Title:
# Last Updated April 15, 2021
# This script cleans data to living herbaceous species only and calculates per species biomass according to total plot biomass or
# life form group (graminoid, forb, legume etc.)

# packages
library(tidyverse)

# master data
comb   <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/NutNet data/comb-by-plot-02-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
cover   <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/NutNet data/full-cover-02-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
biomass <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/NutNet data/full-biomass-02-April-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

# seasonal data
seas_cover   <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/seasonal data/full-cover-by-date-15-04-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
seas_biomass <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/seasonal data/full-biomass-by-date-15-04-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))

# seasonal data for Jena
seas_cover %>% filter( site_code %in% "jena.de")  %>% distinct(site_code, date, year_trt)

# jena year 0 was only sampled in September so we remove this aggregated data from main data set and use 
#seasonal data for september only for every subsequent year
jena_cover <- seas_cover %>% filter( site_code %in% "jena.de" ) %>%
  separate(date, c("samp_yr", "samp_month", "samp_day"), sep="-") %>%
  filter(samp_month %in% c("08" , "09")) # only august- september data

head(jena_cover)

# same for biomass
seas_biomass %>% filter( site_code %in% "jena.de")  %>% distinct(site_code, date, year_trt)

jena_biomass <- seas_biomass %>% filter( site_code %in% "jena.de" ) %>%
  separate(date, c("samp_yr", "samp_month", "samp_day"), sep="-") %>%
  filter(samp_month == "08"| samp_month ==  "09"| is.na(samp_month)  ) # only august september data and 'NA' in first years

jena_biomass %>% filter( site_code %in% "jena.de")  %>% distinct(site_code, samp_yr, samp_month, samp_day, year_trt)

colnames(cover)
cover$functional_group <- as.factor(cover$functional_group)
levels(cover$functional_group)

# clean cover data
clean_cover <- cover %>%
  filter(!site_code %in% c("amlr.is", # fertilized 2 months before 1st measurement, no true year 0
                           "ethamc.au", # 0 biomass at year 0 in strip, but plants in plot- cannot be used
                           "jena.de" # seasonal issue
  )) %>%
  bind_rows(jena_cover) %>% # seasonal jena data
  filter( live == 1, # keep only live cover
          !functional_group %in% c("BRYOPHYTE", "LICHEN", "CLUBMOSS", "LIVERWORT", "NON-LIVE", "WOODY"), # drop non vascular plants and woody plants
          complete.cases(Family), # only complete cases for 'Family' (bare ground is blank for this field)
  )  %>% 
  unite("id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE) # create unique id

clean_cover %>% filter(site_code == "jena.de")
summary(clean_cover)
head(clean_cover)


# biomass clean up
head(biomass)
biomass$category<- as.factor(biomass$category)
levels(biomass$category)

# clean biomass
clean_biomass <- biomass %>% 
  #remove special sites
  filter(!site_code %in% c("amlr.is", # fertilized 2 months before 1st measurement, no true year 0
                           "ethamc.au", # 0 biomass at year 0 in strip, but plants in plot- cannot be used
                           "jena.de"  # seasonal issue
  )) %>%
  bind_rows(jena_biomass) %>% # seasonal jena data
  unite( "id", c("site_code","year","year_trt","trt","block","plot"), remove=FALSE) %>% # make unique id for biomass
  filter( live == 1, # keep only live biomass
          # remove functional groups
          !category %in% c("BRYOPHYTE", "DOWNED WOODY DEBRIS","FUNGUS",
                           "LICHEN","LITTER","NON-VASCULAR","STANDING DEAD","WOODY" ) # drop non-vascular plants and woody plants
  ) 

summary(clean_biomass)  
head(clean_biomass)


# calculate plot level cover 
colnames(clean_cover)

calc_cover <- clean_cover %>% group_by(id,site_code,year,year_trt,trt,block,plot,subplot) %>% 
  summarise(plot.cover = sum(max_cover)) %>% # summarize by subplot because  at least one site measured 2 subplots in a year (but think its an observational site)
  left_join(clean_cover) %>%  # rejoin with cover data
  rename(subplot.cov = subplot) %>% 
  arrange(id,Taxon) 

head(calc_cover)

#  calculate biomass from total biomass samples

head(clean_biomass)

calc_biomass <- clean_biomass %>% 
  group_by(id,site_code,year,year_trt,trt,block,plot,subplot) %>%
  summarise(strip.mass = sum(mass)) %>% # sum strip biomass to strip, specify this is strip mass
  left_join(clean_biomass) %>% 
  arrange(site_code,year_trt,trt,block,plot,subplot,category) %>% ungroup() %>%
  mutate(orig.bm.cat = category, # identify biomass functional group categories
         subplot.bm = subplot) # identify subplot biomass strip taken from

head(calc_biomass)

# sites that measured total biomass
total_biomass   <- calc_biomass %>% 
  # remove these sites because they sorted mass by mostly functional group but occasional pooling of live, vascular or other  mass
  filter(!site_code %in% c("ahth.is", "cbgb.us", "lakta.se", "look.us", "sevi.us", "thth.is" , "vargrass.no", "varheath.no")) %>%
  ungroup() %>% select(id,site_code,year,year_trt,trt,block,plot,subplot.bm,orig.bm.cat,mass,strip.mass) %>%
  filter(orig.bm.cat %in% c("TOTAL","VASCULAR", "LIVE")) %>% # filter by total vascular or live biomass
 arrange(id)

head(total_biomass)

# sites that separated biomass - remove the totals
sep_biomass <- calc_biomass %>%  
  select(id,site_code,year,year_trt,trt,block,plot,subplot.bm,orig.bm.cat,category,mass,strip.mass) %>%
  filter(!orig.bm.cat %in% c("TOTAL","VASCULAR", "LIVE")) # drop total measures of biomass

head(sep_biomass)

# per species biomass estimates for sites that measures total biomass
total_biomass_calc <- total_biomass %>% left_join(calc_cover, by= c("id", "site_code", "year", "year_trt", "trt", "block", "plot" ) ) %>%
  select(-c(site_name, Family, live, local_provenance)) %>% 
  group_by(id) %>% 
  mutate(biomass.sp.full = c( max_cover/plot.cover * strip.mass), # estimate per species biomass from total samples
         biomass.m.full = "total") %>% # label this method
  mutate(orig.mass = mass) %>% select(-mass) %>% arrange(id, subplot.cov)

head(total_biomass_calc) 

  
# special case 
# shps sorted biomass by annual perennial
ap_dat <- calc_cover %>%  filter(site_code == "shps.us" ) %>%  # special case
  #select(id, site_code, year, year_trt, trt, block, plot,subplot.cov, plot.cover,local_lifespan,Taxon,max_cover) %>%
  select(id, site_code, year, year_trt, trt, block, plot,subplot.cov,local_lifeform,local_lifespan,functional_group,Taxon, plot.cover,max_cover) %>%
  mutate(category = local_lifespan) %>%
  mutate(category = if_else(Taxon == "ANTENNARIA SP.", # categorize Antennaria as perennial so it will match
                            "PERENNIAL",
                            category)) 

ap_cov <- ap_dat  %>%
  group_by(id,site_code,year,year_trt,trt,block,plot,category) %>%  # group by category
  summarise(cat.cover = sum(max_cover)) %>% # calculate category cover
  ungroup() %>%
  left_join(ap_dat ) %>% 
  mutate( category.mod = category) %>%
  select(id, site_code, year, year_trt, trt, block, plot,subplot.cov,local_lifeform,local_lifespan,functional_group,category.mod,Taxon, plot.cover,max_cover) 
 

# remove previous site from last step
sep_cover <- calc_cover %>%  ungroup() %>% filter(!site_code == "shps.us") %>% 
  mutate( category.mod  = fct_recode(functional_group, c("GRAMINOID" = "GRASS")) ) %>% # put grass into graminoid category to match biomass categories
  select(id, site_code, year, year_trt, trt, block, plot,subplot.cov,local_lifeform,local_lifespan,functional_group,category.mod,Taxon, plot.cover,max_cover) %>%
  bind_rows(ap_cov) # add ap_cov back in

head(sep_cover)
sep_cover$category.mod <- as.character(sep_cover$category.mod)
# rename cover category to match biomass for phlox diffusa in  bnch.us
sep_cover_mod <- sep_cover %>% 
  mutate(category.mod = if_else(Taxon == "PHLOX DIFFUSA", "FORB + PHLOX DIFFUSA", category.mod)) 

sep_cover_calc <- sep_cover_mod %>%
  group_by(id,site_code,year,year_trt,trt,block,plot, category.mod) %>% 
  summarise(cat.cover = sum(max_cover)) %>%
  left_join(sep_cover_mod) %>% ungroup() 

head(sep_cover_calc)

# double check phlox diffusa case
bnch <- sep_cover_calc %>% filter(site_code=="bnch.us" & year=="2018" & trt == "NPK") 
head(bnch, n=20) # yep all good

# prep biomass data
colnames(sep_biomass)

sep_biomass_calc <- sep_biomass %>% ungroup() %>% 
  mutate(category.mod = fct_recode(category, c("FERN" = "PTERIDOPHYTE"  ))) %>% # change fern to match cover and biomass
  group_by(id,site_code,year,year_trt,trt,block,plot,category.mod) %>% 
  mutate(cat.mass = mass) %>%
  left_join(sep_biomass) %>%
  select(id, site_code, year, year_trt, trt, block, plot,subplot.bm,category,category.mod,orig.bm.cat,mass, strip.mass,cat.mass) %>%
  mutate(orig.mass = mass) %>% select(-mass) 

# sanity check phlox diffusa
bnch <- sep_biomass_calc %>% filter(site_code =="bnch.us" & year == "2018" & trt == "NPK") 
bnch

head(sep_cover_calc)
head(sep_biomass_calc)

sep_dat_strip <- sep_cover_calc %>% select(-category.mod, id, Taxon, max_cover, plot.cover) %>%
  left_join(sep_biomass_calc, by= c("id") ) %>%
  distinct(id, Taxon, max_cover, plot.cover, strip.mass ) %>%
  mutate( biomass.sp.plot = c(max_cover / plot.cover * strip.mass), # estimate per species biomass from total samples for comparison
  ) %>% ungroup() %>% select(id, Taxon, biomass.sp.plot)

head(sep_dat_strip)

sep_dat_cat <- sep_cover_calc %>% 
  left_join(sep_biomass_calc, by= c("id", "site_code", "year", "year_trt", "trt", "block", "plot", "category.mod" ) ) %>%
  group_by(id,site_code,year,year_trt,trt,block,plot) %>% # group by plot
  mutate(biomass.sp.cat = c( max_cover/cat.cover * cat.mass), # estimate per species biomass from category groups
         biomass.sp.full = biomass.sp.cat, # rename columns to match down the road
  ) %>% ungroup() 

head(sep_dat_strip)
head(sep_dat_cat)

sep_dat <- sep_dat_cat %>% left_join(sep_dat_strip) 

head(sep_dat)

# last steps! almost there!
# some plots have a functional group that was not recorded in biomass but was in cover
# so the biomass for that particular species comes up as a 0
# for these whole plots, we use plot level biomass to calculate per species biomass
# we replace all per species biomass estimates done from catgoeries with plot level, only on a plot case-by-case basis where this happened


# replace Na's with 0's (theres a mixture)
sep_dat_na <- sep_dat %>% 
  select(id,cat.mass,biomass.sp.cat,biomass.sp.plot,Taxon) %>%
  replace(is.na(.), 0) %>%
  filter(cat.mass == 0 , biomass.sp.cat == 0, biomass.sp.plot > 0 ) %>% # where category mass is 0 and biomass sp calc'd by category is 0
  distinct(id) %>%
  arrange(id) %>% left_join(sep_dat) %>% 
  mutate(biomass.m.plot = "plot") %>% # label these calculation's as being calc'd from plot level results (biomass method)
  mutate(biomass.sp = biomass.sp.plot)  

head(sep_dat_na)
head(sep_dat)

sep_dat_combine <- sep_dat %>% # combine sep dat and NA dat
  left_join(sep_dat_na) %>%
  mutate(biomass.m.cat = "category") # label biomass method 'category'

head(sep_dat_combine)

sep_dat_clean <- sep_dat_combine %>% 
  mutate(biomass.sp.full = ifelse(is.na(biomass.sp), # create a column called  biomass.sp.full
                                  biomass.sp.cat, # we replace rows with 'NA's' in biomass.sp (plot level) with biomass.sp.cat to complete the rows
                                  biomass.sp),
         biomass.m.full = ifelse(is.na(biomass.m.plot), # biomass.m.full becomes our column that tells us how each row was calculated (method)
                                 biomass.m.cat,   # this was done at the plot level
                                 biomass.m.plot) ) %>%
  select(-biomass.sp,-biomass.m.plot,-biomass.m.cat) %>% 
  #bind_rows(ap_dat) %>% # add in annual perennial sorted data
  distinct() %>% arrange(id)

head(sep_dat_clean)

strip_mass <- calc_biomass %>% distinct(id, strip.mass)

complete_dat_calc <- total_biomass_calc %>% 
  bind_rows(sep_dat_clean) %>% select(-strip.mass) %>% 
  left_join(strip_mass) %>% 
  arrange(id)

head(complete_dat_calc)
# we should have 3  biomass species calculation methods; 
# total (where total biomass measurements were taken),   
# category (where biomass was sorted by functional group categories,
# or plot, where strip biomass was substituted for functional group biomass because functional groups between biomass strip
# and cover plot don't fully match (e.g., a legume was present in cover but not in biomass samp
complete_dat_calc$biomass.m.full<- as.factor(complete_dat_calc$biomass.m.full)
levels(complete_dat_calc$biomass.m.full)

# sanity check: any duplicates?
# include subplot.cov, because some sites measured multiple subplots within plots for cover, but only one for biomass

dup.summary <- complete_dat_calc %>% group_by(id, subplot.cov, subplot.bm, Taxon) %>% filter(n()>1) %>% summarize(n=n()) %>%
  select(id,subplot.cov,Taxon,n)

head(dup.summary)


# what's the diff between calc's?
complete_dat_calc$biomass.sp.diff <-  abs(complete_dat_calc$biomass.sp.plot - complete_dat_calc$biomass.sp.cat) 

colnames(complete_dat_calc)

samp <- complete_dat_calc %>% top_frac(.5) %>% arrange(desc(biomass.sp.diff)) %>%
  select(id,Taxon,strip.mass,category,strip.mass,biomass.sp.plot,biomass.sp.cat,biomass.sp.diff) 
head(samp)

# join with comb_by_plot data
summary(complete_dat_calc)
final_dat <- complete_dat_calc %>% left_join(comb) %>% left_join(cover) %>%
  filter(!is.na(biomass.sp.full))

summary(final_dat)

# what are the largest bimoass values / species and are they weird ?
biggest.bm.values <- final_dat %>% select(id, site_code, year_trt, trt, Taxon, category, category.mod, orig.bm.cat,
                                          orig.mass, strip.mass, orig.bm.cat, biomass.sp.full, biomass.m.full) %>%
  top_frac(.5) %>%arrange(desc(biomass.sp.full)) 
# need help cleaning this, but lets just pay attention to treatments and data we need
# in the next script we will do this, and then repeat this excercise, and then revisit this script to fix any errors
# or special cases
head(biggest.bm.values)

colnames(final_dat)
colnames(comb)

sum.method <- final_dat %>% distinct(site_code,biomass.m.full)
head(sum.method)


write.csv(final_dat, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/new/biomass_sp.csv")







