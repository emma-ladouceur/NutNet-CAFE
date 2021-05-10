

# Authors: Emma Ladouceur 
# Title:
# Last Updated April 18, 2021

# 14_Supplementary_Tables
# This workflow makes Supplementary Table S1 & S2

# packages
library(tidyverse)


# Table S1
comb <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/comb-by-plot-06-May-2021.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na","NULL"))
# this dataset was pulled from the internet
country_codes <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/country_codes.csv", stringsAsFactors = FALSE)
pis <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/pi-contact-list-8-March-2021.csv", stringsAsFactors = FALSE)
quads <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv", stringsAsFactors = FALSE)

colnames(comb)
head(country_codes)

prep <- comb %>% distinct(site_code, site_name, country,habitat) 

prep <- comb %>%
  group_by(site_code) %>%
  summarise(`Experiment Length` = max(year_trt)) %>%
  filter(`Experiment Length` >= 3) %>% left_join(prep) %>%
  mutate(countrycode = country) %>% select(-country) %>% left_join(country_codes) %>%
  select(-countrycode)

head(prep)

pis_prep <- pis %>% select(site_code, firstname, lastname) %>%
  unite(`Site Manager`, firstname:lastname, remove=TRUE, sep=" ") %>%
  group_by(site_code) %>%
  summarise(
    `Site Managers` = paste(`Site Manager`, collapse = ', ') ) 

head(pis_prep)


site_info <- prep %>% left_join(pis_prep)

head(site_info)

table_s1 <- site_info %>% left_join(quads) %>%
  filter(!is.na(Quadrant)) %>% select(-X) %>% arrange(site_code) 


View(table_s1)

write.csv(table_s1,"~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_S1.csv")



# Table S2

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Posteriors/global.p.effs.Rdata')

global.rich.p$Model <- "Species richness"
global.bm.p$Model <- "Biomass"
global.sloss.p$Model <- "Species loss (s.loss)"
global.sgain.p$Model <- "Species gain (s.gain)"
global.sl.p$Model <- "Biomass chage associated with species loss (SL)"
global.sg.p$Model <- "Biomass chage associated with species gain (SG)"
global.cde.p$Model <- "Biomass chage associated with Pprsistent species (PS)"

p.all <- global.rich.p %>% bind_rows(global.bm.p) %>% bind_rows(global.sloss.p) %>% bind_rows(global.sgain.p) %>%
  bind_rows(global.sl.p) %>% bind_rows(global.sg.p) %>% bind_rows(global.cde.p) %>%
  mutate(Treatment = response,
         Estimate = eff,
         Upper_Estimate = eff_upper,
         Lower_Estimate = eff_lower) %>%
  select(-response,-eff,-eff_upper,-eff_lower,
         Model, Treatment, Estimate, Upper_Estimate, Lower_Estimate)

head(p.all)

write.csv(p.all, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_S2.csv')



tabs1 <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_S1.csv", sep=",", header=T)
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv", sep=",", header=T)
comb <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/comb-by-plot-06-May-2021.csv", stringsAsFactors = FALSE)
figs2_dat <-read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Figure_S2_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


comb_deets <- comb %>% select(site_code,site_name, latitude, longitude, continent)

nn_plot <-  plot %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  left_join(tabs1, by= "site_code") %>%
  ungroup()  %>% left_join(comb_deets) %>% left_join(figs2_dat) %>%
  select(site_code,site_name,country, habitat, Quadrant, year_max,  rich.start, rich.end, mass.start,mass.end,latitude,longitude) %>%
  distinct()

View(nn_plot)

write.csv(nn_plot, '~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Table_App.csv')



