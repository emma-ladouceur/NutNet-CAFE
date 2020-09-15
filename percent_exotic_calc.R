




library(tidyverse)
library(vegan)
library(ggplot2)
library(ggpubr)


sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


sp.calc  <- sp %>% select(site_code,Taxon,local_provenance,year_trt) %>%
  group_by(site_code,year_trt,local_provenance) %>%
  summarise(cat_richness = vegan::specnumber(Taxon)) %>%
  ungroup()  %>%  group_by(site_code,year_trt) %>%
  mutate(percent_rich = (cat_richness/sum(cat_richness) * 100))


yearzero <- sp.calc %>% 
  filter(year_trt == 0)


yearmax <- sp.calc %>% 
  group_by(site_code) %>% top_n(1, year_trt)  


sp.years <-  bind_rows(yearzero, yearmax) %>%
  arrange(site_code,year_trt)


View(sp.years)



 sp.rich.p <- sp.years %>%  select(-cat_richness) %>%
  spread(year_trt,local_provenance,percent_rich) 

 View(sp.rich.p)

sp.rich.zero <- sp.rich.p %>% filter(year_trt == 0) %>%
  rename(NAT_0 = NAT) %>%
  rename(INT_0 = INT) %>%
  rename(UNK_0 = UNK) %>%
  rename(NA_0 = `<NA>`) %>%
  ungroup() %>%
  select(-year_trt)


View(sp.rich.zero)

sp.rich.max <- sp.rich.p %>% filter(!year_trt == 0) %>%
  rename(NAT_max = NAT) %>%
  rename(INT_max = INT) %>%
  rename(UNK_max = UNK) %>%
  rename(NA_max = `<NA>`) %>%
  ungroup() %>%
  select(-year_trt)

View(sp.rich.max)


site.max.year <- sp.rich.p %>% filter(!year_trt == 0) %>% select(site_code,year_trt)


sp.change <- sp.rich.max %>% left_join(sp.rich.zero) %>%
mutate(NAT_change = NAT_max - NAT_0) %>%
  mutate(INT_change = INT_max - INT_0) %>%
  mutate(UNK_change = UNK_max - NAT_0) %>%
  mutate(NA_change = NA_max - NA_0) %>%
 #select(site_code,INT_change,INT_0,INT_max) %>%
  left_join(site.max.year)

View(sp.change)



spacetime.calc  <- sp %>% select(site_code,Taxon,local_provenance) %>%
  group_by(site_code,local_provenance) %>%
  summarise(t_richness = vegan::specnumber(Taxon)) %>%
  ungroup()  %>%  group_by(site_code) %>%
  mutate(percent_spacetime = (t_richness/sum(t_richness) * 100)) %>% 
  select(site_code,local_provenance,percent_spacetime) %>%
  left_join(site.max.year) %>% spread(local_provenance,percent_spacetime) %>%
  #select(site_code,INT) %>%
  rename(NAT_st = NAT) %>%
  rename(INT_st = INT) %>%
  rename(UNK_st = UNK) %>%
  rename(NA_st = `<NA>`) %>%
  left_join(sp.change) %>% filter(year_trt >= 3) 


colnames(spacetime.calc)

pp_calc<-spacetime.calc %>% gather(pp_cat,percent,INT_st:NA_change) %>%
  separate(pp_cat,c("provenance","measure"), sep="_") %>%
  arrange(site_code)



write.csv(pp_calc, "~/Dropbox/Projects/NutNet/Data/provenance_percent.csv")


pp <- read.csv("~/Dropbox/Projects/NutNet/Data/provenance_percent.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


colnames(pp)

pp_int <- pp %>% filter(provenance == "INT") %>%
  filter(!is.na(percent)) 
  

View(pp_int)

ggplot(data=pp_int, aes(x=measure, y=percent, color=measure))+
  facet_wrap(~site_code) +
  geom_bar(stat="identity",  fill="white") +
  labs(
    x = 'Measure',
    y = 'Percent Exotic') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                     plot.title = element_text(size=12))




