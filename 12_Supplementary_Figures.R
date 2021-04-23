# Authors: Emma Ladouceur 
# Title:
# Last Updated April 18, 2021

# 12_Supplementary_Figures.R
# This workflow make Supplementary Figures S1, S2, & S5
# Figures S3 a-# found in ###

# packages
library(tidyverse)
library(ggplot2)
library(mapdata)
library(ggrepel)
library(viridis)


# data
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv", sep=",", header=T)
quads <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/quads.csv", stringsAsFactors = FALSE)


nn_plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  left_join(quads, by= "site_code") %>%
  ungroup()

colnames(nn_plot)

# Supplementary Figures

# Figure S1
n_map_dat <- nn_plot %>% distinct( site_code, latitude, longitude, year_trt,continent, Quadrant) %>%
  group_by(site_code, latitude, longitude, continent, Quadrant) %>%
  summarise('Length of study' = max(year_trt)) %>% filter(!`Length of study` == 0) %>% droplevels() %>%
  ungroup %>%
  mutate( 'Overall Site Response to NPK' = Quadrant) 

colnames(n_map_dat)

# Get the world polygon
world <- map_data("world")

fig_s1 <- n_map_dat %>%
  group_by(latitude, longitude, site_code, `Length of study`, continent, `Overall Site Response to NPK`) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=longitude, y=latitude, color=`Length of study`, shape= `Overall Site Response to NPK`),size = 2, alpha=0.8) +
  geom_label_repel(
    aes(x=longitude, y=latitude, label = site_code),family = 'Times',
    segment.size = 0.5, segment.alpha = 0.5,
    size = 3,
    box.padding = 0.1, point.padding = 0.3, fill = NA,
    segment.color = 'grey50') +
  scale_color_viridis(discrete=FALSE,name="Length of Study") +
  scale_size_continuous(range=c(2,8), name="Length of Study") +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(1,1,1,1), "cm"),
    legend.position=c(0.25,0.001),
    legend.direction="horizontal"
  ) +
  ggplot2::annotate("text", x = -185, y = -34, hjust = 0, size = 7, label = paste("The Nutrient Network"), color = "Black") +
  ggplot2::annotate("text", x = -181, y = -44, hjust = 0, size = 4, label = paste("Experimental Locations"), color = "black", alpha = 0.5) +
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 

fig_s1

# LANDSCAPE 9 X 14

# Figure S2
colnames(nn_plot)

 plot_clean <- nn_plot %>% filter( trt == "NPK") %>%
  drop_na(plot.mass) %>%
   group_by(site_code, block,plot,year_trt) %>%
   select(continent,id,site_code,block, plot,year_trt,all.div,plot.mass) 
 
plotzero <- plot_clean %>% filter(year_trt %in% '0') %>% 
    arrange(site_code)

plotmax <- plot_clean %>% 
  group_by(site_code) %>% 
  top_n(1, year_trt) %>% arrange(site_code)


plot_mx_mn <- plotzero %>% 
  bind_rows(plotmax) %>% 
  arrange(site_code) %>%
  group_by(site_code,year_trt) %>%
  summarise(m.rich = mean(all.div),
            m.mass = mean(plot.mass)) %>% 
  mutate( startend = ifelse(year_trt < 1 , 'start',
                         ifelse(year_trt >= 1, 'end', 'other')) )


head(plot_mx_mn)

rich.mean <- plot_mx_mn %>%
  select(site_code,m.rich,startend) %>%
  spread(startend,m.rich) %>%
  as_tibble() %>% 
  mutate(rich.start = start,
         rich.end = end ) %>% 
  select(-start, -end)


mass.mean <- plot_mx_mn %>%
  select(site_code,m.mass,startend) %>%
  spread(startend,m.mass) %>%
  as_tibble() %>% 
  mutate(mass.start = start,
         mass.end = end ) %>% 
  select(-start, -end)

yr.range <- plot_mx_mn %>%
  select(site_code,year_trt,startend) %>%
  spread(startend,year_trt) %>%
  as_tibble() %>% 
  mutate(year.min = start,
         year.max = end ) %>% 
  select(-start, -end)

plot.means <- yr.range %>% left_join(rich.mean) %>%
  left_join(mass.mean)

head(plot.means)

write.csv(plot.means, "~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Figure1_dat.csv")



fig1_dat <-read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Figure1_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

head(fig1_dat)

ggplot() +
  geom_point(data = fig1_dat, aes(x = rich.start, y = mass.start),size=1.5, fill="white", shape=1) +
  geom_point(data = fig1_dat, aes(x = rich.end, y = mass.end),size=1.5, colour="white", shape=2) +
  geom_segment(data = fig1_dat,aes(x = rich.start,
                                 xend = rich.end,
                                 y = mass.start,
                                 yend = mass.end,
                                 group = site_code,
                                 color = year.max),  
               arrow=arrow(type = "closed",length=unit(0.2,"cm"))) +
  scale_color_viridis(discrete=F, name="Length of Study") +
  labs(x = 'Species Richness',
       y = expression(paste('Biomass (g/' ,m^2, ')')), 
       title= '') +
  scale_y_continuous(limits=c(0,1500)) +
  scale_x_continuous(limits=c(0,35)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     strip.background = element_blank(),plot.title = element_text(size=12),
                     legend.position="bottom")


# Figure S3: See  ####

# Figure S4 See 13_Supplementary_Figure_S4


# Figure S5

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/study.p.effs.Rdata')

rich.p2 <-rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) %>%
  filter(response == "NPK")

bm.p2<-bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) %>%
  filter(response == "NPK")

effs.p <- rich.p2 %>% left_join(bm.p2)

effs.p

study.rich.p2 <-study.rich.p %>% rename(r.eff=eff,r.eff_upper=eff_upper,r.eff_lower=eff_lower) 

study.bm.p2<-study.bm.p %>% rename(b.eff=eff,b.eff_upper=eff_upper,b.eff_lower=eff_lower) 

study.effs.p <- study.rich.p2 %>% left_join(study.bm.p2) %>% filter(response == "NPK")

colnames(study.effs.p)

study.effs.p$Quadrant <- ifelse(study.effs.p$r.eff < 0 & study.effs.p$b.eff > 0, '-rich +biomass',
                                ifelse(study.effs.p$r.eff < 0 & study.effs.p$b.eff < 0,  '-rich -biomass',
                                       ifelse(study.effs.p$r.eff  > 0 & study.effs.p$b.eff > 0,  '+rich +biomass',
                                              ifelse(study.effs.p$r.eff > 0 & study.effs.p$b.eff < 0, '+rich -biomass','other'))))
View(study.effs.p)

study.effs.p$Quadrant <- factor(study.effs.p$Quadrant, levels= c("-rich +biomass",  "+rich +biomass", "-rich -biomass", "+rich -biomass"))


fig_s5 <- ggplot()+
  facet_wrap(~Quadrant) +
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_point(data= study.effs.p, aes(x= r.eff , y= b.eff), colour="black",alpha=0.2,size=2) +
  geom_errorbar(data=study.effs.p,aes(x= r.eff, y= b.eff,ymin = b.eff_lower, ymax = b.eff_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  geom_errorbarh(data=study.effs.p,aes(x= r.eff, y= b.eff,xmin =  r.eff_lower, xmax =r.eff_upper), colour="black", alpha=0.2, width = 0, size = 0.75) +
  scale_x_continuous(breaks=c(2.5,0,-2.5,-0.5)) +
  scale_y_continuous(breaks=c(200,100,25,0,-25,-100,-200)) +
  labs(x = 'Rate of change in species richness (species/year)',
       y = expression(paste('Rate of change in plot biomass (g/' ,m^2, '/year)')),
       title = ' ')+ theme_classic(base_size=14) + theme(strip.text = element_text(size=14))

fig_s5


