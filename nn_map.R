
library("ggplot2")
theme_set(theme_bw())
library("sf")                 
library(ggrepel)


library("rnaturalearth")
library("rnaturalearthdata")


nnmap<- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)                 


ggplot(data = world) +
  geom_sf()



# Libraries
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/17_ListGPSCoordinates.csv", sep=",", header=T)

# Get the world polygon
world <- map_data("world")

# Reformat data: I count the occurence of each unique position
p <- data %>%
  mutate(homelat=round(homelat,1)) %>%
  mutate(homelon=round(homelon,1)) %>%
  #head(1000) %>%
  group_by(homelat, homelon, homecontinent) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.1) +
  geom_point(aes(x=homelon, y=homelat, color=homecontinent, size=n), alpha=0.5) +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  scale_size_continuous(range=c(0.2,68)) +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
    legend.position=c(0.15,0.07),
    legend.direction="horizontal"
  ) +
  ggplot2::annotate("text", x = -165, y = -30, hjust = 0, size = 11, label = paste("Where surfers live."), color = "Black") +
  ggplot2::annotate("text", x = -165, y = -36, hjust = 0, size = 8, label = paste("data-to-viz.com | 200,000 #surf tweets recovered"), color = "black", alpha = 0.5) +
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 

p

# Save at PNG
ggsave("IMG/Surfer_bubble.png", width = 36, height = 15.22, units = "in", dpi = 90)

# Libraries
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)
library(ggrepel)

# Load dataset from github
data <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv", sep=",", header=T)
start.rich <-read.csv("~/Dropbox/Projects/NutNet/Data/start.rich.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


colnames(data)
data <- distinct(data, site_code, latitude, longitude, year_trt,continent)


data2 <- data %>%
  group_by(site_code) %>%
  summarise('Length of study' = max(year_trt))


data.l <- data %>% distinct( site_code, latitude, longitude, continent) %>% left_join(start.rich) %>%
  rename(`Starting Richness`=starting.richness)

data.f <- left_join(data.l,data2)
View(data.f)


# Get the world polygon
world <- map_data("world")


data.f$`Starting Richness` <- factor(data.f$`Starting Richness`, levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


# Reformat data: I count the occurence of each unique position
n <- data.f %>%
  group_by(latitude, longitude, site_code, `Length of study`, continent) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=longitude, y=latitude, color=`Starting Richness`, size=`Length of study`), alpha=0.5) +
  geom_label_repel(
    aes(x=longitude, y=latitude, label = site_code),family = 'Times',
    segment.size = 0.5, segment.alpha = 0.5,
    size = 3,
    box.padding = 0.1, point.padding = 0.3, fill = NA,
    segment.color = 'grey50') +
  scale_colour_manual(values = c("1-5 species" = "#E5BA3AFF",
                                 "6-10" = "#75B41EFF",
                                 "11-15" ="#5AC2F1FF",
                                 "16-20"= "#0C5BB0FF",
                                 "21-25" = "#972C8DFF",
                                 ">26" = "#E0363AFF", drop =FALSE))+
  #scale_color_viridis(discrete=FALSE,name="Length of Study") +
  scale_size_continuous(range=c(2,8), name="Length of Study") +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(1,1,1,1), "cm"),
    legend.position=c(0.13,0.001),
    legend.direction="horizontal"
  ) +
   ggplot2::annotate("text", x = -185, y = -34, hjust = 0, size = 7, label = paste("The Nuterient Network"), color = "Black") +
   ggplot2::annotate("text", x = -181, y = -44, hjust = 0, size = 4, label = paste("Experimental Locations"), color = "black", alpha = 0.5) +
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 

n

# Save at PNG
ggsave('~/Dropbox/Projects/NutNet/Plots/nn_map.png', width = 36, height = 15.22, units = "in", dpi = 90)
