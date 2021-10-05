
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

# plot <- plot %>% group_by(site_code) %>% filter(year_max >= 3) %>%
#   ungroup()

bm.all <- brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot),
                  data = plot , family=student(),  cores = 4, iter=6000, warmup = 1000, chains = 4)


save(bm.all,
     file=Sys.getenv('OFILE'))

