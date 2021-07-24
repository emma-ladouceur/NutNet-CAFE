
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot <- plot %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()

# bm.3 <- brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
#                   data = plot , family=student(),  cores = 4, iter=6000, warmup = 1000, chains = 4)

bm.3_p <- brm(strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                 data = plot, family=student(), cores = 4, chains = 4,
                 #iter=5000, warmup = 1000,
                 prior = c(
                   prior(normal(285,287), class = Intercept),
                   prior(normal(257,10), class = b, coef = trt.yNPK),
                   prior(normal(0,10), class = b, coef = year.y.m),
                   prior(normal(0,10), class = b, coef = trt.yNPK:year.y.m),
                   prior(normal(0,10), class = sd),
                   prior(normal(0,10), class = sigma),
                   prior(constant(10), class = nu)),
                 control = list(max_treedepth = 12))

save(bm.3_p,
     file=Sys.getenv('OFILE'))

