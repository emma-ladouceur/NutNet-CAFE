
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)


 plot <- plot %>% group_by(site_code) %>% filter(year_max >= 3) %>%
 ungroup()
 
 rich.3 <- brm(rich ~ trt * year_trt + (trt * year_trt | site_code/block/plot),
                 data = plot, cores = 4, chains = 4,
                 iter=5000, warmup = 1000,
                 prior = c(
                   prior(normal(8,5), class = Intercept),
                   prior(normal(5,1), class = b, coef = trtNPK),
                   prior(normal(0,1), class = b, coef = year_trt),
                   prior(normal(0,1), class = b, coef = trtNPK:year_trt),
                   prior(normal(0,1), class = sd),
                   prior(normal(0,1), class = sigma)),
                 control = list(adapt_delta = 0.99)
 )

save(rich.3,
     file=Sys.getenv('OFILE'))



