
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
params <- read.csv(paste0(path, '/nn_time.rich.parameters.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

task_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

year_max <- params[task_id]$year_max
model_name <- params[task_id]$model_name


# richness
rich.3 <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
                    data = plot, cores = 4,iter=6000, warmup = 1000, chains = 4)

rich.5 <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
              data = plot, cores = 4,iter=6000, warmup = 1000, chains = 4)


rich.6 <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
              data = plot, cores = 4,iter=6000, warmup = 1000, chains = 4)

rich.10 <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
              data = plot, cores = 4,iter=6000, warmup = 1000, chains = 4)

rich.all <- brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
               data = plot, cores = 4,iter=6000, warmup = 1000, chains = 4)

#biomass
bm.3 <- brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                  data = plot , family=student(),  cores = 4,iter=6000, warmup = 1000, chains = 4)

bm.5 <- brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
             data = plot , family=student(),  cores = 4,iter=6000, warmup = 1000, chains = 4)

bm.6 <- brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
             data = plot , family=student(),  cores = 4,iter=6000, warmup = 1000, chains = 4)


bm.10 <- brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
             data = plot , family=student(),  cores = 4,iter=6000, warmup = 1000, chains = 4)

bm.all <- brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
             data = plot , family=student(),  cores = 4,iter=6000, warmup = 1000, chains = 4)


save(rich.3,
     file=Sys.getenv('OFILE'))

save(rich.5,
     file=Sys.getenv('OFILE'))

save(rich.6,
     file=Sys.getenv('OFILE'))

save(rich.10,
     file=Sys.getenv('OFILE'))

save(rich.all,
     file=Sys.getenv('OFILE'))


save(bm.3,
     file=Sys.getenv('OFILE'))

save(bm.5,
     file=Sys.getenv('OFILE'))

save(bm.6,
     file=Sys.getenv('OFILE'))

save(bm.10,
     file=Sys.getenv('OFILE'))

save(bm.all,
     file=Sys.getenv('OFILE'))
