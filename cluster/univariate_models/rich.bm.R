
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
brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
                    data = plot, cores = 4,iter=6000, warmup = 1000, chains = 4)


#biomass
brm( strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
                  data = plot , family=student(),  cores = 4,iter=6000, warmup = 1000, chains = 4)



save( ,
     file=Sys.getenv('OFILE'))
