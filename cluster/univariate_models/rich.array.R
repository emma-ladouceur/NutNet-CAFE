
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/NutNet'
plot <- read.csv(paste0(path, '/plot.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
params <- read.csv(paste0(path, '/rich.parameters.csv'), header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

task_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

param_year_max <- as.numeric(params[task_id]$year_max)
model_name <- params[task_id]$model_name

if(is.na(year_max)){
plot <- plot %>% group_by(site_code) %>% 
  ungroup()
}else{
  plot <- plot %>% group_by(site_code) %>% filter(year_max >= param_year_max) %>%
    ungroup()
}


output <-  brm(rich ~  trt * year_trt + (trt * year_trt | site_code/block/plot), 
                    data = plot , cores = 4, iter=6000, warmup = 1000, chains = 4)


output_filename <- paste0(model_name, ".rds")
output_file <- file.path(Sys.getenv('ODIR'), output_filename)


saveRDS(output ,
     file = output_file )


