#!/bin/bash

#$ -S /bin/bash
#$ -N nutnet_price_total

#$ -l h_rt=24:00:00
#$ -l h_vmem=8G

#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.log
#$ -j y

#$ -binding linear:1

#$ -t 1-2

#$ -m beas
#$ -M emma.ladouceur@ufz.de

#$ -cwd

module load R/3.4.3-1

output_dir=/work/$USER/SeedAdd/$JOB_ID
mkdir -p $output_dir

Rscript \
  Price_Open_C.R \
  /data/idiv_chase/emmala/SeedAdd/input/input-$SGE_TASK_ID.rds \
  $output_dir/output-$SGE_TASK_ID.rds
