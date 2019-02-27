#!/bin/bash

#$ -S /bin/bash
#$ -N nutnet_price_total

#$ -l h_rt=24:00:00
#$ -l h_vmem=8G

#$ -o /work/$USER/$JOB_NAME-$JOB_ID-$TASK_ID.log
#$ -j y

#$ -binding linear:1

#$ -m a
#$ -M emma.ladouceur@ufz.de

#$ -cwd

module load R/3.4.3-1

prefix=$1
output_dir=/work/$USER/$JOB_NAME/$JOB_ID
mkdir -p $output_dir

Rscript \
  nutnet_price_total.R \
  /data/idiv_chase/emmala/NutNet/input/${prefix}_$SGE_TASK_ID.rds \
  $output_dir/output-$SGE_TASK_ID.rds
