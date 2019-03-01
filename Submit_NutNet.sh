#!/bin/bash

#$ -S /bin/bash
#$ -N nutnet_price_total

#$ -l h_rt=24:00:00
#$ -l h_vmem=8G

#$ -o /work/$USER/$JOB_NAME-$JOB_ID.log
#$ -j y

#$ -binding linear:1

#$ -m a
#$ -M emma.ladouceur@ufz.de

#$ -cwd

module load R/3.4.3-1

experiment=$1
input=$2
output_dir=/work/$USER/$JOB_NAME/$experiment
mkdir -p $output_dir

prefix=$(basename "$input" .rds)

Rscript \
  nutnet_price_total.R \
  $input \
  $output_dir/$prefix.rds
