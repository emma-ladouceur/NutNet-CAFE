#!/bin/bash

#$ -S /bin/bash
#$ -N 3_Price_Pairs

#$ -l h_rt=24:00:00
#$ -l h_vmem=8G

#$ -o /work/$USER/$JOB_NAME-$JOB_ID.log
#$ -j y

#$ -binding linear:1

#$ -m a
#$ -M emma.ladouceur@ufz.de

#$ -cwd

module load foss/2019b R/3.6.2-2

experiment=$1
input=$2
output_dir=/work/$USER/$JOB_NAME/$experiment
mkdir -p "$output_dir"

prefix=$(basename "$input" .rds)

Rscript \
  3_Price_Pairs.R \
  "$input" \
  "$output_dir/$prefix.rds"
