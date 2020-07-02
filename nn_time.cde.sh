#!/bin/bash

#$ -N nn_time_p.cde
#$ -S /bin/bash
#$ -l h_rt=24:00:00
#$ -l h_vmem=8G
#$ -pe smp 4

#$ -o /home/$USER/$JOB_NAME-$JOB_ID.out
#$ -j y

module load R/3.4.3-1
export OFILE=/work/$USER/NutNet/$JOB_NAME-$JOB_ID.Rdata
export LANG=en_US.UTF-8
Rscript --vanilla /home/ladouceu/projects/NutNet/nn_time.cde.R


