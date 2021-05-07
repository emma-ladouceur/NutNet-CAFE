#!/bin/bash

#SBATCH -J rich.array
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4

# Output files should ideally go to /work instead of /home
#SBATCH --output=/work/%u/%x-%A-%a.out

# lines=$(wc -l /gpfs1/data/idiv_chase/emmala/NutNet/rich.parameters.csv)
# sbatch -a 2-$lines /home/ladouceu/projects/NutNet-CAFE/cluster/univariate_models/rich.array.R
#to submit this job run these line


module load foss/2019b R/3.6.2-2
export ODIR=/work/$USER/NutNet/
export LANG=en_US.UTF-8
Rscript --vanilla /home/ladouceu/projects/NutNet-CAFE/cluster/univariate_models/rich.array.R