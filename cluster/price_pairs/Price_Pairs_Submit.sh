#!/bin/bash

#SBATCH -J Price_Pairs

#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=8G

# %u is the UserName
# %x is the JobName
# %j is the JobID
# Note: These %patterns ONLY work for specifying --output/-o and --error/-e
# If -e is not specified, stdout and stderr will automatically be merged together
#SBATCH -o /work/%u/%x-%j.log

# Only send E-Mail when the job has ended
#SBATCH --mail-type=END
#SBATCH --mail-user=emma.ladouceur@ufz.de

# Current Working directory is automatically set to the directory the job is submitted from
# Can be changed with the --chdir option, e.g:
# #SBATCH --chdir /work/ladouceur/...
# Note that $USER variable can't be used in any lines that are prefixed with #SBATCH

module load foss/2019b R/3.6.2-2

experiment=$1
input=$2
output_dir=/work/$USER/$SLURM_JOB_NAME/$experiment
mkdir -p "$output_dir"

prefix=$(basename "$input" .rds)

Rscript \
  Price_Pairs.R \
  "$input" \
  "$output_dir/$prefix.rds"
  
  