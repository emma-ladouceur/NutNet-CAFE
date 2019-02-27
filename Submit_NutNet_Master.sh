#!/bin/bash

find /data/idiv_chase/emmala/NutNet/input/ -name '*.rds' -printf '%f\n' |
  awk -F_ '{ print $1 }' |
  sort |
  uniq -c |
  while read -r nfiles prefix
  do
    qsub -t 1-$nfiles Submit_NutNet.sh $prefix
  done
  