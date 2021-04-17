#!/bin/bash

export experiment=$(date +%s)

find /data/idiv_chase/emmala/NutNet/input/ -name '*.rds' |
  while read -r input
  do
    qsub Price_Pairs_Submit.sh $experiment $input
  done
  
