#! bin/bash

n=5000
fname="../../data/paper/rsme_riley_$n.txt"

for i in $(seq 1 $n); do
  Rscript auto_analysis.r >>  $fname
  echo -e "\e[1A\e[K$i out of $n"
done

Rscript variance.r -f $fname