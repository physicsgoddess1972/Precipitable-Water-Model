#! bin/bash

for i in `seq 1 10000`; do
  Rscript paper_figures.r >>  ./rsme_riley.txt
  echo "$i out of 10000"
done