#! bin/bash
for i in `seq 1 10000`; do
  Rscript paper_figures.r >>  ./rsme_riley_75.txt
  echo -e "\e[1A\e[K$i out of 10000"
done