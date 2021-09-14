#! bin/bash
for i in `seq 1 5000`; do
  Rscript paper_figures.r >>  ./rsme_riley_55.txt
  echo -e "\e[1A\e[K$i out of 5000"
done