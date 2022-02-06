#!/bin/bash
mkdir -p figs/results/; mv /src/ src/
if [[ ! -e data/master_data.csv ]]; then
  touch data/master_data.csv
fi
cd src/
Rscript pmat_run.r --dir ./data/ -all
Rscript pmat_run.r --dir ./data/ -all -o
cd ../;  rm -r src/

