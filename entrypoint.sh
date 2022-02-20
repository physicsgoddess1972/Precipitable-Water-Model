#!/bin/bash
mkdir -p out/figs/ out/data/; mv /src/ src/
if [[ ! -e data/master_data.csv ]]; then
  touch data/master_data.csv
fi
cd src/
Rscript pmat_run.r --dir ../data/ -all -u
Rscript pmat_run.r --dir ../data/ -all -u -o
cd ../;  rm -r src/

