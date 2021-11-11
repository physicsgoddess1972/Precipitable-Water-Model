#!/bin/bash
mkdir -p figs/results/; mv /src/ src/
if [[ ! -e data/master_data.csv ]]; then
  touch data/master_data.csv
fi
cd src/; python3 pmat_import.py
bash run.sh -a
cd ../;  rm -r src/

