#!/bin/bash
mkdir -p figs/results/; mv /src/ src/
if [[ ! -e data/master_data.csv ]]; then
  touch data/master_data.csv
fi
cd src/util/; python3 pmat_import.py
cd ../; bash run.sh -a
cd ../;  rm -r src/

