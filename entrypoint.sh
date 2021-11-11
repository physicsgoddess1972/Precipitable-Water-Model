#!/bin/bash
mkdir -p figs/results/; mv /src/ src/
if [[ ! -e data/master_data.csv ]]; then
  touch data/master_data.csv
fi
cd src/
echo "Starting Data Aquistion"
python3 pmat_import.py
echo "Running PMAT"
bash run.sh -a
cd ../;  rm -r src/

