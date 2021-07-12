#!/bin/bash
mkdir -p figs/results/; mv /src/ src/
touch data/master_data.csv
cd src/util/; python3 instruments.py; python3 data_import.py
cd ../; bash run.sh -a
sudo chmod -R ug+w figs/
cd ../;  rm -r src/ && rm data/instruments.conf
