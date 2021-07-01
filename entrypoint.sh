#!/bin/bash
ls ; ls /
mkdir -p figs/results/; mv /src/ src/
touch data/master_data.csv
cd src/util/; python3 instruments.py; python3 data_import.py
cd ../; bash run.sh -a
cd ../;  rm -r src/ && rm data/instruments.conf
