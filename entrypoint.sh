#!/bin/bash
mkdir -p figs/results/ src/ src/util
ls
mv main/src/model.r src/model.r
mv main/src/run.sh src/run.sh
mv main/src/util/*.py src/util/
rm -r main
touch data/master_data.csv
cd src/util/; python3 instruments.py; python3 data_import.py
cd ../; bash run.sh -a
cd ../;  rm -r src/ && rm data/instruments.conf
