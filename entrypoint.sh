#!/bin/bash
mv /src/ src/; cd src/
Rscript pmat_run.r --dir ../data/ -all -u
Rscript pmat_run.r --dir ../data/ -all -u -o
cd ../;  rm -r src/
