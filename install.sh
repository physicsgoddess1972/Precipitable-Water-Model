#!/usr/bin/env bash
## System Requirements
sudo apt-get install r-base

## R Package Requirements
sudo su - -c "R -e \"install.packages('plotrix', dependencies=TRUE,  repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
