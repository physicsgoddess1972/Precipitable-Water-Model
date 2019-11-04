#!/usr/bin/env bash
## System Requirements
sudo apt-get install r-base libv8-dev libcurl4-openssl-dev python-dev python-matplotlib libxml2-dev libssl-dev

## R Package Requirements
sudo su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)\""
sudo su - -c "R -e \"devtools::install_version('plotrix', version='3.5', repos='https://cran.rstudio.com/')\""
#sudo su - -c "R -e \"devtools::install_github('mpastell/Rpyplot')\""
sudo su - -c "R -e \"devtools::install_github('ronammar/randomcoloR')\""