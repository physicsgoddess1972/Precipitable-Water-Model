#!/usr/bin/env bash
## System Requirements
<<<<<<< HEAD
sudo apt-get install r-base libv8-dev libcurl4-openssl-dev
=======
apt-get install r-base libv8-dev python-dev python-matplotlib
>>>>>>> ca1466bb526ebf013956cc2af66e724e0b5ff6c8

## R Package Requirements
su - -c "R -e \"install.packages('randomcoloR', repos='https://cran.rstudio.com/')\""
su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/')\""
su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
su - -c "R -e \"library(devtools)\""
su - -c "R -e \"install_github('mpastell/Rpyplot)\""