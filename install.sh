#!/usr/bin/env bash
## System Requirements
apt-get install r-base libv8-dev python-dev python-matplotlib

## R Package Requirements
su - -c "R -e \"install.packages('randomcoloR', repos='https://cran.rstudio.com/')\""
su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/')\""
su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
su - -c "R -e \"library(devtools)\""
su - -c "R -e \"install_github('mpastell/Rpyplot)\""