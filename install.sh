#!/usr/bin/env bash
## System Requirements
<<<<<<< HEAD
sudo apt-get install r-base libv8-dev libcurl4-openssl-dev python-dev python-matplotlib libxml2-dev libssl-dev

## R Package Requirements
sudo su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('randomcoloR', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('devtools', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"devtools::install_github('mpastell/Rpyplot')\""
=======
sudo apt-get install r-base libv8-dev libcurl4-openssl-dev python-dev python-matplotlib

## R Package Requirements
sudo su - -c "R -e \"install.packages('randomcoloR', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('devtools', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"devtools::install_github('mpastell/Rpyplot')\""
>>>>>>> becf4a12a23f0f78982bb53e064d0fbe29168af7
