#!/usr/bin/env bash
### Introduction
echo -e "\e[96m ~~~~ Setup Starting ~~~~\e[0m"
## Install Packages
echo -e "\e[96m ~~~~ Installing Packages ~~~~\e[0m"
# System Requirements
apt-get install r-base libv8-dev libcurl4-openssl-dev libxml2-dev libssl-dev unzip
## R Package Requirements
R -e "install.packages('crayon', repos='https://cran.rstudio.com/')"
R -e "install.packages('argparse', repos='https://cran.rstudio.com/')"
R -e "install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)"
R -e "devtools::install_version('plotrix', version='3.5', repos='https://cran.rstudio.com/')"
R -e "devtools::install_github('ronammar/randomcoloR')"
### Download and Unzip Repository
echo -e "\e[96m ~~~~ Downloading Repository ~~~~\e[0m"
wget https://github.com/physicsgoddess1972/Precipitable-Water-Model/archive/master.zip
unzip master.zip
### Clear Data Files
echo -e "\e[96m ~~~~ Clearing Directories and Files ~~~~\e[0m"
rm -r ./Precipitable-Water-Model-master/data/
rm -r ./Precipitable-Water-Model-master/docs/
rm -r ./Precipitable-Water-Model-master/src/archive/
rm -r ././Precipitable-Water-Model-master/src/demo/
mkdir ./Precipitable-Water-Model-master/data/
### Done
echo -e "\e[96m ~~~~ Setup Complete ~~~~\e[0m"
