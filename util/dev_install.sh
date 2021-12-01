#!/usr/bin/env bash
####
## Title: 	setup.sh
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [bash -h]
####
### Introduction
echo -e "\e[96m ~~~~ Setup Starting ~~~~\e[0m"
rvers="R-4.0.3"
## Flag
echo -e "\e[96m ~~~~ Installing Packages ~~~~\e[0m"
# System Requirements
sudo apt install gfortran libbz2-dev libv8-dev libcurl4-openssl-dev libxml2-dev libssl-dev unzip libfontconfig1-dev
sudo apt install zlib1g-dev libpcre2-dev liblzma-dev g++ libgit2-dev default-jdk libharfbuzz-dev libfribidi-dev
sudo apt install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
sudo python3 -m pip install -r ../requirements.txt
curl -O https://cran.rstudio.com/src/base/R-${rvers:2:1}/${rvers}.tar.gz
sudo tar -zxf ./R-*.tar.gz -C /opt/
cd /opt/R-*/ && sudo ./configure --with-readline=no --with-x=no --with-cairo=yes --with-pcre1 && sudo make && sudo make install
sudo cp /opt/R-*/bin/R /bin/R && sudo cp /opt/R-*/bin/R /usr/bin/R && sudo cp /opt/R-*/bin/R /usr/local/bin/R
sudo cp /opt/R-*/bin/Rscript /bin/Rscript && sudo cp /opt/R-*/bin/Rscript /usr/bin/Rscript && sudo cp /opt/R-*/bin/Rscript /usr/local/bin/Rscript
cd - && rm R-*.tar.gz

## R Package Requirements
sudo R -e "install.packages('remotes', repos='https://cran.r-project.org/')"
sudo R -e "remotes::install_deps(pkgdir='../', dependencies = TRUE)"
echo -e "\n\e[93mPlease check to make sure that your version is ${rvers}\e[39m"
R --version
Rscript --version
### Done
echo -e "\e[96m ~~~~ Setup Complete ~~~~\e[0m"
