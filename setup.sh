####
## Title: 	setup.sh
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [bash -h]
####

#!/usr/bin/env bash
## Flags
while getopts "aighx" opt; do
	case "${opt}" in
		a) a_flag="true";;
		i) i_flag="true";;
		g) g_flag="true";;
    h) h_flag="true";;
		x) x_flag="true";;
	esac
done
## Help Flag
if [[ ${h_flag} ]]; then
	echo "usage: setup.sh [-hcoma]"
	echo ""
	echo "arguments:"
	echo "   -h         show this help message and exit"
	echo "   -a         run complete setup procedure"
	echo "   -i         run installation procedure"
	echo "   -g         run repo download procedure"
	echo "   -x         run clear data procedure"
fi
if [[ ${a_flag} ]]; then
  i_flag="true"
  g_flag="true"
  x_flag="true"
fi
### Introduction
echo -e "\e[96m ~~~~ Setup Starting ~~~~\e[0m"
if [[ ${i_flag} ]]; then
## Install Packages
  echo -e "\e[96m ~~~~ Installing Packages ~~~~\e[0m"
# System Requirements
  sudo apt-get install r-base libv8-dev libcurl4-openssl-dev libxml2-dev libssl-dev unzip
## R Package Requirements
  sudo su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/')\""
  sudo su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
  sudo su - -c "R -e \"install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)\""
  sudo su - -c "R -e \"devtools::install_version('plotrix', version='3.5', repos='https://cran.rstudio.com/')\""
  sudo su - -c "R -e \"devtools::install_github('ronammar/randomcoloR')\""
fi
if [[ ${g_flag} ]]; then
### Download and Unzip Repository
  echo -e "\e[96m ~~~~ Downloading Repository ~~~~\e[0m"
  wget https://github.com/physicsgoddess1972/Precipitable-Water-Model/archive/master.zip
  unzip master.zip
fi
if [[ ${x_flag} ]]; then
### Clear Data Files
  echo -e "\e[96m ~~~~ Clearing Directories and Files ~~~~\e[0m"
  rm -r ./Precipitable-Water-Model-master/data/
  rm -r ./Precipitable-Water-Model-master/docs/
  rm -r ./Precipitable-Water-Model-master/src/archive/
  rm ./Precipitable-Water-Model-master/*.md
  mkdir ./Precipitable-Water-Model-master/data/ml
  mkdir ./Precipitable-Water-Model-master/data/
  echo "Sensor,Error [C],Color Code,D to S,Poster,Temp Range [C]" > ./Precipitable-Water-Model-master/data/instruments.txt
fi
### Done
echo -e "\e[96m ~~~~ Setup Complete ~~~~\e[0m"
