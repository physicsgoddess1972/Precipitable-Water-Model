####
## Title: 	setup.sh
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [bash -h]
####

#!/usr/bin/env bash
### Introduction
echo -e "\e[96m ~~~~ Setup Starting ~~~~\e[0m"
## Flag
while getopts "ighxca" opt; do
	case "${opt}" in
## Install Packages
	i)
		echo -e "\e[96m ~~~~ Installing Packages ~~~~\e[0m"
# System Requirements
                sudo apt-get install gfortran libbz2-dev libv8-dev libcurl4-openssl-dev libxml2-dev libssl-d$
                sudo apt-get install zlib1g-dev libpcre3-dev liblzma-dev python3-pip python3-dev g++
                sudo pip3 install numpy pandas metpy requests rich sklearn seaborn cvxopt
                sudo pip3 install git+https://github.com/Unidata/siphon.git
                curl -O https://cran.rstudio.com/src/base/R-3/R-3.6.1.tar.gz
                tar -zxf ./R-3.6.1.tar.gz
                cd ./R-3.6.1/ && ./configure --with-readline=no --with-x=no --with-cairo=yes && sudo make &&$
                cd ../ && sudo rm -r ./R-3.6.1/ && rm R-3.6.1.tar.gz
## R Package Requirements
	  sudo su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/', dependencies=TRUE)\""
	  sudo su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
	  sudo su - -c "R -e \"install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)\""
	  sudo su - -c "R -e \"devtools::install_version('plotrix', version='3.5', repos='https://cran.rstudio.com/')\""
	  sudo su - -c "R -e \"install.packages('RColorBrewer', repos='https://cran.rstudio.com/')\""
		sudo su - -c "R -e \"devtools::install_github('PharaohCola13/pacviz')\""
		;;
	c)
## Configure database import sites
		echo "https://mesowest.utah.edu/cgi-bin/droman/meso_station.cgi?area=1"
		echo -n "Please input the appropriate MesoWest site identifier: "
		read mesowest
		echo "http://weather.uwyo.edu/upperair/sounding.html"
		echo -n "Please input the appropriate University of Wyoming site identifier: "
		read wyoming

		echo -e "MesoWest: $mesowest\nWyoming: $wyoming" >> ./data/config.txt
		;;
	h)
		echo "usage: setup.sh [-higx]"
		echo ""
		echo "arguments:"
		echo "   -h         show this help message and exit"
		echo "   -i         run installation procedure"
		echo "   -x         run clear data procedure"
		echo "   -c         run site id configuration procedure"
		echo "   -a         run all setup procedures"
		;;
	x)
### Clear Data Files
	  echo -e "\e[96m ~~~~ Clearing Directories and Files ~~~~\e[0m"
	  rm -r ./data/ ./docs/
	  rm -r ./src/archive/ ./src/web-app/
	  rm ./*.md
	  mkdir ./data/ ./data/ml
	  echo "Sensor,Error [C],Color Code,D to S,Poster,Temp Range [C]" > ./data/instruments.txt;;
	a)
		bash ./setup.sh -i
		bash ./setup.sh -x
		bash ./setup.sh -c
	;;
	esac
done
### Done
echo -e "\e[96m ~~~~ Setup Complete ~~~~\e[0m"
