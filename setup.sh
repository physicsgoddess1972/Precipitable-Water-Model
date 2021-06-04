	####
## Title: 	setup.sh
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [bash -h]
####

#!/usr/bin/env bash
### Introduction
echo -e "\e[96m ~~~~ Setup Starting ~~~~\e[0m"
rvers="R-4.0.3"
## Flag
while getopts "ighxca" opt; do
	case "${opt}" in
## Install Packages
	i)
		echo -e "\e[96m ~~~~ Installing Packages ~~~~\e[0m"
# System Requirements
		sudo apt install gfortran libbz2-dev libv8-dev libcurl4-openssl-dev libxml2-dev libssl-dev unzip libfontconfig1-dev
		sudo apt install zlib1g-dev libpcre3-dev liblzma-dev python3-pip python3-dev g++ libgit2-dev default-jdk libharfbuzz-dev libfribidi-dev
		sudo apt install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
		sudo pip3 install numpy pandas metpy requests rich sklearn seaborn cvxopt
		sudo pip3 install git+https://github.com/Unidata/siphon.git
		curl -O https://cran.rstudio.com/src/base/R-${rvers:2:1}/${rvers}.tar.gz
		sudo tar -zxf ./R-*.tar.gz -C /opt/
		cd /opt/R-*/ && sudo ./configure --with-readline=no --with-x=no --with-cairo=yes --with-pcre1 && sudo make && sudo make install
		sudo cp ./bin/R /bin/R && sudo cp ./bin/R /usr/bin/R && sudo cp ./bin/R /usr/local/bin/R
		sudo cp ./bin/Rscript /bin/Rscript && sudo cp ./bin/Rscript /usr/bin/Rscript && sudo cp ./bin/Rscript /usr/local/bin/Rscript
		cd - && rm R-*.tar.gz

## R Package Requirements
	  sudo R -e "install.packages('argparse', repos='https://cran.rstudio.com/')"
	  sudo R -e "install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)"
	  sudo R -e "devtools::install_version('plotrix', version='3.5', repos='https://cran.rstudio.com/')"
	  sudo R -e "install.packages('RColorBrewer', repos='https://cran.rstudio.com/')"
		sudo R -e "install.packages('Metrics', repos='https://cran.rstudio.com/')"
		sudo R -e "install.packages('pacviz', repos='https://cran.rstudio.com/')"
		sudo R -e "install.packages('Hmisc', repos='https://cran.rstudio.com/')"
		echo -e "\n\e[93mPlease check to make sure that your version is ${rvers}\e[39m"
		R --version
		Rscript --version
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
	  echo "Sensor,Error [C],Color Code,D to S,Poster,Temp Range [C]" > ./data/instruments.conf;;
	a)
		bash ./setup.sh -i
		bash ./setup.sh -x
		bash ./setup.sh -c
	;;
	esac
done
### Done
echo -e "\e[96m ~~~~ Setup Complete ~~~~\e[0m"
