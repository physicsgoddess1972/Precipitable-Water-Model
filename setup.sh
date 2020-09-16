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
while getopts "ighx" opt; do
	case "${opt}" in
## Install Packages
	i)
		echo -e "\e[96m ~~~~ Installing Packages ~~~~\e[0m"
# System Requirements
		sudo apt-get install gfortran libbz2-dev libv8-dev libcurl4-openssl-dev libxml2-dev libssl-dev unzip
		sudo apt-get install zlib1g-dev libpcre3-dev liblzma-dev
		sudo pip3 install numpy pandas metpy requests rich sklearn seaborn cvxopt
		sudo pip3 install git+https://github.com/Unidata/siphon.git
		tar -zxf ./R-3.6.1.tar.gz
		cd ./R-3.6.1/ && ./configure --with-readline=no --with-x=no && sudo make && sudo make install
		cd ../ && sudo rm -r ./R-3.6.1/ && rm R-3.6.1.tar.gz
## R Package Requirements
	  sudo su - -c "R -e \"install.packages('crayon', repos='https://cran.rstudio.com/', dependencies=TRUE)\""
	  sudo su - -c "R -e \"install.packages('argparse', repos='https://cran.rstudio.com/')\""
	  sudo su - -c "R -e \"install.packages('devtools', repos='https://cran.rstudio.com/', dependencies=TRUE)\""
	  sudo su - -c "R -e \"devtools::install_version('plotrix', version='3.5', repos='https://cran.rstudio.com/')\""
	  sudo su - -c "R -e \"install.packages('RColorBrewer', repos='https://cran.rstudio.com/')\""
		sudo su - -c "R -e \"devtools::install_github('PharaohCola13/pacviz')\""
		;;
	h)
		echo "usage: setup.sh [-higx]"
		echo ""
		echo "arguments:"
		echo "   -h         show this help message and exit"
		echo "   -i         run installation procedure"
		echo "   -g         run repo download procedure"
		echo "   -x         run clear data procedure"
		;;
### Download and Unzip Repository
	g)
	  echo -e "\e[96m ~~~~ Downloading Repository ~~~~\e[0m"
	  wget https://github.com/physicsgoddess1972/Precipitable-Water-Model/archive/master.zip
	  unzip master.zip
		;;
	x)
### Clear Data Files
	  echo -e "\e[96m ~~~~ Clearing Directories and Files ~~~~\e[0m"
	  rm -r ./Precipitable-Water-Model-master/data/
	  rm -r ./Precipitable-Water-Model-master/docs/
	  rm -r ./Precipitable-Water-Model-master/src/archive/
	  rm ./Precipitable-Water-Model-master/*.md
	  mkdir ./Precipitable-Water-Model-master/data/ ./Precipitable-Water-Model-master/data/ml
	  echo "Sensor,Error [C],Color Code,D to S,Poster,Temp Range [C]" > ./Precipitable-Water-Model-master/data/instruments.txt
		;;
	esac
done
### Done
echo -e "\e[96m ~~~~ Setup Complete ~~~~\e[0m"
