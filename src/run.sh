####
## Title: 	run.sh
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [bash -h]
####

#!/usr/bin/env bash
## Flags
c_flag=1
while getopts "omahc" opt; do
	case "${opt}" in
		o) o_flag="true";
		    c_flag=0;;
		m) m_flag="true";
		    c_flag=0;;
		a) a_flag="true";
		    c_flag=0;;
		h) h_flag="true";
		    c_flag=0;;
        c) c_flag=1;;
	esac
done
## Help Flag
if [[ ${h_flag} ]]; then
	echo "usage: run.sh [-hcoma]"
	echo ""
	echo "arguments:"
	echo "   -h         show this help message and exit"
	echo "   -c         save clear sky plots [Default option]"
	echo "   -o         save overcast plots"
	echo "   -m         save modtran plots"
	echo "   -a         save all plots"
fi
## Overcast Flag
if [[ ${o_flag} ]]; then
	echo -e "\e[96m ~~~~ Overcast ~~~~\e[0m"
	Rscript model.r --set t -o --save &> /dev/null & Rscript model.r --set a -o --save &> /dev/null
	echo -e "\e[92m[Overcast] Time Series plots saved to ~/Downloads/\e[0m"
	echo -e "\e[92m[Overcast] Analytical plots saved to ~/Downloads/\e[0m"
	Rscript model.r --set c --save &> /dev/null & Rscript model.r --set i -o --save &> /dev/null
	echo -e "\e[92m[Overcast] Charts saved to ~/Downloads/\e[0m"
	echo -e "\e[92m[Overcast] Individual Sensor plots saved to ~/Downloads/\e[0m"
	Rscript model.r --poster --save &> /dev/null
	echo -e "\e[92m[Overcast] Poster plots saved to ~/Downloads/\e[0m"
fi
## Modtran Flag
if [[ ${m_flag} ]]; then
	echo -e "\e[96m ~~~~ Modtran Plots ~~~~\e[0m"
	Rscript modtran.r
	echo -e "\e[92mModtran Plot saved to ~/Downloads/\e[0m"
fi
## All Flag
if [[ ${a_flag} ]]; then
	echo -e "\e[96m ~~~~ All Plots ~~~~\e[0m"
	Rscript model.r --set t -o --save &> /dev/null & Rscript model.r --set a -o --save &> /dev/null
	echo -e "\e[92m[Overcast] Time Series plots saved to ~/Downloads/\e[0m"
	echo -e "\e[92m[Overcast] Analytical plots saved to ~/Downloads/\e[0m"
	Rscript model.r --set i -o --save &> /dev/null & Rscript model.r --set t --save &> /dev/null
	echo -e "\e[92m[Overcast] Individual Sensor plots saved to ~/Downloads/\e[0m"
	echo -e "\e[92m[Clear Sky] Time Series plots saved to ~/Downloads/\e[0m"
	Rscript model.r --set a --save &> /dev/null & Rscript model.r --set i --save &> /dev/null
	echo -e "\e[92m[Clear Sky] Analytical plots saved to ~/Downloads/\e[0m"
	echo -e "\e[92m[Clear Sky] Individual Sensor plots saved to ~/Downloads/\e[0m"
	Rscript model.r --poster --save &> /dev/null & Rscript model.r --set c --save &> /dev/null
	echo -e "\e[92m[All] Poster plots saved to ~/Downloads/\e[0m"
	echo -e "\e[92m[All] Charts saved to ~/Downloads/\e[0m"
	Rscript modtran.r
	echo -e "\e[92mModtran Plot saved to ~/Downloads/\e[0m"
fi
## Default Flag (Clear Sky)
if (( ${c_flag} == 1 )); then
	echo -e "\e[96m ~~~~ Clear Sky ~~~~\e[0m"
	Rscript model.r --set t --save &> /dev/null & Rscript model.r --set a --save &> /dev/null
	echo -e "\e[92m[Clear Sky] Time Series plots saved to ~/Downloads/\e[0m"
	echo -e "\e[92m[Clear Sky] Analytical plots saved to ~/Downloads/\e[0m"
	Rscript model.r --set i --save &> /dev/null & Rscript model.r --set c --save &> /dev/null
	echo -e "\e[92m[Clear Sky] Individual Sensor plots haved to ~/Downloads/\e[0m"
	echo -e "\e[92m[Clear Sky] Charts saved to ~/Downloads/\e[0m"
	Rscript model.r --poster --save &> /dev/null
	echo -e "\e[92m[Clear Sky] Poster plots saved to ~/Downloads/\e[0m"
fi
## Closing
echo -e "\e[96m ~~~~ Complete ~~~~\e[0m"