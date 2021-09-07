####
## Title: 	run.sh
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https:/git.io/fjVHo
## To get a list of arguments run [bash -h]
####

# #!/usr/bin/env bash
N=100
mlN="${3:-$N}"
ts="\e[92mTime Series plots saved to figs/results/\e[0m"
ap="\e[92mAnalytical plots saved to figs/results/\e[0m"
in="\e[92mIndividual Sensor plots saved to figs/results/\e[0m"
ch="\e[92mCharts saved to figs/results/\e[0m"
po="\e[92mPoster plots saved to figs/results/\e[0m"
pa="\e[92mPac-man plots saved to figs/results/\e[0m"

## Flags
while getopts "oahcs" opt; do
	case "${opt}" in
		c)
				echo -e "\e[96m ~~~~ Clear Sky ~~~~\e[0m"
				Rscript pmat_run.r --set t &> /dev/null & Rscript pmat_run.r --set a &> /dev/null
				echo -e "[Clear Sky] ${ts}"
				echo -e "[Clear Sky] ${ap}"
				Rscript pmat_run.r --set i &> /dev/null & Rscript pmat_run.r --set c &> /dev/null
				echo -e "[Clear Sky] ${in}"
				echo -e "[Clear Sky] ${ch}"
				Rscript pmat_run.r --poster &> /dev/null & Rscript pmat_run.r --pacman  &> /dev/null
				echo -e "[Clear Sky] ${po}"
				echo -e "[Clear Sky] ${pa}"
				echo -e "\e[96m ~~~~ Complete ~~~~\e[0m" && exit 0;;
		o)
				echo -e "\e[96m ~~~~ Overcast ~~~~\e[0m"
				Rscript pmat_run.r --set t -o &> /dev/null & Rscript pmat_run.r --set a -o &> /dev/null
				echo -e "[Overcast] ${ts}"
				echo -e "[Overcast] ${ap}"
				Rscript pmat_run.r --set c &> /dev/null & Rscript pmat_run.r --set i -o &> /dev/null
				echo -e "[Overcast] ${ch}"
				echo -e "[Overcast] ${in}"
				Rscript pmat_run.r --poster &> /dev/null & Rscript pmat_run.r --pacman -o &> /dev/null
				echo -e "[Overcast] ${po}"
				echo -e "[Overcast] ${pa}"
				echo -e "\e[96m ~~~~ Complete ~~~~\e[0m" && exit 0;;
		a)
				echo -e "\e[96m ~~~~ All Plots ~~~~\e[0m"
				Rscript pmat_run.r --set t -o &> /dev/null & Rscript pmat_run.r --pacman -o &> /dev/null
				echo -e "[Overcast] ${ts}"
				echo -e "[Overcast] ${pa}"
				Rscript pmat_run.r --set a -o &> /dev/null & Rscript pmat_run.r --set i -o &> /dev/null
				echo -e "[Overcast] ${ap}"
				echo -e "[Overcast] ${in}"
				Rscript pmat_run.r --set t &> /dev/null & Rscript pmat_run.r --pacman &> /dev/null
				echo -e "[Clear Sky] ${ts}"
				echo -e "[Clear Sky] ${pa}"
				Rscript pmat_run.r --set a &> /dev/null & Rscript pmat_run.r --set i &> /dev/null
				echo -e "[Clear Sky] ${ap}"
				echo -e "[Clear Sky] ${in}"
				Rscript pmat_run.r --poster &> /dev/null & Rscript pmat_run.r --set c &> /dev/null
				echo -e "[All] ${po}"
				echo -e "[All] ${ch}"
				Rscript pmat_run.r --data -ml &> /dev/null
				echo -e "\e[92mMachine Learning Data saved\e[0m"
				echo -e "\e[96m ~~~~ Complete ~~~~\e[0m" && exit 0;;
		s)
				Rscript pmat_run.r --data -ml &> /dev/null
				echo -e "\e[92mMachine Learning Data saved\e[0m"
				python3 ./ml/svm/class_svm_tf.py -dfile "../data/ml/" -N $mlN -ffile "../figs/ml/"
				echo -e "\e[96m ~~~~ Complete ~~~~\e[0m" && exit 0;;
		h)
				echo "usage: run.sh [-hcoas] [-N int]";
				echo "";
				echo "arguments:";
				echo "   -h         show this help message and exit";
				echo "   -c         save clear sky plots [Default option]";
				echo "   -o         save overcast plots";
				echo "   -a         save all plots";
				echo "   -s         runs SVM Module";
				echo "   -N         number of random states for SVM Module [Default: 100]"
				exit 0;;
		*)
		  echo "Invalid option"
	esac
done
if [ "${opt}" = "?" ]; then
		./run.sh -c
fi
