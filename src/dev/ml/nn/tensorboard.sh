#!/usr/bin/env bash
while getopts "a:o:c:p:l:e:b:tixh" opt; do
    case "${opt}" in
        a) a_flag="${OPTARG}";;
        b) b_flag="${OPTARG}";;
        c) c_flag="${OPTARG}";;
        e) e_flag="${OPTARG}";;
        h) h_flag="true";;
        i) i_flag="true";;
        l) l_flag="${OPTARG}";;
        o) o_flag="${OPTARG}";;
        p) p_flag="${OPTARG}";;
        t) t_flag="true";;
        x) x_flag="true";;
    esac
done
if [[ ${h_flag} ]]; then
    echo "usage: tensorboard.sh [-ihtx] [abceo -NUM] [-l LOC] [-p PORT]"
    echo ""
    echo "arguments:"
    echo "   -h         show this help message and exit"
    echo "   -o NUM     optimizer
                1) relu"
    echo "   -a NUM     activation
                1) adam"
    echo "   -c NUM     number of configurations"
    echo "   -l LOC     designation for run"
    echo "   -e NUM     number of epochs"
    echo "   -b NUM     batch size"
    echo "   -t         initialize tensorboard"
    echo "   -i         install requirements"
    echo "   -x         clear output and log files"
    echo "   -p PORT    port number for tensorboard"
elif [[ ${i_flag} ]]; then
    echo -e "\e[92m~~~ Installation Starting ~~~\e[0m"
    sudo pip3 install tensorflow==1.4.1
    sudo pip3 install keras==2.1.3
    sudo su - -c "R -e \"install.packages('tidyverse', repos='https://cran.rstudio.com/')\""
    sudo su - -c "R -e \"install.packages('caret', repos='https://cran.rstudio.com/')\""
    sudo su - -c "R -e \"install.packages('fastDummies', repos='https://cran.rstudio.com/')\""
    sudo su - -c "R -e \"install.packages('devtools', repos='https:/cran.rstudio.com/')\""
    sudo su - -c "R -e \"devtools::install_version('keras',version='2.1.3', repos='https://cran.rstudio.com/')\""
    echo -e "\e[92m~~~ Installation Complete ~~~\e[0m"
elif [[ ${t_flag} ]]; then
    tensorboard --logdir ./model/ --port ${p_flag:=12345}
elif [[ ${x_flag} ]]; then
    echo -e "\e[93mAttempting to delete all model and output files.\e[0m"
    echo -ne "\e[91mAre you sure? [y/n]:\e[0m "; read ans
    if [[ $ans == "y" ]]; then
        if [[ ./model/* ]]; then
            rm -r ./model/*
            echo -e "\e[92mLog files have been successfully removed\e[0m"
        fi
        if [[ ./output/* ]]; then
            if rm ./output/* ; then
                rm ./output/*
                echo -e "\e[92mOutput files have been successfully removed\e[0m"
            fi
        fi
    else
        echo -e "\e[92mOperation Aborted\e[0m"
    fi
fi

if [[ ! -d "./model/logs_${l_flag}/" ]]; then
    if [[ ! -z ${l_flag} ]]; then
        mkdir "./model/logs_${l_flag}/"
        echo -e "\e[92mThe directory ./model/logs_${l_flag}/ has been created.\e[0m"
    fi
else
    echo "Directory already exists"
fi

for ((i = 1 ; i < ${c_flag}+1 ; i++ )); do
    Rscript cloud_magic.r -r $i -l "${l_flag}" -e "${e_flag:=100}" -b "${b_flag:=10}" -o "${o_flag}" -a "${a_flag}"
done
