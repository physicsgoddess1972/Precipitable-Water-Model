#!/usr/bin/env bash
while getopts ":c:p:l:e:b:tix" opt; do
    case "${opt}" in
        c) c_flag="${OPTARG}";;
        p) p_flag="${OPTARG}";;
        l) l_flag="${OPTARG}";;
        e) e_flag="${OPTARG}";;
        b) b_flag="${OPTARG}";;
        t) t_flag="true";;
        i) i_flag="true";;
        x) x_flag="true";;
    esac
done
if [[ ${i_flag} ]]; then
    sudo pip install tensorflow==1.4.1
    sudo pip install keras==2.1.3
    sudo su - -c "R -e \"install.packages('tidyverse', repos='https://cran.rstudio.com/')\""
    sudo su - -c "R -e \"install.packages('caret', repos='https://cran.rstudio.com/')\""
    sudo su - -c "R -e \"install.packages('fastDummies', repos='https://cran.rstudio.com/')\""
    sudo su - -c "R -e \"install.packages('devtools', repos='https:/cran.rstudio.com/')\""
    sudo su - -c "R -e \"devtools::install_version('keras',version='2.1.3', repos='https://cran.rstudio.com/')\""
fi
if [[ ! -d "./model/logs_${l_flag}/" ]]; then
    mkdir ./model/logs_${l_flag}/
fi

if [[ ${x_flag} ]]; then
    rm -r ./model/*
fi

for ((i = 1 ; i < ${c_flag}+1 ; i++ )); do
    Rscript cloud_magic.r -r $i -l "${l_flag}" -e "${e_flag:=100}" -b "${b_flag:=10}"
done

if [[ ${t_flag} ]]; then
    tensorboard --logdir ./model/ --port ${p_flag:=12345}
fi
