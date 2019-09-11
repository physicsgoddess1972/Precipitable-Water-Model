#!/usr/bin/env bash
while getopts "o" opt; do
    case "${opt}" in
        o) o_flag="true";;
    esac
done

if [[ ${o_flag} ]]; then
    echo -e "\e[96m ~~~~ Overcast ~~~~\e[0m"
    Rscript model.r --set t -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Time Series plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set a -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Analytical plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set c --save &> /dev/null
    echo -e "\e[92m[Overcast] Charts saved to ~/Downloads/\e[0m"
    Rscript model.r --set i -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Individual Sensor plots haved to ~/Downloads/\e[0m"
    Rscript model.r --poster -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Poster plots saved to ~/Downloads/\e[0m"
else
    echo -e "\e[96m ~~~~ Clear Sky ~~~~\e[0m"
    Rscript model.r --set t --save &> /dev/null
    echo -e "\e[92m[Clear Sky] Time Series plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set a --save &> /dev/null
    echo -e "\e[92m[Clear Sky] Analytical plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set c --save &> /dev/null
    echo -e "\e[92m[Clear Sky] Charts saved to ~/Downloads/\e[0m"
    Rscript model.r --set i --save &> /dev/null
    echo -e "\e[92m[Clear Sky] Individual Sensor plots haved to ~/Downloads/\e[0m"
    Rscript model.r --poster --save &> /dev/null
    echo -e "\e[92m[Clear Sky] Poster plots saved to ~/Downloads/\e[0m"
fi
