#!/usr/bin/env bash
while getopts "omah" opt; do
    case "${opt}" in
        o) o_flag="true";;
        m) m_flag="true";;
        a) a_flag="true";;
        h) h_flag="true";;
    esac
done

if [[ ${o_flag} ]]; then
    echo -e "\e[96m ~~~~ Overcast ~~~~\e[0m"
    Rscript model.r --set t -o --save &> /dev/null & Rscript model.r --set a -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Time Series plots saved to ~/Downloads/\e[0m"
    echo -e "\e[92m[Overcast] Analytical plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set c --save &> /dev/null & Rscript model.r --set i -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Charts saved to ~/Downloads/\e[0m"
    echo -e "\e[92m[Overcast] Individual Sensor plots haved to ~/Downloads/\e[0m"
    Rscript model.r --poster -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Poster plots saved to ~/Downloads/\e[0m"
elif [[ ${m_flag} ]]; then
    echo -e "\e[96m ~~~~ Modtran Plots ~~~~\e[0m"
    Rscript modtran.r
    echo -e "\e[92mModtran Plot saved to ~/Downloads/\e[0m"
elif [[ ${a_flag} ]]; then
    echo -e "\e[96m ~~~~ All Plots ~~~~\e[0m"
    Rscript model.r --set t -o --save &> /dev/null & Rscript model.r --set a -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Time Series plots saved to ~/Downloads/\e[0m"
    echo -e "\e[92m[Overcast] Analytical plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set c --save &> /dev/null & Rscript model.r --set i -o --save &> /dev/null
    echo -e "\e[92m[Overcast] Charts saved to ~/Downloads/\e[0m"
    echo -e "\e[92m[Overcast] Individual Sensor plots haved to ~/Downloads/\e[0m"
    Rscript model.r --poster -o --save &> /dev/null & Rscript model.r --poster --save &> /dev/null
    echo -e "\e[92m[Overcast] Poster plots saved to ~/Downloads/\e[0m"
    echo -e "\e[92m[Clear Sky] Poster plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set t --save &> /dev/null & Rscript model.r --set a --save &> /dev/null
    echo -e "\e[92m[Clear Sky] Time Series plots saved to ~/Downloads/\e[0m"
    echo -e "\e[92m[Clear Sky] Analytical plots saved to ~/Downloads/\e[0m"
    Rscript model.r --set i --save &> /dev/null & Rscript model.r --set c --save &> /dev/null
    echo -e "\e[92m[Clear Sky] Individual Sensor plots haved to ~/Downloads/\e[0m"
    echo -e "\e[92m[Clear Sky] Charts saved to ~/Downloads/\e[0m"
    Rscript modtran.r
    echo -e "\e[92mModtran Plot saved to ~/Downloads/\e[0m"
elif [[ ${h_flag} ]]; then
    echo "usage: run.sh [-hoam]"
    echo ""
    echo "arguments:"
    echo "   -h         show this help message and exit"
    echo "   -o         save overcast plots"
    echo "   -a         save all plots"
    echo "   -m         save modtran plots"
else
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
