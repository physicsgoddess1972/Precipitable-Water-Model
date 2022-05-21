#! bin/bash
TIMEFORMAT='%3R'

dir="./util/tests/data"
cd ../
for j in {1..10000}; do
  k=$((j+1))
  mytime="$(time ( Rscript pmat_run.r --dir $dir/ --set a -u ) 2>&1 1>/dev/null )"
  echo -n "$j" >> ./util/time.txt
  echo -e "\t$mytime" >> ./util/time.txt
  sed -i "s/step: $j/step: $k/g" $dir/_pmat.yml
done