#!/usr/bin/env bash
####
## Title: 	pmat.sh
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [bash pmat.sh -h]
####
while getopts "iRU" opt; do
  case "${opt}" in
    i)
        codename=$(lsb_release -a | cut -d':' -f 2 | sed -n 4p)
        os=$(lsb_release -a | cut -d':' -f 2 | sed -n 1p | xargs)
        repo="deb [arch=amd64] https://download.docker.com/linux/${os,,} ${codename} stable"

        sudo apt update
        sudo apt install apt-transport-https ca-certificates curl software-properties-common
        curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -
        sudo add-apt-repository "${repo}"
        sudo apt update
        apt-cache policy docker-ce
        sudo apt install docker-ce
        sudo docker pull ghcr.io/physicsgoddess1972/pmat
        ;;
    R)
        sudo docker container create --name pmat ghcr.io/physicsgoddess1972/pmat
        sudo docker start pmat
        sudo docker cp data/. pmat:/data
        sudo docker exec pmat bash entrypoint.sh
        sudo docker cp pmat:/figs/. figs
        sudo docker cp pmat:/data/. data
        sudo docker stop pmat
        sudo docker rm pmat
        ;;
    U)
        sudo docker pull ghcr.io/physicsgoddess1972/pmat
        ;;
    *)
        echo "Invalid option"
  esac
done
