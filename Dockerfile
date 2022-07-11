# Container image that runs your code
FROM r-base:latest
ENV DEBIAN_FRONTEND=noninteractive

COPY src/ /pmat/src
COPY requirements.txt /pmat/requirements.txt
COPY DESCRIPTION /pmat/DESCRIPTION

RUN apt-get update && apt-get install -y --no-install-recommends build-essential libpq-dev python3 python3-pip python3-setuptools python3-dev
ENV PYTHONPATH "${PYTHONPATH}:/pmat"

WORKDIR /pmat

RUN pip3 install -r requirements.txt
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_deps()"

ENTRYPOINT Rscript /pmat/src/pmat_run.r --dir $INPUT_DIR --out $INPUT_OUT $INPUT_FLAGS
