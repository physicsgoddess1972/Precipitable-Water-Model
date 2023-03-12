# Container image that runs your code
FROM r-base:latest
ENV DEBIAN_FRONTEND=noninteractive
ENV PYTHONUNBUFFERED 1

COPY src/ /pmat/src
COPY requirements.txt /pmat/requirements.txt
COPY DESCRIPTION /pmat/DESCRIPTION

RUN apt-get update && apt-get install -y --no-install-recommends build-essential libpq-dev python3 python3-pip python3-setuptools python3-dev python3-venv

RUN python3 -m venv .venv
ENV VENVPATH="./venv/bin:${VENVPATH}"
ENV PYTHONPATH "${PYTHONPATH}:/pmat"

RUN python3 -m pip install -Ur requirements.txt

COPY --from=compiler ./venv ./venv

WORKDIR /pmat
ENV VENVPATH="./venv/bin:${VENVPATH}"
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_deps()"

ENTRYPOINT Rscript /pmat/src/pmat_run.r --dir $INPUT_DIR --out $INPUT_OUT $INPUT_FLAGS
