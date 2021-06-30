# Container image that runs your code
FROM r-base:4.0.5
RUN apt-get update && apt-get install -y python3 \
                                         python3-pip \
                                         gfortran \
                                         libbz2-dev \
                                         libv8-dev \
                                         libcurl4-openssl-dev \
                                         libxml2-dev \
                                         libssl-dev \
                                         libfontconfig1-dev \
                                         zlib1g-dev \
                                         libpcre3-dev \
                                         liblzma-dev \
                                         g++ \
                                         libgit2-dev \
                                         default-jdk \
                                         libharfbuzz-dev \
                                         libfribidi-dev \
                                         libfreetype6-dev \
                                         libpng-dev \
                                         libtiff5-dev \
                                         libjpeg-dev
RUN cp /usr/bin/python3 /usr/bin/python
COPY DESCRIPTION /DESCRIPTION
COPY requirements.txt /requirements.txt
RUN python3 -m pip install -r /requirements.txt
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_deps(dependencies = TRUE)"
# Copies your code file from your action repository to the filesystem path `/` of the container

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
