FROM rocker/binder:latest
MAINTAINER "Shinya Uryu" <r.sh1nya.uryu@gmail.com>

## Copies your repo files into the Docker Container
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    fonts-ipafont-gothic

## Become normal user again
USER ${NB_USER}

RUN Rscript -e 'install.packages(c("config", "drake", "estatapi", "ggforce", "here", "hrbrthemes", "jpmesh", "jpndistrict", "naniar", "RSelenium", "skimr"))'
