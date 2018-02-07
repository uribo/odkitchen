FROM rocker/geospatial:latest
MAINTAINER "Shinya Uryu" <r.sh1nya.uryu@gmail.com>

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    fonts-ipafont-gothic
    # fonts-ipafont-mincho

RUN install2.r \
  config drake estatapi ggforce here hrbrthemes \
  jpmesh jpndistrict liftr naniar RSelenium skimr \
  usethis rlang
