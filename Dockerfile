FROM uribo/tiny_rocker_minimum:latest
MAINTAINER Shinya Uryu <r.sh1nya.uryu@gmail.com>

RUN install2.r \
  caTools bitops \
  config estatapi here hrbrthemes liftr \
  remotes RSelenium
