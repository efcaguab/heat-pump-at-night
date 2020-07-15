FROM rocker/tidyverse:4.0.2

RUN apt-get update \
  && apt-get -y --no-install-recommends install libglpk-dev libnode-dev

RUN install2.r brms