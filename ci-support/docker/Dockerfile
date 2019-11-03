FROM ubuntu:16.04

RUN apt-get -qq update &&\
    apt-get -qq install --no-install-recommends curl postgresql libgmp-dev libpq-dev ca-certificates

RUN curl -sSL https://get.haskellstack.org/ | sh
