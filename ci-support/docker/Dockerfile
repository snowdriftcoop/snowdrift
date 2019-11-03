FROM ubuntu:16.04

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

RUN apt-get -qq update &&\
    apt-get -qq install --no-install-recommends curl postgresql libgmp-dev libpq-dev ca-certificates

RUN curl -sSL https://get.haskellstack.org/ | sh
