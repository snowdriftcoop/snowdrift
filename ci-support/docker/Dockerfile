FROM ubuntu:16.04

# The default is POSIX, which doesn't work for us
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

RUN apt-get -qq update &&\
    apt-get -qq install --no-install-recommends \
        ca-certificates \
        curl \
        libgmp-dev \
        libpq-dev \
        sudo \
        upx-ucl

# If we don't install these, stack does -- and it doesn't use
# --no-install-recommends.
RUN apt-get -qq install --no-install-recommends \
        g++ gcc libffi-dev make xz-utils git netbase

RUN curl -sSL https://get.haskellstack.org/ | sh
