#!/usr/bin/env bash

set -Eeuo pipefail

# Project-specific override
stack () {
    command stack ${SD_STACK_ARGS?-} --work-dir .stack-deploy "$@"
}

## System setup

apt-get --quiet update
apt-get --quiet --assume-yes install curl postgresql libgmp-dev libpq-dev
curl -sSL https://get.haskellstack.org/ | sh


## Dependencies installation

stack test --no-run-tests --only-dependencies
