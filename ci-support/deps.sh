#!/usr/bin/env bash

set -Eeuo pipefail

## System setup

apt --quiet update
apt --quiet --assume-yes install curl postgresql libgmp-dev libpq-dev
curl -sSL https://get.haskellstack.org/ | sh


## Dependencies installation

$STACK test --no-run-tests --only-dependencies
