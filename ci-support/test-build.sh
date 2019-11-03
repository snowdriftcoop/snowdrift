#!/usr/bin/env bash

set -Eeuo pipefail

## Build all, including tests

touch website/src/Settings/StaticFiles.hs
stack --stack-root $(pwd)/stack-root test --no-run-tests
