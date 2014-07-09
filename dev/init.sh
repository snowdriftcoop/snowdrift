#!/bin/bash

if [ ! -f "README.md" ]; then
  echo "Please run from the root directory."
  exit 1;
fi

source ./dev/utils.sh

requireWriteOn "/tmp"

require ghc
require cabal
require happy
require postgres
require ./dev/reset-db.sh

ghc --version
cabal --version

echo ""
echo "We require ghc version 7.8.2 or later, and Cabal (the library) version 1.20.0.1 or later."
read -p "Do you have the correct versions (printed above)? " yn
case $yn in
  [Yy]*) ;;
  *)
      echo "For ghc: www.haskell.org/ghc/download"
      echo "For Cabal: cabal update && cabal install cabal-install && cabal install Cabal"
      exit 1
      ;;
esac

# cabal sandbox init
# cabal install --only-dependencies --enable-tests

# cp config/postgres.template config/postgres.yml

# sudo -u postgres createuser -S -D -R snowdrift_development
# sudo -u postgres createuser -S -d -R snowdrift_test

./dev/reset-db.sh
