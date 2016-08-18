#!/bin/bash

read -d '' usage <<EOF
.
.  build.sh CMD [OPTIONS]
.

  Used to do standard Haskell/yesod things like run tests, run the devel
  site, and run ghci.

  It uses the shake build system to make sure a dev/test database is
  running.

  CMD can be:

      devel
      ghci
      test

  'ghci' and 'test' both accept any additional options native to those
  commands.
EOF

SHAKEDIR=.shake
SHAKE=$SHAKEDIR/build

run_devel () {
    cd `dirname $0`/website
    stack exec yesod devel
}

run_test () {
    stack test --fast $@
}

run_ghci () {
    cd `dirname $0`/website
    stack ghci $@
}

dbenv () {
    mkdir -p $SHAKEDIR
    stack ghc -- \
        --make sdb.hs \
        -rtsopts -with-rtsopts=-I0 \
        -outputdir=$SHAKEDIR \
        -o $SHAKE
    $SHAKE start
    . <($SHAKE env)
}

main () {
    if [ -z "$1" ]; then
        2>&1 echo "$usage"
        exit 1
    fi
    CMD=$1
    shift

    case $CMD in
        devel)
            dbenv
            run_devel
            ;;
        test)
            dbenv
            run_test $@
            ;;
        ghci)
            dbenv
            run_ghci $@
            ;;
        *)
            echo "$usage"
            ;;
    esac
}

main $@
