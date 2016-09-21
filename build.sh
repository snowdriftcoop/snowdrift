#!/bin/bash

read -d '' usage <<EOF
.
.  build.sh CMD [OPTIONS]
.

  Used to do standard Haskell/yesod things like run tests, run the devel
  site, and run ghci.

  It uses make *and* the shake build system to make sure a dev/test
  database is running. The system should probably be refactored. :)

  CMD can be:

      devel
      ghci
      test
      psql
      shell

  'ghci', 'test', and 'psql' all accept any additional options native to
  those commands.

  'shell' is an advanced command, which sets up the Postgres and stack
  environments and spawns a new shell.
EOF

SHAKEDIR=.shake
SHAKE=$SHAKEDIR/build

run_devel () {
    cd `dirname $0`/website
    exec stack exec yesod devel
}

run_ghci () {
    cd `dirname $0`/website
    exec stack ghci --package foreign-store $@
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

    make
    dbenv
    case $CMD in
        devel)
            run_devel
            ;;
        test)
            exec stack test --fast $@
            ;;
        ghci)
            run_ghci $@
            ;;
        psql)
            exec psql $@
            ;;
        shell)
            exec stack exec bash
            ;;
        *)
            echo "$usage"
            ;;
    esac
}

main $@
