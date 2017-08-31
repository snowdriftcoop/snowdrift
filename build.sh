#!/usr/bin/env bash

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

run_devel () {
    cd `dirname $0`/website
    if [ -z "$IN_NIX_SHELL" ]; then
        exec stack exec yesod devel
    else
        exec yesod devel
    fi
}

run_ghci () {
    cd `dirname $0`/website
    exec stack ghci --package foreign-store $@
}

dbenv () {
    ./sdb.hs start
    source <(./sdb.hs env)
}

main () {
    if [ -z "$1" ]; then
        CMD=devel
    else
        CMD=$1
        shift
    fi

    make &&
    dbenv &&
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
