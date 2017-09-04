#!/usr/bin/env bash

read -d '' usage <<EOF
.
.  build.sh CMD [OPTIONS]
.

  Used to do standard Haskell/yesod things like run tests and run the
  devel site.

  It uses the shake build system to make sure a dev/test database is
  running.

  CMD can be:

      devel
      test
      psql
      shell

  'test' and 'psql' both accept any additional options native to those
  commands.

  'shell' is an advanced command, which sets up the Postgres and stack
  environments and spawns a new shell.
EOF

run_devel () {
    cd `dirname $0`/website
    if [ -z "$IN_NIX_SHELL" ]; then
        stack build yesod-bin &&
        exec stack exec yesod devel
    else
        exec yesod devel
    fi
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

    # Configure local Stripe keys for shell, devel, and test.
    [ -e .stripe_keys ] && source .stripe_keys

    stack build --flag Snowdrift:library-only --only-dependencies --install-ghc Snowdrift:test &&
    dbenv &&
    case $CMD in
        devel)
            run_devel
            ;;
        test)
            exec stack test --flag Snowdrift:library-only --fast $@
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
