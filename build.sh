#!/usr/bin/env bash
set -e

usage="
.
.  build.sh CMD [OPTIONS]
.

  Used to run tests and run the devel site.

  Uses Make to ensure the dev database is running. Consult db.makefile for more
  DB control options.

  CMD can be:

      devel    -- Assumed if no CMD is given. Launches development site.
      test     -- Runs Stack tests. Does not launch site.

      cleandb  -- Blow away the database.

  'test' accepts any additional options native to 'stack test'

"

#  Figure out project root from this script's location and make it absolute:
projRoot=$(cd $(dirname "$0"); git rev-parse --show-toplevel)

export PGDATA="$projRoot"/.postgres-work
export PGHOST="$PGDATA"
export PGDATABASE="snowdrift_development"

# Using stack ensures postgres exists for Nix users, thanks to stack's Nix
# support.
dbmake=(stack exec -- make -s -C "$projRoot" -f db.makefile)

run_devel () {
    cd "$projRoot"/website
    stack --work-dir .stack-devel build yesod-bin
    stack --work-dir .stack-devel exec yesod devel
}

with_db () {
    # Shut down the database on exit
    ( trap "${dbmake[*]} stop" EXIT
    # . . . and start it now
    PGDATABASE=$1 ${dbmake[*]}
    shift
    $@
    ) # DB shutdown happens now
}

main () {
    if [ -z "$1" ]; then
        CMD=devel
    else
        CMD="$1"
        shift
    fi

    # Configure local Stripe keys for shell, devel, and test.
    [ -e .stripe_keys ] && source .stripe_keys

    case $CMD in
        devel)
            with_db snowdrift_development run_devel
            ;;
        test)
            touch website/src/Settings/StaticFiles.hs
            with_db snowdrift_test stack --work-dir .stack-test test "$@"
            ;;
        cleandb)
            ${dbmake[*]} clean
            ;;
        *)
            echo "$usage"
            ;;
    esac
}

main "$@"
