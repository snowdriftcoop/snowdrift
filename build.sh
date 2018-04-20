#!/usr/bin/env bash

#  Figure out project root from this script's location and make it absolute:
projRoot=$(realpath "`dirname "$0"`")

export PGDATA="$projRoot"/.postgres-work
export PGHOST="$PGDATA"
export PGDATABASE="snowdrift"
pgClusterNoun=(make -f "$projRoot"/.sdc) # We'll build a sentence from this.

# If we have been told to use the Nix shell, and we are not already in it, then
# we must instruct Make to use it. But if we were not told to use the Nix shell,
# or if we are already in it, we have no Nix-y things to address. Note that Make
# itself would be running in a Nix environment as a child of this script if this
# script is already in a Nix environment.
if [ "$USE_NIX_SHELL" ] && ! [ "$IN_NIX_SHELL" ]; then
    # Just needs a verb:
    pgClusterSubject=${pgClusterNoun[*]}+=('SHELL=nix-shell')
else
    pgClusterSubject=${pgClusterNoun[*]} # Ditto.
fi

read -d '' usage <<EOF
.
.  build.sh CMD [OPTIONS]
.

  Used to do standard Haskell/Yesod things like run tests and run the
  devel site.

  It uses a Makefile to ensure the dev/test database is running.

  CMD can be:

      devel  -- Assumed if no CMD is given. Launches development site.
      test   -- Runs Stack tests. Does not launch site.

      start
      stop
      clean

      psql
      shell

  'clean' stops the PostgreSQL backend program (server) and "rm -rf"s the
  cluster data files.

  'test' and 'psql' both accept any additional options native to those
  commands.

  'shell' is an advanced command, which sets up the Postgres and Stack
  environments and spawns a new shell.

  'start' is usually ran indirectly through 'devel' or 'test'. It only starts
  the PostgreSQL server, not the website. Similarly, 'stop' simply stops the
  PostgreSQL server.
EOF

run_devel () {
    cd "$projRoot"/website
    if ! [ "$IN_NIX_SHELL" ]; then
        stack build yesod-bin &&
        exec stack exec yesod devel
    else
        exec yesod devel
    fi
}

start_cluster () {
    ${pgClusterSubject[*]} start # subject + verb = sentence
}

build_deps_and_install_ghc () {
    # Some dependencies are considered local packages by Stack, but we don't
    # want './build.sh test --pedantic' to error out on warnings in
    # dependencies. Therefore we build dependencies first without passing on
    # flags.
    #
    # This is fixed in Stack 1.6.1 (and 1.6.1 installs GHC by default, making
    # this function entirely redundant).
    stack build --flag Snowdrift:library-only --only-dependencies --install-ghc Snowdrift:test
}

main () {
    if [ -z "$1" ]; then
        CMD=devel
    else
        CMD="$1"
        shift
    fi

    # Configure local Stripe keys for devel, test and shell.
    [ -e .stripe_keys ] && source .stripe_keys

    case $CMD in
        devel)
            build_deps_and_install_ghc &&
            start_cluster &&
            run_devel
            ;;
        test)
            build_deps_and_install_ghc &&
            start_cluster &&
            exec stack test --flag Snowdrift:library-only --fast "$@"
            ;;
        start)
            start_cluster
            ;;
        stop)
            ${pgClusterSubject[*]} stop
            ;;
        clean)
            ${pgClusterSubject[*]} clean
            ;;
        psql)
            start_cluster &&
            exec psql "$@"
            ;;
        shell)
            start_cluster &&
            exec stack exec bash
            ;;
        *)
            echo "$usage"
            ;;
    esac
}

main "$@"
