#!/usr/bin/env bash

projRoot="`dirname "$0"`" # Proj. root figured out from this script's location.
export PGDATA="$projRoot"/.postgres-work
export PGHOST="$PGDATA"
export PGDATABASE="snowdrift"
makeCmdBase=(make -f "$projRoot"/.sdc)
if [ -z "$IN_NIX_SHELL" ]; then
    makeCmd=makeCmdBase # If we are not to use the Nix shell, don't add its arg.
else
    makeCmd=makeCmdBase+=('SHELL=nix-shell') # But if we are, append it.
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
    if [ -z "$IN_NIX_SHELL" ]; then
        stack build yesod-bin &&
        exec stack exec yesod devel
    else
        exec yesod devel
    fi
}

start_cluster () {
    ${makeCmd[*]} start
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

    # Configure local Stripe keys for shell, devel, and test.
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
        stop
            ${makeCmd[*]} stop
            ;;
        clean
            ${makeCmd[*]} clean
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
