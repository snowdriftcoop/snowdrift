#!/usr/bin/env bash

read -d '' usage <<EOF
.
.  build.sh CMD [OPTIONS]
.

  Used to do standard Haskell/Yesod things like run tests and run the
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
    cd "`dirname "$0"`/website"
    if [ -z "$IN_NIX_SHELL" ]; then
        stack build yesod-bin &&
        exec stack exec yesod devel
    else
        exec yesod devel
    fi
}

dbenv () {
    ./admin-tools/sdb.sh start
    source <(./admin-tools/sdb.sh env)
}

main () {
    if [ -z "$1" ]; then
        CMD=devel
    else
        CMD="$1"
        shift
    fi

    # Make sure the current directory is where this script is, and is given
    # in absolute form.
    #
    # "$0" is the path to this script, as the kernel gave it: it could be
    # absolute, or it could be relative.
    # Dirname takes the "/build.sh" off the end, giving us the path of the
    # script's containing directory.
    scriptDirAsGiven="`dirname "$0"`"
    #
    # Now we use `readlink -f` to ensure the path is absolute.
    cd "`readlink -f "$scriptDirAsGiven"`"

    # Configure local Stripe keys for shell, devel, and test.
    #
    # (Recall the current directory is where the script is, due to the "cd"
    # above.)
    # If the .stripe_keys file exists in the current directory, then ...
    if [ -e .stripe_keys ]; then

        # ... execute it. Note from BUILD.md that .stripe_keys really is written
        # in Bash (export commands). It exports your keys as environment
        # variables visible to child processes. So be careful what you launch.
        source .stripe_keys;

    # Otherwise, print a friendly reminder:
    else
        echo
        echo "Friendly reminder (not an error): there is no \".stripe_keys\" file"
        echo "in the project root. You may wish to consult BUILD.md about that."
        echo
    fi

    # De-mystification: Recall from stack.yaml that we have 4 packages to compile:
    # Snowdrift (in the website subdir), crowdmatch, run-persist and admin-tools.
    # The next line explicitly compiles the Snowdrift package.
    # This causes a chain-reaction: Snowdrift has crowdmatch built, because the
    # latter is a direct dependency of the former. Then, crowdmatch causes
    # run-persist to be built for the same reason. You may run `stack dot` for
    # a visualizaion of this. Just be sure to run it from within the project.
    # See the next comment for the remaining package.
    stack build --flag Snowdrift:library-only --only-dependencies --install-ghc Snowdrift:test &&

    # At time of writing, `stack build` takes strictly zero or one arguments and
    # does not give an error if you give more than one. It just does the first.
    # Thus it is important to do this with multiple lines.
    stack build admin-tools &&

    dbenv &&
    case "$CMD" in
        devel)
            run_devel
            ;;
        test)
            exec stack test --flag Snowdrift:library-only --fast "$@"
            ;;
        psql)
            exec psql "$@"
            ;;
        shell)
            exec stack exec bash
            ;;
        *)
            echo "$usage"
            ;;
    esac
}

main "$@"
