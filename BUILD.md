# Building and Running Snowdrift

Snowdrift has been built successfully on GNU/Linux distributions of all sorts
and on OpenBSD and OS X.

Windows is not currently supported, but we will assist any efforts to add
such support. See below for partial setup instructions.

## Install System Dependencies

[Git], [PostgreSQL], and [Stack] are the only dependencies needed at the system
level. Stack takes care of finding or installing the correct GHC version. Some
systems need a few additional libraries to support the core dependencies.

**Follow the details for your system, then skip to the "Get the Snowdrift Code"
section.**

### Debian, Ubuntu, and any related derivatives

Install Git and PostgreSQL with needed libraries:

    sudo apt-get update
    sudo apt-get install git postgresql postgresql-client libgmp-dev zlib1g-dev libpq-dev libtinfo-dev

Then follow the
[Debian Stack install](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#debian)
or
[Ubuntu Stack install](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
instructions as appropriate.

### CentOS/RHEL and Fedora

Install Git and needed libraries:

    sudo yum update
    sudo yum install ncurses-devel gmp-devel zlib-devel git
    
For newer versions of Fedora, replace the `yum` commands with `dnf`. Install the 
following libraries if you don't already have them:

    sudo dnf install libstdc++-static gcc-c++

You'll also need PostgreSQL >= 9.3:

    sudo yum install postgres-server postgres-devel

If the version in the base repositories is too old, follow the [instructions on the 
PostgreSQL wiki](https://wiki.postgresql.org/wiki/YUM_Installation) to install from 
their repositories. Get the postgresXX-server and postgresXX-devel packages, where 
XX is the version number. 

So that the Snowdrift database cluster tool sees the pgsql executables at 
`/usr/pgsql-X.X/bin` on your PATH, either add that route (with the correct numbers 
instead of X.X) to your PATH (e.g. in `~/.bash_profile`, `~/.bashrc` or 
`~/.profile`) or create symlinks somewhere already on your PATH.

Then follow the [Stack install instructions](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md) for your distribution.

### Arch Linux

Install Git and PostgreSQL by running this command as `root`:

    pacman -S git postgresql

Finally, install the
[haskell-stack](https://aur.archlinux.org/packages/haskell-stack)
package from the AUR.

### NixOS

Install Git as usual under NixOS.

Then, follow the [NixOS Stack install instructions](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#nixos).

For PostgreSQL, add these lines to `/etc/nixos/configuration.nix`:

    services.postgresql.enable = true;
    services.postgresql.package = pkgs.postgresql94;

Then install PostgreSQL with:

    sudo nixos-rebuild switch

Afterwards, you may need to create the postgres user, like so:

    sudo -su root
    createuser -s -r postgres

#### Building Snowdrift and GHC with NixOS

Stack can fetch and build the required version of GHC, but this
doesn't work well on NixOS due to an unusual filesystem hierarchy,
among other things. Instead, just use `nix-shell` to get into an
environment with the right compiler version:

    nix-shell -p haskell.compiler.ghc7102

You can now attempt to build Snowdrift via the general instructions below. Stack
will likely complain about some missing items (like zlib). To continue, install
listed items manually via `nix-env` or `nox`, then specify their location like
this:

    stack build --extra-include-dirs ~/.nix-profile/include \
                --extra-lib-dirs ~/.nix-profile/lib

### \*BSD

*Any knowledgeable reader: please help us document any important notes about
installing the Git, PostgreSQL, and Stack dependencies on \*BSD.*

### OS X

If you don't have [brew](http://brew.sh/) yet, install it with:

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

With brew, install the core dependencies:

    brew install git
    brew install postgres
    brew install haskell-stack

### Windows

*Status:* We do not officially support Windows, but we welcome testing. From
reports so far, SnowdriftEmailDaemon won't build on Windows, so `stack test`
will fail. Our database management utility (sdb.hs) is also untested on
Windows.

Install [Git] per instructions on the website.

Install PostgreSQL 32-bit version from
<http://www.enterprisedb.com/products-services-training/pgdownload#windows>

Add the PostgreSQL bin directory to the path
`C:\Program Files (x86)\PostgreSQL\9.4\bin`

Follow the instructions to
[install Stack for Windows](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#windows)


## Get the Snowdrift code

NB: We primarily use a completely FLO (Free/Libre/Open) host for our code:
[git.snowdrift.coop/sd/snowdrift], and our instructions assume that repository.
For convenience and redundancy, we also mirror at [GitHub], a popular but
proprietary platform.

From within your preferred directory, get the code with:

    git clone https://git.snowdrift.coop/sd/snowdrift.git

## Initial Build

### Compile the code

Change to the new snowdrift directory:

    cd snowdrift

Then, fetch all Haskell dependencies and build everything:

    stack setup
    stack build

NB: this will take a while!

### Set up the database and run initial tests

We have a simple tool that helps ensure a private database cluster is set
up and available. Use it to run tests:

    ./sdb.hs test

NB: The sdb.hs tool won't work on Windows because it uses UNIX sockets. We
welcome any patches, feedback, or Postgres-on-Windows help to get an alternative
working.

NB: To see sdb commands for other operations, run `./sdb.hs help`

## Running the site

### Option 1: run via `Snowdrift Development`

From the snowdrift project directory, run the site in development mode via:

    stack exec Snowdrift

(to stop the site, use ctrl-C)

### Option 2: run via `yesod devel`

NB: `yesod devel` provides automatic rebuilding and rerunning of the site
whenever it detects changes to the code, but it requires extra compile processes
the first time you use it. It also uses some minor extra drive space and
additional resources to run the file-watching process, and yesod devel is
currently incompatible with the optional ghc-mod tool mentioned in
[TEXTEDITORS.md](TEXTEDITORS.md).

To set up `yesod devel`, run:

    stack build cabal-install yesod-bin

From now on, you may run the site in development mode via:

    ./sdb.hs devel

NB: The first run will take a long time.

(To stop yesod devel, type `quit` in terminal and then press Enter)

### Option 3: run from within `stack ghci` (the repl)

This option is somewhat advanced, because you can get some funny errors
sometimes. However, it has the fastest rebuild time by far. Start the repl
with:

    ./sdb.hs ghci

Then from the repl's prompt, enter

    :load app/DevelMain.hs
    :update

To reload the site after changes, type

    :reload
    :update

## Using the local site

### View in your browser

Access the site in your browser at <http://localhost:3000>

### Log-in options

The development database comes with three default users (with username and
passphrase the same):

* admin
* established
* guest

### Testing

Run the test suite with:

    stack build && stack test --pedantic

NB: we include `stack build` because our current cabal setup does not fully
recognize test dependencies on executables such as SnowdriftProcessPayments.

### Manual rebuild

To rebuild the site, run:

    stack build

NB: As mentioned above, if you run the site with `Snowdrift Development`, then
to see any changes, you must stop the site, manually rebuild, then restart the
site. If you use `yesod devel`, the site will rebuild and restart automatically
for most changes. However, **manual rebuild is always required whenever you:**

* add new dependencies (i.e. edit the `build-depends` in `Snowdrift.cabal`)
* update any extra binaries such as the payment processing script or the
  email daemon.

NB: In rare cases, you may need to run `stack clean` if building fails to
recognize a change.

#### Updating static files

To make builds recognize changes to the static directory, run:

    touch website/src/Settings/StaticFiles.hs

### Exploring the code via REPL

To start the REPL where you can run code from the site in an interpreter, use:

    stack ghci

If you want to run the devel site through ghci, use `./sdb.hs ghci`
instead.

## Database notes

See [DATABASE-MANAGEMENT.md] for instructions on resetting the database and
more.

## Getting help, learning, contributing etc.

We welcome any and all feedback on these build instructions, on the site itself,
etc. We will happily help you with any questions. See the [README](README.md)
for further general links, and the [Contributing Guide](CONTRIBUTING.md) for
more thorough resources about technical development.

[DATABASE-MANAGEMENT.md]: DATABASE-MANAGEMENT.md
[Git]: http://www.git-scm.com/downloads
[git.snowdrift.coop/sd/snowdrift]: https://git.snowdrift.coop/sd/snowdrift
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[PostgreSQL]: http://www.postgresql.org/download/
[Stack]: https://github.com/commercialhaskell/stack#the-haskell-tool-stack
