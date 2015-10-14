# Building and Running Snowdrift Locally

Snowdrift has been built successfully on Debian, Ubuntu, Arch, Fedora, Gentoo,
NixOS, and other distros of GNU/Linux as well as on OpenBSD and OS X.

Although some volunteers have built on Windows, some issues remain unsettled.

## Install System Dependencies

The only system-level dependencies are [Git], [PostgreSQL], and [Stack].

Note: You do *not* need to install GHC or Haskell Platform.
Stack will automatically install the correct GHC version if you don't have it,
and this won't affect any installation you already have of other versions.

**Follow the details for your system, then skip to the "Get the Snowdrift Code"
section.**

### Debian, Ubuntu, and any related derivatives

To install dependencies, run the following commands:

    sudo apt-get update &&
    sudo apt-get install git postgresql postgresql-client libgmp-dev zlib1g-dev libpq-dev libtinfo-dev

Then follow the
[Debian Stack install](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#debian)
or
[Ubuntu Stack install](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
instructions as appropriate.

### Arch Linux

To install dependencies, run this command as `root`:

    pacman -S git postgresql

To initialize the PostgreSQL database, first become the `postgres` user.

    sudo -i -u postgres

As the `postgres` user, run this command:

    initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'

Then, as `root`:

    systemctl enable postgresql.service
    systemctl start postgresql.service

Finally, install the
[haskell-stack](https://aur.archlinux.org/packages/haskell-stack)
package from the AUR.

### NixOS

If not installed yet, get Git as usual under NixOS.

Then, follow the [NixOS Stack install instructions](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#nixos).

For postgres, add these lines to `/etc/nixos/configuration.nix`:

    services.postgresql.enable = true;
    services.postgresql.package = pkgs.postgresql94;

Then issue `sudo nixos-rebuild switch` to install.
Afterwards you may need to create the postgres user, like so:

    sudo -su root
    createuser -s -r postgres

#### Building Snowdrift and GHC with NixOS

Stack can fetch and build the required version of GHC, but this
doesn't work well on NixOS due to an unusual filesystem hierarchy,
among other things. Instead, just use `nix-shell` to get into an
environment with the right compiler version:

    nix-shell -p haskell.compiler.ghc784

Then you should be able to build the project as described below, but Stack will
likely complain about missing system libraries (like zlib), which you'll need to
install manually via `nix-env` or `nox`. Once installed, you can specify the
location of such libraries like this:

    stack build --extra-include-dirs ~/.nix-profile/include \
                --extra-lib-dirs ~/.nix-profile/lib

### \*BSD

*Any knowledgeable reader: please help us document any important notes about
installing the Git, PostgreSQL, and Stack dependencies on \*BSD.* Notes below
cover some related steps regarding the database setup.

### OS X

Follow the instructions on their sites for installing each of the dependencies:
[Git], [PostgreSQL], and [Stack].

### Windows

Note: At this time, Windows builds have some issues. To test building directly
on Windows:

Install [Git] per instructions on the website.

Install PostgreSQL 32-bit version from
<http://www.enterprisedb.com/products-services-training/pgdownload#windows>

Add the PostgreSQL bin directory to the path
`C:\Program Files (x86)\PostgreSQL\9.4\bin`

Follow the instructions to
[install Stack for Windows](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#windows)

*Note:* SnowdriftEmailDaemon won't build on Windows, so the `stack test` will
fail. Building, running, and working on the site might still work otherwise.

## Get the Snowdrift code

Once the dependencies are installed, clone the Snowdrift code to your computer.

We primarily use a completely free/libre/open host for our code:
[git.gnu.io/snowdrift/snowdrift]. For convenience and redundancy, we also mirror
at [GitHub], a popular but proprietary platform.

The standard command to clone the code to your local system is:

    git clone https://git.gnu.io/snowdrift/snowdrift.git

## Initial Build

### Compile the code

From within the snowdrift code directory, fetch all Haskell dependencies
and build everything:

    cd snowdrift
    stack setup
    stack build

NB: this will take a while!

### Set up the database

We offer a simple script (`sdm` for "Snowdrift database manager") that will
setup the PostgreSQL databases for you. At this time, it requires root access,
so it will ask for your passphrase.

To set up databases manually, see [DATABASE-MANAGEMENT.md].

#### GNU/Linux database setup

simply run `stack exec sdm init`

#### \*BSD database setup

* OpenBSD: `stack exec -- sdm init --sudoUser _postgresql`
* FreeBSD: *Untested but should work*:
  `stack exec -- sdm init --sudoUser pgsql --pgUser pgsql`

#### OS X database setup

run `stack exec -- sdm init --sudoUser=_postgres`

#### Windows database setup

Instead of using sdm, follow the *manual database* setup instructions in
[DATABASE-MANAGEMENT.md]. Some of the precise commands may need slight adapting
such as using `psql -U postgres` to enter the psql prompt.

### Run initial tests

For various reasons, we need to run the tests initially to compile all test
dependencies. Simply run `stack test`.

## Useful Development Commands

With everything initialized, you can now use the following commands:

* `stack build && stack test --pedantic`: run the test suite

    Note that `stack build` must be run since our current cabal setup does not
    fully recognize test dependencies on executables such as
    SnowdriftProcessPayments. 

* `stack build`: rebuild manually
    * Usually running the site in development mode is sufficient, but you must
      run `stack build` whenever you:
        * add new dependencies (i.e. edit the `build-depends` field in
          `Snowdrift.cabal`)
        * update any extra binaries such as the payment processing script, the
          sdm database configuration script, or the email daemon.
    * In rare cases, you may need to run `stack clean` if the development site
      fails to recognize a change.
    * Specifically for changes to files in the static directory, run
      `touch src/Settings/StaticFiles.hs` before rebuilding.

* `stack ghci --no-load`: Start the REPL... and the site![^alt-run]
    * to reload the site, enter `:r` and then `main` in ghci
        * Unfortunately there's still some rough edges at this time. Changes
          to template files and config/* files aren't recognized and you must
          touch the files that depend on them manually. Also, you may
          occasionally hit ghc "panics". :( Shut down ghci and restart it at
          that case.
    * to stop the site, enter `shutdown` in ghci
    * access the site in your browser at <http://localhost:3000>
    * log in with any of the three default users via the built-in log-in:
      `admin`; `guest`; `established`
      (username and passphrase are the same)


[^alt-run]: An alternative approach to building and running the site:
    * Run the site with `stack exec Snowdrift Development`
      (to stop the site in this case, use ctrl-C).
    * This doesn't automatically rebuild, so you must manually run `stack build`
      whenever you want to compile any updates to the code.
    * Advantages to this approach: less running processes (no need to watch for
      file changes) and no need to do extra compiling into special yesod-devel
      directory (although that should soon stop being an issue given planned
      yesod-bin updates).

## Database notes

See [DATABASE-MANAGEMENT.md] for instructions on resetting the database and
more.

[DATABASE-MANAGEMENT.md]: DATABASE-MANAGEMENT.md
[Git]: http://www.git-scm.com/downloads
[git.gnu.io/snowdrift/snowdrift]: https://git.gnu.io/snowdrift/snowdrift
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[PostgreSQL]: http://www.postgresql.org/download/
[Stack]: https://github.com/commercialhaskell/stack#the-haskell-tool-stack
