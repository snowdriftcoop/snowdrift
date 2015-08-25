# Building and Running Snowdrift Locally

## Install System Dependencies

Install [Git], [PostgreSQL] and [Stack].

Note: You do *not* need to install GHC or Haskell Platform.
Stack will automatically install the correct GHC version if you don't have it,
and this won't affect any installation you already have of other verisons.

### System-specific notes

Snowdrift has been built successfully on Debian, Ubuntu, Arch, Fedora, Gentoo,
NixOS, and other distros of GNU/Linux as well as on OpenBSD and OS X.

Although some volunteers have built on Windows, not everything seems to work
quite right there.

**Follow the details for your system here, then skip down to the "Get the Code"
section.**

#### Debian, Ubuntu, and any related derivatives

To install dependencies, run the following commands:

    sudo apt-get update &&
    sudo apt-get install git postgresql postgresql-client libgmp-dev zlib1g-dev libpq-dev

Then follow the
[Debian Stack install](https://github.com/commercialhaskell/stack/wiki/Downloads#debian)
or
[Ubuntu Stack install](https://github.com/commercialhaskell/stack/wiki/Downloads#ubuntu)
instructions as appropriate.

#### Arch Linux

To install dependencies, run this command as `root`:

    pacman -S git postgresql

To initialize the PostgreSQL database, first become the `postgres` user.

    sudo -i -u postgres

As the `postgres` user, run this command:

    initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'

Then, as `root`:

    systemctl enable postgresql.service
    systemctl start postgresql.service

Install the
[haskell-stack-git](https://aur4.archlinux.org/packages/haskell-stack-git/)
package from the AUR. Run:

    stack upgrade --git

#### NixOS

##### Installing Stack under NixOS

Clone the Stack git repo:

    git clone https://github.com/commercialhaskell/stack.git

Create a `shell.nix` file:

    cd stack
    cabal2nix --shell ./. --no-check > shell.nix

(Note that the tests fail on NixOS, so disable them with
`--no-check`.)

Install Stack to your user profile:

    nix-env -i -f shell.nix

##### Building Snowdrift and GHC with NixOS

Stack can fetch and build the required version of GHC, but this
doesn't work great on NixOS due to an unusual filesystem hierarchy,
among other things. Instead, just use `nix-shell` to get into an
environment with the right compiler version:

    nix-shell -p haskell.compiler.ghc784

Then you should be able to build the project as described in the
[README], but Stack will likely complain about missing system
libraries (like zlib), which you'll need to install manually via
`nix-env` or `nox`. Once installed, you can specify the
location of such libraries like this:

    stack build --extra-include-dirs ~/.nix-profile/include \
                --extra-lib-dirs ~/.nix-profile/lib

##### PostgreSQL and sdm under NixOS

To get the sdm script to work, NixOS users should install postgres
by adding these lines to `/etc/nixos/configuration.nix`:

    services.postgresql.enable = true;
    services.postgresql.package = pkgs.postgresql94;

Then issue `sudo nixos-rebuild switch` to install.
Afterwards you may need to create the postgres user, like so:

    sudo -su root
    createuser -s -r postgres


#### \*BSD

*Not yet documented: installing dependencies on \*BSD*

The BSDs use different postgres configurations than the common
GNU/Linux distros, so we must pass different paramaters to sdm.
Where our instructions say `sdm init` add arguments as shown below:

* OpenBSD: `sdm init --sudoUser _postgresql`
* FreeBSD: Untested but we believe `sdm init --sudoUser pgsql --pgUser
  pgsql` will work.


#### OS X

Follow the instructions on their sites for each of the dependencies:
[Git], [PostgreSQL] and [Stack].
We don't think any extras are needed otherwise.

The OS X build process seems to have some issues with postgres user names;
until we update sdm to accept special arguments for OS X, the database set-up
will need to be done manually. See the appendix at the end of this file.

Another option is a VM via [Vagrant].

#### Windows

Note: At this time, Windows builds have some issues.
The guaranteed approach is to use a GNU/Linux Virtual Machine,
perhaps via [Vagrant].

To test building directly on Windows:

Install PostgreSQL 32-bit version from
<http://www.enterprisedb.com/products-services-training/pgdownload#windows>

Add the PostgreSQL bin directory to the path
`C:\Program Files (x86)\PostgreSQL\9.4\bin`

Instead of using sdm, follow the appendix at the end of this file for setting up
the development database manually. Some of the precise commands may need slight
adapting such as using `psql -U postgres` to enter the psql prompt.

The rest of the build instructions should work (although we're not sure about
the use of Stack reliably on Windows yet).

*Note:* SnowdriftEmailDaemon won't build on Windows, so `stack test` will fail.
Building, running, and working on the site might still work otherwise.

#### Virtual Machine options / Vagrant

Anyone could use a Virtual Machine with a system known to work and then
follow the steps for that system, working entirely within the VM.

Alternatively, to better *integrate* with your existing system, we offer an
option with [Vagrant]. Our Vagrant instance uses a Debian system preset with our
core dependencies. Vagrant then allows the build to work in the virtual machine
while you continue using your regular text editor, file system, web browser etc.

## Get the code

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
    stack build cabal-install yesod-bin
    stack build

NB: this will take a while!

### Set up the database

We offer a simple script that will setup the PostgreSQL databases for you.
For most GNU/Linux systems, simply run:

    stack exec sdm init

It will prompt you for your sudo password.

To set up databases manually, see the appendix at the end of this guide.

### Run initial tests

For various reasons, we need to run the tests initially to compile all test
dependencies. Simply run `stack test`.

## Useful Development Commands

With everything initialized, you can now use the following commands:

* `stack exec yesod devel`: run the site in development mode[^alt-run]
    * NB: this may take a while when first run, faster after that
    * access the site in your browser at <http://localhost:3000>
    * log in with any of the three default users via the built-in log-in:
      `admin`; `guest`; `established`.
      (username and passphrase are the same)
    * to stop the site, type `quit` in terminal and then press Enter
* `stack build && stack test`: run the test suite

    Note that `stack build` must be run since the tests depend on
    executables like SnowdriftProcessPayments. Cabal is not clever enough
    to notice test dependencies on executables.
* `stack build`: rebuild manually
    * Usually running the site in development mode is sufficient, but you must
      run `stack build` whenever you:
        * add new dependencies (i.e. edit the `build-depends` field in
          `Snowdrift.cabal`)
        * update any extra binaries such as the payment processing script, the
          sdm database configuration script, or the email daemon.
    * In rare cases, you may need to run `stack clean` if the development site
      fails to recognize a change.
* `stack ghci`: Start the REPL

[^alt-run]: An alternative approach to building and running the site:
    * Run the site with `stack exec Snowdrift Development`
      (to stop the site in this case, use ctrl-C).
    * This doesn't automatically rebuild, so you must manually run `stack build`
      whenever you want to compile any updates to the code.
    * Advantages to this approach: less running processes (no need to watch for
      file changes) and no need to do extra compiling into special yesod-devel
      directory (although that should soon stop being an issue given planned
      yesod-bin updates).

## Running tests

After making various changes to the code and running locally to verify that
everything compiles and appears to work as desired, you should then run our
automated tests before sharing your changes with the main project.

Run the tests with:

    stack build && stack test

If tests fail, try to figure out what is wrong. Ask us for help if needed.

## Database notes

See [DATABASE-MANAGEMENT.md](DATABASE-MANAGEMENT.md)

[Git]: http://www.git-scm.com/downloads
[git.gnu.io/snowdrift/snowdrift]: https://git.gnu.io/snowdrift/snowdrift
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[PostgreSQL]: http://www.postgresql.org/download/
[Stack]: https://github.com/commercialhaskell/stack#how-to-install
[Vagrant]: VAGRANT_SETUP.md
