# Building and Running Snowdrift

Snowdrift has been built successfully on many GNU/Linux distros and on macOS.

Windows and \*BSD distributions are not currently supported, but we will assist
any efforts to add such support. See below for partial setup instructions.

## Install System Dependencies

[Git], [PostgreSQL], [Stack], and [Make] are the only dependencies needed at the
system level. Stack takes care of finding or installing the correct GHC version.
Some systems need a few additional libraries to support the core dependencies.

**Follow the details for your system, then skip to the "Get the Snowdrift Code"
section.**

### Debian, Ubuntu, and any related derivatives

Install Git, PostgreSQL and Make with needed libraries:

    sudo apt-get update
    sudo apt-get install git postgresql libgmp-dev libpq-dev libssl-dev make

Then follow the [Debian Stack install] or [Ubuntu Stack install] instructions
as appropriate.

### CentOS/RHEL and Fedora

* Install Git, needed libraries and Make:

        sudo dnf install git gcc-c++ gmp-devel ncurses-devel openssl-devel zlib-devel make

    For CentOS and Fedora <=22, replace the `dnf` commands with `yum`. Fedora <=23
may also need the `libstdc++-static` package.

* You'll also need PostgreSQL >= 9.3:

        sudo dnf install postgresql-server postgresql-devel

    If the version in the distro repositories is too old, follow the
    [instructions on the PostgreSQL wiki] to install from their repositories.
    Get the postgresqlXX-server and postgresqlXX-devel packages, where XX is the
    version number. In this case, the pgsql executables will be installed in
    `/usr/pgsql-X.X/bin`. For the Snowdrift database cluster tool to see the
    executables, either add that route (with the correct numbers instead of X.X)
    to your PATH (e.g. in `~/.bash_profile`, `~/.bashrc` or `~/.profile`) or
    create symlinks somewhere already on your PATH.

* Follow the [Stack install instructions] for your distribution.

### Arch Linux

Install Git, PostgreSQL, Stack and Make by running this command as `root`:

    pacman -S git postgresql stack make

Arch also requires installation of the ncurses5-compat-libs package, found in
the AUR.  If you are unsure how to install from the AUR, please refer to the
[AUR ArchWiki Page].

Once ncurses5-compat-libs is installed, add the following line to
~/.stack/config.yaml:

~~~~~
ghc-build: nopie
~~~~~

**NOTE: Entering this into ~/.stack/config.yaml will apply to *all* Haskell
projects that you build using stack going forward.** This command tells stack
to use a version of ghc that compiles without PIE. We were unable to find a way
to set this configuration *just* for Snowdrift, however we don't believe making
this change will cause any issues with compiling other Haskell projects.

### NixOS

Use `nix-shell` to provide Stack and the Yesod executable:

    nix-shell -p stack haskellPackages.yesod-bin

Add the following to `~/.stack/config.yaml`:

    nix:
      enable: true

### \*BSD

*Status:* We welcome testing but do not officially support \*BSD distributions
due to Stack being unavailable for some \*BSD platforms. The site has run
successfully on FreeBSD with non-trivial changes to the build process
(due to hlibsass linking of C libraries). For more details, please refer to this
[discussion about FreeBSD].

### OS X

If you don't have [brew] yet, install it with:

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

With brew, install the core dependencies:

    brew install git
    brew install postgres
    brew install haskell-stack
    brew install make

### Windows

**Status:** We welcome testing but do not officially support Windows.

In the past, we have had only partial success running on Windows. We know that
our development database scripts won't work on Windows because they use UNIX
sockets. We welcome any patches, feedback, or Postgres-on-Windows help to get an
alternative working.

## Get the Snowdrift code

Our primary repository is at [git.snowdrift.coop/sd/snowdrift], and our
instructions assume that repository. For convenience and redundancy, we also
mirror at [GitHub].

From within your preferred directory, get the code with:

    git clone https://git.snowdrift.coop/sd/snowdrift.git

Change to the new snowdrift directory:

    cd snowdrift

## Run the tests

Run the tests to fetch and build all Haskell dependencies:

    ./build.sh test

This will take a while!

## Running the site

Run the site in development mode with

    ./build.sh

This will automatically rebuild and rerun the site whenever it detects
changes to the code.

To stop the site, type `quit` in the terminal, then press Enter.

### Troubleshooting

If `./build.sh` commands fail in any way, try running `./build.sh cleandb` and
then building again.

## Connecting to the local database

The script `./s/dev_psql` will spawn a psql shell that is connected to the dev
database. Refer to [Postgres' psql
documentation](https://www.postgresql.org/docs/9.6/app-psql.html).

## Using the local site

### View in your browser

Access the site in your browser at <http://localhost:3000>

### Using auth in development

You can log in to the locally-running site using the default user
`user@example.com` and passphrase `userpassphrase`.

To create a new account, follow these instructions:

As the development site does not normally send out emails (the authentication
approach for the live Snowdrift.coop system), you can get the necessary auth
tokens from the debug output in the terminal.

After sending an auth form request in the web interface, look for the following
line

    (AuthToken {fromAuthToken = "TOKEN"})

but where `TOKEN` is instead a random alpha-numerical string that you can input
when prompted to verify the request (and don't get confused by other shorter
random strings that also show up nearby in the terminal).

Although it's not recommended, you can also enable sending mail (if you have an
appropriate mail server accessible) by setting the environment variable
`SD_EMAILS` to "true", or by modifying `website/config/settings.yml` with the
setting `send-email: "_env:SD_EMAILS:true"`. Just be careful who you try to send
mail to.

### Using Stripe in development

1. Register an account on the [Stripe] website.

2. At your Stripe dashboard, make sure "View test data" is turned **on**
   (which is default for new accounts but worth double-checking).

3. Click the "API" link to obtain your publishable and secret test keys.

4. In the top-level directory of the snowdrift project (where we have build.sh),
   create a new text file named `.stripe_keys` and add your keys as environment
   variables:
>>>
export STRIPE_PUBLISHABLE_KEY=your_stripe_pub_key  
export STRIPE_SECRET_KEY=your_stripe_sec_key
>>>

5. Build your snowdrift development site and log in.

6. Make sure Javascript is enabled and that no plugins block Stripe's JavaScript
   form.

7. Navigate to the dashboard payment settings page. Press the "Set up Stripe"
   button and provide the following details on the Checkout form:
   * One of the [Stripe test card numbers], e.g. "4242 4242 4242 4242"
   * Any future expiration date
   * Any 3-digit CVC code
   * Any postal code, e.g. "12345"

8. Click "Register" to submit the form and complete the setup.

#### Updating static files

To make builds recognize changes to the static directory, run:

    touch website/src/Settings/StaticFiles.hs

## Getting help, learning, contributing etc.

We welcome any and all feedback on these build instructions, on the site
itself, etc. We will happily help you with any questions. See the [README] for
further general links, and the [Contributing Guide] for more thorough resources
about technical development.

[AUR ArchWiki Page]: https://wiki.archlinux.org/index.php/Arch_User_Repository
[brew]: http://brew.sh/
[Contributing Guide]: CONTRIBUTING.md
[Debian Stack install]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#debian
[discussion about FreeBSD]: https://git.snowdrift.coop/sd/snowdrift/issues/67
[Git]: http://www.git-scm.com/downloads
[git.snowdrift.coop/sd/snowdrift]: https://git.snowdrift.coop/sd/snowdrift
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[instructions on the PostgreSQL wiki]: https://wiki.postgresql.org/wiki/YUM_Installation
[Make]: https://www.gnu.org/software/make/manual/html_node/index.html
[PostgreSQL]: http://www.postgresql.org/download/
[README]: README.md
[Stack]: https://github.com/commercialhaskell/stack#the-haskell-tool-stack
[Stack install instructions]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
[Stripe]: https://stripe.com
[Stripe test card numbers]: https://stripe.com/docs/testing#cards
[Ubuntu Stack install]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu
