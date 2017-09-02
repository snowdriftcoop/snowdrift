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
    sudo apt-get install git postgresql libgmp-dev libpq-dev

Then follow the [Debian Stack install] or [Ubuntu Stack install] instructions
as appropriate.

### CentOS/RHEL and Fedora

Install Git and needed libraries:

    sudo yum update
    sudo yum install ncurses-devel gmp-devel zlib-devel git openssl-devel gcc-c++

For newer versions of Fedora, replace the `yum` commands with `dnf`.

You'll also need PostgreSQL >= 9.3:

    sudo yum install postgres-server postgres-devel

If the version in the base repositories is too old, follow the
[instructions on the PostgreSQL wiki] to install from their repositories. Get
the postgresqlXX-server and postgresqlXX-devel packages, where XX is the version
number.

So that the Snowdrift database cluster tool sees the pgsql executables at
`/usr/pgsql-X.X/bin` on your PATH, either add that route (with the correct
numbers instead of X.X) to your PATH (e.g. in `~/.bash_profile`, `~/.bashrc` or
`~/.profile`) or create symlinks somewhere already on your PATH.

Then follow the [Stack install instructions] for your distribution.

### Arch Linux

Install Git, PostgreSQL, and Stack by running this command as `root`:

    pacman -S git postgresql stack

Arch also requires installation of the ncurses5-compat-libs package, found in
the AUR.  If you are unsure how to install from the AUR, please refer to the
[AUR ArchWiki Page].


### NixOS

Use `nix-shell` to provide Stack and the Yesod executable:

    nix-shell -p stack haskellPackages.yesod-bin

Add the following to `~/.stack/config.yaml`:

    nix:
      enable: true

### \*BSD

*Any knowledgeable reader: please help us document any important notes about
installing the Git, PostgreSQL, and Stack dependencies on \*BSD.*

### OS X

If you don't have [brew] yet, install it with:

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

With brew, install the core dependencies:

    brew install git
    brew install postgres
    brew install haskell-stack

### Windows

*Status:* We do not officially support Windows, but we welcome testing. In the
past, we have had only partial success running on Windows. We know that the
sdb.hs tool won't work on Windows because it uses UNIX sockets. We welcome any
patches, feedback, or Postgres-on-Windows help to get an alternative working.

Install [Git] per instructions on the website.

Install PostgreSQL 32-bit version from
<http://www.enterprisedb.com/products-services-training/pgdownload#windows>

Add the PostgreSQL bin directory to the path
`C:\Program Files (x86)\PostgreSQL\9.4\bin`

Follow the instructions to [install Stack for Windows].

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

Then, fetch all Haskell dependencies and build all dependencies:

    make

This will take a while!

### Run the tests

Running the tests for the first time will also build the project.

    ./build.sh test

## Running the site via `yesod devel`

This will automatically rebuild and rerun the site whenever it detects
changes to the code. The downsides are: it uses some minor extra drive
space, uses some additional resources to run the file-watching process, and
is currently incompatible with the optional ghc-mod tool mentioned in
[TEXTEDITORS.md](TEXTEDITORS.md).

Run the site in development mode via:

    ./build.sh

To stop the site, type `quit` in the terminal and then press Enter.

## Troubleshooting

If `./build.sh` commands fail in any way, try running `./sdb.hs clean` and then
try building again.

## Using the local site

### View in your browser

Access the site in your browser at <http://localhost:3000>

### Using auth in development

You may need to add a new account to reach certain places in the locally-running
site.

By default, the development site does not send out emails, which the
authentication system uses to send out tokens for verification during
registration and password reset. Instead, the auth token can be found on the
debug output in the terminal. After sending an auth form request, look for the
following line, where `TOKEN` is a random alpha-numerical string that you can
input when prompted to verify the request:

    (AuthToken {fromAuthToken = "TOKEN"})

Although it's not recommended, you can also enable sending mail by setting
the environment variable `SD_EMAILS` to "true", or by modifying
`website/config/settings.yml` with the setting
`send-email: "_env:SD_EMAILS:true"`. Just be careful who you try to send
mail to.

### Using Stripe in development

1. Register an account on the [Stripe] website. Go to your new account
   dashboard and click the "API" link to obtain your publishable and secret
   test keys.

2. Create a new file named `.stripe_keys` in the top-level directory of the
   snowdrift project and add your keys as environment variables:

```
export STRIPE_PUBLISHABLE_KEY="stripe_pub_key"
export STRIPE_SECRET_KEY="stripe_sec_key"
```

3. Build your snowdrift development site, log in and navigate to the dashboard
   payment settings page. Press the "Set up Stripe" button and provide the
   following details on the Checkout form:

- One of the [Stripe test card numbers], e.g. "4242 4242 4242 4242"
- Any future expiration date
- Any 3-digit CVC code
- Any postal code, e.g. "12345"

4. Click "Register" to submit the form and complete the setup.

Note: If the "Set up Stripe" button does not appear and you have an ad or
general-purpose blocker extension installed on your browser, you may need to
add an exception to your blocker settings for the development site.

### Manual rebuild

As an alternate build option, you can run the repl via `./build.sh ghci`.
In that case, you must manually rebuild and restart the site, whereas the plain
`./build.sh` option (which is the same as `./build.sh devel`) will
rebuild and restart automatically for most changes.

Also, **manual rebuild is always required whenever you:**

* add new dependencies (i.e. edit the `build-depends` in `Snowdrift.cabal`)
* update any extra binaries such as the payment processing script or the
  email daemon.

To manually rebuild the site when otherwise using `./build.sh`, run:

    stack build

NB: In rare cases, you may need to run `stack clean` if building fails to
recognize a change.

#### Updating static files

To make builds recognize changes to the static directory, run:

    touch website/src/Settings/StaticFiles.hs

#### Adding and updating Sass files

We currently use the [Sass] language extension for authoring stylesheets, which
are then compiled into CSS with shakespeare-sass. There are two main Sass file
extensions currently supported by the modified shakespeare-sass package:
`.sass` and `.silius`. Use `.silius` when the file contains Haskell splices,
and `.sass` for Sass-only files for faster compiling. (While shakespeare-sass
also recognizes the SCSS style with the `.scss` extension, the indented syntax
is preferred for conciseness.)

After editing a `.sass` file, touch the handler file that uses it for the
changes to register, for example:

    touch website/src/Foundation.hs

Haskell variables can be added to Silius files with the syntax
`%{someVariable}` and will be parsed automatically each rebuild.

## Database notes

See [DATABASE-MANAGEMENT.md] for details on resetting the database and more.

## Getting help, learning, contributing etc.

We welcome any and all feedback on these build instructions, on the site
itself, etc. We will happily help you with any questions. See the [README] for
further general links, and the [Contributing Guide] for more thorough resources
about technical development.

[DATABASE-MANAGEMENT.md]: DATABASE-MANAGEMENT.md
[brew]: http://brew.sh/
[Contributing Guide]: CONTRIBUTING.md
[Debian Stack install]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#debian
[Git]: http://www.git-scm.com/downloads
[git.snowdrift.coop/sd/snowdrift]: https://git.snowdrift.coop/sd/snowdrift
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[haskell-stack]: https://aur.archlinux.org/packages/haskell-stack
[install Stack for Windows]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#windows
[instructions on the PostgreSQL wiki]: https://wiki.postgresql.org/wiki/YUM_Installation
[AUR ArchWiki Page]: https://wiki.archlinux.org/index.php/Arch_User_Repository
[PostgreSQL]: http://www.postgresql.org/download/
[README]: README.md
[Sass]: http://sass-lang.com/
[Stack]: https://github.com/commercialhaskell/stack#the-haskell-tool-stack
[Stack install instructions]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
[Stripe]: https://stripe.com
[Stripe test card numbers]: https://stripe.com/docs/testing#cards
[Ubuntu Stack install]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu
