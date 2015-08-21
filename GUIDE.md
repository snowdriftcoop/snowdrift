Snowdrift.coop
==============

This guide covers the [Snowdrift.coop](https://snowdrift.coop) codebase
and development process.

For those with little programming experience or new to Git
(or just wanting to review basics), start with our
[Beginners' Guide](BEGINNERS.md).


About the frameworks and tools we use
=====================================

Snowdrift uses the **[Yesod web framework](http://www.yesodweb.com/)**.

Yesod uses the Haskell programming language alongside its
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates).

We use the [PostgreSQL](http://www.postgresql.org/) database system.

We handle version control via [Git](https://git-scm.com/).

Our front-end uses **[Twitter Bootstrap](http://getbootstrap.com/)**,
although we use our own custom CSS in many cases.


Development guidelines and notes
================================

Overall, we strive to follow universal standards, be fully accessible,
and avoid browser-specific code.

We generally build with *progressive enhancement* in mind.
Content and functions should work with simple HTML/CSS
along with Yesod/Haskell server-side functions.
Later, we add JavaScript as appropriate for enhancement.
Consider the ideas of
[Unobtrusive JavaScript](http://en.wikipedia.org/wiki/Unobtrusive_JavaScript).
Use of NoScript should never cause a broken experience.
All our JavaScript should be recognized by the FSF's
[LibreJS plugin](https://www.gnu.org/software/librejs/).

We have separate wiki and discussion pages on the site
for [web-design issues](https://snowdrift.coop/p/snowdrift/w/site-design)
and [coding issues](https://snowdrift.coop/p/snowdrift/w/coding).

The [complete list of Snowdrift tickets](https://snowdrift.coop/p/snowdrift/t)
shows all items from all discussion pages associated with the project.

Consider adding concise comments to your code following the syntax for
[Haddock documentation](http://www.haskell.org/haddock/doc/html/markup.html).


Working on the code
===================

Again, see our [Beginners' Guide](BEGINNERS.md) for the simplest setup
if you have only minimal development experience.
The Beginners' Guide also has links to various support and learning resources.

The details below specify more advanced and particular items.

Text-editor settings
--------------------

We recommend setting your text editor to have the TAB key do indentation of
four spaces generally. However, we use 2-space indentation for .hamlet files.
We also use 80-character maximum line widths. See our
[code style guide](https://snowdrift.coop/p/snowdrift/w/en/coding#code-style-guide)
for more details.

### vim

For [vim](http://www.vim.org/) users, we recommend that your
.vimrc file include these lines:

    set textwidth=80
    set expandtab
    set shiftwidth=4
    set tabstop=4
    au FileType hamlet setl sw=2 sts=2 et
    syntax on
    set number

Among many vim plugins available, we recommend using a
[vim plugin manager](https://github.com/gmarik/Vundle.vim)
and the following plugins particularly relevant to snowdrift:

* [vim Shakespearean syntax](https://github.com/pbrisbin/vim-syntax-shakespeare)
* [Haskell-Vim extra syntax](https://github.com/neovimhaskell/haskell-vim)
* [vim-markdown](https://github.com/hallison/vim-markdown)
* [vim-gitgutter](https://github.com/airblade/vim-gitgutter)
* [vim2hs](https://github.com/dag/vim2hs).
    * optionally add `set nofoldenable` to .vimrc to skip folding of functions

*Many* other plugins make sense, but preferences vary. Those listed above mostly
do syntax highlighting and do not affect any commands or basic operations, so
they are safe for everyone to use without hesitation or learning process.

### Emacs

Emacs users should use a package manager (preferably Marmalade) to install
[Haskell Mode](https://github.com/haskell/haskell-mode)
and
[Shakespeare Mode](https://github.com/CodyReichert/shakespeare-mode).

Our included file
[`.dir-locals.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
makes Emacs use the recommended 4-space indentation.


Git branching
-------------

We suggest keeping your local master matched to the main project master,
and do *all* editing on separate git branches.
Use as many branches as needed to separate all work that functions
independently (you can, of course, remove merged branches later).


Building
========

See our [README] for basic build process.
The only core dependencies are Git, PostgreSQL, and Stack.
However, some system-specific libraries may be needed (see below)

Note: You do *not* need to install GHC or Haskell Platform.
Stack will automatically install the correct GHC version if you don't have it,
and this won't affect any installation you already have of other verisons.

System-specific notes
---------------------

Snowdrift has been built successfully on Debian, Ubuntu, Arch, Fedora, Gentoo,
NixOS, and other distros of GNU/Linux as well as on OpenBSD.
Snowdrift also has been built on OS X and Windows, although some manual
adjustments may be needed for those systems.

### Debian, Ubuntu, and related

To install dependencies, run the following commands:

    sudo apt-get update &&
    sudo apt-get install git postgresql postgresql-client libgmp-dev zlib1g-dev libpq-dev

Then follow the
[Debian Stack install](https://github.com/commercialhaskell/stack/wiki/Downloads#debian)
or
[Ubuntu Stack install](https://github.com/commercialhaskell/stack/wiki/Downloads#ubuntu)
instructions as appropriate.

Next, follow the rest of the basic [README] instructions for building
and running snowdrift.


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

Install the
[haskell-stack-git](https://aur4.archlinux.org/packages/haskell-stack-git/)
package from the AUR. Run:

    stack upgrade --git

Follow the rest of the instructions in the [README].


### NixOS

#### Installing Stack

Clone the git repo:

    git clone https://github.com/commercialhaskell/stack.git

Create a `shell.nix` file:

    cd stack
    cabal2nix --shell ./. --no-check > shell.nix

(Note that the tests fail on NixOS, so disable them with
`--no-check`.)

Install Stack to your user profile:

    nix-env -i -f shell.nix

#### Building Snowdrift

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

#### PostgreSQL and sdm

To get the sdm script to work, NixOS users should install postgres
by adding these lines to `/etc/nixos/configuration.nix`:

    services.postgresql.enable = true;
    services.postgresql.package = pkgs.postgresql94;

Then issue `sudo nixos-rebuild switch` to install.
Afterwards you may need to create the postgres user, like so:

    sudo -su root
    createuser -s -r postgres


### \*BSD

*Not yet documented: installing dependencies on \*BSD*

The BSDs use different postgres configurations than the common
GNU/Linux distros, so we must pass different paramaters to sdm.
Where our instructions say `sdm init` add arguments as shown below:

* OpenBSD: `sdm init --sudoUser _postgresql`
* FreeBSD: Untested but we believe `sdm init --sudoUser pgsql --pgUser
  pgsql` will work.


### OS X

The links in the [README] should have instructions for each of the
dependencies. We don't think any extras are needed otherwise.

The OS X build process seems to have some issues with postgres user names;
until we update sdm to accept special arguments for OS X, the database set-up
will need to be done manually. See the appendix at the end of this file.

Another option is a VM via Vagrant (see below).


### Windows

Note: At this time, Windows builds have some issues.
The guaranteed approach is to use a GNU/Linux Virtual Machine,
perhaps via Vagrant (see below).
To test building directly on Windows:

Install PostgreSQL 32-bit version from
<http://www.enterprisedb.com/products-services-training/pgdownload#windows>

Add the PostgreSQL bin directory to the path
`C:\Program Files (x86)\PostgreSQL\9.4\bin`

Instead of using sdm, follow the appendix at the end of this file for setting up
the development database manually. Some of the precise commands may need slight
adapting such as using `psql -U postgres` to enter the psql prompt.

The rest of the [README] instructions should work (although we're not sure about
the use of Stack reliably on Windows yet).

*Note:* SnowdriftEmailDaemon won't build on Windows, so `stack test` will fail.
Building, running, and working on the site might still work otherwise.


### Virtual Machine options / Vagrant

Anyone could use a Virtual Machine with a system known to work and then
follow the steps for that system, working entirely within the VM.

Alternatively, to better *integrate* with your existing system,
we offer an option with Vagrant.

Our Vagrant instance uses a Debian system preset with our core dependencies.
Vagrant then allows the build to work in the virtual machine while you continue
using your regular text editor, file system, web browser etc.

To use this option, see our [Vagrant instructions](SETUP_VAGRANT.md).


General installation process
============================

While the [README] has all the core install instructions,
here we document the details more thoroughly.

## Extra tools

In addition to the required tools, we also suggest:

    stack install hlint

`hlint` followed by a filename will show suggestions for Haskell style.

Setting up the database
-----------------------

We offer a simple script that will setup the PostgreSQL databases for you.
For most GNU/Linux systems, simply run:

    stack exec sdm init

It will prompt you for your sudo password.

To set up databases manually, see the appendix at the end of this guide.

Running the site
----------------

### Running the development version of the site

The standard approach for running and working on the site is to run
`stack exec yesod devel` from the project directory.
It can stay running in one terminal while you work elsewhere.
It will automatically rebuild and rerun the site whenever it detects changes.

To stop the development site, type `quit`, then press the Enter key.

In rare cases, you may need to run `stack clean` if the development site
fails to recognize a change.

### Using `stack build`

You must run `stack build` whenever you:

* add new dependencies (i.e. edit the `build-depends` field in
  `Snowdrift.cabal`)
* update any extra binaries such as the payment processing script, the sdm
  database configuration script, or the email daemon.

#### Alternative development option

A more manual approach to building and running the site:

* Run the site with `stack exec Snowdrift Development`
  (to stop the site in this case, use ctrl-C).
* This does not automatically rebuild, so you must manually run `stack build`
  whenever you want to compile any updates to the code.
* Advantages to this approach: less running processes (no need to watch for file
  changes) and no need to do extra compiling into special yesod-devel directory.

Using the live test site
------------------------

Test the running site by directing your web browser to <http://localhost:3000>.

The Dev DB comes with several users via the built-in log-in:
(username and passphrase are the same)
`admin`; `guest`; `established`.

You can now register new users, make pledges, add discussion comments,
tickets, wiki pages, blog posts, and test and work on all aspects of the site.

Running tests
=============

After making various changes to the code and running locally
to verify that everything compiles and also appears to work as desired,
best practice involves then running our automated tests before sharing
your changes with the main project.

Assuming you ran `sdm init` to set up the databases,
you can now run the tests with:

    stack build && stack test

If tests fail, try to figure out what is wrong. Ask us for help if needed.

Additional notes about databases
================================

Database migrations
-------------------

After any change to the database schema (in config/models),
the first time you recompile and then start the server,
a migration script will be automatically generated and placed in /migrations.

If there are no unsafe statements in the migration,
the migrations will be run and the server will continue to start normally.

If there are any unsafe (may destroy data) statements,
they are placed in migrations/migrate.unsafe, and the server will abort.

In an unsafe case, if the data *is* intended to be lost
(e.g. destroying a column storing data we no longer want),
copy the statements to the new migrateN file (creating it if necessary).

If you don't want to lose the data
(a column is being moved to a different table, a column is being renamed, &c),
modify the migration file to use the appropriate intended SQL commands.


Committing database migrations
------------------------------

NOTE: THE MIGRATION SITUATION IS BEING ADJUSTED, THIS MAY BE OUTDATED

In the course of testing and/or resetting your database,
you might generate extra migrations. When that happens,
be sure to reset your database andremove any extraneous migration files.
Once you have a final version of the code, you can run the site once to
generate the correct final migration.

Ideally consolidate all migrations to only one migration file per commit.

Make sure to add the associated migration file to git when you commit
the corresponding schema changes.

When merging migrations, put any you've added on the end in separate file(s).
Don't merge them into migration files others may have already run.


Resetting or updating your development database
-----------------------------------------------

To remove any changes and reset your database to the devDB default
(such as when others have provided a new update you want to try
or to start clean before making changes you plan to commit) run:

    sdm reset


Sharing updates to the devDB database
-------------------------------------

If you make specific improvements or additions to your database
that you think will make for a better start for other contributors
(and also when you have updated the basic database with migration files),
you can use the following command to export the changes
(which can then be committed via git as usual).

While in your project directory:

    stack exec -- sdm export --db=dev

which is the same as running:

    sudo -u postgres pg_dump snowdrift_development >devDB.sql

You can test that the export worked by running `sdm reset` and verifying
in the running site that everything is as expected.

Then, the new devDB.sql file may be committed and shared like other changes.

Updating to the latest test database
------------------------------------

When the testDB.sql file is updated, you'll need to update your template.

Simply run `stack exec -- sdm reset --db=test` to reset/update your test databases.

See the appendix section of this guide for how to reset manually.

---

Happy hacking!


APPENDIX: Manual database management
======================================

Our sdm script makes database management quick and easy.
All the steps below can be done simply with the sdm script,
but here we explain what it does and how to handle databases manually.
The commands below are written with GNU/Linux in mind.

***

Notes for OS X
------------------

Assuming the postgres server is running,
where `sudo -u postgres psql` is seen below,
run `psql postgres` instead.
The commands that don't use psql can be adapted
to run within the psql command line.

For OS X, instead of `sudo -u postgres psql snowdrift_development <devDB.sql`
follow these steps:

1) Run `psql snowdrift_development`
2) At snowdrift_development=# prompt, run `\i devDB.sql`

Similar adjustments will be needed for the
test database setup and resetting databases.

***


Setting up the development database manually
--------------------------------------------

Copy the config/postgresql.template to a new config/postgresql.yml file:

    cp config/postgresql.template config/postgresql.yml

Create database user called "snowdrift_development"
*without* superuser, createdb, or createuser privileges:

    sudo -u postgres createuser -S -D -R snowdrift_development

Run postgres psql:

    sudo -u postgres psql

You should see a line that looks like:

    postgres=#

(NOTE: all of the commands run from the postgres shell must end with a `;`)

Create snowdrift_development database:

    postgres=# create database snowdrift_development;

Add a password to the snowdrift_development user
(for reference, the sdm script generates a random passphrase for this step;
you may substitute any arbitrary passphrase instead of 'somepassphrase'):

    postgres=# alter user snowdrift_development with encrypted password 'somepassphrase';

Then add user to database:

    postgres=# grant all privileges on database snowdrift_development to snowdrift_development;

Leave postgres (with ctrl-D).

Edit config/postgresql.yml and update the password to match the one you entered.

Import development database:

    sudo -u postgres psql snowdrift_development < devDB.sql


Reset the development database manually
---------------------------------------

Start by deleting your database:

    sudo -u postgres psql <<<'drop database snowdrift_development'

Then simply re-create the database by rerunning two of the commands
from the "Setting up" section above.

First the "Create snowdrift database" command:

    sudo -u postgres createdb snowdrift_development

and then the "Import development database" command:

    sudo -u postgres psql snowdrift_development <devDB.sql

That's it. You will *not* need to re-run the database user commands.


Setting up the test template database manually
----------------------------------------------

Like setting up the original development database,
we need to set up a database and user for testing.

Create database user *without* superuser or createrole privileges
but *with* createdb privileges:

    sudo -u postgres createuser -S -d -R snowdrift_test

Create the snowdrift_test database *template*:

    sudo -u postgres createdb snowdrift_test_template

Run postgres psql to bring up the postgres=# prompt:

    sudo -u postgres psql

At the postgres=# prompt, mark the new database as a template:

    postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';

Then, add any arbitrary passphrase to the snowdrift_test user
(substitute whatever you like instead of somepassphrase):

    postgres=# alter user snowdrift_test with encrypted password 'somepassphrase';

Leave postgres (with ctrl-D).

If you used a different password than the one
you used for snowdrift_development,
then edit config/postgresql.yml
and add a "password:" line under "Testing:"
along with your new passphrase.

Finally, import the testDB.sql to the new template database:

    sudo -u postgres psql snowdrift_test_template <testDB.sql


Resetting the testDB manually
-----------------------------

Go to the postgres=# prompt:

    sudo -u postgres psql

Unmark the template (don't include the postgres=# prompt part):

    postgres=# update pg_database set datistemplate=false where datname='snowdrift_test_template';

Use ctrl-D to leave the prompt.

Drop the template DB:

    sudo -u postgres psql <<<'drop database snowdrift_test_template'

Then we repeat the commands above for setting up the test DB,
skipping the dependencies/user-creation/password parts (those don't need updating).

    sudo -u postgres createdb snowdrift_test_template
    sudo -u postgres psql
    postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';
    sudo -u postgres psql snowdrift_test_template <testDB.sql

[README]: README.md
