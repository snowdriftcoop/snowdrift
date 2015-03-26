Snowdrift.coop
==============

This guide covers the [Snowdrift.coop](https://snowdrift.coop) codebase
and development process.

Step-by-step instructions require no more than beginning-level programming
or web designer experience.

This guide assumes you are running a GNU/Linux system and have at least a
*basic* understanding of command-line operations.

If you are on a different system or need *any* other help,
come say "hi" at our freenode.net IRC channel
[#snowdrift](https://snowdrift.coop/p/snowdrift/w/irc).
We are always happy to assist and answer *any* questions!


About the frameworks and tools we use
=====================================

Snowdrift.coop uses the **[Yesod web framework](http://www.yesodweb.com/)**.
Like the software itself, the associated book and documentation are all
Free/Libre/Open (FLO) and quite thorough.

Yesod uses the Haskell programming language alongside its
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates).
With some minor variations (mainly use of indentation instead of closing tags),
these templates use standard HTML, CSS, and JavaScript.

Our front-end uses **[Twitter Bootstrap](http://getbootstrap.com/)**,
although we use our own custom CSS in many cases.

A suggestion for beginners: Firefox's built-in developer tools
and the [Firebug](https://getfirebug.com) plugin both offer great
(and complementary) functions for testing and experimenting with the live site.


Learning Haskell
----------------

Because everything is integrated, some familiarity with Haskell syntax
is helpful even if you stay mostly with front-end development.
At any rate, Haskell is a powerful and enjoyable language worth learning
for anyone interested in programming.

To learn Haskell, we recommend these FLO resources:

* The [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell) offers a superb
  introductory overview and includes links to many additional resources.

* At Stack Overflow, see the tags for
  [yesod](http://stackoverflow.com/questions/tagged/yesod) and
  [haskell](http://stackoverflow.com/questions/tagged/yesod)

* Alongside #snowdrift on freenode.net, check out #yesod #haskell
  and #haskell-beginners

* A useful development tool  is "cabal repl" — a command that loads
  [ghci](https://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively)
  in a mode connected to the project. Using that, you can easily import
  files from the code and explore the functions.

* To help write clean Haskell code and learn conventions, run hlint on your
  files to get suggestions for possible improvements.
    * Given a working Haskell installation,
      add hlint with the command "cabal install hlint"


Text-editor settings
--------------------

We recommend setting your text editor to have the TAB key do indentation of
four spaces generally. However, we use 2-space indentation for .hamlet files.

### vim

For vim users, your config file .vimrc should include these four lines:

    set expandtab
    set shiftwidth=4
    set tabstop=4
    au FileType hamlet setl sw=2 sts=2 et

You should also install
[vim Shakespearean Highlighting](https://github.com/pbrisbin/vim-syntax-shakespeare)

Some other optional vim plugins to consider (among many available):
[Haskell-Vim extra syntax](https://github.com/raichoo/haskell-vim)
and
[vim2hs](https://github.com/dag/vim2hs)


### Emacs

Emacs users should use a package manager (preferably Marmalade) to install
[Haskell Mode](https://github.com/haskell/haskell-mode)
and
[Hamlet Mode](https://github.com/lightquake/hamlet-mode).

Our included [`.dir-locals.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html) file
makes Emacs use the recommended 4-space indentation.


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
Use of NoScript should never causes a broken experience.
We also make sure all our JavaScript is recognized
by the FSF's [LibreJS plugin](https://www.gnu.org/software/librejs/).

We have separate wiki and discussion pages on the site
for [web-design issues](https://snowdrift.coop/p/snowdrift/w/site-design)
and [coding issues](https://snowdrift.coop/p/snowdrift/w/coding).

The [complete list of Snowdrift tickets](https://snowdrift.coop/p/snowdrift/t)
shows all items from all discussion pages associated with the project.

Consider adding concise comments to your code following the syntax for
[Haddock documentation](http://www.haskell.org/haddock/doc/html/markup.html).


Working on the code
===================

See the [Git documentation](http://git-scm.com/documentation) if new to Git.
You only need basic understanding to start contributing to our code.

Cloning the repository
----------------------

1. Have or make an account on Gitorious or GitHub
2. Clone (Gitorious) or Fork (GitHub) the snowdrift repository to your account
3. Tell Gitorious/GitHub the public side of your local SSH key
    * If not done already, create a key on your local machine with `ssh-keygen`
    * The public part is in the .pub file (such as id_rsa.pub)
    * Both sites have further instructions if this isn't clear enough
4. On your local machine, use the `git clone` command
   with the Gitorious or Github address for your account

This will create a directory and download the code to it.
In the future, when in the new directory,
`git pull` will update your local machine from the Gitorious or GitHub account,
and `git push` will go the other direction.

After pushing to your online account,
alert us to the changes with Gitorious' "request merge"
or GitHub's "pull request" commands on their websites.

To get updates from our main code, run

    git remote add snowdrift-main git@gitorious.org:snowdrift/snowdrift.git

Then you can get the latest updates with

    git fetch snowdrift-main

Then, to see the diff for the updates, use

    git diff snowdrift-main/master

To merge the updates, checkout a branch where you want the updates, then

    git merge snowdrift-main/master

Note: when working with others, you can use the same fetch process
to set up remotes for each of collaborator and send them merge-requests /
pull-requests by choosing their repository as the target. Alternatively,
simply communicate by e-mail or IRC or other options about fetching updates.


Building
--------

### Notes for different operating systems:

We don't yet have everything documented, but Snowdrift has been built
successfully on Debian, Ubuntu, Arch, Gentoo, and related distros of GNU/Linux
and should work on all other distros.

For NixOS, see the notes in the appendix here.

Snowdrift also has been built on Mac OS Yosemite.
The Mac OS build process seems to have some issues with postgres user names;
so for now, the database set-up for Mac OS will need to be done manually.
See the appendix at the end of this file for more.


### Build steps

Install the essential dependencies: ghc, cabal, postgresql, happy, alex, zlib1g

Additional dependency (which may vary for different systems): libpq-dev

On Debian-based GNU/Linux distros, use this command:

    sudo apt-get install ghc cabal-install postgresql zlib1g-dev libpq-dev happy alex

**Note: we are now using GHC 7.8.x**
If your system's GHC version is older,
get the update from <https://www.haskell.org/ghc/>

Next, update cabal's package list:

    cabal update

Add cabal location(s) to your PATH;
the location may vary by system and set-up.
Below are the most common situations:

* for GNU/Linux, add `export PATH=.cabal-sandbox/bin:~/.cabal/bin:$PATH`
  to your ~/.bashrc (or equivalent) file

* for Mac OS, try adding `export PATH="$HOME/Library/Haskell/bin:$PATH"`
  to ~/.bash_profile

Start a new terminal (or run `source ~/.bashrc` or similar)
to make the PATH active.

Now, upgrade cabal itself:

    cabal install cabal-install

**change to your snowdrift project directory (if not already there).**

Then, initiate a cabal sandbox:

    cabal sandbox init

Install dependencies and build Snowdrift

    cabal install --enable-tests -fdev

This will take a *long* time but should ultimately tell you it installed.
Note: the `-fdev` flag skips optimization to make build faster.
It should be ommited for building the actual live site.

Contact us for help if the build is not successful.


Setting up the database
-----------------------

We offer a simple script that will setup the PostgreSQL databases for you.
Some systems may need extra set-up, but for most GNU/Linux systems, simply run:

    sdm init

It will prompt you for your sudo password.

To set up databases manually, see the appendix at the end of this guide.


Running the site
----------------

After completing all the steps above,
start the server from within your snowdrift directory with the command:

    Snowdrift Development

To stop the running server, press ctrl-C

To rebuild after code changes, run `cabal install`
(perhaps with `-fdev` to skip optimization).

(`cabal build` works also but fails to recognize changes to template files)


After the server starts, it may print a bunch of text about creating tables,
and it will then sit ready, waiting for connections.
When you need to update an executable, use cabal install.


Using the live test site
------------------------

Test the running site by directing your web browser to localhost:3000

You can log into the site via the built-in system with
user: `admin` pass: `admin`

You can now register new users, make pledges, add discussion comments,
tickets, wiki pages, blog posts, and test and work on all aspects of the site.


Running tests
=============

After making various changes to the code and running locally
to verify that everything compiles and also appears to work as desired,
best practice involves then running our automated tests before sharing
your changes with the main project.

Assuming you ran `sdm init` to set up the databases, run the tests with:

    yesod test

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

    sdm export --db=dev

which is the same as running:

    sudo -u postgres pg_dump snowdrift_development >devDB.sql


Updating to the latest test database
------------------------------------

When the testDB.sql file is updated, you'll need to update your template.

Simply run `sdm reset --db=test` to reset/update your test databases.

See the appendix section of this guide for how to reset manually.

---

Happy hacking!

---

APPENDIX A: Using yesod devel
=============================

`yesod devel` is a command that will rebuild snowdrift, start the server,
*and* can stay running and automatically update the build after file changes
(although it fails to auto-recognize changes in some file types like .cassius).

To enable yesod devel, you must first install yesod-bin.
However, yesod-bin will not currently build in the main sandbox.

So, at this time, to enable yesod devel, make a new directory for yesod-bin.
Call it "yesod-bin-sandbox" perhaps.

Then, inside the new directory, run `cabal sandbox init`
followed by `cabal install yesod-bin`.

Next, add to the new directory to your PATH. Put in your .bashrc the line:

    export PATH=~/yesod-bin-sandbox/.cabal-sandbox/bin:$PATH

(change the ~/ to wherever you actually put the directory)

In a new terminal (so it recognizes the new path),
you can rebuild and start the server in your snowdrift directory by running

     yesod devel

To stop yesod devel, press ENTER a couple times

Note that `yesod devel` builds just the library,
so `cabal install` and related commands are needed to update other resources
like sdm or the payment processing script.


APPENDIX B: Using the Nix package manager
=========================================

We're now testing the use of Nix as a reliable, simple way
to manage packages for Snowdrift.
Once we have it fully working, it should help simplify building overall.

**The instructions in this appendix are just draft and need cleaning up.**

We're not sure each of these commands is best,
it may change as we continue testing.

To install Nix, visit [NixOS.org/nix](https://nixos.org/nix/)
and follow the "Get Nix" instructions (works for GNU/Linux and Mac OS).

*Note: Nix can take a lot of drive space, so if you do not have many GB
of free space on your root partition, you may need to find another approach.
Free up space or put the `nix` directory somewhere else with more space
and edit `/etc/fstab` to bind the location to mount at `/nix`.*

Next, log out and back into your whole system (the environment variables
command shown at the end of the install script's output works for the
immediate terminal session for a temporary fix).

[Nixpkgs](https://nixos.org/nixpkgs/), a collection of packages used by Nix,
usually has only the latest packaged version and is a rolling-release
distribution, which leaves us with two options:

* Update our code and dependencies whenever the unstable channel
  (or the master branch) is changed.

* Maintain our own collection of package versions that are known to work.

The former is clearly too much work and is not reliable anyway,
so we use the latter approach. Get a copy of our repository with this command:

    git clone https://github.com/nkaretnikov/nixpkgs.git -b snowdrift

It automatically switches to the right branch,
so the only thing left is to point the
[`NIX_PATH`](https://nixos.org/nix/manual/#sec-common-env)
environment variable to the directory *containing* the `nixpkgs` repository.
For example, if a user cloned it to `/home/user`,
that's the value they need to use:

    export NIX_PATH=/home/user

Within the snowdrift project directory,
run `nix-shell --pure -j4 shell.nix` to get necessary libraries
and set `PATH`
(the `-j4` part should be adapted to fit the number of cores on your machine).

The first time this is run, it will take a long time,
but then will present you a new prompt within `nix-shell`.

Within the nix shell, run

    cabal configure -fdev --enable-tests && cabal build -j4

*Note the `-fdev` argument speeds up the build by bypassing optimization,
which means the site runs slower, but that's not a problem for development work.*

This will take a *long* time but should ultimately tell you it built `Snowdrift`.

Since the `nix-shell` command changed your `PATH`,
it doesn't have things like `sudo`, which is used by the `sdm` script.
Run `dist/build/sdm/sdm init` *outside* the nix shell
(in a different terminal window) if you need to setup the databases.
Then, you can go back to the nix shell to run `cabal test`,
which runs the testsuite.

You can run the application with `dist/build/Snowdrift/Snowdrift Development`.


Note for users of NixOS
-----------------------

To get the sdm script to work, NixOS users should install postgres
by adding these lines to /etc/nixos/configuration.nix:

  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql94;

Then issue `sudo nixos-rebuild switch` to install.
Afterwards you may need to create the postgres user, like so:

    sudo -su root
    createuser -s -r postgres


APPENDIX C: Manual database management
======================================

Our sdm script makes database management quick and easy.
All the steps below can be done simply with the sdm script,
but here we explain what it does and how to handle databases manually.
The commands below are written with GNU/Linux in mind.

***

Notes for Mac OS X
------------------

Assuming the postgres server is running,
where `sudo -u postgres psql` is seen below,
run `psql postgres` instead.
The commands that don't use psql can be adapted
to run within the psql command line.

For Mac OS, instead of `sudo -u postgres psql snowdrift_development <devDB.sql`
follow these steps:

1) Run `psql snowdrift_development`
2) At snowdrift_development=# prompt, run `\i devDB.sql`

Similar adjustments will be needed for the
test database setup and resetting databases.

***


Setting up the development database manually
--------------------------------------------

Go to the config/ directory within the project directory,
make a copy of postgresql.template, and name the new file postgresql.yml

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

    sudo -u postgres psql snowdrift_development <devDB.sql


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

Create database user *without* superuser or createrole priveleges
but *with* createdb priveleges:

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
