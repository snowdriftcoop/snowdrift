Snowdrift.coop
==============

Infrastructure for [Snowdrift.coop](https://snowdrift.coop).

Our code is mirrored at [GitHub](https://github.com/dlthomas/snowdrift) (which is popular but proprietary) and [Gitorious](https://gitorious.org/snowdrift/snowdrift) (which is FLO, licensed AGPL, but less popular).


Work to do / how to help
========================

On the live site, we have [volunteer info](https://snowdrift.coop/p/snowdrift/w/how-to-help) which includes a general guide around the site. The site wiki and discussion system is our main organizational tool, but our #snowdrift IRC channel on freenode.net is a good place to get help or ask casual questions.

We have [our own in-progress ticketing system](http://snowdrift.coop/p/snowdrift/t) which lists most of the specific tasks of all sorts, organized with tags indicating priority, skills involved, and more.

This README assumes you are running a GNU/Linux system and have a basic understanding of command-line operations.
If you are on a different system, we will still work to help you get set up.

If you need help with *any* issues here, let us know. We are always happy to assist and answer *any* questions!
                                                                                                                                        

About the frameworks and tools we use
=====================================

The Snowdrift.coop site uses the **[Yesod web framework](http://www.yesodweb.com/)**.

Yesod uses the Haskell programming language alongside its
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates) which generate HTML/CSS/JS using indentation with no need for closing tags or bracketing.

HTML, CSS, and JavaScript work on the site may be done without knowing Haskell, but because everything is integrated, some familiarity with the Haskell syntax is helpful.

* For learning Haskell, check out the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell), which also has links to additional resources.
* Stack Overflow user postings are FLO (CC-BY-SA), see the tags for [yesod](http://stackoverflow.com/questions/tagged/yesod) and [haskell](http://stackoverflow.com/questions/tagged/yesod)
* Alongside #snowdrift on freenode.net, the #yesod and #haskell channels are also active and helpful
* "cabal repl" is a command that loads [ghci](https://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively) with connection to the project code. Then, you can easily import particular files from the code and explore the functions.

Our front-end uses **[Twitter Bootstrap](http://getbootstrap.com/)** for layout and styles, although there are many cases where we use our own custom CSS.

On the site, we have a page about [site-design](https://snowdrift.coop/p/snowdrift/w/site-design) with associated tickets and discussion.

As a suggestion: Firefox's built-in developer tools offer lots of great ways to test the site, although [Firebug](https://getfirebug.com) may also be a useful addition for certain features.


Text-editor settings
--------------------

We recommend setting your text editor to have the TAB key do indentation of four spaces.
For VIM, for example, the config file .vimrc should have these three lines:

    set expandtab
    set shiftwidth=4
    set tabstop=4

VIM users should also install [Syntax Highlighting Files for Haskell](https://github.com/pbrisbin/html-template-syntax).

Emacs users should use a package manager (preferably Marmalade) to install [Haskell Mode](https://github.com/haskell/haskell-mode).


Development guidelines and notes
================================

Overall, we strive to follow universal standards, be fully accessible, and avoid browser-specific code.

We generally build with *progressive enhancement* in mind.
Content and functions should be built with simple HTML/CSS along with Yesod/Haskell server-side functions.
JavaScript may then added for enhancement. Consider the ideas of [Unobtrusive JavaScript](http://en.wikipedia.org/wiki/Unobtrusive_JavaScript).
Use of NoScript should never causes a broken experience.
We also make sure all our JavaScript is recognized by the FSF's [LibreJS plugin](https://www.gnu.org/software/librejs/).

We have a separate wiki and discussion page on the site for discussing specific [web-design issues](https://snowdrift.coop/p/snowdrift/w/site-design).

Please consider adding concise comments to your code explaining to others anything you think may be unclear.
Ideally, follow the syntax for [Haddock Haskell documentation](http://www.haskell.org/haddock/doc/html/markup.html).

Working on the code
===================

See the main [Git documentation](http://git-scm.com/documentation) if you are new to Git.
You only need rudimentary understanding of Git to start contributing to our code.
You will need to know how to push, pull, and commit.
It will help to understand basic branching so that you can segregate work on different features.
                                                                      
Cloning the repository
----------------------

1. Have or make an account on Gitorious or GitHub
2. Clone (Gitorious' term) / Fork (GitHub's term) the snowdrift repository to your account
3. Tell Gitorious/GitHub the public side of your local SSH key
    * If you don't yet have a key, create one on your local machine with the command "ssh-keygen"
    * The public part is in the .pub file (such as id_rsa.pub)
    * Both sites have further instructions if this isn't clear enough
4. On your local machine, use the "git clone" command with the Gitorious or Github address for your account

This will create a directory and download the code to it.
In the future, when in the new directory,
"git pull" will update your local machine from your Gitorious or GitHub account,
and "git push" will go the other direction, sending any local commits to Gitorious or GitHub.

After pushing to your online account, alert us to the changes with Gitorious' "request merge" or GitHub's "pull request" commands on their websites.

To pull updates from our main code, use

    git pull git@gitorious.org:snowdrift/snowdrift.git

Note: if you are collaborating with others before a patch is ready to go live and some participants use Gitorious and others use GitHub, you can pull by just using the full git address and just sending your collaborator a an e-mail or otherwise alerting them to pull your updates.


Building
--------

Install ghc, cabal, and postgresql, however you do that on your system.

On Debian-based GNU/Linux distros, that's:

    sudo apt-get install ghc cabal-install haskell-platform postgresql zlib1g-dev libpq-dev happy alex

   *note: there have been some errors reported with older versions of ghc and the haskell-platform* At this time, we are using GHC 7.6.3 and Haskell Platform 2013.2.0.0 — both are included in the latest Ubuntu, and there are [instructions for building updated GHC on older Ubuntu-based systems](https://gist.github.com/Dexyne/5791465). We tested this with Ubuntu 12.04 LTS and it should work on derivatives as well (such as the fully-FLO Trisquel 6). These instructions or similar should work for other systems as well, but see <http://www.haskell.org/platform/> for more general info.

(There are a few non-Haskell libraries with some dependencies which you may
need to install and which presumably will be in your system's package manager.
We don't have a full list compiled yet, but they can be picked out of the error
messages if the commands below fail. If you make a list,
please update this README and send a pull request!)


Update cabal's package list:

    cabal update

Add ~/.cabal/bin to your PATH; for bash, run the following command
(also add it to your .bashrc (or equivalent) file so it will run automatically in the future):

    export PATH=~/.cabal/bin:$PATH

Update cabal so you can use the new [sandbox](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) feature in cabal 1.18+:

    cabal install Cabal cabal-install

**change to your snowdrift directory (if you're not already working there).**

For isolation in case you have multiple snowdrift checkouts or other Haskell projects, initiate a cabal sandbox:

    cabal sandbox init

Next, add your sandboxed binaries to your PATH: (For the future, you may also wish to add this to your .bashrc or equivalent.)

*Replace "~/snowdrift/" in the command below with your correct directory.*
GitHub clones are normally just "snowdrift" but Gitorious clones are "*username*-snowdrift".
It is also possible to place the directory wherever you like with whatever name.

    export PATH=~/snowdrift/.cabal-sandbox/bin:$PATH

Install dependencies and build Snowdrift:

    cabal install

This will take a *long* time but should ultimately tell you it installed Snowdrift.

You can also use cabal install to test your changes later, and then it will run much faster.

Setting up the database
-----------------------

*This can be done while building is in progress*

Go to the config/ directory within the project directory and make a copy of postgresql.template and name the new file postgresql.yml

Create database user called "snowdrift_development" *without* superuser, createdb, or createuser priveleges:

    sudo -u postgres createuser -S -D -R snowdrift_development

Create snowdrift_development database:

    sudo -u postgres createdb snowdrift_development

Run postgres psql:

    sudo -u postgres psql

You should see a line that looks like:

    postgres=#

Add a password to the snowdrift_development user (you may substitute your chosen passphrase instead of 'somepassphrase'):

    postgres=# alter user snowdrift_development with encrypted password 'somepassphrase';

Then add user to database:

    postgres=# grant all privileges on database snowdrift_development to snowdrift_development;

Leave postgres (with ctrl-D).

Edit config/postgresql.yml and update the password to match the one you entered.

Import development database:

    sudo -u postgres psql snowdrift_development <devDB.sql


Running the site
----------------

Once snowdrift is built and assuming you're using a cabal sandbox, have set your PATH correctly, and are in your snowdrift directory,
you can start the server with the command:

    Snowdrift Development

To rebuild the site after changes to the code, run cabal install first before starting the server.
    
Alternately, you may use the yesod devel command which does a combined rebuild and server start.
In rare cases, yesod devel may succeed where cabal install failed (or vice versa),
but the main advantage to yesod devel is that it can be left running and will automatically update your build after each saved change to any file.
                                             
To enable yesod devel, first install yesod-bin:

    cabal install yesod-bin

From now on, you can rebuild and start the server with:

    yesod devel

After the server starts, it may print a bunch of text about creating tables, and it will then sit ready, waiting for connections.

For either build approach, access the server by directing your web browser to localhost:3000


Using the live test site
------------------------

You can log into the site via the built-in system with user: admin pass: admin

With that user, create wiki pages at localhost:3000/p/snowdrift/w/*pagename*/new

See the wiki documentation on the live site [about the wiki](https://snowdrift.coop/p/snowdrift/w/wiki) and more.

That's all you need to get started!
You can now register new users, make pledges, and test and work on all aspects of the site.


Additional notes about database and testing
===========================================

Database migrations
-------------------

When you first start the server (following a compile) after changing the database schema (in config/models) a migration script will be automatically generated and placed in migrations.

The safe (i.e. guaranteed not to lose data) statements, if any, are placed in migration/migrateN where N is the next number in sequence.

If there are no unsafe statements in the migration, the safe statements will be run and the server will continue to start normally.

If there are any unsafe (may destroy data) statements, they are placed in migrations/migrate.unsafe, and the server will abort.

In this case, if the data *is* intended to be lost (e.g. destroying a column storing data we no longer want) just copy the statements to the new migrateN file (creating it if necessary).

If you don't want to lose the data (a column is being moved to a different table, a column is being renamed, &c) modify the migration file as appropriate.

If you try different things in the course of testing and/or reset your database, you might generate extra migrations.
Be sure to clear and reset any migrations once you have a final version.
Then, you can run the site once to generate the correct final migration.

Committing database updates
---------------------------

Add any valid new migrations/migrateN files to git when you commit the corresponding schema changes.

When merging migrations, always put any you've added on the end - don't merge them into migration files others have probably already run.

Although it will run the migrations anyway, it will be efficient to also update devDB.sql, as described below.


Updating the devDB database
--------------------------------

If you make specific improvements or additions to your database that aren't just playing around but that you think will make for a better starting database for other contributors (and also when you have updated the basic database with migration files), you can use the following command to export the changes (which can then be committed via git as usual).

While in your project directory:

    sudo -u postgres pg_dump snowdrift_development >devDB.sql

Resetting your database
---------------------------

To remove any changes and reset your database to the devDB default
(such as when others have provided a new update you want to try or when you want to start clean before making changes you plan to commit),
follow these steps:

Start by deleting your database:

    sudo -u postgres psql <<<'drop database snowdrift_development'

Then simply re-create the database by rerunning two of the commands from the "Setting up the database" section above.

First the "Create snowdrift database" command:

    sudo -u postgres createdb snowdrift_development
    
and then the "Import development database" command:

    sudo -u postgres psql snowdrift_development <devDB.sql

That's it. You will *not* need to re-run the database user commands.


Running tests
=============

After making various changes to the code and running locally to verify that everything compiles and also appears to work as desired,
it is best to then run our automated tests before sharing your changes with the main project.

Setting up the test template database
-------------------------------------

To prepare for running tests, you will need to install some extra dependencies with the command:

    cabal install --only-dependencies --enable-tests

Like setting up the original development database, we then need to set up a database and user for testing.

Create database user *without* superuser or createrole priveleges but *with* createdb priveleges:

    sudo -u postgres createuser -S -d -R snowdrift_test

Create the snowdrift_test database *template*:

    sudo -u postgres createdb snowdrift_test_template

Run postgres psql to bring up the postgres=# prompt:

    sudo -u postgres psql

At the postgres=# prompt, mark the new database as a template:

    postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';

Then, add a password to the snowdrift_test user
(as with snowdrift_development, you may substitute your chosen passphrase instead of 'somepassphrase'):

    postgres=# alter user snowdrift_test with encrypted password 'somepassphrase';

Leave postgres (with ctrl-D).

If you used a different password than the one you used for snowdrift_development,
then edit config/postgresql.yml and add a "password:" line under "Testing:" along with your new passphrase.

Finally, import the testDB.sql to the new template database:

    sudo -u postgres psql snowdrift_test_template <testDB.sql


Running the tests
-----------------

To run the tests, do

    yesod test

If tests fail, try to figure out what is wrong. Ask us for help if needed.


Updating to the latest test database
------------------------------------

When the testDB.sql file is updated, you'll need to update your template.

Go to the postgres=# prompt:

    sudo -u postgres psql

Unmark the template (don't include the postgres=# prompt part):

    postgres=# update pg_database set datistemplate=false where datname='snowdrift_test_template';
          
Use ctrl-D to leave the prompt.

Drop the template DB:

    sudo -u postgres psql <<<'drop database snowdrift_test_template'

Then follow the instructions above about setting up the test DB, skipping the dependencies and the items about user creation and user password (those don't need updating).

---

Happy hacking!
