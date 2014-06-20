Snowdrift.coop
==============

This guide is a thorough introduction to the [Snowdrift.coop](https://snowdrift.coop) codebase and development process.

We have written step-by-step instructions to be accessible to even beginning programmers and web designers.

This guide assumes you are running a GNU/Linux system and have at least a *basic* understanding of command-line operations.
If you are on a different system or need any other help, come say "hi" at #snowdrift on [freenode.net](http://webchat.freenode.net/?channels=#snowdrift). We are always happy to assist and answer *any* questions!
                                                                                                                                        

About the frameworks and tools we use
=====================================

The Snowdrift.coop site uses the **[Yesod web framework](http://www.yesodweb.com/)**.
Like the software itself, the associated book and documentation are all FLO and quite thorough.

Yesod uses the Haskell programming language alongside its
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates).
With some minor variations (such as indentation instead of closing tags or bracketing),
normal HTML/CSS/JavaScript can be used directly in the templates.

Our front-end uses **[Twitter Bootstrap](http://getbootstrap.com/)** for layout and styles,
although there are many cases where we use our own custom CSS.

As a suggestion for beginners: Firefox's built-in developer tools
and the [Firebug](https://getfirebug.com) plugin both offer great (and complementary) functions
for testing and experimenting with the live site.


Learning Haskell
----------------

Because everything is integrated, some familiarity with the Haskell syntax is helpful
even if you stay mostly with front-end development.
We also encourage anyone interested to go deeper and learn how
powerful and enjoyable programming in Haskell can be.

* To learn Haskell, check out the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell), which is a great introduction and includes links to many additional resources.
* Stack Overflow user postings are FLO (CC-BY-SA), see the tags for [yesod](http://stackoverflow.com/questions/tagged/yesod) and [haskell](http://stackoverflow.com/questions/tagged/yesod)
* Alongside #snowdrift on freenode.net, the #yesod and #haskell channels are also active and helpful
* A useful tool is "cabal repl" — a command that loads [ghci](https://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively) in a mode connected to the project. Using that, you can easily import files from the code and explore the functions.
* To help write clean Haskell code and learn conventions, run hlint on your files to get suggestions for possible improvements.
    * Given a working Haskell installation, add hlint with the command "cabal install hlint"


Text-editor settings
--------------------

We recommend setting your text editor to have the TAB key do indentation of four spaces.
For VIM, for example, the config file .vimrc should have these three lines:

    set expandtab
    set shiftwidth=4
    set tabstop=4

VIM users should also install
[Syntax Highlighting Files for Haskell](https://github.com/pbrisbin/html-template-syntax).

Emacs users should use a package manager (preferably Marmalade) to install
[Haskell Mode](https://github.com/haskell/haskell-mode).


Development guidelines and notes
================================

Overall, we strive to follow universal standards, be fully accessible, and avoid browser-specific code.

We generally build with *progressive enhancement* in mind.
Content and functions should be built with simple HTML/CSS along with Yesod/Haskell server-side functions.
JavaScript may then added for enhancement.
Consider the ideas of [Unobtrusive JavaScript](http://en.wikipedia.org/wiki/Unobtrusive_JavaScript).
Use of NoScript should never causes a broken experience.
We also make sure all our JavaScript is recognized
by the FSF's [LibreJS plugin](https://www.gnu.org/software/librejs/).

We have a separate wiki and discussion page on the site
for discussing specific [web-design issues](https://snowdrift.coop/p/snowdrift/w/site-design).
We also have a complete [list of tickets](https://snowdrift.coop/p/snowdrift/t)
with a range of front-end, back-end, and more or less technical items.
We're also working to tag thing to clarify the types of skills needed for different tasks.

Please consider adding concise comments to your code explaining to others anything you think may be unclear.
Ideally, follow the syntax for
[Haddock Haskell documentation](http://www.haskell.org/haddock/doc/html/markup.html).


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

After pushing to your online account,
alert us to the changes with Gitorious' "request merge" or GitHub's "pull request" commands on their websites.

To pull updates from our main code, use

    git pull git@gitorious.org:snowdrift/snowdrift.git

Note: if you are collaborating with others before a patch is ready to go live
land some participants use Gitorious and others use GitHub (or other hosts),
you can pull by using the full git address
and then sending your collaborator an e-mail or otherwise alerting them to pull your updates.


Building
--------

Install ghc, cabal, and postgresql, however you do that on your system.
Yesod also requires a few other items such as "happy" and "alex".

On Debian-based GNU/Linux distros, the full install command is:

    sudo apt-get install ghc cabal-install haskell-platform postgresql zlib1g-dev libpq-dev happy alex

**Note: we are now using GHC 7.8.2**
The Haskell Platform has not yet updated to it, but many systems have.
If your system is still using an older version, you may need to manually upgrade.
See <http://www.haskell.org/ghc/download>.

(There are also a few non-Haskell libraries with some dependencies which you may
need to install and which presumably will be in your system's package manager.
We don't have a full list compiled yet, but they can be picked out of the error
messages if the commands below fail. If you make a list,
please update this guide and send a pull request!)


After installing everything, update cabal's package list:

    cabal update

Add ~/.cabal/bin locations to your PATH;
for bash, edit your .bashrc (or equivalent) file and add the following line:

    export PATH=.cabal-sandbox/bin:~/.cabal/bin:$PATH

(you'll need to start a new terminal or run "source ~/.bashrc" to make the PATH active)

Now, upgrade cabal itself:

    cabal install Cabal cabal-install

**change to your snowdrift project directory (if you're not already working there).**

Initiate a cabal sandbox:

    cabal sandbox init

Install dependencies and build Snowdrift:

    cabal install

This will take a *long* time but should ultimately tell you it installed Snowdrift.
(Contact us for help if it says otherwise)

You can also use "cabal install" to update your build later, and then it will run much faster.


Setting up the database
-----------------------

*This can be done while building is in progress*

Go to the config/ directory within the project directory
and make a copy of postgresql.template and name the new file postgresql.yml

Create database user called "snowdrift_development" *without* superuser, createdb, or createuser priveleges:

    sudo -u postgres createuser -S -D -R snowdrift_development

Create snowdrift_development database:

    sudo -u postgres createdb snowdrift_development

Run postgres psql:

    sudo -u postgres psql

You should see a line that looks like:

    postgres=#

Add a password to the snowdrift_development user
(you may substitute your chosen passphrase instead of 'somepassphrase'):

    postgres=# alter user snowdrift_development with encrypted password 'somepassphrase';

Then add user to database:

    postgres=# grant all privileges on database snowdrift_development to snowdrift_development;

Leave postgres (with ctrl-D).

Edit config/postgresql.yml and update the password to match the one you entered.

Import development database:

    sudo -u postgres psql snowdrift_development <devDB.sql


Running the site
----------------

Once snowdrift is built and assuming you're using a cabal sandbox,
have set your PATH correctly, and are in your snowdrift directory,
you can start the server with the command:

    Snowdrift Development

To rebuild the site after changes to the code, run "cabal install" first before starting the server.
    
Alternately, you may use the yesod devel command which does a combined rebuild and server start.
In rare cases, yesod devel may succeed where cabal install failed (or vice versa),
but the main advantage to yesod devel is that it can be left running
and will automatically update your build after each saved change to any file.
                                             
To enable yesod devel, first install yesod-bin:

    cabal install yesod-bin

From now on, you can rebuild and start the server with:

    yesod devel

After the server starts, it may print a bunch of text about creating tables,
and it will then sit ready, waiting for connections.

For either build approach, access the server by directing your web browser to localhost:3000


Using the live test site
------------------------

You can log into the site via the built-in system with user: admin pass: admin

With that user, create wiki pages at localhost:3000/p/snowdrift/w/*pagename*/new

See the documentation [about the wiki](https://snowdrift.coop/p/snowdrift/w/wiki) for more on how that works.

That's all you need to get started!

You can now register new users, make pledges, and test and work on all aspects of the site.


Additional notes about database and testing
===========================================

Database migrations
-------------------

After changing the database schema (in config/models),
the first time you recompile and then start the server,
a migration script will be automatically generated and placed in /migrations.

The safe (i.e. guaranteed not to lose data) statements, if any,
are placed in migration/migrateN where N is the next number in sequence.

If there are no unsafe statements in the migration,
the safe statements will be run and the server will continue to start normally.

If there are any unsafe (may destroy data) statements,
they are placed in migrations/migrate.unsafe, and the server will abort.

In an unsafe case, if the data *is* intended to be lost
(e.g. destroying a column storing data we no longer want),
just copy the statements to the new migrateN file (creating it if necessary).

If you don't want to lose the data
(a column is being moved to a different table, a column is being renamed, &c),
modify the migration file as appropriate.

If you try different things in the course of testing and/or reset your database,
you might generate extra migrations.
Be sure to clear and reset any migrations once you have a final version.
Then, you can run the site once to generate the correct final migration.

Committing database updates
---------------------------

Add any valid new migrations/migrateN files to git when you commit the corresponding schema changes.

When merging migrations, always put any you've added on the end.
Don't merge them into migration files others may have already run.

Although it will run the migrations anyway, it will be efficient to also update devDB.sql, as described below.


Sharing updates to the devDB database
-------------------------------------

If you make specific improvements or additions to your database
that aren't just playing around but that you think will make for a better starting database
for other contributors (and also when you have updated the basic database with migration files),
you can use the following command to export the changes (which can then be committed via git as usual).

While in your project directory:

    sudo -u postgres pg_dump snowdrift_development >devDB.sql

Resetting your database
---------------------------

To remove any changes and reset your database to the devDB default
(such as when others have provided a new update you want to try
or to start clean before making changes you plan to commit),
follow these steps:

Start by deleting your database:

    sudo -u postgres psql <<<'drop database snowdrift_development'

Then simply re-create the database by rerunning two of the commands
from the "Setting up the database" section above.

First the "Create snowdrift database" command:

    sudo -u postgres createdb snowdrift_development
    
and then the "Import development database" command:

    sudo -u postgres psql snowdrift_development <devDB.sql

That's it. You will *not* need to re-run the database user commands.


Running tests
=============

After making various changes to the code and running locally
to verify that everything compiles and also appears to work as desired,
it is best to then run our automated tests before sharing your changes with the main project.

Setting up the test template database
-------------------------------------

To prepare for running tests,
you will need to install some extra dependencies with the command:

    cabal install --only-dependencies --enable-tests

Like setting up the original development database,
we then need to set up a database and user for testing.

Create database user *without* superuser or createrole priveleges but *with* createdb priveleges:

    sudo -u postgres createuser -S -d -R snowdrift_test

Create the snowdrift_test database *template*:

    sudo -u postgres createdb snowdrift_test_template

Run postgres psql to bring up the postgres=# prompt:

    sudo -u postgres psql

At the postgres=# prompt, mark the new database as a template:

    postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';

Then, add a password to the snowdrift_test user
(as with snowdrift_development, you may substitute your chosen passphrase):

    postgres=# alter user snowdrift_test with encrypted password 'somepassphrase';

Leave postgres (with ctrl-D).

If you used a different password than the one you used for snowdrift_development,
then edit config/postgresql.yml
and add a "password:" line under "Testing:" along with your new passphrase.

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

Then we repeat the commands above for setting up the test DB,
skipping the dependencies/user-creation/password parts (those don't need updating).
In brief:

    sudo -u postgres createdb snowdrift_test_template
    sudo -u postgres psql
    postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';
    sudo -u postgres psql snowdrift_test_template <testDB.sql

---

Happy hacking!

