snowdrift
=========

Infrastructure for [Snowdrift.coop](https://snowdrift.coop).

Code is mirrored at [GitHub](https://github.com/dlthomas/snowdrift) (which is popular but proprietary) and [Gitorious](https://gitorious.org/snowdrift/snowdrift) (which is FLO, licensed AGPL, but less popular).

See the main [Git documentation](http://git-scm.com/documentation) if you are new to Git and need to learn the basics. You only need rudimentary understanding to start contributing to our code.


Work to do / how to help
========================

On the live site, we have [volunteer info](https://snowdrift.coop/p/snowdrift/w/how-to-help) which includes a general guide around the site. The site wiki and discussion system is our main organizational tool, but our #snowdrift IRC channel on freenode.net is a good place to get help or ask casual questions.

We have [our own in-progress ticketing system](http://snowdrift.coop/p/snowdrift/t) which lists most of the specific tasks of all sorts, organized with tags indicating priority, skills involved, and more.

This README assumes a basic understanding of command-line operations. If you need help with that or *any other* issues, let us know and we'll be happy to guide you!
                                                                                                                                        
                                                                                                                                        
Development guidelines and notes
================================

Overall, we strive to follow universal standards, be fully accessible, and avoid browser-specific code.

We generally build with *progressive enhancement* in mind.
Content and functions should be built with simple HTML/CSS along with Yesod/Haskell server-side functions.
JavaScript may then added for enhancement. Consider the ideas of [Unobtrusive JavaScript](http://en.wikipedia.org/wiki/Unobtrusive_JavaScript).
Use of NoScript should never causes a broken experience.
We also make sure all our JavaScript is recognized by the FSF's [LibreJS plugin](https://www.gnu.org/software/librejs/).

We have a separate wiki and discussion page on the site for discussing specific [web-design issues](https://snowdrift.coop/p/snowdrift/w/site-design).


About the frameworks and tools we use
=====================================

The Snowdrift.coop site uses the [Yesod web framework](http://www.yesodweb.com/).

Yesod uses the Haskell programming language alongside its
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates) which generate HTML/CSS/JS using indentation with no need for closing tags or bracketing.

HTML, CSS, and JavaScript work on the site may be done without knowing Haskell, but because everything is integrated, some familiarity with the Haskell syntax is helpful.

* For learning Haskell, check out the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell), which also has links to additional resources.
* Stack Overflow user postings are FLO (CC-BY-SA), see the tags for [yesod](http://stackoverflow.com/questions/tagged/yesod) and [haskell](http://stackoverflow.com/questions/tagged/yesod)
* The #yesod and #haskell IRC channels on freenode.net are active and helpful

Our front-end design uses **[Twitter Bootstrap](http://getbootstrap.com/)** for layout and styles, although there are cases where we use our own custom CSS.

Note: Bootstrap uses a 12-column layout, and some column specification is always required. So, our internal convention is to use .col-xs-12 as the default code. This default will generate full-width for all screen sizes, and then when we want to specify a particular thought-out decision about rendering on larger screens (including potentially staying full-width), we add classes for the other screen sizes (sm, md, lg) as appropriate. Otherwise, see the Bootstrap docs at the link above.

On the site we have a page about [site-design](https://snowdrift.coop/p/snowdrift/w/site-design), and the tickets and discussion there cover specific issues.

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


Working on the code
===================

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


Building
--------

Install ghc, cabal, and postgresql, however you do that on your system.

On Debian-based GNU/Linux distros, that's:

    sudo apt-get install ghc cabal-install haskell-platform postgresql zlib1g-dev libpq-dev happy alex

(There are a few non-Haskell libraries that some dependencies which you may
need to install, presumably in your system's package manager as well.
I don't have the list at hand, but they can be picked out of the error
messages when the below fails for want of them - if you make a list,
please update this and send a pull request!)

   *note: there have been some errors reported with older versions of ghc and the haskell-platform* At this time, we are using GHC 7.6.3 and Haskell Platform 2013.2.0.0 — both are included in the latest Ubuntu, and there are [instructions for building updated GHC on older Ubuntu-based systems](https://gist.github.com/Dexyne/5791465). We tested this with Ubuntu 12.04 LTS and it should work on derivatives as well (such as the fully-FLO Trisquel 6). These instructions or similar should work for other systems as well, but see <http://www.haskell.org/platform/> for more general info.

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

Add your sandboxed binaries to your PATH: (For the future, you may also wish to add this to your .bashrc or equivalent.)  
*note: replace "snowdrift" in the command with whatever the name of your directory.
GitHub clones are normally just "snowdrift" but Gitorious clones are "username-snowdrift"*

    export PATH=~/snowdrift/.cabal-sandbox/bin:$PATH

Install dependencies and build Snowdrift:

    cabal install

This will take a *long* time but should ultimately tell you it installed Snowdrift.

You will also use cabal install to test your changes later, and then it will be much faster.

Setting up the database
-----------------------

*This can be done while building is in progress*

Go to the config/ directory within the project directory and make a copy of postgresql.template and name the new file postgresql.yml

Create database user:

    sudo -u postgres createuser

Answer prompts accordingly:

* add name snowdrift_development
* do not make super user
* do not allow role to create databases
* do not allow role to be allowed to create more new roles

Create snowdrift database:

    sudo -u postgres createdb snowdrift_development

Run postgres psql:

    sudo -u postgres psql

You should see a line that looks like:

    postgres=#

Add password to user (you may substitute your chosen passphrase instead of 'somepassphrase'):

    postgres=# alter user snowdrift_development with encrypted password 'somepassphrase';

Then to add user to database:

    postgres=# grant all privileges on database snowdrift_development to snowdrift_development;

Leave postgres (with ctrl-D), then edit config/postgresql.yml and update the password to match the one you entered.

Import development database:

    sudo -u postgres psql snowdrift_development <devDB.sql


Running the site
----------------

Once snowdrift is built, assuming you're using a cabal sandbox and have set your PATH correctly, make sure you are in your project directory, then start the server by running:

    Snowdrift Development

If you aren't using a cabal sandbox and/or don't have your PATH set correctly, you can always run the following from the snowdrift source directory:

    ./dist/build/Snowdrift/Snowdrift Development

To rebuild the site after changes to the code, run cabal install first before starting the server.
    
Alternately, you may use the yesod devel command which does a combined rebuild and server start.
In rare cases, yesod devel may succeed where cabal install failed (or vice versa), but the main advantage to yesod devel is that it can be left running and will automatically update your build after each saved change.
                                             
To enable this, first install yesod-bin:

    cabal install yesod-bin

Then, you can rebuild and start the server with:

    yesod devel

After the server starts, it may print a bunch of text about creating tables, and it will then sit ready, waiting for connections.

For either build approach, access the server by directing your web browser to localhost:3000


Using the live test site
------------------------

You can log into the site via the built-in system with user: admin pass: admin

With that user, create wiki pages at localhost:3000/p/snowdrift/w/*pagename*/new

See the documentation on the live site [about the wiki](https://snowdrift.coop/p/snowdrift/w/wiki) and more.
    
    
Database migrations
-------------------

When you first start the server (following a compile) after changing the database schema (in config/models) a migration script will be automatically generated and placed in migrations.

The safe (i.e. guaranteed not to lose data) statements, if any, are placed in migration/migrateN where N is the next number in sequence.

If there are no unsafe statements in the migration, the safe statements will be run and the server will continue to start normally.

If there are any unsafe (may destroy data) statements, they are placed in migrations/migrate.unsafe, and the server will abort.

In this case, if the data *is* intended to be lost (e.g. destroying a column storing data we no longer want) just copy the statements to the new migrateN file (creating it if necessary).

If you don't want to lose the data (a column is being moved to a different table, a column is being renamed, &c) modify the migration file as appropriate.

In any event, be sure to add the new migrations/migrateN file to git when you commit the corresponding schema changes, and update devDB.sql to match.

When merging migrations, always put any you've added on the end - don't merge them into migration files others have probably already run.


Updating the test devDB database
--------------------------------

If you make specific improvements or additions to your test DB that aren't just playing around but that you think will make for a better starting test DB for other contributors, use the following command in your main project directory to export the changes (which can then be committed via git as usual):

    sudo -u postgres pg_dump snowdrift_development >devDB.sql

Resetting the test database
---------------------------

If you want to remove your test changes and reset your database to the devDB default, first delete your database with the following command:

    sudo -u postgres psql <<<'drop database snowdrift_development'

To then re-create the database, simply rerun two of the commands from the "Setting up the database" section above.
First the "Create snowdrift database" command:

    sudo -u postgres createdb snowdrift_development
    
and then the "Import development database" command:

    sudo -u postgres psql snowdrift_development <devDB.sql

That's it. You will *not* need to re-run the database user commands.

---

Happy hacking!
