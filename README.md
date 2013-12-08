snowdrift
=========

Infrastructure for [Snowdrift.coop](https://snowdrift.coop).

Code is mirrored at [GitHub](https://github.com/dlthomas/snowdrift) (which is popular but proprietary) and [Gitorious](https://gitorious.org/snowdrift/snowdrift) (which is FLO, licensed AGPL, but less popular or fully featured).

Work to do / how to help
========================

Along with [tickets at GitHub](https://github.com/dlthomas/snowdrift/issues),
we have [our own in-progress ticketing system](http://snowdrift.coop/p/snowdrift/t) which lists all tickets from both systems.

The infrastructure here includes the site's wiki backend,
but the contents of the wiki are held separately in the site database.

We have [volunteer info](https://snowdrift.coop/p/snowdrift/w/how-to-help) and discussion forums on the site and a #snowdrift IRC channel on freenode.net

Development guidelines and notes
================================

Overall, we strive to follow universal standards, be fully accessible, and avoid browser-specific code.

All JavaScript should be recognized as acceptable by the FSF's [LibreJS plugin](https://www.gnu.org/software/librejs/)

**When in doubt, make sure things work well enough without JavaScript.**
NoScript should not cause a broken experience.
JS is fine for amplification and beautification but should not be required for essential functions.


About the frameworks and tools we use
=====================================

The Snowdrift.coop site uses the [Yesod web framework](http://www.yesodweb.com/)

Yesod uses the Haskell programming language alongside its
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates) which generate HTML/CSS/JS using indentation with no need for closing tags or bracketing.

* For learning Haskell, check out the [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell), which also has links to additional gratis resources.
* Stack Overflow user postings are FLO (CC-BY-SA), see the tags for [yesod](http://stackoverflow.com/questions/tagged/yesod) and [haskell](http://stackoverflow.com/questions/tagged/yesod)
* The #yesod and #haskell IRC channels on freenode.net are active and helpful
* [School of Haskell](https://www.fpcomplete.com/school) is an interactive system that is proprietary but gratis

We also include [Twitter Bootstrap](http://twitter.github.io/bootstrap/index.html) CSS stuff, although we use our own custom CSS for many things.

[Firebug](https://getfirebug.com) is a useful tool to explore and test things on the live site


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

*note: there have been some errors reported with older versions of ghc and the haskell-platform* At this time, we are using GHC 7.6.3 and Haskell Platform 2013.2.0.0 — both are included in the latest Ubuntu, but there are [instructions for building updated GHC on older Ubuntu systems](https://gist.github.com/Dexyne/5791465). We have tested this as working with Ubuntu 12.04 LTS. These instructions or similar should work for other systems as well, but see <http://www.haskell.org/platform/> for more general info for all systems.

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

*After the initial build, you will not need to go through all of this again unless dependencies have changed.*

To rebuild the site in the future (i.e. to test your changes), use "cabal build"


Setting up the database
-----------------------

*This can be done while building is in progress*

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

Add password to user (substitute your chosen passphrase instead of 'somepassphrase':

    postgres=# alter user snowdrift_development with encrypted password 'somepassphrase';

Then to add user to database:

    postgres=# grant all privileges on database snowdrift_development to snowdrift_development;

Edit config/postgresql.yml and update the password to match the one you chose.

Import development database:

    sudo -u postgres psql snowdrift_development <devDB.sql


Running the site
----------------

Once snowdrift is built, assuming you're using a cabal sandbox and have set your PATH correctly, you can start the server by running:

    Snowdrift Development

If you aren't using a cabal sandbox and/or don't have your PATH set correctly, you can always run the following from the snowdrift source directory:

    ./dist/build/Snowdrift/Snowdrift Development

You may also wish to use the yesod devel command (which will rebuild any changed files for you) to start the server.
To enable this, first install yesod-bin:

    cabal install yesod-bin

Then, you can start the server with:

    yesod devel

Either method for starting the server will print a bunch of text about creating tables, and then sit waiting for connections.

Access the server by directing your web browser to localhost:3000


Using the live test site
------------------------

You can log into the site via the built-in system with user: admin pass: admin

With that user, create wiki pages at localhost:3000/p/snowdrift/w/*pagename*/new

See the documentation on the live site [about the wiki](https://snowdrift.coop/p/snowdrift/w/wiki) and more.

Happy hacking!
