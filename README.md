snowdrift
=========

Infrastructure for [Snowdrift.coop](https://snowdrift.coop).

Code is mirrored at [GitHub](https://github.com/dlthomas/snowdrift) and [Gitorious](https://gitorious.org/snowdrift/snowdrift).

Work to do / how to help
========================

We have our own in-progress ticketing system along with tickets at GitHub,
see the [combined ticket list](http://snowdrift.coop/p/snowdrift/t) from both systems.

The infrastructure here includes the site's wiki backend,
but the contents of the wiki are held separately in the site database.

We have volunteer info and discussion forums on the site and a #snowdrift IRC channel on freenode.net

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


Building
========

Install ghc, cabal, and postgresql, however you do that on your system.

On Debian-based Linux distros, that's:

    sudo apt-get install ghc cabal-install haskell-platform postgresql zlib1g-dev libpq-dev happy alex


(There are a few non-Haskell libraries that some dependencies which you may
need to install, presumably in your system's package manager as well.
I don't have the list at hand, but they can be picked out of the error
messages when the below fails for want of them - if you make a list,
please update this and send a pull request!)

Update cabal's package list:

    cabal update

Update cabal so you can use the new [sandbox](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) feature in cabal 1.18+:

    cabal install Cabal cabal-install

Add ~/.cabal/bin to your PATH; in bash this is:

    export PATH=~/.cabal/bin:$PATH

Add this to your .bashrc or equivalent.

Change to your snowdrift directory. Initiate a cabal sandbox, which will keep your work isolated and sane if you have multiple snowdrift chekcouts, or other Haskell projects:

    cabal sandbox init

Add your sandboxed binaries to your PATH:

    export PATH=~/snowdrift/.cabal-sandbox/bin:$PATH

(You may also wish to add this to your .bashrc or equivalent.)

Install dependencies and build Snowdrift:

    cabal install

This will take a long time, but should ultimately tell you it installed Snowdrift.
(Rebuilding goes much faster with cabal build, but only if dependency information hasn't changed.)

While it goes, create a snowdrift database and user in postgresql.

Create database user:

    sudo -u postgres createuser

Answer prompts accordingly:

* add name snowdrift
* do not make super user
* do not allow role to create databases
* do not allow role to be allowed to create more new roles

Create snowdrift database:

    sudo -u postgres createdb snowdrift

Run postgres psql:

    sudo -u postgres psql

You should see a line that looks like:

    postgres=# 

Add password to user:

    postgres=# alter user snowdrift with encrypted password 'somepassword';

Then to add user to database:

    postgres=# grant all privileges on database snowdrift to snowdrift;

Edit config/postgresql.yml and update the credentials to match the user you created.

Once snowdrift is built, assuming you're using a cabal sandbox and have set your PATH correctly, you can start the server by running:

    Snowdrift Development
    
If you aren't using a cabal sandbox and/or don't have your PATH set correctly, you can always run the following from the snowdrift source directory:

    ./dist/build/Snowdrift/Snowdrift Development

You may also wish to use the yesod devel command (which will rebuild any changed files for you) to start the server.
To enable this, first install yesod-bin:

    cabal install yesod-bin
    
Then, you can start the server with:

    yesod devel

Either will print a bunch of text about creating tables, and then sit waiting for connections.  You can access it by directing your web browser to localhost:3000.

Happy hacking!
