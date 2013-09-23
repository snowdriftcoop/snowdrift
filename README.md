snowdrift
=========

Infrastructure for [Snowdrift.coop](https://snowdrift.coop).

Code is mirrored at [GitHub](https://github.com/dlthomas/snowdrift) and [Gitorious](https://gitorious.org/snowdrift/snowdrift).

We are developing our own internal ticketing, but at this point we still have most issues ticketed at the GitHub site.

The infrastructure here includes the site's wiki backend,
but the contents of the wiki are held separately in the site database.

The live Snowdrift.coop site has public and invitation-only sections. As work continues, more will be published.
[Contact us](https://snowdrift.coop/contact) if you're interested in checking out the unpublished sections, including our overall next-steps page, or if you're interested in joining the steering committee.

We're often on the #snowdrift IRC channel on freenode; feel free to drop in!

Development guidelines and notes
================================

Overall, we strive to follow universal standards, be fully accessible, and avoid any browser-specific items.

All JavaScript should be recognized as acceptable by the FSF's [LibreJS plugin](https://www.gnu.org/software/librejs/)

**When in doubt, first make sure things work well enough without JS.**
There should not be a broken experience with NoScript.
JS is fine for amplification and beautification, but no essential function should require JS.


About the frameworks and tools we use
=====================================

The Snowdridt.coop site uses the Yesod web framework which uses the Haskell programming language alongside HTML/CSS/JavaScript elements.

We also include [Twitter Bootstrap](http://twitter.github.io/bootstrap/index.html) CSS stuff, so feel free to use any of that as appropriate (within the guidelines mentioned above).

Yesod uses a simpler, indendation-based variant of HTML/CSS/JS formats called [Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates).

Use the same HTML tags as normal, but there is no need to add the closing tag.
Instead, items within a tag are placed on new lines underneath and indented an extra four spaces.
See the link above to understand the many other shortcuts and additional features this system offers.

We recommend setting your text editor to have the TAB key do indentation of four spaces.
For VIM, for example, the config file .vimrc should have these three lines:

    set expandtab
    set shiftwidth=4
    set tabstop=4 

VIM users should also install [Syntax Highlighting Files for Haskell](https://github.com/pbrisbin/html-template-syntax).

Of course, the live site is rendered in standard HTML/CSS etc.

Try the superb [Firebug](https://getfirebug.com) tool to explore and test things on the live site.

If you are just learning Haskell or Yesod, here are some quality Free/Libre/Open resources:


* [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
* [Haskell Wiki](http://www.haskell.org/haskellwiki/Haskell)
* Stack Overflow questions tagged [yesod](http://stackoverflow.com/questions/tagged/yesod) or [haskell](http://stackoverflow.com/questions/tagged/yesod)

And some gratis resources:

* [Learn You A Haskell](http://learnyouahaskell.com/)
* [Real World Haskell](http://book.realworldhaskell.org)
* [Yesod Website](http://www.yesodweb.com/)
    * [Yesod Book](http://www.yesodweb.com/book)
    * [Yesod Wiki](https://github.com/yesodweb/yesod/wiki)
    * [Yesod Cookbook](https://github.com/yesodweb/yesod/wiki/Cookbook)
* [School of Haskell](https://www.fpcomplete.com/school)
* #yesod and #haskell IRC channels on freenode


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
