snowdrift
=========

Infrastructure for Snowdrift.coop

Note: code is mirrored at both:
[GitHub](https://github.com/dlthomas/snowdrift)
and
[Gitorious](https://gitorious.org/snowdrift/snowdrift)

At this point, we're using the Issues tickets at GitHub (although we are developing our own internal ticketing which will be ready soon)
but we appreciate that Gitorious is itself FLOSS whereas GitHub is propriety.

The infrastructure here includes the wiki system for the site,
but the contents of the wiki are held in the site database.

The Snowdrift.coop site itself is invitation-only as it is not ready for public presentation yet, but contact us if you're interested in checking it out or joining the steering committee to work on the proposals and details.


building
========

Install ghc, cabal, and postgresql, however you do that on your system.

On Debian-based Linux distros, that's:

    sudo apt-get install ghc cabal-install haskell-platform postgresql zlib1g-dev libpq-dev


(There are a few non-Haskell libraries that some dependencies which you may
need to install, presumably in your system's package manager as well.
I don't have the list at hand, but they can be picked out of the error
messages when the below fails for want of them - if you make a list,
please update this and send a pull request!)

* If you have multiple Haskell projects you'll be working on, you'll
    want to use cabal-dev; that'd involve some tweaks starting from here.
    If you'll only be hacking at Snowdrift, using cabal will be simpler.


Update cabal's package list:

    cabal update

Install happy:

    cabal install happy


Add ~/.cabal to your PATH; in bash this is:

    PATH=~/.cabal:$PATH

You may well want to add this to your .bashrc or equivalent.


Change to the snowdrift directory.

Install dependencies and build Snowdrift:

    cabal install

It will take a long time, but should ultimately tell you it installed Snowdrift.
(Rebuilding goes much faster with cabal build, but only if dependency information hasn't changed.)

While it goes, create a snowdrift database and user in postgresql:

create database user

	sudo -u postgres createuser

add name snowdrift
do not make super user
do not allow role to create databases
do not allow role to be allowed to create more new roles

create snowdrift database

	sudo -u postgres createdb snowdrift

run postgrespsql

	sudo -u postgres psql

you should see a line that looks like

	postgres=# 

add password to user. 

	postgres=# alter user snowdrift with encrypted password 'somepassword';

then to add user to database

	postgres=# grant all privileges on database snowdrift to snowdrift;

Edit config/postgresql.yml and update the credentials to match the user you created.

Once snowdrift is built, you can start the server by running the following from the snowdrift source directory:

    ./dist/build/Snowdrift/Snowdrift Development

It will print a bunch of text about creating tables, and then sit waiting for connections.  You can access it by directing your web browser to localhost:3000.



development guidelines
======================

All JavaScript should be recognized as acceptable by the FSF's [LibreJS plugin](https://www.gnu.org/software/librejs/)

No JavaScript should be used in a way that results in a broken experience with NoScript.
In other words, JS is fine for all amplification and beautification of the site, but no function should be impossible and no page should look clearly off or broken without the JS.
When in doubt, make sure things work well enough without JS. Otherwise, JS is welcome where it helps improve the site experience.

Happy hacking!
