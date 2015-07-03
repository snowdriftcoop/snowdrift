# Arch Linux setup for developing Snowdrift.coop

If you have any issues or questions, please raise them in the
[`#snowdrift` IRC channel on FreeNode][3], or email the author of this
guide at `peter@harpending.org`. The author goes by `pharpend` in the
IRC channel.

For a more involved tutorial, please see [GUIDE.md] or [BEGINNERS.md].

## Installation

Stack is written in Haskell, so the first thing you need to do is
install the Haskell components, as well as Git (the revision control
system) and PostgreSQL (the database server software):

    # pacman -S ghc cabal-install alex happy haddock git postgresql

As a normal user, become the PostgreSQL user with

    $ su - postgres

As the `postgres` user,
    
    $ initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'

As root:

    # systemctl start postgresql.service
    # systemctl enable postgresql.service

If the database is on a Btrfs file system, you should disable
copy-on-write for the directory before creating a database. For more
details, see the [PostgreSQL page on the Arch Wiki][2].

Then, install [`haskell-stack`][1] from the AUR.

If you're already familiar with the Haskell tool chain, but not with
Stack, Stack is a re-implementation of cabal-the-tool. Stack is much
more intelligent, and "just works", to a certain extent (at least, to a
much greater extent than cabal-the-tool).

Once you've done all that, run these commands as a normal user:

    $ git clone https://git.gnu.io/snowdrift/snowdrift.git
    $ cd snowdrift
    $ stack setup
    $ stack build
    $ stack exec sdm init
    $ stack build yesod-bin-1.4.11 .
    $ stack exec yesod devel

You can then see a copy of the Snowdrift site on
<http://localhost:3000>.

## Contributing

If you don't know Haskell, that's fine. You don't need to know Haskell
to make simple contributions. You should at least have some familiarity
with using Git, the version control system. If not, please read
[Chapter 2 of the Git book][4].

You should roughly follow the [Erlang OTP git commit guide][5] for
writing commit messages. You should also attempt to follow the
[Snowdrift.coop code style guide][6] when writing code.

For simple contributions (i.e. without knowing Haskell), you would want
to restrict your editing to the files in the `templates/`
directory.

`yesod devel` should automatically recompile the site whenever you edit
one of the templates. If it doesn't, kill `yesod devel`, and run `stack
build && stack exec -- yesod devel` again.

## Further resources

* [The Git book](https://git-scm.com/book/en/v2), for learning Git.
* [The Haskell WikiBook](https://en.wikibooks.org/wiki/Haskell), an
  absolute beginner's guide to Haskell.
* [CIS 194](https://www.seas.upenn.edu/~cis194/), a guide to Haskell
  suited for somewhat experienced programmers.
* [The Yesod book](http://www.yesodweb.com/book), for learning the
  intricacies of the Yesod framework.

[1]: https://aur.archlinux.org/packages/haskell-stack/
[2]: https://wiki.archlinux.org/index.php/Postgresql
[3]: https://webchat.freenode.net/?channels=snowdrift
[4]: https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository
[5]: https://github.com/erlang/otp/wiki/Writing-good-commit-messages
[6]: https://snowdrift.coop/p/snowdrift/w/en/coding#code-style-guide
