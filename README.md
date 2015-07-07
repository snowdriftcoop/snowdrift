# [Snowdrift.coop]

A non-profit, cooperative platform for funding Free/Libre/Open (FLO) works.

Using a many-to-many matching pledge, we aim to empower the global
community to better promote freedom-respecting projects of all sorts.

For the basic idea, see our [illustrated intro].

Other pages on the site explain our [mission] and include discussion and
research on issues like the [economics of FLO projects], the [incentives
behind donations], and how our model departs from that of [other funding
sites].

## Contributing

Snowdrift.coop welcomes contributions of all types, from people of all
technical ability. Our [how-to-help page] includes further notes about the
site and info about volunteering (including in non-programming ways).

For bugs and feature requests, we have a self-hosted [ticket system].

Snowdrift.coop is built with the **[Haskell]** programming language, and
**[Yesod web framework]**. Even if you don't know Haskell, however, you can
still contribute front-end design or content! We welcome contributions from
developers of all skill levels.

Whatever your background, we're happy to answer questions or get any
comments. Hop on our active chat channel, [#snowdrift on Freenode IRC], and
say hello!

## Building and testing the website

### Install System Dependencies

Install [Git], [Postgres] and [Stack].

See [GUIDE.md#Building] for specifics for various operating systems.

An alternative process is to use a virtual machine via our [Vagrant setup].
Vagrant works for nearly all operating systems.

## Get the code

Since you are reading this, you surely know where to get the code already.

For reference, we primarily use a completely free/libre/open host for our code:
[git.gnu.io/snowdrift/snowdrift]. For convenience and redundancy, we also mirror
at [GitHub], a popular but proprietary platform.

The standard command to clone the code to your local system is:

    git clone https://git.gnu.io/snowdrift/snowdrift.git

### Initial Build

From within the snowdrift code directory (`cd snowdrift`) run:

    stack setup &&
    stack build cabal-install yesod-bin-1.4.11 &&
    stack test

NB: this will take a while!

### Set up the database

(This works on plainly on GNU/Linux, see [GUIDE.md#Building] for other systems)

    stack exec sdm init

### Useful Development Commands

With everything initialized, you can now use the following commands:

* `stack exec yesod devel`: run the site in development mode
    * access the site in your browser at <http://localhost:3000>
    * log in as admin with built-in system and user: `admin` pass: `admin`
    * press Enter key in terminal to stop the site
* `stack build && stack test`: run the test suite
* `stack build`: rebuild manually (usually running the site in development
  mode is sufficient, however)
* `stack ghci`: Start the REPL

Further Information
-------------------

Those with minimal technical background should follow our [Beginner's Guide],
which can get anyone started making basic contributions and also includes
educational links and other info about the tools we use.

As mentioned, the comprehensive [GUIDE.md] describes further technical details
including notes for various operating systems, manual database operations, and
more.

[Beginner's Guide]: BEGINNERS.md
[economics of FLO projects]: https://snowdrift.coop/p/snowdrift/w/en/economics
[#snowdrift on Freenode IRC]: http://webchat.freenode.net/?channels=#snowdrift
[git.gnu.io/snowdrift/snowdrift]: https://git.gnu.io/snowdrift/snowdrift
[Git]: http://www.git-scm.com/downloads
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[GUIDE.md]: GUIDE.md
[GUIDE.md#Building]: GUIDE.md#Building
[Haskell]: https://www.haskell.org/
[how-to-help page]: https://snowdrift.coop/p/snowdrift/w/how-to-help
[illustrated intro]: https://snowdrift.coop/p/snowdrift/w/en/intro
[incentives behind donations]: https://snowdrift.coop/p/snowdrift/w/en/psychology
[mission]: https://snowdrift.coop/p/snowdrift/w/en/mission
[other funding sites]: https://snowdrift.coop/p/snowdrift/w/en/othercrowdfunding
[Postgres]: http://www.postgresql.org/download/
[Snowdrift.coop]: https://snowdrift.coop
[Stack]: https://github.com/commercialhaskell/stack#how-to-install
[ticket system]: http://snowdrift.coop/p/snowdrift/t
[Vagrant setup]: SETUP_VAGRANT.md
[Yesod web framework]: http://www.yesodweb.com/
