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

Snowdrift.coop is built with **[Haskell]** and the **[Yesod web
framework]**. Even if you don't know Haskell, however, you can still
contribute HTML/CSS/Javascript work! We welcome contributions from
developers of all skill levels.

Whatever your background, we're happy to answer questions or get any
comments. Hop on #snowdrift on [Freenode IRC], and say hello!

## Where to Get the Code

If you're reading this, you probably already have it. :)

The canonical location for our code is [git.gnu.io/snowdrift/snowdrift],
which is hosted on a completely free/libre/open system. For convenience and
redundancy, we also mirror at [GitHub], a popular but proprietary platform.

## Building and Testing the Website

Here we provide a quick overview. In-depth documentation is also available,
and will be listed later.

1. Install system dependencies: [Git], [Postgres] and [stack]
2. Build the project
3. Set up the test and development databases
4. Run the site in development-mode

An alternative process is to use a virtual machine, via our [Vagrant setup].
Vagrant works for nearly all operating systems.

### Installing System Dependencies

Installing Postgres and stack depends heavily on your operating system.
Consult their documentation listed above, or read further instructions in
[Guide.md].

### Building the Project

To get started, run `stack setup` followed by `stack build`. Do this from
within the Snowdrift code directory. Both of these steps will take a while
the first time.

Afterwards, `stack build` is all that is needed to rebuild the project.

### Setting Up and Running Snowdrift

To set up the database, run `stack exec -- sdm init`.

Now you can do the following actions:

* `stack exec -- yesod devel`: run the site in development mode
* `stack test`: run the test suite
* `stack build`: rebuild manually (usually running the site in development
  mode is sufficient, however)
* `stack ghci`: Start the REPL

Further Information
-------------------

Beginners with minimal technical background should follow our [Beginners'
Snowdrift Set Up], which can get anyone started making basic contributions.
It also includes links and info to help learn more about the tools we use.

As mentioned, there is a comprehensive [GUIDE.md] that describes the
particular steps for getting started with each supported operating system.

[Beginners' Snowdrift Set Up]: BEGINNERS.md
[Debian/Ubuntu]: SETUP_DEBIAN.md
[economics of FLO projects]: https://snowdrift.coop/p/snowdrift/w/en/economics
[Freenode IRC]: http://webchat.freenode.net/?channels=#snowdrift
[git.gnu.io/snowdrift/snowdrift]: https://git.gnu.io/snowdrift/snowdrift
[Git]: http://www.git-scm.com/downloads
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[GUIDE.md]: GUIDE.md
[Haskell]: https://www.haskell.org/
[how-to-help page]: https://snowdrift.coop/p/snowdrift/w/how-to-help
[illustrated intro]: https://snowdrift.coop/p/snowdrift/w/en/intro
[incentives behind donations]: https://snowdrift.coop/p/snowdrift/w/en/psychology
[mission]: https://snowdrift.coop/p/snowdrift/w/en/mission
[other funding sites]: https://snowdrift.coop/p/snowdrift/w/en/othercrowdfunding
[Postgres]: http://www.postgresql.org/download/
[Snowdrift.coop]: https://snowdrift.coop
[stack]: https://github.com/commercialhaskell/stack#how-to-install
[ticket system]: http://snowdrift.coop/p/snowdrift/t
[Vagrant setup]: SETUP_VAGRANT.md
[Windows]: SETUP_WINDOWS.md
[Yesod web framework]: http://www.yesodweb.com/
