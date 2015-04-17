Snowdrift.coop
==============

[Snowdrift.coop](https://snowdrift.coop) is a non-profit, cooperative platform
for funding Free/Libre/Open (FLO) works. Using a many-to-many matching pledge,
we aim to empower the global community to better promote freedom-respecting
projects of all sorts.

For the basic idea, see our
[illustrated intro](https://snowdrift.coop/p/snowdrift/w/en/intro).

Other pages on the site explain our
[mission](https://snowdrift.coop/p/snowdrift/w/en/mission)
and include discussion and research on issues like
the
[economics of FLO projects](https://snowdrift.coop/p/snowdrift/w/en/economics),
the
[incentives behind donations](https://snowdrift.coop/p/snowdrift/w/en/psychology),
how our model departs from that of
[other funding sites](https://snowdrift.coop/p/snowdrift/w/en/othercrowdfunding),
and more.


Contributing
===========

Our [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help)
includes further notes about the site and info about volunteering (including
in non-programming ways). We also have an in-progress, self-hosted
[ticket system](http://snowdrift.coop/p/snowdrift/t).

Snowdrift.coop is built with **Haskell** and the
**[Yesod web framework](http://www.yesodweb.com/)**,
but even if you don't yet know Haskell,
you may still put your HTML/CSS/Javascript skills to work!
We welcome contributions from developers of all skill levels.

Whatever your background, we're happy to answer questions or get any comments.
Hop on #snowdrift at
[freenode.net](http://webchat.freenode.net/?channels=#snowdrift), and say hello!


Essential build instructions
----------------------------

Currently, our code is hosted at
[GitHub](https://github.com/snowdriftcoop/snowdrift),
a popular but proprietary platform.
We plan to mirror at a platform more aligned with our values but details are
not set yet.

**Our full [guide to our code](GUIDE.md) has precise setup details
and clarifications about technical items.

Beginners with minimal technical background can get set up by following our
[Beginners' Snowdrift Set Up](BEGINNERS.md) which can get anyone started for
making basic contributions along with links and info to help learn more about
the tools we use.

For advanced programmers experienced with Git, Haskell, PostgreSQL,
and perhaps even Yesod, here's quick and dirty minimal start instructions:

```
// Install any dependencies you don't have:
// GHC **7.8.x**, cabal, PostgreSQL, happy, alex, git
// update cabal, set PATH, etc. — see GUIDE.md for more detailed instructions

// Fork, clone and install
git clone [your remote address]
cd snowdrift
cabal sandbox init
cabal install --enable-tests -fdev

// Set up the database with our quick script.
// To understand the script does or to run the commands manually, see GUIDE.md
sdm init

// Launch the development site
Snowdrift Development

// To see the live site, point your browser to localhost:3000

// To rebuild after making changes run
cabal install -fdev

Read GUIDE.md for thorough details about development, testing, and so on.
```
