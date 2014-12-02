Snowdrift.coop
==============

[Snowdrift.coop](https://snowdrift.coop) is a non-profit, cooperative platform for funding Free/Libre/Open (FLO) works. Using a many-to-many matching pledge, we aim to empower the global community to better promote freedom-respecting projects of all sorts.

For the basic idea, see our [illustrated intro](https://snowdrift.coop/p/snowdrift/w/intro).

Other pages on the site explain our [mission](https://snowdrift.coop/p/snowdrift/w/mission)
and include discussion and research on issues like
the [economics of FLO projects](https://snowdrift.coop/p/snowdrift/w/economics),
the [incentives behind donations](https://snowdrift.coop/p/snowdrift/w/psychology),
how our model departs from that of the other [600+ donation-style crowdfunding sites](https://snowdrift.coop/p/snowdrift/w/othercrowdfunding),
and more.

As a work in progress, we are still clarifying the details and adjusting our presentation.


Contributing
===========

Our [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help) includes further notes about the site and info about volunteering (including in non-programming ways).
We also have an in-progress, self-hosted [ticket system](http://snowdrift.coop/p/snowdrift/t).

Snowdrift.coop is built in **Haskell** using the **[Yesod web framework](http://www.yesodweb.com/)**,
but even if you don't know any Haskell, you may still put your HTML/CSS/Javascript skills to work!
We welcome contributions from developers of all skill levels.

Whatever your background, we're happy to answer questions or get any comments.
Hop on #snowdrift on [freenode.net](http://webchat.freenode.net/?channels=#snowdrift), and say hello!


Essential build instructions
----------------------------

Note: our code is mirrored at
[GitHub](https://github.com/dlthomas/snowdrift) (which is popular but proprietary)
and [Gitorious](https://gitorious.org/snowdrift/snowdrift) (which is FLO, licensed AGPL, but less popular).

**You really should read our full [guide to our code](GUIDE.md)
which has step-by-step instructions that even a true beginner can follow.**
It also contains links for learning Haskell, comments about our development methods, and more.

But for those experienced with Git, Haskell, PostgreSQL, and perhaps even Yesod,
here's quick and dirty minimal instructions to get started:

```
// Install any dependencies you don't have:
// GHC **7.8.3** (7.8.2 should be ok), cabal, PostgreSQL, zlib1g-dev, libpq-dev, happy, alex
// update cabal, set PATH, etc. — see GUIDE.md for more detailed instructions

// Fork, clone and install
git clone [your remote address, e.g. git@gitorious.org:snowdrift/yourusername-snowdrift.git]
cd snowdrift
cabal sandbox init
cabal install arithmoi -f-llvm
cabal install --enable-tests -j

// Set up the database with our quick script.
// To understand what the script does or to run the commands manually, see GUIDE.md
sdm init

// Launch the development site
Snowdrift Development

// To see the live site, point your browser to localhost:3000

//To rebuild after making changes run
cabal install

// For fast, automatic rebuilding whenever you change the code, install the yesod-bin package
cabal install yesod-bin
// then instead of "Snowdrift Development" run the site with
yesod devel
// which can be left running in a terminal while working elsewhere

```

Read through GUIDE.md for thorough details and more about development, testing, and so on.
