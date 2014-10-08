Snowdrift.coop
==============

[Snowdrift.coop](https://snowdrift.coop) is a non-profit, cooperative platform for funding Free/Libre/Open (FLO) works. Using a many-to-many matching pledge, we aim to empower the global community to better promote freedom-respecting projects of all sorts.

For the basic idea, see our [illustrated introduction](https://snowdrift.coop/p/snowdrift/w/intro).

Other pages on the site explain our [mission](https://snowdrift.coop/p/snowdrift/w/mission) and include discussion and research on issues like the [economics of FLO projects](https://snowdrift.coop/p/snowdrift/w/economics), the [incentives behind donations](https://snowdrift.coop/p/snowdrift/w/psychology), and how our model departs from that of the other [600+ donation-style crowdfunding sites](https://snowdrift.coop/p/snowdrift/w/othercrowdfunding).

As a work in progress, we are still clarifying the details and adjusting our presentation.


Contributing
===========

Our [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help) includes further notes about the site and info about volunteering (including in non-programming ways). We also have an in-progress, self-hosted [ticket system](http://snowdrift.coop/p/snowdrift/t).

Snowdrift.coop is built in **Haskell** using the **[Yesod web framework](http://www.yesodweb.com/)**,
but even if you don't know any Haskell, you may still put your HTML/CSS/Javascript skills to work!
We welcome contributions from developers of all skill levels.

Whatever your background, we're happy to answer questions or get any comments. Hop on #snowdrift on [freenode.net](http://webchat.freenode.net/?channels=#snowdrift), and say hello!


Essential build instructions
----------------------------

Note: our code is mirrored at [GitHub](https://github.com/dlthomas/snowdrift) (which is popular but proprietary) and [Gitorious](https://gitorious.org/snowdrift/snowdrift) (which is FLO, licensed AGPL, but less popular).

**We have a full [guide to our code](GUIDE.md) with step-by-step instructions that even a true beginner can follow.**
It also contains links for learning Haskell, comments about our development methods, and more.

For those experienced with Git, Haskell, PostgreSQL, and perhaps even Yesod,
here's a quick-and-dirty init script:

```
// Install any dependencies you don't have:
// GHC **7.8.2**, cabal, PostgreSQL, zlib1g-dev, libpq-dev, happy, alex, llvm
// update cabal, set PATH, etc. — see GUIDE.md for more detailed instructions

// Fork, clone and install
git clone [your remote address, e.g. git@gitorious.org:snowdrift/yourusername-snowdrift.git]
cd snowdrift
cabal sandbox init
cabal install --only-dependencies --enable-tests
cp config/postgresql.template config/postgresql.yml

// Set up the database
sudo -u postgres createuser -S -D -R snowdrift_development
sudo -u postgres createdb snowdrift_development
sudo -u postgres createuser -S -d -R snowdrift_test
sudo -u postgres createdb snowdrift_test_template
sudo -u postgres psql
// use whatever passphrase you prefer instead of YOURPASSPHRASE
postgres=# alter user snowdrift_development with encrypted password 'YOURPASSPHRASE'; 
postgres=# alter user snowdrift_test with encrypted password 'YOURPASSPHRASE';
postgres=# grant all privileges on database snowdrift_development to snowdrift_development;
postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';
vi config/postgresql.yml // set 'password' to 'YOURPASSPHRASE', same as you chose above
sudo -u postgres psql snowdrift_development <devDB.sql
sudo -u postgres psql snowdrift_test_template <testDB.sql

// Hack away
cabal install yesod-bin
yesod devel
```
