Snowdrift.coop
==============

[Snowdrift.coop](https://snowdrift.coop) is non-profit, cooperative platform for funding Free/Libre/Open (FLO) works. Please see our [illustrated introduction](https://snowdrift.coop/p/snowdrift/w/intro) for more information about what distinguishes Snowdrift from [over 500 other donation-style crowdfunding sites](https://snowdrift.coop/p/snowdrift/w/othercrowdfunding).

More information
================

Please leaf through our [self-hosted wiki](https://snowdrift.coop/p/snowdrift/w) for more about us, including our [mission statement](https://snowdrift.coop/p/snowdrift/w/mission) and [background research](https://snowdrift.coop/p/snowdrift/w/economics) into the economics and incentives behind donations.

We also hang out in #snowdrift on [freenode.net](http://webchat.freenode.net/), so feel free to stop by!

Conributing
===========

We welcome contributions from developers of all skill levels. Snowdrift.coop is built entirely in Haskell using the [Yesod](http://www.yesodweb.com/) web framework, but even if you don't know any Haskell, you may still put your HTML/CSS/Javascript skills to work!

If you'd like to support the project but have no idea how to get started, please see our [beginners' guide](http://www.example.com), hop on #snowdrift on [freenode.net](http://webchat.freenode.net/), and say hello!

If you're experienced with Git, Haskell, PostgreSQL, and perhaps even Yesod, here's a quick-and-dirty init script:

```
// Fork, clone and install
git clone git@github.com:my-forked-repo/snowdrift.git
cd snowdrift
cabal sandbox init
cabal install --only-dependencies --enable-tests
cp config/postgresql.template config/postgresql.yml

// Set up the database
sudo -u postgres createuser -S -D -R snowdrift_development
sudo -u postgres createuser -S -d -R snowdrift_test
sudo -u postgres createdb snowdrift_test_template
sudo -u postgres psql
postgres=# alter user snowdrift_development with encrypted password 'snowdrift';
postgres=# alter user snowdrift_test with encrypted password 'snowdrift';
postgres=# grant all privileges on database snowdrift_development to snowdrift_development;
postgres=# update pg_database set datistemplate=true where datname='snowdrift_test_template';
vi config/postgres.yml // set 'password' to 'snowdrift' as above
sudo -u postgres psql snowdrift_development <devDB.sql
sudo -u postgres psql snowdrift_test_template <testDB.sql

// Hack away
yesod devel
```
