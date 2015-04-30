# Debian-based Setup for Snowdrift

## Installing

For Debian-based systems (including Ubuntu-based systems),
run these commands:

    sudo aptitude update
    sudo aptitude install curl git postgresql postgresql-client libgmp-dev zlib1g-dev libpq-dev
    mkdir builds
    cd builds
    curl -ssL \
      https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz |
      tar xJv
    cd ghc-7.8.4
    ./configure && make && sudo make install
    cd ..
    curl -ssL \
      https://www.haskell.org/cabal/release/cabal-install-1.22.2.0/cabal-install-1.22.2.0.tar.gz |
      tar xzv
    cd cabal-install-1.22.2.0
    sudo ./bootstrap.sh
    cd ..
    cabal update
    cabal install alex happy haddock yesod-bin
    echo 'export PATH=$PATH:$HOME/cabal/bin:.cabal-sandbox/bin' >> ~/.bashrc
    . ~/.bashrc
    git clone https://git.gnu.io/snowdrift/snowdrift.git
    cd snowdrift
    cabal sandbox init
    cabal install -fdev
    sdm init
    cabal install -fdev --enable-tests
    Snowdrift Development

Go to http://localhost:3000 in your web browser to see the Snowdrift site.

Now you can play with Snowdrift locally.
To log into the site, use the built-in system with
user: `admin` pass: `admin`

Ctrl+C will stop the server.

## Workflow

To work on the site from now on,
Go to the snowdrift directory and run `yesod devel`.
That can be left running and will automatically rebuild
and rerun the site whenever it detects file changes.

To quit yesod devel, hit ENTER a few times.

In rare cases, you may need to touch certain files
or run `cabal install -fdev` if yesod devel fails to recognize a change.

## More resources

See [BEGINNERS.md](BEGINNERS.md) for general info about contributing
and learning about the tools we use,
and see [GUIDE.md](GUIDE.md) for more technical details.
