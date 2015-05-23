# Debian-based Setup for Snowdrift

## Installing

For Debian-based systems (including Ubuntu-based systems),
run these commands:

    sudo aptitude update
    sudo aptitude install curl git postgresql postgresql-client libgmp-dev zlib1g-dev libpq-dev
    mkdir builds
    cd builds
    curl -ssL \
      https://www.haskell.org/ghc/dist/7.10.1/ghc-7.10.1-x86_64-unknown-linux-deb7.tar.xz |
      tar xJv
    cd ghc-7.10.1
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
    ln -s cabal.config.7.10 cabal.config
    cabal install -fdev
    sdm init
    cabal install -fdev --enable-tests
    yesod devel

Go to http://localhost:3000 in your web browser to see the Snowdrift site.

Now you can play with Snowdrift locally.
To log into the site, use the built-in system with
user: `admin` pass: `admin`

## Workflow

`yesod devel` can stay running in one terminal while work is done elsewhere.
It will automatically rebuild and rerun the site whenever it detects changes.

In rare cases, you may need to run `cabal clean` if yesod devel
fails to recognize a change.

To stop yesod devel, press ENTER a few times.

## More resources

See [BEGINNERS.md](BEGINNERS.md) for general info about contributing
and learning about the tools we use,
and see [GUIDE.md](GUIDE.md) for more technical details.
