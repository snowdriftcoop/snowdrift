# Debian-based Setup for Snowdrift

## Installing

Open up a terminal, and run these commands, exactly as you see them. You
could even copy and paste all of them into your terminal at once, and it
would work.

Note that you must use Ctrl+Shift+V to paste something into the terminal.
For historical reasons, Ctrl+V does something else in most terminals.

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
    Snowdrift Development

Go to http://localhost:3000 in your web browser to see the Snowdrift site.

Now you can play with Snowdrift locally.
To log into the site, use the built-in system with
user: `admin` pass: `admin`
