# Debian-based Setup for Snowdrift

## Installing

For Debian-based systems (including Ubuntu-based systems),
run these commands:

    sudo aptitude update
    sudo aptitude install curl git postgresql postgresql-client libgmp-dev zlib1g-dev libpq-dev
    git clone https://git.gnu.io/snowdrift/snowdrift.git
    cd snowdrift
    stack setup && stack build # && get some coffee
    stack exec sdm init
    stack exec yesod devel

The site should now be running on <http://localhost:3000>.

Now you can play with Snowdrift locally.
To log into the site, use the built-in system with
user: `admin` pass: `admin`

## Workflow

Once going, the development site can stay running in one terminal while you
do work elsewhere. It will rebuild and rerun the site whenever it detects
file changes.

To stop the site, hit the Enter key.

In cases where the development site fails to detect changes, stop it with
the Enter key, then run:

    stack clean && stack exec yesod devel

If you add new dependencies (i.e. edit the `build-depends` field in
`Snowdrift.cabal`), you will need to run:

    stack build

## More resources

See [BEGINNERS.md](BEGINNERS.md) for general info about contributing
and learning about the tools we use,
and see [GUIDE.md](GUIDE.md) for more technical details.
