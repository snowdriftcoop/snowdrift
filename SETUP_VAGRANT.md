# Vagrant-based Setup for Snowdrift

## Install Vagrant

[Grab the latest Vagrant version](https://www.vagrantup.com/downloads.html)
for your system.

If you do not have a virtual machine program installed yet,
[install VirtualBox](https://www.virtualbox.org/wiki/Downloads).

## Install and run Snowdrift

    git clone https://git.gnu.io/snowdrift/snowdrift.git
    cd snowdrift
    vagrant up
    vagrant ssh
    cd /vagrant
    ln -s cabal.config.7.8 cabal.config
    cabal install -fdev
    sdm init
    cabal install --enable-tests -fdev
    yesod devel

Go to http://localhost:3000 in your web browser to see the Snowdrift site.

Now you can play with Snowdrift locally.
To log into the site, use the built-in system with
user: `admin` pass: `admin`

To stop the site, use Ctrl+C
and to quit vagrant, run:

    exit
    vagrant halt


## Workflow

After your initial setup, when you want to later work on the site,
open a terminal and run the following commands
**from your snowdrift directory**:

    vagrant up
    vagrant ssh
    cd /vagrant
    yesod devel

`yesod devel` can stay running in one terminal while work is done elsewhere.
It will automatically rebuild and rerun the site whenever it detects changes.

In rare cases, you may need to run `cabal clean` if yesod devel
fails to recognize a change.

To stop yesod devel, press ENTER a few times.

As stated above, to fully quit vagrant, run:

    exit
    vagrant halt

## More resources

See [BEGINNERS.md](BEGINNERS.md) for general info about contributing
and learning about the tools we use,
and see [GUIDE.md](GUIDE.md) for more technical details.
