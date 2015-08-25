# Vagrant-based Setup for Snowdrift

Vagrant runs in a virtual machine but can integrate with tools and files on your
local computer. We offer it as a back-up option aside from installing everything
on one's main system (especially useful for contributors running Windows who are
not ready to work entirely in a regular GNU/Linux installation).

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
    cabal install
    sdm init
    cabal install --enable-tests
    yesod devel

The site should now be running on <http://localhost:3000>.

To stop the site, hit the Enter key.

To then quit vagrant, run:

    exit
    vagrant halt

To run the site again later, open a terminal and then
**from your snowdrift directory** run:

    vagrant up
    vagrant ssh
    cd /vagrant
    yesod devel


## Workflow

Most of the details in [BUILD.md](BUILD.md) and
[CONTRIBUTING.md](CONTRIBUTING.md) work the same when using Vagrant. However,
the Vagrant option currently does not use Stack. So:

Instead of `stack exec yesod devel`, just run `yesod devel`

In cases where `yesod devel` fails to detect changes,
stop it with the Enter key, then run:

    cabal clean && yesod devel

If you add new dependencies (i.e. edit the `build-depends` field in
`Snowdrift.cabal`), you will need to run:

    cabal install
