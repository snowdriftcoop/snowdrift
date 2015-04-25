# Vagrant-based Setup for Snowdrift

## Install Vagrant

[Grab the latest version](https://www.vagrantup.com/downloads.html) of
Vagrant for your system.

## Install Git

Most systems will have git in their software repositories.  For example,
on Debian or Ubuntu you can enter the following in a terminal:

    sudo apt-get install git

If you are on a system that does not package git yet, you may choose to
[get it from the git website](https://git-scm.herokuapp.com/downloads).

## First run

Open up a terminal, and run these commands, exactly as you see them. You
could even copy and paste all of them into your terminal at once, and it
would work.

Note that you must use Ctrl+Shift+V to paste something into the terminal.
For historical reasons, Ctrl+V does something else in most terminals.

    git clone https://git.gnu.io/snowdrift/snowdrift.git
    cd snowdrift
    vagrant up
    vagrant ssh
    cd /vagrant
    cabal install -fdev
    sdm init
    Snowdrift Development

Go to http://localhost:3000 in your web browser to see the Snowdrift site.

Now you can play with Snowdrift locally.
To log into the site, use the built-in system with
user: `admin` pass: `admin`

## Workflow

When you want to work on the site, open a terminal and run the following
commands from your snowdrift directory to start the environment:

    vagrant up
    vagrant ssh
    cd /vagrant

When you want to test changes, run the following:

    cabal install --enable-tests -fdev
    Snowdrift Development

When you are done, kill the server and run:

    exit
    vagrant halt
