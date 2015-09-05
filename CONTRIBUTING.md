# Snowdrift Beginning Contributor's Guide

This is a tutorial on contributing to Snowdrift. You don't have to be tech
savvy, but it helps. This guide will cover pretty much all you need to do to get
Snowdrift running locally and to start making and submitting updates.

Although we recommend deeper study to understand how all of this works,
you *can* simply follow the steps here to get started effectively.

This guide is written to work to work for even novice programmers.
Advanced readers may adapt these instructions as they see fit.

## Prerequisites

To get started, the only absolute requirements are that you have a reasonably
new laptop or desktop computer system (less than 10 years old generally),
and know how to use a terminal to enter commands in the command-line.

Generally, everything works smoothly with GNU/Linux, \*BSD systems, and OS X.

We do not fully support Windows at this time, so we generally suggest Windows
users switch systems or run a virtual machine. As proprietary systems, Windows
and OS X go against the Snowdrift.coop mission. Still, we would prefer the
options to work rather than not. Our [Build guide](BUILD.md) has instructions
for testing Windows as far as we've figured out so far.

## Command-line basics

In general, when you see a list of commands to enter, we recommend entering them
in one-at-a-time exactly as you see them. However, it should work to copy and
paste a collection of commands all at once.

Note that, in most cases, you must use Ctrl-Shift-V to paste into the terminal.
For historical reasons, Ctrl-V does something else in most terminals.

We also highly recommend use of tab-completion. See the wikibook "A Quick
Introduction to Unix" in the resources at the end of this file for more
command-line basics.

## Installing

### Install Git

If you don't have Git already, install it now.

Most systems will have Git in their software repositories.
For example, on Debian or Ubuntu you can enter the following in a terminal:

    sudo apt-get install git

If you are on a system that does not package it yet, you may choose to
[download Git from the website](https://git-scm.herokuapp.com/downloads).

### Install Snowdrift

With Git installed, you can **follow the [BUILD.md](BUILD.me) instructions**
to get Snowdrift going on your computer.

## Working on the code

**All the following assumes you have done the initial clone and install
according to the instructions linked above.**

### Text-editors and settings

All systems should come with a code-appropriate text-editor (or two), e.g.
Gedit, Kate, Pluma, Scratch, etc. Any of those will work for now, but we
recommend installing an editor with stronger Haskell and Yesod support. See
[TEXTEDITORS.md](TEXTEDITORS.md) for our specific recommendations and settings.

### Working with Git

Short version for experienced Git users:

**We suggest keeping your local master matched to the main project master.
Do your work *only* on other git branches.
Use as many branches as needed to separate all work that functions
independently** (you can, of course, remove merged branches later).

The following covers the bare minimum process for those new to Git.

#### Basic Git setup for collaboration

##### Forking at the host platform

We collaborate on the code via the free/libre/open site
[git.gnu.io](https://git.gnu.io/snowdrift/snowdrift).
We also mirror on the popular but proprietary site
[GitHub](https://github.com/snowdriftcoop/snowdrift),
which you may use if you already have an account there.
We encourage everyone to use free/libre/open tools,
so the instructions below assume git.gnu.io.

To contribute changes to the project, first
[create an account on git.gnu.io](https://git.gnu.io/users/sign_in)
(or sign in if you already have an account).

Once signed in, go to <https://git.gnu.io/snowdrift/snowdrift>,
and click the "Fork" link on the top right.

After choosing your account for the fork, you should be at the page
for your fork of the project. To check, see that the header
at the top of the page has has your account name followed by "/Snowdrift".

##### Adding your fork as a remote

At the top of the main page, below the header,
you'll see a box with an address.
By default, the SSH option is selected, and we recommend SSH ideally.
However, [SSH setup](https://git.gnu.io/help/ssh/README)
is kind of tricky, especially for those new to SSH.
If you want to stick with the easier and faster option for now,
click "HTTPS" and use that address which will look like:
"https://git.gnu.io/YOURNAME/snowdrift.git"

Where `YOURNAME` is your git.gnu.io username.

Copy that address to your clipboard.

In your snowdrift directory, paste the address into your terminal
as part of the following command:

    git remote add my-snow https://git.gnu.io/YOURNAME/snowdrift.git

Finally, run these additional Git setup commands, replace `YOUR NAME
GOES HERE` and `YOUR EMAIL GOES HERE` with your actual name and email.

    git config --global user.name "YOUR NAME GOES HERE"
    git config --global user.email "YOUR EMAIL GOES HERE"
    git config --global core.editor "nano"

(if you know you prefer a different text editor, use that in place of 'nano')

#### Updating your local code to snowdrift master

Whenever you begin new work, you should generally start with the latest
master code from the Snowdrift project.

The following assumes you originally cloned the code from one of our main hosts
(as described in the BUILD guide), so you will have the main snowdrift code
as your "origin".

To download the latest updates of the snowdrift code:

* Go to your snowdrift directory, if not there already
* if not already on your master branch, run `git checkout master`
* run `git pull`

You should have no conflicts because this is the only situation where you
should ever change your local master.
**All your work should be done on other branches.**

#### Branching and committing

To start making edits:

Given you are on your master branch and have pulled the latest updates,
create a new branch:

    git checkout -b some_branch

Replace `some_branch` with a one- or two-word (with hyphens, not spaces)
description of your planned changes. For example, when fixing a problem in the
header, a good branch name would be `header-fix`.

#### Building your updates

Follow the instructions in the [BUILD.md](BUILD.md) guide for running the site
and running the tests.

If there are any failures either when compiling or testing, and you don't know
how to fix the issue or don't understand the error, contact us for help.

#### Committing your changes

When your updates all compile, tests pass, and you are ready to submit to the
main Snowdrift project, you can *commit* your changes.

If you are new to Git, we urge you to learn about the process of staging with
`git add` before committing and about review tools like `git status` and
`git diff`. See the links at the end of this file for learning resources.

For now, though it isn't best practice, you can quickly commit
all your changes with the command:

    git commit -a

An editor will show asking you to summarize your changes.
Make the message one that will be meaningful to people skimming
all the commits in the future. Then save and close the editor.

#### Getting your changes merged

After committing, send your changes to your git.gnu.io account with:

    git push -u my-snow some_branch

Change `some_branch` to the name of the branch where you made the commit(s).

Note: if you make additional changes to the same branch later,
you can push those new updates with just `git push`.

To notify the snowdrift team about your updates, go to your web browser and
visit the git.gnu.io page with your fork. You should see a button **"Create
Merge Request"** Clicking that will bring up a form where you can add further
notes about your work (especially useful if you are merging multiple commits).
You may ignore "Assign to", "Milestone", and "Labels" at this point.

Someone should comment on your submission soon (hopefully within a few hours).

## Choosing what to work on

With the code built and everything all set for making contributions,
we suggest a few different options for moving forward:

* Look through the
  [newbie-friendly tickets](https://snowdrift.coop/p/snowdrift/t?_hasdata=&f1=newbie-friendly)
  and see what looks interesting and/or doable.
  If you decide to work on a specific ticket, you can "claim" it once you have
  logged into the main site with a fully-established user.

* Play around with the site locally, and see if you can understand what does
  what. You may find bits that seem incomplete or confusing, and you can explore
  them and/or check with others about the status, such as whether the issue is
  known or tickets exist already.

* Explore the code itself by just opening files and see if you can figure out
  what does what and how things fit together.

    * As you go, you may find places that could use fixes to match our
    [code style](https://snowdrift.coop/p/snowdrift/w/en/coding#code-style-guide).

    * For those new to Haskell: Until you understand more about the Yesod web
      framework, you probably don't want to touch the nitty-gritty parts of the
      source code. However, the files in the /templates directory are comparable
      to basic HTML, CSS, and JavaScript. Beginners can quickly learn how to
      make changes to those files. Basically, Hamlet=HTML and Cassius=CSS but
      with easier, more concise syntax that uses indentation instead of closing
      tags. Julius files are effectively just containers for JavaScript. For
      more details, see the documentation on
      [Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates).

* Read our [coding wiki page](https://snowdrift.coop/p/snowdrift/w/en/coding),
  the related discussion board, and other pages throughout the Snowdrift.coop
  site.

* Read on below and check out links to learn more about the overall ecosystem,
  our development practices, and the tools we use.

## Development guidelines and notes

Overall, we strive to follow universal standards, be fully accessible,
and avoid browser-specific code.

### Design considerations

We have separate wiki and discussion pages on the site
for [web-design issues](https://snowdrift.coop/p/snowdrift/w/site-design).

### Code style

When making edits, follow our
[code style guide](https://snowdrift.coop/p/snowdrift/w/en/coding#code-style-guide).

### Use of JavaScript

**We generally build with *progressive enhancement* in mind.**
Content and functions should work with simple HTML/CSS
along with Yesod/Haskell server-side functions.
Later, we add JavaScript as appropriate for enhancement.
Consider the ideas of
[Unobtrusive JavaScript](http://en.wikipedia.org/wiki/Unobtrusive_JavaScript).
Use of NoScript should never cause a broken experience.
All our JavaScript should be recognized by the FSF's
[LibreJS plugin](https://www.gnu.org/software/librejs/).

Although we haven't used them as of August 2015, we have considered
[GHCJS](https://github.com/ghcjs/ghcjs) and
[PureScript](http://www.purescript.org/)
as options for more Haskell-connected ways to generate JavaScript.
If contributors want to work with either of those, we would happily accept that.
[Yesod JavaScript Options](https://github.com/yesodweb/yesod/wiki/JavaScript-Options)
explains further about those or other possibilities.

## Learning resources and helpful tools

For deeper understanding of various elements in our development,
here are some resources:

*   The following WikiBooks are fully FLO and include links to
    further resources as well. As they are wikis, you can and should
    *improve* them yourself as you read!

    * [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
      — one of the few *featured* Wikibooks, the Haskell Wikibook is
      *exceptionally* high quality and among the best overall introductions to
      Haskell.
    * [A Quick Introduction to Unix](https://en.wikibooks.org/wiki/A_Quick_Introduction_to_Unix)
      is a practical overview of command-line and Unix basics.
    * [SQL Wikibook](https://en.wikibooks.org/wiki/Structured_Query_Language)
      is a good general overview
    * [Git Wikibook](https://en.wikibooks.org/wiki/Git)
      is an incomplete book but has some useful bits.
    * [HTML Wikibook](https://en.wikibooks.org/wiki/HyperText_Markup_Language)
      is a workable but dated intro (of course, countless HTML guides exist)
    * [CSS Wikibook](https://en.wikibooks.org/wiki/Cascading_Style_Sheets)
      is pretty thorough though needs updating.
    * [JavaScript Wikibook](https://en.wikibooks.org/wiki/JavaScript)
      is incomplete, but seems a decent intro.

*   Git resources:

    * [Git for Ages 4 and Up](https://www.youtube.com/watch?v=1ffBJ4sVUb4&ab)
      is an excellent video introduction to the core commands and concepts of
      Git. Consider making a new folder like `git-test` in your `Home` directory
      and following along with the commands in the video if you really want to
      accelerate your proficiency with Git.

    * [Git Magic](http://www-cs-students.stanford.edu/~blynn/gitmagic/)
      is a fully-FLO book written in a more narrative style.

    * The [Git Docs](http://git-scm.com/doc/) page includes many links, an
      online version of the core Git manuals, and the full Pro Git book which
      uses the CC-BY-NC-SA license, so it is shareable but not fully FLO,
      unfortunately.

*   Web browsers today have built-in developer tools which enable testing
    and experimenting with live websites. The [Firebug](https://getfirebug.com)
    plugin offers some additional functions as well.

*   [CanIUse.com](http://caniuse.com/) is a reference website to check that all
    web features you use are compatible with various browsers and standards.
    The CanIUse data is fully FLO under a CC-BY license.

*   We use [Twitter Bootstrap](http://getbootstrap.com/) for much (but not all)
    of our CSS.

*   [The Yesod Book](http://www.yesodweb.com/book/) is the primary resource
    for learning about Yesod, the web framework we use to develop Snowdrift.

    Quite unfortunately, the Yesod book is not FLO (it uses the CC-BY-NC-ND
    license). Complaints to the publisher, O'Reilly Media, may help
    as the author of both Yesod and the Yesod book is otherwise willing
    to use a FLO license if the publisher will agree…
    (At least Yesod itself is FLO, including the internal documentation that
    comes with the code in Haddock format.)

*   The [School of Haskell](https://www.fpcomplete.com/school) includes
    basic and advanced topics including some Yesod sections.
    In April 2015, they announced plans to change to using FLO terms
    (the FLO release has not been done yet as of this writing).

*   At Stack Overflow (which uses FLO licensing for content), see tags for
    [yesod](http://stackoverflow.com/questions/tagged/yesod) and
    [haskell](http://stackoverflow.com/questions/tagged/yesod)
    (and, of course, other topics like HTML, CSS, Git, and so on)

*   Alongside #snowdrift on freenode.net, check out #yesod , #haskell ,
    and #haskell-beginners (among other relevant channels).

*   A useful development tool is `stack ghci` — a command that loads
    [ghci](https://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively)
    in a mode connected to the project. Using that, you can easily import
    files from the code and explore the functions.

*   As you encounter Haskell (and specifically Yesod) functions, a fast way
    to find the types and definitions is to search
    [Hayhoo](http://hayoo.fh-wedel.de/).

*   Add hlint with `stack install hlint`, and then you can run `hlint` followed
    by a .hs filepath to get suggestions for improving the Haskell code style.
