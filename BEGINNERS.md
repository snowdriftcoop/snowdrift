# Minimal Snowdrift set up instructions

This is a small tutorial on contributing to Snowdrift. You don't have to
be tech savvy, but it helps. This is a minimal-ish description of what
you need to do to get Snowdrift running locally and to start making and
submitting updates.

Although we recommend deeper study to understand how all of this works,
you *can* simply follow the steps below, and it will work well enough
to get started.

Advanced users may, of course, adapt these instructions as they see fit.

If you need *any* help at any point, come say "hi" at our freenode.net IRC
channel [#snowdrift](https://snowdrift.coop/p/snowdrift/w/irc).
We are always happy to assist and answer *any* questions!

## Prerequisites

To get started, the only absolute requirements are that you have a reasonably
new laptop or desktop computer system (less than 10 years old generally),
and know how to use a terminal to enter commands in the command-line.

In general, when you see a list of commands to enter,
you may enter them in one-at-a-time exactly as you see them,
or you can copy and paste a collection of commands all at once.

Note that, in most cases, you must use Ctrl+Shift+V to paste into the terminal.
For historical reasons, Ctrl+V does something else in most terminals.

## Installing

### Install Git

We use a program called [Git](http://git-scm.com/) to make and share
changes while keeping the versions tracked and organized.

Whatever system you use, you will need Git installed
before installing snowdrift.
If you don't have Git installed locally already, install it now.

Most systems will have Git in their software repositories.
For example, on Debian or Ubuntu you can enter the following in a terminal:

    sudo apt-get install git

If you are on a system that does not package git yet, you may choose to
[download Git from the website](https://git-scm.herokuapp.com/downloads).

### Install Snowdrift

To get a quick virtual machine with the core dependencies for snowdrift,
follow the [Vagrant setup instructions](SETUP_VAGRANT.md).
**This is the best and easiest option for all systems including
GNU/Linux, BSD, Mac OS X, and Windows.**

As an alternative option, we also have instructions for local installation.
[Debian-based instructions](SETUP_DEBIAN.md) work for Debian or Ubuntu or
related, and our general [GUIDE](GUIDE.md) has some notes for other systems.

## Working on the code

### Basic Git setup

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
and click the "Fork" button on the top right.

After choosing your account for the fork, you should be at the page
for your fork of the project. To check, see that the header
at the top of the page has has your account name followed by "/Snowdrift".
At the right side, middle way down, you should also see
"Forked from: snowdrift / Snowdrift".

At the top of the main page, below the header,
you'll see a box with an address that looks like
"https://git.gnu.io/YOURNAME/snowdrift.git"

Where `YOURNAME` is your git.gnu.io username.

Paste that address into your terminal as part of the following command:

    git remote add my-snow https://git.gnu.io/YOURNAME/snowdrift.git
 
Finally, run these additional Git setup commands, replace `YOUR NAME
GOES HERE` and `YOUR EMAIL GOES HERE` with your actual name and email.

    git config --global user.name "YOUR NAME GOES HERE"
    git config --global user.email "YOUR EMAIL GOES HERE"
    git config --global core.editor "nano"

\* Note that overall, we recommend that you use SSH rather than HTTPS.
However, [SSH setup](https://git.gnu.io/help/ssh/README)
is kind of tricky, especially for those new to SSH.

### Updating your local code to snowdrift master

Whenever you begin new work, you should generally start with the latest
master code from the Snowdrift project.

If you do not have it yet, setup a remote for the main Snowdrift code:

    git remote add snowdrift-main https://git.gnu.io/snowdrift/snowdrift.git

*Note:* if you have set up ssh with git.gnu.io (mentioned above),
use `git@git.gnu.io:snowdrift/snowdrift.git` as the remote address
instead of the https version.

From now on, you can download all the latest master code via:

    git fetch snowdrift-main

You should then update your own master branch to match the main code.
If not already on your local master branch, run `git checkout master`.
Then run:

    git merge snowdrift-main/master

You should have no conflicts because this is the only situation where you
should ever change your local master. All your work should be done on
other branches. 

### Branching and committing

Given you are on an up-to-date master branch and you want to start making edits,
first create a new branch:

    git checkout -b some_branch

Replace `some_branch` with a one- or two-word description of your planned
changes. For example, when fixing a problem in the header, a good branch name
would be `header-fix`.

### Notes about editing and files

Until you understand more about the Yesod web framework, you probably
don't want to touch the nitty-gritty parts of the source code.  However,
the files in the project's `templates` directory are comparable to basic
HTML, CSS, and JavaScript. Beginners can quickly learn how to make
changes to those files. Basically, Hamlet=HTML and Cassius=CSS but with
easier, more concise syntax that uses indentation instead of closing tags.
Julius files are simply containers for JavaScript.
For more details, see the documentation on
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates).

Never edit files with a traditional word processor like LibreOffice.
Instead, use a code-appropriate text editor, like Pluma, Kate, or Gedit.
Advanced programmers often use Vim or Emacs.

When making edits, follow our
[code style guide](https://snowdrift.coop/p/snowdrift/w/en/coding#code-style-guide).

### Building your updates

The `yesod devel` command can be left running in a terminal while work is
done elsewhere. It will automatically recompile and restart the site
whenever it detects file changes.

Refresh your browser view at localhost:3000 to see the updates.

In rare cases, you may need to run `cabal clean` if yesod devel
fails to recognize a change.

To stop yesod devel, press ENTER a few times.

### Testing changes

After successfully compiling and checking that the changes seem good,
do a final test with:

    yesod test

If there are any failures either when compiling or testing,
and you don't know how to fix the issue or don't understand the error,
ask on the [IRC channel](https://snowdrift.coop/p/snowdrift/w/en/irc),
and someone will probably help you.

Sometimes the tests just need updating, and for that run:

    cabal clean && cabal configure -fdev && cabal build && yesod test

### Committing your changes

When your updates all compile, tests pass,
and you are ready to submit to the main Snowdrift project,
you can *commit* your changes.

If you don't already know git basics, we urge you to learn
about the process of staging with `git add` before committing
and build good review habits with tools like `git status` and `git diff`.
See the links for learning resources at the end of this file.

For now, though it isn't best practice, you can quickly commit
all your changes with the command:

    git commit -a

An editor will show asking you to summarize your changes.
Make the message one that will be meaningful to people skimming
all the commits in the future. Then save and close the editor.
For reference, here's a decent guide to
[writing good commit messages](https://github.com/erlang/otp/wiki/Writing-good-commit-messages).

### Getting your changes merged

After committing, send your changes to your git.gnu.io account with:

    git push -u my-snow some_branch

Change `some_branch` to the name of the branch where you made the commit(s).

Reload the git.gnu.io page with your fork.
You should see a button **"Create Merge Request"**
    
Clicking that will bring up a form where you can add further notes about your
work (especially useful if you are merging multiple requests).
You may ignore "Assign to", "Milestone", and "Labels" at this point.

Someone should comment on your submission soon (hopefully within a few hours).

## Learning resources and helpful tools

Besides reading the content on the Snowdrift.coop site itself,
we suggest reviewing our [GUIDE.md](GUIDE.md), specifically sections on
"Development guidelines and notes" and "Additional notes about databases".

If you want a deeper understanding of various elements in our development,
here are some resources:

*   The following WikiBooks are fully FLO and include links to
    further resources as well. As they are wikis, you can and should
    *improve* them yourself as you read!

    + [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
      — one of the few *featured* Wikibooks, the Haskell Wikibook
      is *exceptionally* high quality and arguably the best overall
      introduction to Haskell anywhere.
    + [A Quick Introduction to Unix](https://en.wikibooks.org/wiki/A_Quick_Introduction_to_Unix)
      is a practical overview of command-line and Unix basics.
    + [SQL Wikibook](https://en.wikibooks.org/wiki/Structured_Query_Language)
      is a good general overview
    + [Git Wikibook](https://en.wikibooks.org/wiki/Git)
      is an incomplete book but has some useful bits.
    + [HTML Wikibook](https://en.wikibooks.org/wiki/HyperText_Markup_Language)
      is a workable but dated intro (of course, countless HTML guides exist)
    + [CSS Wikibook](https://en.wikibooks.org/wiki/Cascading_Style_Sheets)
      is pretty thorough though needs updating.
    + [JavaScript Wikibook](https://en.wikibooks.org/wiki/JavaScript)
      is incomplete, but seems a decent intro.

*   Web browsers today have built-in developer tools which enable testing
    and experimenting with live websites. The [Firebug](https://getfirebug.com)
    plugin offers some additional functions as well.

*   [CanIUse.com](http://caniuse.com/) is a reference website to check that all
    web features you use are compatible with various browsers and standards.
    The CanIUse data is fully FLO under a CC-BY license.

*   We use [Twitter Bootstrap](http://getbootstrap.com/) for much (but not all)
    of our CSS.

*   The [Git Docs](http://git-scm.com/doc/) page includes many links, an online
    version of the core Git manuals, and the full Pro Git book which uses the
    CC-BY-NC-SA license, so it is shareable but not fully FLO, unfortunately.

*   [Git Magic](http://www-cs-students.stanford.edu/~blynn/gitmagic/)
    is a fully-FLO book written in a more narrative style.

*   [The Yesod Book](http://www.yesodweb.com/book/) is the primary resource
    for learning about Yesod, the web framework we use to develop Snowdrift.
    
    Quite unfortunately, the Yesod book is not FLO (it uses the CC-BY-NC-ND
    license). Perhaps complaints to the publisher, O'Reilly media may help,
    as the author of both Yesod and the Yesod book is otherwise willing
    to use a FLO license…
    At least Yesod itself is FLO, including the internal documentation that
    comes with the code in Haddock format.

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

*   A useful development tool is `cabal repl` — a command that loads
    [ghci](https://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively)
    in a mode connected to the project. Using that, you can easily import
    files from the code and explore the functions.

*   To help write clean Haskell code and learn conventions, run `hlint`
    on your files to get suggestions for possible improvements.
    Add hlint to your system with the command `cabal install hlint`
