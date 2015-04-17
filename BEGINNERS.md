# Minimal Snowdrift set up instructions

This is a small tutorial on contributing to Snowdrift. You don't have to
be tech savvy, but it helps. This is a minimal-ish description of what
you need to do to get Snowdrift running locally and to start making and
submitting updates.

Although we recommend deeper study to understand how all of this works,
you *can* simply follow the steps below, and it will work well enough
to get started.

Advanced users should adapt any of these instructions as seen fit.

These instructions assume you use Debian testing or some variant of Ubuntu.

## Installing

Open up a terminal, and run these commands, exactly as you see them. You
could even copy and paste all of them into your terminal at once, and it
would work.

    sudo -i
    aptitude update
    aptitude install curl git postgresql postgresql-client
    cd /usr/lib/x86_64-linux-gnu
    ln -s libgmp.so.10 libgmp.so
    exit
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
    cabal install cabal-install alex happy haddock yesod-bin
    nano ~/.bashrc

A simple editor will pop up. Go to the end of the file, and add this
line:

    PATH=$PATH:$HOME/cabal/bin:.cabal-sandbox/bin

Then run these commands:

    git clone https://github.com/snowdriftcoop/snowdrift.git
    cd snowdrift
    cabal sandbox init
    cabal install --enable-tests -fdev
    sdm init
    Snowdrift Development

Open up http://localhost:3000 in your browser to see the Snowdrift site.

Now you can play with Snowdrift locally.

## Basic Git setup

We use a program called [Git](https://git-scm.com/) to make and share
changes while keeping the versions tracked and organized. Right now,
we use a proprietary website called [GitHub](https://github.com/)
to share the Git data. We plan to switch to gitlab.gnu.io when it is ready.

To get set to contribute changes to the project,
first [create an account on GitHub](https://github.com/join)
if you don't already have one.

Once signed in, go to <https://github.com/snowdriftcoop/snowdrift>, and
click the "Fork" button on the top right:

![](https://a.pomf.se/jrarfe.png)

GitHub should redirect you to your fork. To check, see that the header
at the top of the page looks something like this:

![](https://a.pomf.se/fivuqa.png)

The important bit is the "forked from snowdriftcoop/snowdrift".

On the right side of the GitHub page for your fork, about halfway down,
you'll see "HTTPS clone URL".\*

![](https://a.pomf.se/cmtfif.png)
 
Copy the address in the box, and type this in your local terminal
(don't hit enter yet)

    git remote add my-snow

Use Ctrl+Shift+V to paste the address in the terminal. For historical
reasons, Ctrl+V does something else in most terminals.  The final
command should look like this:

    git remote add my-snow https://github.com/YOURNAME/snowdrift.git

Where `YOURNAME` is your GitHub username.

Finally, run these additional Git setup commands, replace `YOUR NAME
GOES HERE` and `YOUR EMAIL GOES HERE` with your actual name and email.

    git config --global user.name "YOUR NAME GOES HERE"
    git config --global user.email "YOUR EMAIL GOES HERE"
    git config --global core.editor "nano"

\* Note that overall, we recommend that you use SSH rather than HTTPS.
However, [SSH setup](https://help.github.com/articles/generating-ssh-keys/)
is kind of tricky, especially for those new to SSH.

## Branching and committing

When planning edits, first create a new branch:

    git checkout -b some_branch

Replace `some_branch` with a one- or two-word description of your planned
changes. For example, when fixing a problem in the header, a good branch name
would be `header-fix`.

## Editing

Until you understand more about the Yesod web framework, you probably
don't want to edit the nitty-gritty parts of the source code.  However,
the files in the project's `templates` directory are comparable to basic
HTML, CSS, and JavaScript. Beginners can quickly learn how to make basic
changes to those files. For more, see the documentation on
[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates).

Never edit files with a traditional word processor like LibreOffice.
Instead, use a code-appropriate text editor, like Pluma, Kate, or Gedit.
Advanced programmers often use Vim or Emacs.

When making edits, follow our
[code style guide](https://snowdrift.coop/p/snowdrift/w/en/coding#code-style-guide).

After making and saving changes, you can use `cabal install` followed by
`Snowdrift Development` recompile and run the updates.  If you prefer,
`yesod devel` is a command that does both of these and can stay running,
and it will automatically recompile after most changes (you can use
`cabal install` in cases where `yesod devel` fails to recognize a
change).  Refresh your browser view at localhost:3000 to see the
updates.

In the respective terminal window, you can quit `Snowdrift Development`
with Ctrl+C or stop `yesod devel` by hitting ENTER a few times.

After successfully compiling and seeing that the changes seem good,
do a final test with:

    yesod test

If there are any failures either when compiling or testing,
and you don't know how to fix the issue or don't understand the error,
ask on the [IRC channel](https://snowdrift.coop/p/snowdrift/w/en/irc),
and someone will probably help you.

## Committing your changes

When you have saved changes and everything seems fine, commit your changes with:

    git commit -a

An editor will pop open asking you to summarize your changes.
The Erlang people have a nice guide to
[writing good commit messages](https://github.com/erlang/otp/wiki/Writing-good-commit-messages).

## Getting your changes merged

Now you can send your changes to your github account with:

    git push -u my-snow some_branch

Change `some_branch` to the name of the branch where you made the
commit(s).

Reload the GitHub page with your fork.  You should see something like
this:

![](https://a.pomf.se/paqzzx.png)
    
Click the "Pull request" button. There'll be a nice little form there
for submitting the pull request. Fill out the form, and send it.

Someone will probably comment on it within an hour or two.

## Further reading

If you want a deeper understanding of all this stuff, here are some
things you can read:

*   The respective WikiBooks on

    + [HTML](https://en.wikibooks.org/wiki/HyperText_Markup_Language),
    + [CSS](https://en.wikibooks.org/wiki/Cascading_Style_Sheets), and
    + [JavaScript](https://en.wikibooks.org/wiki/JavaScript).

    These books are FLO, but I haven't looked to see if they are any
    good. WikiBooks in general are top-notch.

*   [w3schools](http://www.w3schools.com/) is a very good way to learn
    HTML, CSS, and JavaScript. Unfortunately it is proprietary.

*   [The Git Book](https://git-scm.com/book/) for learning about Git. It
    is very good. It is technically not FLO, but it is close. The
    license forbids commercial use.

*   If you're using Mac OS X, Linux, or BSD, you might want to read up
    on [the basics of UNIX][unix]. It's very good, albeit a bit dense,
    and FLO.

*   [The Haskell WikiBook](https://en.wikibooks.org/wiki/Haskell) for
    learning about Haskell. It's very good, and FLO.

*   [The Yesod Book](http://www.yesodweb.com/book/) for learning about
    Yesod, the web stack we use to develop Snowdrift. The Yesod book is
    technically not FLO, but it's close. The license forbids commercial
    use of the book.

    The book is very thorough, although you have to read it
    carefully. If you skim it, you will likely miss critical
    information.

    We don't know of any good FLO resources for learning about
    Yesod. Yesod itself is FLO.

[unix]: https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/basics.html
