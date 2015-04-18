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
Our general [GUIDE](GUIDE.md) has some notes for other systems.

If you need *any* help at any point, come say "hi" at our freenode.net IRC
channel [#snowdrift](https://snowdrift.coop/p/snowdrift/w/irc).
We are always happy to assist and answer *any* questions!

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

    export PATH=$PATH:$HOME/cabal/bin:.cabal-sandbox/bin

Save the file and close the editor.

Next, back in the terminal, run these commands:
(yes, the first is the same as the line you added in the editor)

    export PATH=$PATH:$HOME/cabal/bin:.cabal-sandbox/bin
    git clone https://github.com/snowdriftcoop/snowdrift.git
    cd snowdrift
    cabal sandbox init
    cabal install --enable-tests -fdev
    sdm init
    Snowdrift Development

Open up http://localhost:3000 in your browser to see the Snowdrift site.

Now you can play with Snowdrift locally.
To log into the site, use  the built-in system with
user: `admin` pass: `admin`


## Basic Git setup

We use a program called [Git](http://git-scm.com/) to make and share
changes while keeping the versions tracked and organized. Right now,
we use a proprietary website called [GitHub](https://github.com/)
to share the Git data. We plan to switch to gitlab.gnu.io when it is ready.

To contribute changes to the project, first
[create an account on GitHub](https://github.com/join)
if you don't already have one.

Once signed in, go to <https://github.com/snowdriftcoop/snowdrift>,
and click the "Fork" button on the top right:

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

Use Ctrl+Shift+V to paste the address in the terminal.
For historical reasons, Ctrl+V does something else in most terminals.
The final command should look like this:

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

### Editing

#### Notes about editing and files

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

#### Building your updates

After making and saving changes, you can use `cabal install` followed by
`Snowdrift Development` recompile and run the updates.  If you prefer,
`yesod devel` is a command that does both of these and can stay running,
and it will automatically recompile after most changes (you can use
`cabal install` in cases where `yesod devel` fails to recognize a
change).  Refresh your browser view at localhost:3000 to see the
updates.

In the respective terminal window, you can quit `Snowdrift Development`
with Ctrl+C or stop `yesod devel` by hitting ENTER a few times.

### Testing changes

After successfully compiling and checking that the changes seem good,
do a final test with:

    yesod test

If there are any failures either when compiling or testing,
and you don't know how to fix the issue or don't understand the error,
ask on the [IRC channel](https://snowdrift.coop/p/snowdrift/w/en/irc),
and someone will probably help you.

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

## Getting your changes merged

After committing, send your changes to your github account with:

    git push -u my-snow some_branch

Change `some_branch` to the name of the branch where you made the
commit(s).

Reload the GitHub page with your fork.
You should see something like this:

![](https://a.pomf.se/paqzzx.png)
    
Click the "Pull request" button. There'll be a nice little form there
for submitting the pull request. Fill out the form, and send it.

Someone should comment on it soon (hopefully within a few hours).


## Learning resources and helpful tools

Besides reading the content on the Snowdrift.coop site itself,
we suggest reviewing our [GUIDE.md](GUIDE.md), specifically sections on
"Development guidelines and notes" and "Additional notes about databases".

If you want a deeper understanding of various elements in our development,
here are some resources:

*   The following WikiBooks are fully FLO and include links to
    further resources as well. As they are wikis, you can and should
    *improve* them further yourself as you read!

    + [HTML Wikibook](https://en.wikibooks.org/wiki/HyperText_Markup_Language)
    + [CSS Wikibook](https://en.wikibooks.org/wiki/Cascading_Style_Sheets)
    + [JavaScript Wikibook](https://en.wikibooks.org/wiki/JavaScript)
    + [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
    + [SQL Wikibook](https://en.wikibooks.org/wiki/Structured_Query_Language)
      is a complete book and good general overview
    + [A Quick Introduction to Unix](https://en.wikibooks.org/wiki/A_Quick_Introduction_to_Unix)
      is a good overview to command-line and Unix basics
    + [Git Wikibook](https://en.wikibooks.org/wiki/Git) is only half-done
      but has some useful bits

*   [w3schools](http://www.w3schools.com/) is a very good way to learn
    HTML, CSS, and JavaScript. Unfortunately it is proprietary.

*   Web browsers today have built-in developer tools which enable testing
    and experimenting with live websites. The [Firebug](https://getfirebug.com)
    plugin offers some additional functions as well.

*   We use [Twitter Bootstrap](http://getbootstrap.com/) for much (but not all)
    of our CSS.

*   The [Git Docs](http://git-scm.com/doc/) page includes many links, an online
    version of the core Git manuals, and the Pro Git book which uses the
    CC-BY-NC-SA license, so it is shareable but not fully FLO, unfortunately.
    [Git Magic](http://www-cs-students.stanford.edu/~blynn/gitmagic/)
    is a fully-FLO book written in a more narrative style.

*   [The Basics of UNIX][unix] is FLO and good reading, albiet a bit dense,
    relevant to all UNIX-style systems including Mac OS X, GNU/Linux, and BSD.

*   [The Yesod Book](http://www.yesodweb.com/book/) is the primary resource
    for learning about Yesod, the web framework we use to develop Snowdrift.
    
    Quite unfortunately, the Yesod book is not FLO (it uses the CC-BY-NC-ND
    license). Perhaps complaints to the publisher, O'Reilly media may help,
    as the author of both Yesod and the Yesod book is otherwise willing
    to use a FLO license…
    At least Yesod itself is FLO, including the internal documentation that
    comes with the code in Haddock format.

    The Yesod book is thorough, but you must read it carefully.
    When skimming, you can easily miss critical information.

*   The [School of Haskell](https://www.fpcomplete.com/school) includes
    basic and advanced topics including some Yesod sections.
    In April 2015, they announced plans to change to using FLO terms
    (the FLO release has not been done yet as of this writing).

*   At Stack Overflow (which uses FLO licensing for content), see tags for
    [yesod](http://stackoverflow.com/questions/tagged/yesod) and
    [haskell](http://stackoverflow.com/questions/tagged/yesod)
    (and, of course, other topics like HTML, CSS, Git, and so on)

*   Alongside #snowdrift on freenode.net, check out #yesod #haskell
    and #haskell-beginners

*   A useful development tool is "cabal repl" — a command that loads
    [ghci](https://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively)
    in a mode connected to the project. Using that, you can easily import
    files from the code and explore the functions.

*   To help write clean Haskell code and learn conventions, run `hlint`
    on your files to get suggestions for possible improvements.
    Add hlint to your system with the command `cabal install hlint`

*   The [PostgreSQL Documention](http://www.postgresql.org/docs/9.4/interactive/index.html)
    is not FLO but is quite thorough. You will not really need to know all
    details, but familiarity with PotgreSQL will enhance your ability to help
    with various parts of the Snowdrift code.

[unix]: https://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/basics.html
