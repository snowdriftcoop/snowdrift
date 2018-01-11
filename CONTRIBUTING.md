# Snowdrift Beginning Contributor's Guide

To contribute to Snowdrift, you don't have to be tech savvy. This guide is
written to work for even novice programmers. Advanced readers may adapt the
instructions as they see fit.

In this guide, we will cover pretty much all you need to do to get Snowdrift
running locally and to start making and submitting updates.

## Get in touch!

Our *recommended* approach to getting started *includes* asking questions,
getting mentored, and otherwise having real human engagement with others in the
community. Please don't hesitate to speak up, and don't worry about taking
people's time. We always welcome questions. For efficiency, we will point you to
resources or pair you with others.

## Licensing note

When you contribute patches to this repository, such as via a merge request, you
retain the copyright to your contributions. Of course, given unchanged license
notices in your copy of the repository, you automatically release your work
under the same licenses we use (GNU AGPLv3+ primarily with non-code text and
graphics also under CC BY-SA 4.0 International, as specified in the [README].

## Prerequisites to contributing

To work with Snowdrift, you should have a reasonably new laptop or desktop
computer system (less than 10 years old generally) and know how to open a
terminal and enter commands.

Generally, everything works smoothly with GNU/Linux, \*BSD systems, and OS X.

We do not support Windows at this time, so we suggest Windows users switch
systems or run a virtual machine. We encourage Windows users to switch to a
free/libre/open system anyway, but if you really want to help test Snowdrift on
Windows, our [Build guide] has some notes about that.

## Command-line beginner's basics

In general, when you see a list of commands to enter, we recommend entering them
in one-at-a-time exactly as you see them. However, it should work to copy and
paste a collection of commands all at once.

NB: in most cases, you must use Ctrl-Shift-V to paste into the terminal (for
historic reasons, Ctrl-V does something else usually).

We also highly recommend use of tab-completion. See the wikibook "A Quick
Introduction to Unix" in the resources at the end of this file for more
command-line basics.

## Installing

**Follow the [Build guide]** for instructions to get Snowdrift going on your
computer.

## Working on the code

**All the following assumes you have done the initial clone and install
according to the instructions linked above.**

### Text-editors and settings

All systems should come with a code-appropriate text-editor (or two), e.g.
Gedit, Kate, Pluma, Scratch, etc. Any of those will work for now, but we
recommend installing an editor with stronger Haskell and Yesod support. See
[TEXTEDITORS.md] for our specific recommendations and settings.

### Working with Git

Short version for experienced Git users:

**We suggest keeping your local master matched to the main project master. Work
*only* on other git branches. Use as many branches as needed to separate all
work that functions independently** (and avoid clutter by deleting branches once
no longer needed).

To make it easier to follow changes, please retain the full list of commits
when pushing branch updates, and only rebase when requested.

The following covers the bare minimum process for those new to Git.

#### Basic Git setup for collaboration

##### Forking at the host platform

We collaborate on the code via the FLO (Free/Libre/Open) site
[git.snowdrift.coop] using GitLab CE. We also mirror on the popular but
proprietary site [GitHub], which you may use if you already have an account
there. As we encourage everyone to use FLO tools, the instructions below assume
git.snowdrift.coop.

Fork the project:
* create an account on [git.snowdrift.coop](https://git.snowdrift.coop/users/sign_in) or sign in if you already have an account
* go to the [project repository]
* click the "Fork" link at the top of the main page, below the header
* choose your account for the fork
* you should end up at the page for your fork of the project; to check, see that the header at the top of the page has
your account name followed by "/Snowdrift"

##### Adding your fork as a remote

At the top of the main page, below the header, you'll see a box with an address.
By default, the SSH option is selected, and we recommend SSH ideally. However,
[SSH setup] is kind of tricky, especially for those new to SSH. To stick with
the easier option for now, click "HTTPS" and use that address which will look
like: "https://git.snowdrift.coop/YOURNAME/snowdrift.git"

Where `YOURNAME` is your git.snowdrift.coop username.

Copy that address to your clipboard.

In your snowdrift directory, paste the address into your terminal as part of the
following command:

    git remote add my-snow https://git.snowdrift.coop/YOURNAME/snowdrift.git

If you have not used Git before, run these additional Git setup commands,
replacing `YOUR NAME` and `YOUR EMAIL` with your actual name and email.

    git config --global user.name "YOUR NAME"
    git config --global user.email "YOUR EMAIL"

Optional: specify a particular text editor for commit messages (replace `nano`
with the command for whatever editor you prefer):

    git config --global core.editor "nano"

#### Updating your local code to snowdrift master

Whenever you begin new work, you should first get the latest master code from
the Snowdrift project.

The following assumes you originally cloned the code from one of our main hosts
(as described in the BUILD guide), so you will have the main snowdrift code
as your "origin" (verify this with `git remote -v`).

To download the latest updates of the snowdrift code:

* Go to your snowdrift directory, if not there already
* if not already on your master branch, run `git checkout master`
* run `git pull`

You should have no conflicts because this is the only situation where you
should ever change your local master. **Work should be done on other branches.**

#### Starting a new branch

From the master branch, having pulled the latest updates, create a new branch:

    git checkout -b some-branch

Replace `some-branch` with a one- or two-word (with hyphens, not spaces)
description of your planned changes. For example, when fixing a problem in the
header, a good branch name would be `header-fix`.

Now, you can edit files, save work as you go, etc.

#### Building your updates

To check your work and see the results on the running site, follow the
instructions in the [Build guide] for running the site.

#### Running the tests

When you are happy with your work, it compiles, and looks right, run the tests:

    ./build.sh test

If there are any failures either when compiling or testing, and you don't know
how to fix the issue or don't understand the error, contact us for help.

#### Committing your changes

When your updates all compile, tests pass, and you are ready to submit to the
main Snowdrift project, *commit* your changes.

If you are new to Git, we urge you to learn about the process of staging with
`git add` before committing and about review tools like `git status` and
`git diff`. See the links at the end of this file for learning resources.

For now, though it isn't best practice, you can quickly commit all your changes
with the command:

    git commit -a

An editor will show asking you to summarize your changes. For the first line,
write a short commit title that will be meaningful to people skimming all the
commits in the future. If you want to add further comments about the work, do
that on additional lines below the title. Then save and close the editor.

#### Getting your changes merged

When you are ready to share your work (one or more commits all relevant to the
same overall update), and you have confirmed that everything works and all tests
pass, run `git status` to make sure no work is missing and all new files were
committed.

Next, send your changes to your git.snowdrift.coop account with:

    git push -u my-snow some_branch

Change `some_branch` to the name of the branch where you made the commit(s).

NB: if you make additional changes to the same branch before a maintainer merges
it into master, you can push those new updates with just `git push`.

To notify the snowdrift team about your work, visit the git.snowdrift.coop page
with your fork. You should see a button **"Create Merge Request"**. Click that
to bring up a form where you can add further notes about your work (especially
useful if you are merging multiple commits). You may ignore "Assign to",
"Milestone", and "Labels" at this point.

After you submit the merge request, someone should comment on your submission
soon (hopefully within a few hours, maybe a day or two depending on timing).

## Choosing what to work on

Several ways to get started contributing and/or to learn more overall:

* Visit our [Taiga Issues] page and filter to the "newcomer" tag and see
  what looks interesting and/or doable. Consider exploring other issues based
  on your skills or interests. If you decide to work on a specific ticket, chat
  with us on [IRC] or send an email to the [dev mailing list] to get added to
  the team.

* Play around with the site locally. See if you can understand what does what.
  You may find bits that seem incomplete or confusing, and you can explore them
  and/or check with others about the status, such as whether the issue is known
  or tickets exist already.

* Explore the code files and see if you can figure out what does what and how
  things fit together.

    * For non-Haskell work:
        * The .hamlet files in the /website/templates directory are comparable
          to basic HTML using white-space to avoid the need for closing tags
          (plus some other logic that connects to the Haskell). For more
          details, see the documentation on [Shakespearean Templates].
        * Any .julius files are containers for JavaScript.
        * For CSS, we're moving from .cassius and switching to Sass files
          (TODO: update this with documention for contributing to Sass once the
          process is finalized)

    * For those familiar with Haskell, consider exploring our files and updating
      any code that doesn't match our code style (see the "Code style" section
      below). After you have some familiarity with the project, you can take
      on some real development tasks.

* Read the code documention in this repo and other pages on the Snowdrift.coop
  [wiki].

* Read on below and check out links to learn more about the overall ecosystem,
  our development practices, and the tools we use.

## Development guidelines and notes

Overall, we strive to follow universal standards, be fully accessible,
and avoid browser-specific code.

### Design considerations

We have separate [design] wiki pages for our specific design guide and general
design-related issues.

### Code style

The most important rule is to follow the existing style of whatever function or
file you are in. When in doubt, however, consider the following guidelines.

* Indentation: 4-space indentation default, 2-space indentation for .hamlet
files
* 80-column per line maximum
    * We accept exceptions for cases such as the inclusion of very long
    external URLs and so on
* Sentence-case, 50-column Git commit headers (but no ending period mark)
    * include bits like "fixes SD-#" as appropriate when fixing a ticket
    * consider adding extra comments below commit titles
* Haskell-standard camelCaseNames (not C-standard underscore_names)
* Group qualified imports separate from unqualified imports, like:

    ```
    import System.IO (hFlush, stdout, stderr)
    import System.Log.FastLogger (toLogStr, fromLogStr)
    import Yesod.Default.Config (withYamlEnvironment, DefaultEnv (..))
    import qualified Control.Exception.Lifted as Exception
    import qualified Data.ByteString.Char8 as Char8
    ```

* Imports should be grouped into three paragraphs: "Prelude" or "Import" for
those modules that need them, then external modules, and then internal modules.
* Indent so that groups are logical and easy to read. Here are some examples:

    ```haskell
    -- bad: dangling child
    functionFoo arg1 arg2
                     arg3

    -- bad: not indented
    functionFoo $
    arg1 arg2

    -- bad: chunked children (a special case of dangling child)
    functionFoo arg1 arg2
                arg3 arg4

    -- bad: reverse indent
    functionFoo $ functionBar $
        barArg1
        barArg2

    -- better: indented under proper parent
    functionFoo $ functionBar
                      barArg1
                      barArg2

    -- best #1: stacked arguments
    functionFoo $ functionBar barArg1
                              barArg2
                              barArg3

    -- best #2: "indent the head before the tail"
    functionFoo $
        functionBar
            barArg1
            bargArg2

    -- or
    functionFoo $
        functionBar barArg1 barArg2

    -- or
    functionFoo
        (functionBar barArg1 barArg2)
    ```

* Don't bother aligning delimiters (such as `->` in case statements) if there
are intermediate lines that begin far to the left.

    ```
    -- undesirable
    (Nothing, Just email, True) -> do
        murl <- libravatar $ T.unpack email
        return (fmap fromString murl)
    _                           -> return Nothing

    -- better
    (Nothing, Just email, True) -> do
        murl <- libravatar $ T.unpack email
        return (fmap fromString murl)
    _ -> return Nothing
    ```

* When creating a multiline list of items, put commas on the left and line them
up with the opening delimiter. In the following example I put no space between
the comma and the item, but that is at your discretion. The location of the
ending delimiter is also at your discretion: in line with the other delimiters,
or immediately following the last item.

    ```
    someList = [foo
               ,bar
               ,baz
               ]
    ```
* Indent "where" by two spaces. Example:

    ```
    func = do
        bar
        baz
      where
        baz = the other thing
    ```

* Additionally, you can use hlint to get suggestions for improving code style:

    ```
    stack install hlint
    hlint -XQuasiQuotes -XTemplateHaskell $FILE
    ```

* Alignment of "do" is at your discretion. Any of these is fine:

    ```
    foo1 b = do
        bar
        baz

    foo2 b = do bar
                baz

    foo3 b =
        do bar
           baz
    ```

### Code review

As a best practice, we want to have adequate code review for every merge before
it goes into the master code.

For volunteers interested in helping with code review, please choose to *watch*
the code repositories (the main repo at [git.snowdrift.coop] and also the
[GitHub] mirror if you use GitHub). Then, you can make general and in-line
comments about new code as it is committed and before it goes into the master
repository.

### Use of JavaScript

**We generally build with *progressive enhancement* in mind.** The site should
work with just HTML/CSS along with Yesod/Haskell server-side functions. Given
that basis, we can add JavaScript as appropriate for enhancement, considering
[Unobtrusive JavaScript]. Use of NoScript should never cause a broken
experience. All our JavaScript should be recognized by the FSF's [LibreJS plugin].

Although we haven't used them as of January 2016, we have considered
[GHCJS] and [PureScript] as options for more Haskell-connected ways to generate
JavaScript. If contributors want to work with either of those, we would happily
accept that. [Yesod JavaScript Options] explains further about those or other
possibilities.

### Dependencies

If you introduce a Haskell package dependency, please add it to the appropriate
Cabal file, apart from what is specified in stack.yaml. This is for
compatibility with our definition lookup system.

And if you introduce a Haskell dependency that is from an online git repo and
not Hackage, please add the appropriate code to Step Three of
dev-tools/tag.sh, using the existing comments and code to see how.

Please do not use any other means of specifying package dependencies to the
computer. Such tactics may get out of sync with the Cabal files / stack.yaml.
For example, please do *not* specify dependencies with a special comment to
Stack on line two of an \*.hs file.

## Learning resources and helpful tools

For deeper understanding of various elements in our development,
here are some resources (nearly all fully-FLO):

*   The following WikiBooks are fully FLO (Free/Libre/Open) and include links to
    further resources as well. As they are wikis, you can and should *improve*
    them yourself as you read!

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

*   [CanIUse.com](http://caniuse.com/) is a reference website to check that all
    web features you use are compatible with various browsers and standards.
    The CanIUse data is fully FLO under a CC-BY license.

*   [The Yesod Book](http://www.yesodweb.com/book/) is the primary resource
    for learning about Yesod, the web framework we use to develop Snowdrift.

*   The [School of Haskell](https://www.schoolofhaskell.com) includes
    basic and advanced topics including some Yesod sections.

*   At Stack Overflow (which uses FLO licensing for content), see tags for
    [yesod](http://stackoverflow.com/questions/tagged/yesod) and
    [haskell](http://stackoverflow.com/questions/tagged/yesod)
    (and, of course, other topics like HTML, CSS, Git, and so on)

*   Alongside #snowdrift on freenode.net, check out #yesod , #haskell ,
    and #haskell-beginners (among many other relevant channels).

[Build guide]: BUILD.md
[design]: https://wiki.snowdrift.coop/design
[dev mailing list]: https://lists.snowdrift.coop/mailman/listinfo/dev
[git.snowdrift.coop]: https://git.snowdrift.coop/sd/snowdrift
[GHCJS]: https://github.com/ghcjs/ghcjs
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[IRC]: https://wiki.snowdrift.coop/community/irc
[LibreJS plugin]: https://www.gnu.org/software/librejs/
[project repository]: https://git.snowdrift.coop/sd/snowdrift
[PureScript]: http://www.purescript.org/
[README]: README.md
[Shakespearean Templates]: http://www.yesodweb.com/book/shakespearean-templates
[SSH setup]: https://git.snowdrift.coop/help/ssh/README
[Taiga Issues]: https://tree.taiga.io/project/snowdrift-dev/issues
[TEXTEDITORS.md]: TEXTEDITORS.md
[Unobtrusive JavaScript]: http://en.wikipedia.org/wiki/Unobtrusive_JavaScript
[wiki]: https://wiki.snowdrift.coop/
[Yesod JavaScript Options]: https://github.com/yesodweb/yesod/wiki/JavaScript-Options
