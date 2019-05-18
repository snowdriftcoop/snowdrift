# Snowdrift Beginning Contributor's Guide

This guide is written to work for even novice programmers. Advanced readers may
adapt the instructions as they see fit.

## Get in touch!

Reading this guide is *not* a prerequisite to contacting us. Feel free to
connect and engage with real people in the community right away, if only to let
us know about your interest in contributing. See our [contact options].

## Licensing note

When you contribute patches to this repository, such as via a merge request, you
retain the copyright to your contributions. In doing so, you agree that the
patches are under under the same licenses we use (GNU AGPLv3+ for program code
and non-program text and graphics under CC BY-SA 4.0 International, as specified
in the [README]). Other compatible licensing can work with special notice.

## Prerequisites to contributing

* Decently powerful computer (probably less than 10 years old, ideally 4GB+ RAM)
* GNU/Linux, \*BSD, or macOS
    * We do not support Windows at this time. We suggest Windows users switch
      systems overall or use a virtual machine. If you really want to help test
      Snowdrift on Windows, our [Build guide] has some notes about that.
* Basic ability to use terminal command-line-interface (CLI)

## Installing

**Follow the [Build guide]** to get Snowdrift going on your computer.

## Working on the code

**All the following assumes you have done the initial clone and install
according to the instructions linked above.**

### Text-editors and settings

Any code-appropriate text-editor will work, but we recommend those with stronger
Haskell and Yesod support. See [TEXTEDITORS.md] for our specific recommendations
and settings.

### Working with Git

Recommended workflow:

* Keep local master synced to the upstream main project master
* Do all work on new branches
* Use separate branches for independent work
* Once branches have been published and merge-requests opened, please retain the
  full list of commits and only rebase when requested.

#### Basic Git setup for collaboration

The following covers the bare minimum process for those new to Git.

We collaborate on code using [GitLab]. We also mirror on the popular but proprietary site [GitHub] but do not use that regulary.

1. Locally clone the project from the main Snowdrift repo to your computer as
   described in the BUILD guide
2. Fork the project at GitLab.com
    * sign in or create an account on [GitLab.com](https://gitlab.com/users/sign_in)
    * go to the [project repository]
    * click the "Fork" link (toward the top of the page)
    * choose your account for the fork
    * you should end up at your fork (to check, see that the header at the top
      of the page has your account name followed by "/Snowdrift")
3. Add your GitLab fork as a remote for your local clone
    * While we recommend SSH, those not comfortable with the full [SSH setup]
      can use HTTPS instead.
    * For HTTPS, choose that instead of SSH from the dropdown menu on the page
      of your fork at GitLab
    * copy the address (which looks like
      `https://gitlab.com/YOURNAME/snowdrift.git` where `YOURNAME` is
      your GitLab username).
    * using the correct address, enter a modified version of this command in the
        directory of your local clone:
        `git remote add my-snow https://gitlab.com/YOURNAME/snowdrift.git`

#### Updating your local code to snowdrift master

Whenever you begin new work, you should first get the latest master code from
the Snowdrift project:

* In your local snowdrift project directory,
* assuming you have the main snowdrift code as your "origin" (verify with
  `git remote -v`),
* with the master branch checked out (`git checkout master`),
* run `git pull`

You should have no conflicts because this is the only situation where you
should ever change your local master. **Work should be done on other branches.**

#### Starting a new branch

From the master branch, having pulled the latest updates, create a new branch:

    git checkout -b some-branch

Replace `some-branch` with a short descriptive name (with hyphens, not spaces)
for your planned changes. For example, when fixing a problem in the header, a
good branch name could be `header-fix`.

#### Working on the code

On a working branch, you can make your edits/additions etc. to the code files.

To review the status and save important points in your progress, learn about the
staging process and commands like `git add`, `git status`, and `git diff`. See
the links at the end of this file for learning resources.

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

Though it isn't best practice, the simplest command for beginners to commit all
changes (though any *new* files need `git add` first) is:

    git commit -a

An editor will show asking you to summarize your changes. On the first line,
write a short commit title that will be meaningful to people skimming all the
commits in the future. Add any further comments about the work on additional
lines below the title. Then save and close the editor.

#### Getting your changes merged

When you are ready to share your work (one or more commits all relevant to the
same overall update), you have confirmed that everything works as intended, and
all tests pass, run `git status` to make sure no work is missing and all new
files were committed.

Next, send your changes to your GitLab account with:

    git push -u my-snow some-branch

Changing `some-branch` to the name of the branch where you made the commit(s).

To notify the snowdrift team about your work, visit the GitLab page
with your fork. You should see a button called **"Create Merge Request"**. Click
that to bring up a form where you can add further notes about your work
(especially useful if you are merging multiple commits). You may ignore "Assign
to", "Milestone", and "Labels" at this point.

After you submit the merge request, someone should comment on your submission
soon (hopefully within a few hours, maybe a day or two depending on timing).

## Choosing what to work on

Several ways to get started contributing and/or to learn more overall:

* See the "newcomer-friendly" tag in our [issues] and consider working on any
  item not already assigned to someone and with no "blocked" tag.

* Play around with the site locally. See if you can understand what does what.
  You may find bits that seem incomplete or confusing, and you can explore them
  and/or check with others about the status, such as whether the issue is known
  or tickets exist already.

* Explore the code files and see if you can figure out how things fit together.

    * For non-Haskell work:
        * The .hamlet files in the /website/templates directory are comparable
          to basic HTML using white-space to avoid the need for closing tags
          (plus some other logic that connects to the Haskell). For more
          details, see the documentation on [Shakespearean Templates].
        * Any .julius files are containers for JavaScript.
        * For CSS, we use [Sass]

    * For those familiar with Haskell, one starting option: explore our files
      and update any code that doesn't match our code style described below.

* Read the code documentation in this repo and related pages on our [wiki].

* Read on below and check out links to learn more about the overall ecosystem,
  our development practices, and the tools we use.

## Development guidelines and notes

Overall, we strive to follow universal standards, be fully accessible,
and avoid browser-specific code.

### Design considerations

We have a separate [design] repo for design tasks, our design guide and so on.

### Code style

We suggest using [brittany](https://hackage.haskell.org/package/brittany) as a
code formatter. Besides that, consider:

* Sentence-case, 50-column Git commit headers (but no ending period)
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
  those modules that need them, then external modules, and then internal
  modules.

* Additionally, you can use hlint to get suggestions for improving code style:

    ```
    stack install hlint
    hlint -XQuasiQuotes -XTemplateHaskell $FILE
    ```

### Code review

As a best practice, we want adequate code review for every merge before it goes
into the master code.

To help with code review, please choose to *watch* the code repository at
[] (and perhaps also the [GitHub] mirror if you use GitHub).
Then, you can make general and in-line comments about new code as it is
committed and before it goes into the master repository.

### Use of JavaScript

**We generally build with *progressive enhancement* in mind.** The site should
work with just HTML/CSS along with Yesod/Haskell server-side functions. Given
that basis, we can add JavaScript as appropriate for enhancement, considering
[Unobtrusive JavaScript]. Use of NoScript should never cause a broken
experience. All our JavaScript should be recognized by the FSF's [LibreJS plugin].

Although we haven't used them as of May 2018, we have considered [GHCJS] and
[PureScript] as options for more Haskell-connected ways to generate JavaScript.
If contributors want to work with either of those, we would happily accept that.
[Yesod JavaScript Options] explains further about those or other possibilities.

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

    * The [Git Docs](https://git-scm.com/doc/) page includes many links, an
      online version of the core Git manuals, and the full Pro Git book which
      uses the CC-BY-NC-SA license, so it is shareable but not fully FLO,
      unfortunately.

*   [CanIUse.com](https://caniuse.com/) is a reference website to check that all
    web features you use are compatible with various browsers and standards.
    The CanIUse data is fully FLO under a CC-BY license.

*   [The Yesod Book](https://www.yesodweb.com/book/) is the primary resource
    for learning about Yesod, the web framework we use to develop Snowdrift.

*   The [School of Haskell](https://www.schoolofhaskell.com) includes
    basic and advanced topics including some Yesod sections.

*   At Stack Overflow (which uses FLO licensing for content), see tags for
    [yesod](https://stackoverflow.com/questions/tagged/yesod) and
    [haskell](https://stackoverflow.com/questions/tagged/yesod)
    (and, of course, other topics like HTML, CSS, Sass, Git, and so on)

*   Alongside #snowdrift on freenode.net IRC, check out #yesod , #haskell ,
    and #haskell-beginners (among many other relevant channels).

[Build guide]: BUILD.md
[contact options]: https://snowdrift.coop/contact
[design]: https://gitlab.com/snowdrift/design
[GitLab]: https://gitlab.com/snowdrift/snowdrift
[GHCJS]: https://github.com/ghcjs/ghcjs
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[issues]: https://gitlab.com/snowdrift/snowdrift/issues?label_name%5B%5D=newcomer-friendly
[LibreJS plugin]: https://www.gnu.org/software/librejs/
[project repository]: https://gitlab.com/snowdrift/snowdrift
[PureScript]: http://www.purescript.org/
[README]: README.md
[Sass]: https://sass-lang.com
[Shakespearean Templates]: https://www.yesodweb.com/book/shakespearean-templates
[SSH setup]: https://gitlab.com/help/ssh/README
[TEXTEDITORS.md]: TEXTEDITORS.md
[Unobtrusive JavaScript]: https://en.wikipedia.org/wiki/Unobtrusive_JavaScript
[wiki]: https://wiki.snowdrift.coop/
[Yesod JavaScript Options]: https://github.com/yesodweb/yesod/wiki/JavaScript-Options
