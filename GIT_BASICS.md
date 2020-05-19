# Git Basics

Snowdrift.coop strives to be approachable even to novice programmers. If you are not comfortable working with git, this guide should be enough to get started. If you are comfortable, you probably won't need this.

## Basic Git setup for collaboration

The following covers the bare minimum process for those new to Git.

We collaborate on code using [GitLab]. We also mirror on the popular but proprietary site [GitHub] but do not use that regularly.

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

## Updating your local code to snowdrift master

Whenever you begin new work, you should first get the latest master code from
the Snowdrift project:

* In your local snowdrift project directory,
* assuming you have the main snowdrift code as your "origin" (verify with
  `git remote -v`),
* with the master branch checked out (`git checkout master`),
* run `git pull`

You should have no conflicts because this is the only situation where you
should ever change your local master. **Work should be done on other branches.**

## Starting a new branch

From the master branch, having pulled the latest updates, create a new branch:

    git checkout -b some-branch

Replace `some-branch` with a short descriptive name (with hyphens, not spaces)
for your planned changes. For example, when fixing a problem in the header, a
good branch name could be `header-fix`.

## Working on the code

On a working branch, you can make your edits/additions etc. to the code files.

To review the status and save important points in your progress, learn about the
staging process and commands like `git add`, `git status`, and `git diff`. See
the links at the end of this file for learning resources.

## Building your updates

To check your work and see the results on the running site, follow the
instructions in the [Build guide] for running the site.

## Running the tests

When you are happy with your work, it compiles, and looks right, run the tests:

    ./build.sh test

If there are any failures either when compiling or testing, and you don't know
how to fix the issue or don't understand the error, contact us for help.

## Committing your changes

When your updates all compile, tests pass, and you are ready to submit to the
main Snowdrift project, *commit* your changes.

Though it isn't best practice, the simplest command for beginners to commit all
changes (though any *new* files need `git add` first) is:

    git commit -a

An editor will show asking you to summarize your changes. On the first line,
write a short commit title that will be meaningful to people skimming all the
commits in the future. Add any further comments about the work on additional
lines below the title. Then save and close the editor.

## Getting your changes merged

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

## Further reading

Some Git resources we suggest:

* [Git Wikibook](https://en.wikibooks.org/wiki/Git) is incomplete but has some useful bits.

* [Git for Ages 4 and Up](https://www.youtube.com/watch?v=1ffBJ4sVUb4)
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
