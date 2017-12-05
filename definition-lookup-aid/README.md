# Definition Lookup Aid

## The Need

We need the ability to know what things mean. We can't develop or debug without
it. Hence, we need the ability to look up the definition of any term.

Specifically, we are talking about terms defined in Haskell, since this is
primarily a Haskell project.

We'd like to simply select a term from within a Haskell source code file (\*.hs)
and jump to its definition. However, Haskell tooling is still immature: we're
missing the simple ability to look up definitions, at time-of-writing.

## A Partial Solution

Instead, we have something more like Google. Say, for example, you encounter a
term called _fp_. Now you want its definition. The currently available tooling
searches for all terms named _fp_ in the whole project. You then choose from the
search results, and you are taken to the definition of the _fp_ **you
chose**. This is less than what we know is possible, since the compiler must
know the exact definition of each term. The tooling just isn't up to the level
of the compiler in this regard.

The lookup functionality we do have comes from having a "tags" file in the
project's root directory and using an IDE / text editor that supports such a
tags file. The tags file lists (indexes) the line number and file of each term's
declaration in the project. We use Hasktags to generate this file, and it lets
you choose the tag file format: ctags, etags, or both. The Atom text editor uses
ctags and looks for a file named "tags" in the project root - no file extension,
just "tags".

Running `hasktags -c .` from the project's root directory will search the whole
project for Haskell source code files and index them all, placing a tags file in
the project root. To install Hasktags, do: `stack install hasktags`.

**Note:** [TEXTEDITORS.md] discusses tags and a "git-ified" way of running
Hasktags. You may also be interested in a tag-generating tool called Codex.
Additionally, there are online lookup services for searching within Hackage
packages for functions and other terms: [Stackage], [Hoogle], and [Hayoo].
No one is complete, each searches different parts of Hackage. Thank you, Bryan,
for telling us about these. These tools also allow you to refine your search by
data type: a functionality ctags do not have.

## An Issue

However, the parital solution discussed above is not enough on its own to
produce tags for anything outside the project, such as the _fp_ mentioned
earlier, which is defined in the Turtle package, which is outside the Snowdrift
project. Even though _fp_ is defined outside the project, it *is* used *in* the
project (in [sdb.hs]). So it is still our business to know what it means.

Since the above, on its own, only generates ctags for terms defined in the
project, it would not allow us to look up the definition of _fp_, or any other
term defined outside the project.

## A Whole Solution

First, we unpack every Haskell dependency of the project into a dedicated
directory within in the project directory. Then we run `hasktags -c .` from the
project root directory. Since that command recursively searches the whole
directory, it will generate tags for both project code and dependency code,
now that the dependencies are in the project directory. However, we add the
dependency directory to .gitignore so the dependencies won't clutter our repo.

We automate this unpacking with `unpack.sh`. It makes a directory called
"unpacked-for-tagging" and puts all in the dependencies in there.

Please note there may be some acceptable errors: if it says that a package could
not be unpacked because its directory already exists, that means we've already
unpacked it: either unpack.sh was already ran, or a script dependency happened
to also be a compilation dependency and was unpacked in an earlier step of
unpack.sh. The other acceptable error is if  a package was not found in your
indices, but we got it elsewhere (see below).

## Maintenance

However, this solution only works with maintenance. There are two things we need
to compensate for:

* The script uses "stack unpack" to download and unpack a given dependency. This
command only gets packages (dependencies) from Hackage. Therefore, for each
dependency not available on Hackage, we must add some code to the script to get
it. This is well documented and exemplified in the script. Open it in a text
editor and see Step Three. Caution: a dependency may appear to be on Hackage,
but the needed version is not. In that case, you have to add code to Step Three.
So if you introduce a dependency not from Hackage, please add the needed code
to unpack.sh as well as to the appropriate cabal/yaml file.

* The script uses "stack list-dependencies" to get the list of
dependencies, that it then passes to "stack unpack". The former only lists
dependencies discoverable from a given stack.yaml file. Concerning Haskell
dependencies, the project's [main stack.yaml] only reveals those needed for
compilation. Well, in addition to compiled Haskell code, we have one Haskell
script at time-of-writing: [sdb.hs]. Haskell scripts are not
compiled, but they do have dependencies. In order for [unpack.sh] to know what
they are, we have made files "stack.yaml" and "scripts.cabal" in this directory
to specify script dependencies. But the latter is not automatically updated: if
you create a Haskell script, please add its dependencies to scripts.cabal with a
heading comment saying what script needs what dependency. (Don't worry about
the stack.yaml in this directory, it's a dummy file.)


The following commands may be helpful for maintenance:

1. `cd ./definition-lookup-aid`

    Run unpack.sh, writing to both the terminal and unpack.log, redirecting
    stderr to stdout:
2. `./unpack.sh 2>&1 | tee unpack.log`

    The below lists packages not found in your Hackage indices. Ignore the
    Snowdrift packages Snowdrift, crowdmatch, run-persist and "scripts". They
    are of course local and not pulled from Hacakge. The others need to be
    handled in Step Thee of unpack.sh.
3. `cat unpack.log | grep "not found"`


In the future, it may be worth concocting some script that runs through and
gets the dependencies of all Haskell scripts, saying what script uses what
dependency. But for now, it wouldn't be worth the time because the only Haskell
script in the project is [sdb.hs].

## Files

* [unpack.sh]     -- a tool that ultimately enables us to look up definitions of
                     terms defined outside our project. I.e., terms used from
                     dependencies. However, you still have to run Hasktags and
                     use a supporting IDE / text editor.

* [scripts.cabal] -- Allows [unpack.sh] to know the dependencies of Haskell
                     scripts.

* [stack.yaml]    -- An automatically generated dummy file that exists to
                     appease the commands used in unpack.sh. It and
                    [scripts.cabal] form a fake package, thoroughly explained in
                    [scripts.cabal] and in the comments of [unpack.sh].

[Stackage]: https://www.stackage.org/
[Hoogle]: https://www.haskell.org/hoogle/
[Hayoo]: http://hayoo.fh-wedel.de/

[sdb.hs]: ../sdb.hs
[TEXTEDITORS.md]: ../TEXTEDITORS.md
[unpack.sh]: unpack.sh
[scripts.cabal]: scripts.cabal
[main stack.yaml]: ../stack.yaml
[stack.yaml]: stack.yaml
