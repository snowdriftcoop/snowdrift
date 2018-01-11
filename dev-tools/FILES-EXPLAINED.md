# keter.sh
Builds a deployable Keter bundle. This is like creating a deployable zip or
tarball, but for use with Yesod's deployment system, called "Keter".

Ideally, we wouldn't have this script: we'd use the "yesod keter" command
instead. However, that command currently does not support multi-package
projects. Until Yesod gains that functionality, we'll have to use this makeshift
script.

# tag.sh
The ultimate purpose of this script is to enable us to look up the definition of
any function used in our Haskell code. In fact, it also works for Haskell values
that are not functions, provided the value has its own declaration and is not
a "where" or "let" term in a bigger declaration.

However, doing so is a multi-step process. In one of those steps, we generate a
"tags" file that lists each such value by name, along with the file containing
its declaration and the line number of that declaration. This is called
"tagging" the source. A text editor may load this tags file and let you search
for a particular named value, such as a function.

Our task is made more challenging when we import stuff from outside our repo.
For example, in sdb.hs, we use the "fp" function. The fp function is not defined
in our repo. It is defined in the Turtle module, which comes from a Hackage
package of the same name. Now suppose someone wants to lookup the definition of
"fp". How are we going to get them to that definition if it's not in our repo?

Well, we have to download and unpack all such external source code and tag all
of it in addition to what's in our repo.

So, provided this script is properly maintained, it downloads all of the
project's dependencies into a git-ignored subdirectory and then tags all
declarations in all Haskell source files in the whole project directory. This
tags all Haskell Snowdrift code as well as all of its dependencies.

Like keter.sh, we would ideally *not* have tag.sh. We'd prefer to run
"codex update" to generate a tags file as described: that is the purpose of the
Codex utility. But unfortunately, it only downloads dependencies that are
available from Hackage. At time of writing, we have two dependencies that are
not. One is the particular version of shakespeare-sass we need. Although
shakespeare-sass is available on Hackage, the version we need is not. The other
is a Snowdrift package on another repo for data migration.

So, until Codex is able to download from online git repositories, as specified
by stack.yaml, we have to use this makeshift script.

# prodbuild & devbuild
Intended to print the current production and development version numbers,
respectively. However, they currently do not work.

# build.sh
Though build.sh is a development tool, we keep it and its documentation,
BUILD.md, at the project root, due to convention. It is not here in the
dev-tools subdirectory.
