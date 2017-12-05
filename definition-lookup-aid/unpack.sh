#!/bin/bash

# Summary:
#   1. Make directory to unpack into.
#   2. Download and unpack the packages needed for compilation that are
#       available from Hackage.
#   3. Get the remaining packages needed for compilation. We pull these from
#       their online repos. We do this if the desired version is not available
#       on Hackage or if it is not on Hackage at all.
#   4. Download and unpack the Haskell script dependencies. These are not needed
#       for compilation, since the Haskell scripts are never compiled. Thus
#       their exclusion from steps 2 and 3, which use compilation-related files.



################################################################################
#                                                                              #
#                  STEP ONE: Make dirctory to unpack into.                     #
#                                                                              #
#                    We'll name it "unpacked-for-tagging"                      #
#                and put in the same directory as this script.                 #
#                                                                              #
################################################################################



# "$0" gives us the path to this script. NB: Don't forget to quote these things.
# Applying dirname to "$0" takes off the last part (the script file name and
# preceding slash) to obtain just the path to the directory that contains this
# script.
scriptDir=`dirname "$0"`

# Now we make a directory to unpack the dependencies into, using mkdir.
# We place that directory in the same directory that contains this script.
# No worries: if the directory already exists, mkdir does nothing and exits
# succesfully. And again: quote Bash variables where used.
# NB: I originally had this directory hidden (proceeded with a dot), but
# Hasktags ignores hidden directories.
mkdir "$scriptDir"/unpacked-for-tagging

# Ok. Now we go there.
cd "$scriptDir"/unpacked-for-tagging



################################################################################
#                                                                              #
#    STEP TWO: Download and unpack the packages needed for compilation that    #
#                         are available from Hackage.                          #
#                                                                              #
################################################################################



# The "stack list-dependencies..." command below reads stack.yaml, takes note of
# all dependencies and local packages listed there and recursively follows them,
# noting their dependencies, the dependencies of those dependencies, and so on.
#
# At time of writing, two of these dependencies are not available on Hackage, or
# at least not the desired version. The "list-dependencies" command
# is not hindered by this: it can read the yaml file out of a git repo, provided
# our local yaml/cabal files give the url.
# However, the "stack unpack" command only gets things from Hackage. Therefore,
# those two remaining dependencies will have to be acquired differently, and
# that is what Step Three does.
#
# We have Michael Sloan to thank for this "Bash hack".
# See https://github.com/commercialhaskell/stack/issues/1843 .
# The "../../stack.yaml" specifies that we are using the stack.yaml in the
# project's root directory, not the one in the definition-lookup-aid directory.
# The root one specifies dependencies for compilation, the other, dependencies
# for scripts (which are not compiled).
stack list-dependencies --test --bench --separator "-"\
    --stack-yaml ../../stack.yaml | while read pkg; do stack unpack $pkg; done



################################################################################
#                                                                              #
#       STEP THEE: Get the remaining packages needed for compilation.          #
#                                                                              #
################################################################################



# Grabbing the remaining compilation dependency listed first in the root
# stack.yaml:
git clone https://git.snowdrift.coop/sd/postgresql-simple-migration-patches.git
cd postgresql-simple-migration-patches  # Going in directory.
git checkout 55ae21a7ee5b0386ec7c6909409b1bdacebcc924 # Getting desired version.
cd .. # Comming back out

# And now we grab the second such dependency:
git clone https://notabug.org/fr33domlover/shakespeare-sass.git
cd shakespeare-sass # Going in directory.
git checkout 70dc41d6df75e77d94125ad8a8613868d8d76b03 # Getting desired version.
cd .. # Coming back out.



################################################################################
#                                                                              #
#             STEP FOUR: Unpack the Haskell script dependencies.               #
#                                                                              #
################################################################################



# In the definition-lookup-aid directory, we have a fake Stack project that
# contains no source code of its own. It exists to specify the dependencies of
# our Haskell scripts so that we can use the below commands to gather up their
# dependencies. The "../stack.yaml" specifies that we're using the fake
# project's stack.yaml, not the main project's stack.yaml.
# There may be some errors that a package's directory already exists, so that
# package could not be unpacked. That is OK: that just means we've already
# unpacked one or more dependencies from Step Two that so happen to be both a
# compilation dependency and a script dependency. Or we've already ran this
# script.
stack list-dependencies --test --bench --separator "-"\
    --stack-yaml ../stack.yaml | while read pkg; do stack unpack $pkg; done
