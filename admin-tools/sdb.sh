#!/bin/bash

# NB: Remember to quote Bash variable dereferences, with few exceptions.
# For example, "$0" and "$@" are quoted (usage explained below).

# Here, we decide where we'll put the hidden database directory, .postgres-work.
# At time of writing, we do so relative to the project root. This requires us to
# dertermine where that is and construct the target directory from there.
#
# Note that "$0" is the path to this script. This is key to finding the root.
# More on this later.
#
# We do *not* find the project root relative to where the sdb binary is,
# because, at time of writing, we are not in control of where the binary goes:
# Stack is.
#
# Note our below method is independent of our current tooling: it works no
# matter whether we're using Stack, Nix or what have you, making project
# transitions simpler. (As opposed to "stack path | grep project-root", which
# would obviously be Stack-dependent.)
#
# Recall that dirname takes off the last part of a path string, while the
# double dot is a link to the parent directory. At time of writing, the path to
# this script is: "[project root]/admin-tools/sdb.sh". Dirname takes off the
# "/sdb.sh", double dot takes us to the project root.
#
# Back-ticks plop a returned string into a bigger command.
projectRoot="`dirname "$0"`/.."

# Now that we have the project's root directory, we can place .postgres-work
# relative to it. For the time being, we'll just put it right at the root.
# "dbParent" stands for "database parent directory". This is where we will
# place ".postgres-work".
# NB: update the ".postgres-work" entry in .gitignore if you're moving this
# and are tracking your changes with git.
export dbParent="$projectRoot" # This is the one we export, i.e., make visible
                               # to sdb below.

# The below is pretty simple to adjust if we move away from Stack.
# Use the appropriate command to lauch sdb, passing the parameters given to
# this script onto sdb. Note that "$@" represents all parameters given.
#
# Also, Stack needs the current directory to be at the project root or deeper
# in order to know what project it is on to know which sdb to execute (in case
# there's another project with another executable named "sdb"). Hence the "cd".
# We use "projectRoot" and not "dbParent" for the "cd". This is for if the user
# wishes to place .postgres-work outside of the project directory.
cd "$projectRoot"
stack exec sdb "$@"



###################
# Troubleshooting #
###################

# Make sure "projectRoot" accurately gets to the project root. If this script is
# moved, that must be adjusted.
# Also, make sure the command "stack exec sdb" works on its own. If not, sdb
# may need to be built (have you ran build.sh?), and/or Stack may need to be
# installed. Make sure you run that command from within the project directory
# or deeper.
