#!/bin/bash

# Summary:
#   1. Find some places and `cd`.
#   2. Prep things for Codex so that it generates *all* the tags we could want.
#   3. Run Codex to get our tags!


################################################################################
#                                                                              #
#                      Step One: Find Some Places and `cd`                     #
#                                                                              #
################################################################################

# Getting the directory that contains this script:
scriptDir="`dirname "$0"`" # a.k.a "dev-tools", at time-of-writing.

# Make sure we're in the project so that Stack commands work as desired:
cd "$scriptDir"

# Getting the project root path:
projRoot="`stack path --project-root`"


################################################################################
#                                                                              #
#                       Step Two: Prepare Things for Codex                     #
#                                                                              #
################################################################################

# On its own, Codex cannot get the non-Hackage, Git-hosted deps mentioned in
# stack.yaml. So we make a link to Stack's "downloaded" stash and produce a
# dummy Cabal file to get Codex to tag those non-Hackage packages.

# Make sure the non-Hackage packages are downloaded:
# (Note: `stack update` does not necessarily fetch those non-Hackage packages
#  mentioned in stack.yaml, hence the "--dry-run" tactic instead.)
stack build --dry-run

# Codex does not explore hidden directories. We make a link to get around that:
# (It's ok to force this (-f): we won't delete anything important.)
ln -sf "$projRoot"/.stack-work/downloaded .

# Make sure it has a dummy.cabal file, to get Codex to tag it:
touch ./downloaded/dummy.cabal


################################################################################
#                                                                              #
#                   Step Three: Run Codex to get our tags!                     #
#                                                                              #
################################################################################

# Move into the project root:
cd "$projRoot"

# | Set a flag that tells Codex to search sub-directories for Cabal packages.
# Otherwise, it'd assume that the current directory is its target package.
export CODEX_DISABLE_WORKSPACE=true

# Make those tags:
codex update
