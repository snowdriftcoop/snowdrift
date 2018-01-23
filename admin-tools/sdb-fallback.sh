#!/bin/bash

# | This is a fallback to sdb.sh. It builds the sdb binary locally and runs it.
# For more info, please read the comments in sdb.sh. Very similar commands are
# used and documented there.

dirOfScript="`dirname "$0"`"
projectRoot="$dirOfScript"\..
cd "$projectRoot"

# Build binary locally:
stack exec ghc "$dirOfScript"/sdb.hs "$dirOfScript"/sdb-fallback

# Set it's dbParent environmental variable and run it:
dbParent="$projectRoot" "$dirOfScript"/sdb-fallback "@"
