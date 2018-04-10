# Development Tools

## test
Just runs the Stack tests. Does not launch site. Does not take arguments. 

## tag.sh
[Placeholder section. Script coming from separate MR.]
Produces `dummy.cabal` and `linked-for-non-Cabal-tagging`.

## keter.sh and company
The script `keter.sh` is like Yesod's "keter" command, but for multi-package
projects. It creates a distributable tarball. For more info on similarly named
scripts, please view their comments.

## devbuild & prodbuild
They print the development & production versions of the project, respectively.
They parse strings from the web. Unfortunately, neither work at the moment.

# Other Files
The tagging script produces the following Git-ignored files:

* `linked-for-non-Cabal-tagging`: A soft link to the non-Hackage, Git-hosted
deps downloaded by Stack. This link is part of a plot to get Codex to tag those
deps.

* `dummy.cabal`: An empty file created to get Codex to crawl into
`linked-for-non-Cabal-tagging` and tag away.
