# Development Tools

### Execution Notes

* We recommend adding the absolute path to dev-tools to your PATH variable, such
as by editing ~/.bash_profile. This way, you may simply run `foo [args]` instead
of `dev-tools/foo [args]`.

## Tools

### tag.sh
Produces tags for definition lookup. See *Other Files*.

### keter.sh and company
The script keter.sh is like Yesod's "keter" command, but for multi-package
projects. It creates a distributable tarball. For more info on similarly named
scripts, please view their comments.

### devbuild & prodbuild
They print the development & production versions of the project, respectively.
They parse strings from the web. Unfortunately, neither work at the moment.

## Other Files
The tagging script produces the following Git-ignored files; see comments in
the script for details:

* linked-for-non-Cabal-tagging

* dummy.cabal
