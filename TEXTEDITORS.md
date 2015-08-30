# Text Editor Settings and Recommendations

This guide covers our recommended options for fully-FLO text-editors and IDE
development tools for hacking on Snowdrift.

## Atom

[Atom](https://atom.io/) is a modern, graphical, highly-extensible text-editor,
good for beginners and advanced alike. It is relatively new, and Haskell support
is continually improving.

### Atom settings

Some settings to consider:

* Tab Length: 4 (easy enough to enter 2 spaces for hamlet)
* Packages/Tree View: turn on "Hide Ignored Names" and "Hide VCS Ignored Files"
* Disable the "metrics" package to stop Atom sending data to Google Analytics

### Atom packages

We recommend adding at least the Atom packages:
[language-shakespeare](https://atom.io/packages/language-shakespeare) and
[language-haskell](https://atom.io/packages/language-haskell).

Other useful Atom packages to consider:

* Various Git-related tools:
    * [tree-view-git-status](https://atom.io/packages/tree-view-git-status)
    * [git-blame](https://atom.io/packages/git-blame)
    * [show-origin](https://atom.io/packages/show-origin)
    * searching will show many other git tools worth considering
* General nice tools like [minimap](https://atom.io/packages/minimap)
* [haskell-hoogle](https://atom.io/packages/haskell-hoogle)
* [vim-mode](https://atom.io/packages/vim-mode) (if you like vim style editing)
    * consider the Clipboard as Default Register and Smartcase Search options
    * incidentally, many normal Atom editing operations still work as well
    * search packages for "vim-mode" to see extra addons

### Tags to jump to function definitions in Atom

* run `stack install hasktags`
* in /snowdrift directory, run `hasktags -x -c ./*`
    * This must be re-run any time you want tags updated for newer code.
* Use Ctrl-Shift-R to jump to any tag or Ctrl-Alt-Down to jump to the
  definition of the symbol under the cursor using (or the Vim equivalents Ctrl-]
  and Ctrl-t for vim-mode).
    * This works for our internal functions only. For outside functions, search
      [Hayhoo](http://hayoo.fh-wedel.de/).

### Atom Haskell IDE

[Atom ide-haskell](https://atom.io/packages/ide-haskell) offers many more useful
features but is not yet compatible with Stack. Once it is, we will recommend
installing it and will include instructions here.

## Emacs

[GNU Emacs](https://www.gnu.org/software/emacs/) is a traditional, robust,
keyboard-centric text editor with substantial Haskell support.

### Emacs packages

Emacs users should install
[Haskell Mode](https://github.com/haskell/haskell-mode) and
[Shakespeare Mode](https://github.com/CodyReichert/shakespeare-mode). Both
of these are installable via ELPA.

If you are new to Emacs,
[Emacs Prelude](https://github.com/bbatsov/prelude) is an enhanced
distribution of Emacs with a much saner default configuration. It
includes things like [Helm](https://github.com/emacs-helm/helm) and
[Projectile](http://batsov.com/projectile/) by default.

If you are coming off of Vim,
[Spacemacs](https://github.com/syl20bnr/spacemacs) is another Emacs
distribution that includes Vim's modal keybindings by default, along
with much of Prelude's saner default configuration. If you don't want to
use Spacemacs, you can get the Vim keybindings in any Emacs distribution
using [evil-mode](http://www.emacswiki.org/emacs/Evil).

Also of interest:

* [Magit](http://magit.vc/) is a very high-quality git interface
  integrated within Emacs.
* [Structured Haskell Mode](https://github.com/chrisdone/structured-haskell-mode),
  by the author of haskell-mode, takes a lot of tedium out of Haskell
  editing.
* [HIndent](https://github.com/chrisdone/hindent/), by the same author
  as SHM and haskell-mode, will pretty-print your Haskell-code. It is
  along the same lines as SHM.

### Emacs settings

Our included `.dir-locals.el` file
[makes Emacs use](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
the recommended 4-space indentation.

## Leksah

[Leksah](http://leksah.org/) is a Haskell-dedicated complete IDE in the works,
but we're waiting for integration with Stack before we fully recommend it.

## Vim

[Vim](http://www.vim.org/) is a robust traditional editor with a command-line
style interface and substantial Haskell support.

Note: we recommend all vim users install `gvim` to enable access to system
clipboards, regardless of using terminal-based vim or the gvim interface with
visual menus.

### Vim settings

We recommend that your ~/.vimrc file include:

    syntax on
    set number textwidth=80 expandtab shiftwidth=4 tabstop=4
    au FileType hamlet setl sw=2 sts=2 et

Many other settings are nice, but opinions vary about the details.

### Vim plugins

We recommend using a Vim plugin manager such as
[Vundle](https://github.com/VundleVim/Vundle.vim)
and the following plugins particularly relevant to snowdrift:

* [vim-syntax-shakespeare](https://github.com/pbrisbin/vim-syntax-shakespeare)
* [haskell-vim](https://github.com/neovimhaskell/haskell-vim)
* [vim-markdown](https://github.com/plasticboy/vim-markdown)
* [vim-gitgutter](https://github.com/airblade/vim-gitgutter)
* [vim2hs](https://github.com/dag/vim2hs)
    * optional: add `set nofoldenable` to .vimrc to stop vim2hs function folding

The plugins listed above mostly do syntax highlighting and do not affect
commands or basic operations, so they are safe for everyone to use without
hesitation or learning process.

Other plugins we suggest for consideration include some which take zero or
near-zero learning to use:
[vim-sensible](https://github.com/tpope/vim-sensible),
[vim-repeat](https://github.com/tpope/vim-repeat),
[vim-supertab](https://github.com/ervandew/supertab),
[vim-gitgutter](https://github.com/airblade/vim-gitgutter),
[vim-surround](https://github.com/tpope/vim-surround),
[vim-commentary](https://github.com/tpope/vim-commentary);
and others which take some minimal learning:
[vim-fugitive](https://github.com/tpope/vim-fugitive),
[undotree](https://github.com/mbbill/undotree),
[vim-easyclip](https://github.com/mbbill/undotree).

*Many* other options exist, although we'd rather contributors generally focus
more on building Snowdrift than maximizing their Vim expertise.

### Tags to jump to function definitions in Vim

* Run `stack install fast-tags`
* In /snowdrift directory, run `fast-tags ./*`
* For auto-update, add `au BufWritePost *.hs silent !init-tags %` to ~/.vimrc
* Then, in any Haskell file, use Ctrl-] to jump to the definition of the symbol
  under the cursor, and Ctrl-t to jump back.
    * This works for our internal functions only. For outside functions, search
      [Hayhoo](http://hayoo.fh-wedel.de/).
