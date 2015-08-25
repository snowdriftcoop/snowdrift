# Text Editor Settings and Recommendations

This guide covers our recommended options for fully-FLO text-editors and IDE
development tools for hacking on Snowdrift.

## Atom

[Atom](https://atom.io/) is a modern, graphical text-editor, good for beginners
and advanced alike. It is relatively new and Haskell support is continually
improving.

We recommend adding at least the Atom packages:
[language-shakespeare](https://atom.io/packages/language-shakespeare) and
[language-haskell](https://atom.io/packages/language-haskell)

[Atom ide-haskell](https://atom.io/packages/ide-haskell) offers many more useful
features but is not yet compatible with Stack. Once it is, we recommend
installing it and will include instructions here.

Other useful Atom packages to consider:
[haskell-hoogle](https://atom.io/packages/haskell-hoogle) (search Hoogle within
Atom) and [vim-mode](https://atom.io/packages/vim-mode) (vim keybindings for
those who prefer them).

### Emacs

[GNU Emacs](https://www.gnu.org/software/emacs/) is a traditional, robust,
keyboard-centric text editor with substantial Haskell support.

Emacs users should use a package manager (preferably Marmalade) to install
[Haskell Mode](https://github.com/haskell/haskell-mode)
and
[Shakespeare Mode](https://github.com/CodyReichert/shakespeare-mode).

Our included file
[`.dir-locals.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
makes Emacs use the recommended 4-space indentation.

## Leksah

[Leksah](http://leksah.org/) is a Haskell-dedicated complete IDE in the works,
but we're waiting for integration with Stack before we fully recommend it.

## Vim

[Vim](http://www.vim.org/) is a robust traditional editor with a text-command
interface and substantial Haskell support.

Note: we recommend all vim users install `gvim` to get access to system
clipboards, regardless of using terminal-based vim or the gvim interface with
visual menus.

We recommend that your ~/.vimrc file include these lines:

    set number textwidth=80 expandtab shiftwidth=4 tabstop=4
    au FileType hamlet setl sw=2 sts=2 et
    syntax on

We recommend using a Vim plugin manager such as
[Vundle](https://github.com/VundleVim/Vundle.vim)
and the following plugins particularly relevant to snowdrift:

* [vim-syntax-shakespeare](https://github.com/pbrisbin/vim-syntax-shakespeare)
* [haskell-vim](https://github.com/neovimhaskell/haskell-vim)
* [vim-markdown](https://github.com/plasticboy/vim-markdown)
* [vim-gitgutter](https://github.com/airblade/vim-gitgutter)
* [vim2hs](https://github.com/dag/vim2hs)
    * optionally add `set nofoldenable` to .vimrc to skip folding of functions

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
[vim-commentary](https://github.com/tpope/vim-commentary),
and others which take some minimal learning:
[vim-fugitive](https://github.com/tpope/vim-fugitive),
[undotree](https://github.com/mbbill/undotree),
[vim-easyclip](https://github.com/mbbill/undotree).

*Many* other options exist, although we'd rather contributors focus more on
building Snowdrift than maximizing their Vim expertise.
