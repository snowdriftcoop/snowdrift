# FLO Text Editors and Yesod/Haskell

This guide covers our recommended options for fully-FLO text-editors and IDE
development tools for hacking on Snowdrift (although this pretty well applies to
any Yesod-based project).

## Text editor packages and settings

### Atom

[Atom](https://atom.io/) is a modern, graphical, highly-extensible text-editor,
good for beginners and advanced alike. It is relatively new, and Haskell support
is continually improving.

#### Atom settings

Some settings to consider:

* Tab Length: 4 (easy enough to enter 2 spaces for hamlet)
* Packages/Tree View: turn on "Hide Ignored Names" and "Hide VCS Ignored Files"
* Disable the "metrics" package to stop Atom sending data to Google Analytics

#### Atom packages

We recommend adding at least the Atom packages:
[language-shakespeare](https://atom.io/packages/language-shakespeare) and
[language-haskell](https://atom.io/packages/language-haskell).

Note: the [ide-haskell](https://atom.io/packages/ide-haskell) package and
related tools seems promising, but we haven't yet gotten it successfully working
with Stack and Snowdrift.

Other useful Atom packages to consider:

* Various Git-related tools:
    * [tree-view-git-status](https://atom.io/packages/tree-view-git-status)
    * [git-blame](https://atom.io/packages/git-blame)
    * [show-origin](https://atom.io/packages/show-origin)
    * searching will show many other git tools worth considering
* General nice tools like [minimap](https://atom.io/packages/minimap),
  [cursor-history](https://atom.io/packages/cursor-history),
  [clipboard-history](https://atom.io/packages/clipboard-history),
  [multi-cursor](https://atom.io/packages/multi-cursor)
* [vim-mode](https://atom.io/packages/vim-mode) (if you like vim style editing)
    * consider the Clipboard as Default Register and Smartcase Search options
    * incidentally, many normal Atom editing operations still work as well
    * search packages for "vim-mode" to see extra related addons

### Emacs

[GNU Emacs](https://www.gnu.org/software/emacs/) is a traditional, robust,
keyboard-centric text editor with substantial Haskell support.

#### Emacs settings

Our included `.dir-locals.el` file
[makes Emacs use](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
the recommended 4-space indentation.

#### Emacs packages and variants

Emacs users should install
[Haskell Mode](https://github.com/haskell/haskell-mode) and
[Shakespeare Mode](https://github.com/CodyReichert/shakespeare-mode).

If you are new to Emacs,
[Emacs Prelude](https://github.com/bbatsov/prelude) is an enhanced
distribution of Emacs with better default configuration. It includes things like
[Helm](https://github.com/emacs-helm/helm) and
[Projectile](http://batsov.com/projectile/) by default.

If you are coming from Vim, consider
[Spacemacs](https://github.com/syl20bnr/spacemacs), an Emacs distribution that
includes Vim's modal keybindings by default along with much of Prelude's default
configuration. Aside from Spacemacs, you can get the Vim keybindings in any
Emacs distribution using [evil-mode](http://www.emacswiki.org/emacs/Evil).

Also of interest:

* [Magit](http://magit.vc/) is a high-quality Emacs git interface.
* [Structured Haskell Mode](https://github.com/chrisdone/structured-haskell-mode)
  (same author as Haskell Mode) takes a lot of tedium out of Haskell editing.
* [HIndent](https://github.com/chrisdone/hindent/) (same author again)
  will pretty-print your Haskell-code along the same lines as SHM.

### Leksah

[Leksah](http://leksah.org/) is a Haskell-dedicated complete IDE in the works,
but we're waiting for integration with Stack before we fully recommend it.

### Vim

[Vim](http://www.vim.org/) is a robust traditional editor with a command-line
style interface and great Haskell support.

Note: we recommend all vim users install `vim-gtk` to enable access to system
clipboards, regardless of then using terminal-based vim or the gvim interface
with visual menus.

#### Vim settings

We recommend that your ~/.vimrc file include:

    syntax on
    set number title hlsearch ignorecase smartcase showbreak=↪
    set textwidth=80 expandtab shiftwidth=4 tabstop=4
    au FileType hamlet setl sw=2 sts=2 et
    au Filetype gitcommit setl spell textwidth=72

Many other settings are nice, but opinions vary about the details.

#### Vim plugins

We recommend using a Vim plugin manager such as
[Vundle](https://github.com/VundleVim/Vundle.vim)
and the following plugins particularly relevant to snowdrift:

* [vim-syntax-shakespeare](https://github.com/pbrisbin/vim-syntax-shakespeare)
* [haskell-vim](https://github.com/neovimhaskell/haskell-vim)
* [vim-markdown](https://github.com/plasticboy/vim-markdown)
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
[vim-commentary](https://github.com/tpope/vim-commentary),
[ctrl-P](https://github.com/kien/ctrlp.vim);
and others which take some learning and/or set up:
[vim-fugitive](https://github.com/tpope/vim-fugitive),
[undotree](https://github.com/mbbill/undotree),
[NERD tree](https://github.com/scrooloose/nerdtree) &
[NERD tree git plugin](https://github.com/Xuyuanp/nerdtree-git-plugin),
[ag.vim](https://github.com/rking/ag.vim),
[vim-easyclip](https://github.com/mbbill/undotree),
[vim-airline](https://github.com/bling/vim-airline).

*Many* other options exist, although we'd rather contributors generally focus
more on building Snowdrift than maximizing their Vim expertise.

## Setting up tags to jump to function definitions

The following works for all text-editors that recognize tags files.

* run `stack install hasktags`
    * make sure you have ~/.local/bin on your PATH, if not sure, just run
      `echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc`
* In the snowdrift directory, run this big command to generate your tags file:

        git ls-tree -r HEAD --name-only | grep -E '*.hs' | xargs hasktags -x -c --ignore-close-implementation

    * That command works for Atom, Vim, and other ctags-based editors.
    * For Emacs, change `-x -c` to `-e`

    For edification, that long command is a pipeline that finds all committed
    files in the git repository, filters to just the Haskell ones, and passes
    those for tag processing (technically, there are other types of Haskell
    files, such as \*.hsc and \*.lhs, but we don't have them in Snowdrift.)

    For convenience, you can edit ~/.bashrc and add a line to make an alias for
    the long command above, as in:

        alias git-hasktags="git ls-tree -r HEAD --name-only | grep -E '*.hs' | xargs hasktags -x -c --ignore-close-implementation"

    To auto-update whenever you check out a new branch, put the command into
    .git/hooks/post-checkout and make the hook file executable:

        cat >> .git/hooks/post-checkout <<EOF
        git ls-tree -r HEAD --name-only | grep -E '*.hs' | xargs hasktags -x -c --ignore-close-implementation
        EOF
        chmod u+x .git/hooks/post-checkout

Now, you can quickly jump to tags with whatever mechanism your text editor uses.

Note that tags work for our internal functions only. For outside functions,
[Stackage](https://www.stackage.org/lts-2/hoogle) will have documentation on
almost all of our dependencies.

### Atom tag usage and updating

With tags generated, Atom uses Ctrl-Shift-R to search for any tag,
Ctrl-Alt-Down to jump to the definition of the symbol under the cursor, and
Ctrl-Alt-Up to return (with vim-mode, Ctrl-] and Ctrl-t also work).

We hope to document how to auto-update tags whenever saving files in Atom, but
until we have that clear, you'll need to manually re-run `git-hasktags` (set as
alias in instructions above) as needed.

### Vim tag usage and updating

With a tags file in place, Vim can jump to the definition of the symbol under
the cursor with Ctrl-] and jump back with Ctrl-t. Also, when typing in insert
mode, Ctrl-x followed by Ctrl-] will offer autocompletion for known tags.

To auto-update tags in Vim whenever a Haskell file gets written, use fast-tags:

* run `stack install fast-tags`
* add an autocommand to the Haskell plugin file:

        mkdir -p ~/.vim/after/ftplugin
        cat >> ~/.vim/after/ftplugin/haskell.vim <<EOF
        augroup fasttag
            autocmd!
            autocmd BufWritePost <buffer> silent !fast-tags %
        augroup END
        EOF
