# FLO Text Editors and Yesod/Haskell

This guide covers our recommended options for fully-FLO text-editors and IDE
development tools for hacking on Snowdrift (although this pretty well applies to
any Yesod-based project).

Note: regardless of editor, to use any tool set up with via `stack install`,
make sure you have ~/.local/bin on your PATH. If not already set, run
`echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc`.

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

We recommend adding at least the Atom packages
[language-shakespeare](https://atom.io/packages/language-shakespeare) and
[language-haskell](https://atom.io/packages/language-haskell).

The [ide-haskell](https://atom.io/packages/ide-haskell) package offers further
development tools including error-checking, linting, and type information. To
install ide-haskell for Atom:

* Run `stack install ghc-mod hlint stylish-haskell --resolver nightly`
    * "--resolver nightly" is only needed until we update our lts resolver to
      one that includes ghc-mod (the latest 3.6 did not, as of this writing).
* Install the required Atom packages:
  `apm install language-haskell haskell-ghc-mod ide-haskell autocomplete-haskell`
* If you always start Atom from the command line and have ~/.local/bin in your
  path, you're done.
* To enable starting Atom not from command line, run `which ghc-mod`, copy the
  path that returns, and then, within Atom, in the settings for the
  haskell-ghc-mod package, paste that path in the field for the "Ghc Mod Path"
  and the same path but with an `i` added on the end for the "Ghc Modi Path".
* *Note*: this installation is the easiest for now and will work for any other
  projects that use the same GHC version but will *not* work if you switch
  between projects that use different GHC versions.

Other useful Atom packages to consider:

* Various Git-related tools:
    * [tree-view-git-status](https://atom.io/packages/tree-view-git-status)
    * [git-blame](https://atom.io/packages/git-blame)
    * [show-origin](https://atom.io/packages/show-origin)
    * searching will show other git tools worth considering
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

#### Emacs settings

Our included `.dir-locals.el` file
[makes Emacs use](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
the recommended 4-space indentation.

#### Emacs packages and variants

Emacs users should install
[Haskell Mode](https://github.com/haskell/haskell-mode) and
[Shakespeare Mode](https://github.com/CodyReichert/shakespeare-mode).

Also of interest:

* [Magit](http://magit.vc/) is a high-quality Emacs git interface.
* [Structured Haskell Mode](https://github.com/chrisdone/structured-haskell-mode)
  (same author as Haskell Mode) takes a lot of tedium out of Haskell editing.
* [HIndent](https://github.com/chrisdone/hindent/) (same author again)
  will pretty-print your Haskell-code along the same lines as SHM.

* [Haskell Interactive Mode](https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Setup)
  will enable automatic regeneration of the TAGS file. (Our
  .dir-locals.el file tells haskell-interactive-mode to do that)

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

As an optional tool, Vim can do integrated Haskell error-checking and get type
information via [ghcmod-vim](https://github.com/eagletmt/ghcmod-vim). Follow
its install instructions except run `stack install ghc-mod --resolver nightly`
(note: this "--resolver nightly" bit can be ignored once ghc-mod is in the LTS
version we use) *instead* of the instruction to run "cabal install ghc-mod" (and
make sure ~/.local/bin is on your path). You may want to also try the associated
auto-completion tool [neco-ghc](https://github.com/eagletmt/neco-ghc).

Other general vim plugins we suggest for consideration include many which are
inobtrusive and take zero or near-zero learning to use (roughly in order by most
strongly recommended):
[vim-sensible](https://github.com/tpope/vim-sensible),
[vim-repeat](https://github.com/tpope/vim-repeat),
[vim-supertab](https://github.com/ervandew/supertab),
[vim-gitgutter](https://github.com/airblade/vim-gitgutter),
[vim-surround](https://github.com/tpope/vim-surround),
[vim-commentary](https://github.com/tpope/vim-commentary),
[ctrl-P](https://github.com/kien/ctrlp.vim),
[undotree](https://github.com/mbbill/undotree),
[vim-fugitive](https://github.com/tpope/vim-fugitive),
[gitv](https://github.com/gregsexton/gitv),
[NERD tree](https://github.com/scrooloose/nerdtree) &
[NERD tree git plugin](https://github.com/Xuyuanp/nerdtree-git-plugin);
and others which take some minor learning and/or set up:
[vim-easyclip](https://github.com/mbbill/undotree),
[MiniBufExplorer](https://github.com/fholgado/minibufexpl.vim),
[ag.vim](https://github.com/rking/ag.vim),
[vim-airline](https://github.com/bling/vim-airline).

*Many* other options exist, although we'd rather contributors generally focus
more on building Snowdrift than maximizing their Vim expertise.

## Setting up tags to jump to function definitions

The following works for all text-editors that recognize tags files.

* run `stack install hasktags`
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
[Stackage](https://www.stackage.org/lts-3/hoogle) will have documentation on
almost all of our dependencies.

### Atom tag usage and updating

With tags generated, Atom uses Ctrl-Shift-R to search for any tag,
Ctrl-Alt-Down to jump to the definition of the symbol under the cursor, and
Ctrl-Alt-Up to return (with vim-mode, Ctrl-] and Ctrl-t also work).

We hope to document how to auto-update tags whenever saving files in Atom, but
until we have that clear, you'll need to manually re-run `git-hasktags` (set as
alias in instructions above) as needed.

### Emacs tag usage

If you use Helm, as recommended above, you can run `M-x
helm-etags-select` to select from the `TAGS` file.

If you use haskell-interactive-mode, linked above, Emacs will
automatically regenerate the TAGS file for you (provided you have loaded
a file with `C-c C-l` beforehand). If you don't want to use
haskell-interactive-mode for whatever reason, you can generate the tags
file with

    git ls-tree -r HEAD --name-only | grep -E '*.hs' | xargs hasktags -e --ignore-close-implementation

`M-t` is a good keybinding for `helm-etags-select`, provided you don't
use `transpose-words`:

```elisp
(global-set-key (kbd "M-t") 'helm-etags-select)
```

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

*Note*: so that we don't generate extra tags files in internal directories, make
sure to only open vim from the main snowdrift project directory from now on.
