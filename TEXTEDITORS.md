# FLO Text Editors and Yesod/Haskell

This guide covers our recommended options for FLO (Free/Libre/Open) text-editors
and IDE development tools for hacking on Snowdrift (NB: most of this applies
equally to any Yesod-based project).

## Text editor packages and settings

### Atom

[Atom](https://atom.io/) is a modern, graphical, highly-extensible text-editor,
good for beginners and advanced alike.

#### Atom settings

Some general settings:

* In the main Atom settings, leave soft tabs checked and set 4-space tabs
* Packages/Tree View: consider "Hide Ignored Names" and "Hide VCS Ignored Files"

#### Atom packages

We recommend at least the Atom packages:
* [language-shakespeare](https://atom.io/packages/language-shakespeare)
    * in the language-shakespeare package settings, change the hamlet tab-length
      to 2, leave others at 4
* [language-haskell](https://atom.io/packages/language-haskell)


The [ide-haskell](https://atom.io/packages/ide-haskell) package offers further
development tools including error-checking, linting, and type information. To
install ide-haskell for Atom:

* *Note: after many changes to snowdrift build process and file structure,
    ghc-mod may or may not work, testing is needed.*
* Run `stack build ghc-mod hlint stylish-haskell`
* Install the relevant Atom packages:
  `apm install language-haskell haskell-ghc-mod ide-haskell autocomplete-haskell ide-haskell-cabal`
* Make sure to check "Stack sandbox" in the haskell-ghc-mod settings

Other useful Atom packages to consider:

* Various Git-related tools:
    * [tree-view-git-status](https://atom.io/packages/tree-view-git-status)
    * [git-blame](https://atom.io/packages/git-blame)
    * [show-origin](https://atom.io/packages/show-origin)
    * search the packages for other git tools worth considering
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

**If you are coming from Vim, consider
[Spacemacs](https://github.com/syl20bnr/spacemacs)**, an Emacs distribution that
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
* [Hare](https://github.com/RefactoringTools/HaRe) automates renaming and
  changing among different Haskell code styles.

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

For working on our code, your ~/.vimrc file should include:

    set textwidth=80 expandtab shiftwidth=4 tabstop=4
    au FileType hamlet setl sw=2 sts=2 et
    au Filetype gitcommit setl spell textwidth=72

We also recommend the following minimal .vimrc settings:

    set number title hlsearch ignorecase smartcase showbreak=↪
    set wildmenu wildmode=longest,list,full

#### Vim plugins

We recommend [vim-plug](https://github.com/junegunn/vim-plug) for managing Vim
plugins.

For syntax highlighting, we suggest everyone use the following plugins:

* [vim-syntax-shakespeare](https://github.com/pbrisbin/vim-syntax-shakespeare)
* [vim-yesod](https://github.com/alx741/vim-yesod)
    * add to ~/.vimrc: `let g:yesod_handlers_directories = ['Handler', 'src']`
* [haskell-vim](https://github.com/neovimhaskell/haskell-vim)
* [vim-markdown](https://github.com/plasticboy/vim-markdown)
* [vim2hs](https://github.com/dag/vim2hs)
    * optional: add `set nofoldenable` to .vimrc to stop vim2hs function folding

Given already using vim-plug, these are the lines to use in ~/.vimrc for the
items above:

    Plug 'pbrisbin/vim-syntax-shakespeare', { 'for': ['hamlet', 'cassius', 'julius', 'haskell'] }
    Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
    Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
    Plug 'dag/vim2hs', { 'for': 'haskell' }
    au FileType haskell set nofoldenable "stops vim2hs folding

For advanced Haskell error-checking and type information, install ghc-mod.

* *Note: after many changes to snowdrift build process and file structure,
    ghc-mod may or may not work, testing is needed.* Once working, the
    instructions are still as follows:

To set up ghc-mod, run `stack install ghc-mod` (and make sure ~/.local/bin is on
your path). Then, add [ghcmod-vim](https://github.com/eagletmt/ghcmod-vim) and
[neco-ghc](https://github.com/eagletmt/neco-ghc) as well; for installation
via vim-plug, use these lines in ~/.vimrc:

    Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
    Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
    Plug 'Shougo/vimproc.vim', {'do': 'make', 'for': 'haskell' } "neco-ghc dependency
    " Disable haskell-vim omnifunc per neco-ghc recommendation
    let g:haskellmode_completion_ghc = 0
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

To have ghc-mod auto-check for errors on open or save, add
[syntastic](https://github.com/scrooloose/syntastic) and set it up as desired.

Another great Haskell tool: [vim-HaRe](https://github.com/glittershark/vim-hare)
NB: to get HaRe itself, use `stack install HaRe` instead of cabal install.

General vim plugins:

Other general vim plugins we suggest for consideration include many which are
inobtrusive and take zero or near-zero learning to use (roughly in order by most
strongly recommended):
* [vim-sensible](https://github.com/tpope/vim-sensible),
* [vim-repeat](https://github.com/tpope/vim-repeat),
* [vim-supertab](https://github.com/ervandew/supertab),
* [ctrl-P](https://github.com/ctrlpvim/ctrlp.vim),
* [Recover.vim](https://github.com/chrisbra/Recover.vim),
* [vim-gitgutter](https://github.com/airblade/vim-gitgutter),
* [vim-surround](https://github.com/tpope/vim-surround),
* [vim-commentary](https://github.com/tpope/vim-commentary),
* [vim-easyclip](https://github.com/vim-scripts/EasyClip)
  (NB: vim-easyclip alters common vim behavior),
* [undotree](https://github.com/mbbill/undotree),
* [vim-fugitive](https://github.com/tpope/vim-fugitive),
* [gitv](https://github.com/gregsexton/gitv),
* [NERD tree](https://github.com/scrooloose/nerdtree) &
* [NERD tree git plugin](https://github.com/Xuyuanp/nerdtree-git-plugin);
* [ack.vim](https://github.com/mileszs/ack.vim)
  (NB: ack.vim requires system install of `ack-grep` or `silversearch-ag`),
* [vim-airline](https://github.com/bling/vim-airline).

*Many* other options exist, although we'd rather contributors generally focus
more on building Snowdrift than maximizing their Vim expertise.

## Setting up tags to jump to function definitions

The following works for all text-editors that recognize tags files.

* run `stack install hasktags` (and make sure ~/.local/bin is in your path)
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

The setup above works for functions defined within our local codebase. To add
tags for all the dependencies too, install
[codex](https://github.com/aloiscochard/codex) via `stack install codex` (and
see the codex docs for how to use, including a vim-specific setting). Otherwise,
[Stackage](https://www.stackage.org/lts-5/hoogle) will have documentation on
almost all of our dependencies.

### Atom tag usage and updating

With tags generated, Atom uses Ctrl-Shift-R to search for any tag, Ctrl-Alt-Down
to jump to the definition of the symbol under the cursor, and Ctrl-Alt-Up to
return (with vim-mode, Ctrl-] and Ctrl-t also work).

We hope to document how to auto-update tags whenever saving files in Atom, but
until we have that clear, you'll need to manually re-run `git-hasktags` (set as
alias in instructions above) as needed.

### Emacs tag usage

If you use Helm, as recommended above, you can run `M-x helm-etags-select` to
select from the `TAGS` file.

If you use haskell-interactive-mode, linked above, Emacs will automatically
regenerate the TAGS file for you (provided you have loaded a file with `C-c C-l`
beforehand). If you don't want to use haskell-interactive-mode for whatever
reason, you can generate the tags file with:

    git ls-tree -r HEAD --name-only | grep -E '*.hs' | xargs hasktags -e --ignore-close-implementation

`M-t` is a good keybinding for `helm-etags-select`, provided you don't use
`transpose-words`:

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

NB: so that we don't generate extra tags files in internal directories, make
sure to only open vim from the main snowdrift project directory from now on.
