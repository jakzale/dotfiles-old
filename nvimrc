" vim: tw=78 foldmarker={{{,}}} foldlevel=0 foldmethod=marker spell et sw=2 sts=2 :

" A config file for NeoVim

" Initial Setup {{{
  " Leaders {{{
    let mapleader = "\<Space>"
    let maplocalleader = "_"
  " }}}

  " Putting configuration here as some plugins depend on it
  set ignorecase
  set smartcase
" }}}

" NeoBundle setup {{{
  if has('vim_starting')
    set runtimepath+=~/.vim/bundle/neobundle.vim/
  endif

  call neobundle#begin(expand('~/.nvim/bundle'))

  NeoBundleFetch 'Shougo/neobundle.vim'
" }}}

" Plugins {{{
  " Plugins by tpope
  NeoBundle 'tpope/vim-sensible'
  NeoBundle 'tpope/vim-fugitive'
  NeoBundle 'tpope/vim-surround'
  NeoBundle 'tpope/vim-commentary'

  " Vim Arpeggio for key chords
  NeoBundle 'kana/vim-arpeggio'

  " Vim TMUX navigator
  NeoBundle 'christoomey/vim-tmux-navigator'

  " Vim Airline and Vim Bufferline
  NeoBundle 'bling/vim-airline'
  NeoBundle 'bling/vim-bufferline'

  " Solarized color scheme
  NeoBundle 'altercation/vim-colors-solarized'

  " Deal with syntax files once and for all
  NeoBundle 'sheerun/vim-polyglot'

  " Syntax files for JSX
  NeoBundle 'mxw/vim-jsx'

  " Signify changes with vcs
  NeoBundle 'mhinz/vim-signify'

  " UndoTree
  NeoBundle 'mbbill/undotree'

  " YouCompleteMe
  NeoBundle 'Valloric/YouCompleteMe'

  " UltiSnips with Snippets
  NeoBundle 'SirVer/ultisnips'
  NeoBundle 'honza/vim-snippets'

  " Tabular
  NeoBundle 'godlygeek/tabular'

  " Y U NO COMMIT plugin
  NeoBundle 'esneider/YUNOcommit.vim'

  " Syntastic
  NeoBundle 'scrooloose/syntastic'

  " ctrlp
  NeoBundle 'kien/ctrlp.vim'

  " Ag
  NeoBundle 'rking/ag.vim'

  " TODO: Figure out how to fix those
  " " Neco Ghc
  " NeoBundle 'eagletmt/neco-ghc'
  " Vimproc -- for NeoBundle
  NeoBundle 'Shougo/vimproc.vim', {
        \ 'build' : {
        \     'windows' : 'tools\\update-dll-mingw',
        \     'cygwin' : 'make -f make_cygwin.mak',
        \     'mac' : 'make -f make_mac.mak',
        \     'linux' : 'make',
        \     'unix' : 'gmake',
        \    },
        \ }

  " Install plugn for Idris
  NeoBundle "idris-hackers/idris-vim"

  " Install plugin for floobits
  NeoBundle "floobits/floobits-neovim"
" }}}

" NeoBundle End {{{
  call neobundle#end()

  " Set that just to be safe
  filetype plugin indent on

  " Check for uninstalled bundles
  NeoBundleCheck

"}}}

" Global Key Maps {{{

  " Decided to go for arpeggio for chords.
  " Hopefully, it will not drive me mad.
  call arpeggio#map('i', '', 0, 'jk', '<esc>')

  " Wrapped lines goes down/up to next row, rather than next line in file.
  noremap j gj
  noremap k gk

  " Visual shifting (does not exit Visual mode)
  vnoremap < <gv
  vnoremap > >gv

  " Mappings for terminal mode
  tnoremap <Esc> <C-\><C-n>
  tnoremap jk <C-\><C-n>

  "Assuming TmuxNavigator is installed
  tnoremap <C-h> <C-\><C-n>:TmuxNavigateLeft<CR>
  tnoremap <C-j> <C-\><C-n>:TmuxNavigateDown<CR>
  tnoremap <C-k> <C-\><C-n>:TmuxNavigateUp<CR>
  tnoremap <C-l> <C-\><C-n>:TmuxNavigateRight<CR>

" }}}

" General {{{
  set shortmess+=filmnrxoOtT          " Abbrev. of messages (avoids 'hit enter')
  set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
  set virtualedit=onemore             " Allow for cursor beyond last character
  set history=1000                    " Store a ton of history (default is 20)
  set spell                           " Spell checking on
  set hidden                          " Allow buffer switching without saving
  set iskeyword-=.                    " '.' is an end of word designator
  set iskeyword-=#                    " '#' is an end of word designator
  set iskeyword-=-                    " '-' is an end of word designator

  " Instead of reverting the cursor to the last position in the buffer, we
  " set it to the first line when editing a git commit message
  au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

  " Setting up the directories {{{
  set backup
  if has('persistent_undo')
    set undofile
    set undolevels=1000
    set undoreload=10000
  endif
  " }}}
" }}}

" Setting up NeoVim UI {{{
  " Set default background to dark
  set background=dark

  " Fixing the colorscheme {{{
    function! s:fix_color_scheme()
      highlight clear SignColumn
      highlight clear LineNr

      " Fixing the color of the omnicomplete window
      hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
      hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
      hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE
    endfunction

    augroup FixingColorScheme
      autocmd!
      autocmd ColorScheme * call <SID>fix_color_scheme()
    augroup END
  " }}}
  " Setting up solarized color scheme" {{{
    if filereadable(expand("~/.vim/bundle/vim-colors-solarized/colors/solarized.vim"))
      let g:solarized_termcolors=256
      let g:solarized_termtrans=1
      let g:solarized_contrast="normal"
      let g:solarized_visibility="normal"
      color solarized
    endif
  " }}}

  set tabpagemax=15
  set cursorline

  set backspace=indent,eol,start  " Backspace for dummies
  set linespace=0                 " No extra spaces between rows
  set nu                          " Line numbers on
  set showmatch                   " Do not jump back to previous parenthesis
                                  " can get distracting
  set incsearch                   " Find as you type search
  set hlsearch                    " Highlight search terms
  set winminheight=0              " Windows can be 0 line high
  set ignorecase                  " Case insensitive search
  set smartcase                   " Case sensitive when uc present
  set wildmenu                    " Show list instead of just completing
  set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
  set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
  set scrolljump=5                " Lines to scroll when cursor leaves screen
  set scrolloff=3                 " Minimum lines to keep above and below cursor
  set foldenable                  " Auto fold code
  set list
  set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace

" }}}

" Displaying Text {{{
  set nowrap
  set nojoinspaces
  set splitright
  set splitbelow
  set pastetoggle=<F12>

  " Default indentation
  set ts=2 sw=2 sts=2 et
" }}}

" Plugin Configuration {{{
  " Here are the configurations for all plugins

  " vim-airline {{{
    if isdirectory(expand("~/.vim/bundle/vim-airline/"))
      if !exists('g:airline_theme')
        let g:airline_theme = 'solarized'
      endif
      " Use the default set of separators with a few customizations
      let g:airline_left_sep=''
      let g:airline_right_sep=''
    endif
  " }}}
  " Fugitive {{{
    if isdirectory(expand("~/.nvim/bundle/vim-fugitive/"))
      nnoremap <silent> <leader>gs :Gstatus<CR>
      nnoremap <silent> <leader>gd :Gdiff<CR>
      nnoremap <silent> <leader>gc :Gcommit<CR>
      nnoremap <silent> <leader>gb :Gblame<CR>
      nnoremap <silent> <leader>gl :Glog<CR>
      nnoremap <silent> <leader>gp :Git push<CR>
      nnoremap <silent> <leader>gr :Gread<CR>
      nnoremap <silent> <leader>gw :Gwrite<CR>
      nnoremap <silent> <leader>ge :Gedit<CR>
      " Mnemonic _i_nteractive
      nnoremap <silent> <leader>gi :Git add -p %<CR>
      nnoremap <silent> <leader>gg :SignifyToggle<CR>
    endif
  " }}}
  " UndoTree {{{
      if isdirectory(expand("~/.nvim/bundle/undotree/"))
        nnoremap <Leader>u :UndotreeToggle<CR>
        " If undotree is opened, it is likely one wants to interact with it.
        let g:undotree_SetFocusWhenToggle=1
      endif
  " }}}
  " YouCompleteMe {{{

    " enable completion from tags
    let g:ycm_collect_identifiers_from_tags_files = 1

    " remap Ultisnips for compatibility for YCM
    let g:UltiSnipsExpandTrigger = '<C-j>'
    let g:UltiSnipsJumpForwardTrigger = '<C-j>'
    let g:UltiSnipsJumpBackwardTrigger = '<C-k>'

  " }}}
  " Syntastic {{{
    let g:syntastic_haskell_checkers=[]
  " }}}
    " ctrlp {{{
        if isdirectory(expand("~/.nvim/bundle/ctrlp.vim/"))
            let g:ctrlp_working_path_mode = 'ra'
            nnoremap <silent> <D-t> :CtrlP<CR>
            nnoremap <silent> <D-r> :CtrlPMRU<CR>
            let g:ctrlp_custom_ignore = {
                \ 'dir':  '\.git$\|\.hg$\|\.svn$',
                \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

            " On Windows use "dir" as fallback command.
            if executable('ag')
                let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
            elseif executable('ack-grep')
                let s:ctrlp_fallback = 'ack-grep %s --nocolor -f'
            elseif executable('ack')
                let s:ctrlp_fallback = 'ack %s --nocolor -f'
            else
                let s:ctrlp_fallback = 'find %s -type f'
            endif
            let g:ctrlp_user_command = {
                \ 'types': {
                    \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
                    \ 2: ['.hg', 'hg --cwd %s locate -I .'],
                \ },
                \ 'fallback': s:ctrlp_fallback
            \ }
        endif
    "}}}

    " Neco GHC {{{
    " TODO: Refactor and Fix
    " Doing this as autocmd for now, consider refactoring to a separate
    " package with settings (or at least separate ftplugin file
      " augroup Haskell
      "   autocmd!
      "   autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
      " augroup END
      " let g:ycm_semantic_triggers = {'haskell' : ['.']}
    " }}}
" }}}

" {{{ My Own Stuff
  " TODO: Figure out where to put it

  " Open a simple terminal
  " TODO: Figure out if this will produce a handle to operate on
  " function Terminal()
  "   split term://zsh
  " endfunction

  command Term call Terminal()

  nnoremap <leader>t :Term<CR>
" }}}

" Initialize directories {{{
  function! InitializeDirectories()
    let parent = $HOME
    let prefix = 'nvim'
    let dir_list = {
          \ 'backup': 'backupdir',
          \ 'views': 'viewdir',
          \ 'swap': 'directory' }

    if has('persistent_undo')
      let dir_list['undo'] = 'undodir'
    endif

    " To specify a different directory in which to place the vimbackup,
    " vimviews, vimundo, and vimswap files/directories, add the following to
    " your .vimrc.before.local file:
    "   let g:spf13_consolidated_directory = <full path to desired directory>
    "   eg: let g:spf13_consolidated_directory = $HOME . '/.vim/'
    if exists('g:spf13_consolidated_directory')
      let common_dir = g:spf13_consolidated_directory . prefix
    else
      let common_dir = parent . '/.' . prefix
    endif

    for [dirname, settingname] in items(dir_list)
      let directory = common_dir . dirname . '/'
      if exists("*mkdir")
        if !isdirectory(directory)
          call mkdir(directory)
        endif
      endif
      if !isdirectory(directory)
        echo "Warning: Unable to create backup directory: " . directory
        echo "Try: mkdir -p " . directory
      else
        let directory = substitute(directory, " ", "\\\\ ", "g")
        exec "set " . settingname . "=" . directory
      endif
    endfor
  endfunction
  call InitializeDirectories()
" }}}
