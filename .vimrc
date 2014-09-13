" vim: tw=78 foldmarker={,} foldlevel=0 foldmethod=marker spell:

" Basic Vim Configuration File
" Initially done for NeoVim
" A lot is shamelessly taken from Steve Francia's Spf13-Vim

" Initial Setup {
  " Leaders {
    " Setting up the leader and localleader
    let mapleader = "\<Space>"
  " }

  " Putting configuration here as some plugins depend on it
  set ignorecase
  set smartcase
" }

" NeoBundle setup {
  if has('vim_starting')
    set nocompatible
    set runtimepath+=~/.vim/bundle/neobundle.vim/
  endif

  " Start NeoBundle
  call neobundle#begin(expand('~/.vim/bundle'))

  NeoBundleFetch 'Shougo/neobundle.vim'
" }

" Plugins {
  "  A list of plugins by tpope
  NeoBundle 'tpope/vim-sensible'
  NeoBundle 'tpope/vim-fugitive'
  NeoBundle 'tpope/vim-surround'
  NeoBundle 'tpope/vim-abolish'
  NeoBundle 'tpope/vim-unimpaired'
  NeoBundle 'tpope/vim-commentary'
  NeoBundle 'tpope/vim-vinegar'
  NeoBundle 'tpope/vim-scriptease'
  NeoBundle 'tpope/vim-sleuth'

  " Vim TMUX navigator
  NeoBundle 'christoomey/vim-tmux-navigator'

  " Vim Airline and Vim Bufferline
  NeoBundle 'bling/vim-airline'
  NeoBundle 'bling/vim-bufferline'

  " Solarized color scheme
  NeoBundle 'altercation/vim-colors-solarized'

  " Deal with syntax files once and for all
  NeoBundle 'sheerun/vim-polyglot'

  " Signify changes with vcs
  NeoBundle 'mhinz/vim-signify'

  " Plugins By Shougo
  " NeoBundle 'Shougo/neocomplcache.vim'
  NeoBundle 'Shougo/unite.vim'
  NeoBundle 'Shougo/vimproc.vim', {
        \ 'build' : {
        \     'windows' : 'tools\\update-dll-mingw',
        \     'cygwin' : 'make -f make_cygwin.mak',
        \     'mac' : 'make -f make_mac.mak',
        \     'unix' : 'make -f make_unix.mak',
        \    },
        \ }

  " Add neco-ghc
  NeoBundle 'eagletmt/neco-ghc'

  " YouCompleteMe
  NeoBundle 'Valloric/YouCompleteMe', {
    \ 'build' : {
    \   'mac' : './install.sh'
    \   },
    \ }
" }

" NeoBundle End {
  call neobundle#end()

  " Set that just to be safe
  filetype plugin indent on

  " Check for uninstalled bundles
  NeoBundleCheck

" }

" Global Key Maps {

  " jk is esc, always
  inoremap jk <esc>

  " Wrapped lines goes down/up to next row, rather than next line in file.
  noremap j gj
  noremap k gk

  " Turn of the highlight for the search
  nmap <silent> <leader>/ :set invhlsearch<CR>

  " Find merge conflict markers
  map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>

  " Allow using the repeat operator with a visual selection (!)
  " http://stackoverflow.com/a/8064607/127816
  vnoremap . :normal .<CR>

  " Some binding for fugitive
  nnoremap <leader>gs :Gstatus<CR>

" }

" General {
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

  " Setting up the directories {
  set backup
  if has('persistent_undo')
    set undofile
    set undolevels=1000
    set undoreload=10000
  endif
  " }
" }

" Setting up Vim UI {
  " Ensure that the background is set properly
  if filereadable(expand("~/.vimrc.background"))
    source ~/.vimrc.background
  endif

  " Fixing the colorscheme {
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
  " }
  " Setting up solarized color scheme" {
    if filereadable(expand("~/.vim/bundle/vim-colors-solarized/colors/solarized.vim"))
      let g:solarized_termcolors=256
      let g:solarized_termtrans=1
      let g:solarized_contrast="normal"
      let g:solarized_visibility="normal"
      color solarized
    endif
  " }

  set tabpagemax=15
  set cursorline

  if has('cmdline_info')
      set ruler                   " Show the ruler
      set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
      set showcmd                 " Show partial commands in status line and
                                  " Selected characters/lines in visual mode
  endif

  if has('statusline')
      set laststatus=2

      " Broken down into easily includeable segments
      set statusline=%<%f\                     " Filename
      set statusline+=%w%h%m%r                 " Options
      set statusline+=%{fugitive#statusline()} " Git Hotness
      set statusline+=\ [%{&ff}/%Y]            " Filetype
      set statusline+=\ [%{getcwd()}]          " Current dir
      set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
  endif

  set backspace=indent,eol,start  " Backspace for dummies
  set linespace=0                 " No extra spaces between rows
  set nu                          " Line numbers on
  set showmatch                   " Show matching brackets/parenthesis
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
" }

" Displaying Text {
  set nowrap
  set nojoinspaces
  set splitright
  set splitbelow
  set pastetoggle=<F12>
" }

" Plugins Configuration {
  " Fugitive {
    if isdirectory(expand("~/.vim/bundle/vim-fugitive/"))
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
  " }
  " YouCompleteMe {
    let g:acp_enableAtStartup = 0

    let g:ycm_semantic_triggers = {'haskell' : ['.']}
  " }
  " UndoTree {
    if isdirectory(expand("~/.vim/bundle/undotree/"))
      nnoremap <Leader>u :UndotreeToggle<CR>
      " If undotree is opened, it is likely one wants to interact with it.
      let g:undotree_SetFocusWhenToggle=1
    endif
  " }
" }
