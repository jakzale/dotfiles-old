" vim: tw=78 foldmarker={{{,}}} foldlevel=0 foldmethod=marker spell:

" Basic Vim Configuration File
" Initially done for NeoVim
" A lot is shamelessly taken from Steve Francia's Spf13-Vim

" Initial Setup {{{
  " Leaders {{{
    " Setting up the leader and localleader
    let mapleader = "\<Space>"
    let maplocalleader = "_"
  " }}}

  " Putting configuration here as some plugins depend on it
  set ignorecase
  set smartcase
" }}}

" NeoBundle setup {{{
  if has('vim_starting')
    set nocompatible
    set runtimepath+=~/.vim/bundle/neobundle.vim/
  endif

  " Start NeoBundle
  call neobundle#begin(expand('~/.vim/bundle'))

  NeoBundleFetch 'Shougo/neobundle.vim'
" }}}

" Plugins {{{
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
  NeoBundle 'tpope/vim-markdown'
  NeoBundle 'gkz/vim-ls'

  " Vim TMUX navigator
  NeoBundle 'christoomey/vim-tmux-navigator'

  " Vim Airline and Vim Bufferline
  NeoBundle 'bling/vim-airline'
  NeoBundle 'bling/vim-bufferline'

  " Solarized color scheme
  NeoBundle 'altercation/vim-colors-solarized'

  " Deal with syntax files once and for all
  NeoBundle 'sheerun/vim-polyglot'

  " Racket support
  NeoBundle 'wlangstroth/vim-racket'

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

  " Unite quickfix source
  NeoBundle 'osyo-manga/unite-quickfix'

  " Add neco-ghc
  NeoBundle 'eagletmt/neco-ghc'
  NeoBundle 'eagletmt/ghcmod-vim'

  " YouCompleteMe
  NeoBundle 'Valloric/YouCompleteMe', {
    \ 'build' : {
    \   'mac' : './install.sh'
    \   },
    \ }

  " UndoTree
  NeoBundle 'mbbill/undotree'

  " Tabular
  NeoBundle 'godlygeek/tabular'

  " Latex Support
  NeoBundle 'LaTeX-Box-Team/LaTeX-Box'

  " Goyo and Limelight
  NeoBundle 'junegunn/goyo.vim'
  NeoBundle 'junegunn/limelight.vim'

  " Syntastic
  NeoBundle 'scrooloose/syntastic'

  " Adding support for unicode
  NeoBundle 'chrisbra/unicode.vim'

  " Y U NO COMMIT plugin
  NeoBundle 'esneider/YUNOcommit.vim'
" }}}

" NeoBundle End {{{
  call neobundle#end()

  " Set that just to be safe
  filetype plugin indent on

  " Check for uninstalled bundles
  NeoBundleCheck

" }}}

" Global Key Maps {{{

  " jk is esc, always
  inoremap jk <esc>

  " Wrapped lines goes down/up to next row, rather than next line in file.
  noremap j gj
  noremap k gk

  " Turn of the highlight for the search
  nmap <silent> <leader>/ :set invhlsearch<CR>

  " Find merge conflict markers
  map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>

  " Visual shifting (does not exit Visual mode)
  vnoremap < <gv
  vnoremap > >gv

  " Allow using the repeat operator with a visual selection (!)
  " http://stackoverflow.com/a/8064607/127816
  vnoremap . :normal .<CR>
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

" Setting up Vim UI {{{
  " Set default background to dark
  set background=dark

  " Ensure that the background is set properly
  if filereadable(expand("~/.vimrc.background"))
    source ~/.vimrc.background
  endif

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
" }}}

" Displaying Text {{{
  set nowrap
  set nojoinspaces
  set splitright
  set splitbelow
  set pastetoggle=<F12>
" }}}

" Configuration {{{
  " Fugitive {{{
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
  " }}}
  " YouCompleteMe {{{
    let g:acp_enableAtStartup = 0

    " Use proper completion function for haskell

    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
    let g:ycm_semantic_triggers = {'haskell' : ['.']}
  " }}}
  " UndoTree {{{
    if isdirectory(expand("~/.vim/bundle/undotree/"))
      nnoremap <Leader>u :UndotreeToggle<CR>
      " If undotree is opened, it is likely one wants to interact with it.
      let g:undotree_SetFocusWhenToggle=1
    endif
  " }}}
  " vim-airine {{{
    if isdirectory(expand("~/.vim/bundle/vim-airline/"))
      if !exists('g:airline_theme')
        let g:airline_theme = 'solarized'
      endif
      " Use the default set of separators with a few customizations
      let g:airline_left_sep=''
      let g:airline_right_sep=''
    endif
  " }}}
  " Syntastic {{{
    let g:syntastic_haskell_checkers=[]
  " }}}
  " Latex-Box {{{
  let g:LatexBox_viewer = "open"
  augroup my_latex
    autocmd!
    autocmd FileType tex nnoremap <silent> <LocalLeader>ls :silent
          \ !/Applications/Skim.app/Contents/SharedSupport/displayline
          \ <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>"
          \ "%:p" <CR>:redraw!<CR>
    autocmd FileType tex set textwidth=79
  augroup END
  " }}}
  " Goyo and Limelight {{{

  " Setting up default limelight
  let g:limelight_conceal_ctermfg = 'Grey'
  let g:limelight_conceal_guifg = 'DarkGrey'
  let g:limelight_default_coefficient = 0.7

  " Setting up the distraction free writing
  function! s:goyo_enter()
    " Go to the beginning of the line --- prevents usually shifted screen
    normal ^
    Limelight
  endfunction

  function! s:goyo_leave()
    Limelight!
  endfunction

  autocmd! User GoyoEnter
  autocmd! User GoyoLeave

  autocmd User GoyoEnter call <SID>goyo_enter()
  autocmd User GoyoLeave call <SID>goyo_leave()

  let g:goyo_width = 100
  nnoremap <Leader>, :Goyo<CR>
  " }}}
  " ghcmod.vim {{{
    let g:ghcmod_open_quickfix_function = 'GhcModQuickFix'

    function! GhcModQuickFix()
      :Unite -no-empty -no-quit -direction=botright quickfix -wrap
    endfunction

    
    function! GhcModBufWritePost()
      if !exists("b:ghcmod_disable") && !exists("g:ghcmod_disable")

        if exists("b:ghcmod_lint_only") || exists("g:ghcmod_lint_only")
          :GhcModLint
        else
          :GhcModCheckAndLintAsync
        endif
      endif
    endfunction

    " Automatically check haskell files
    autocmd BufWritePost *.hs call GhcModBufWritePost()
  " }}}

  " Markdown {{{
  augroup markdown
    autocmd!
    " Adding support for Marked.app
    autocmd FileType markdown :nnoremap <Leader>m :silent !open -a Marked\ 2 '%:p'<CR>:redraw!<CR>
    " autocmd FileType markdown set wrap linebreak nolist
    autocmd FileType markdown set wrap linebreak

    " Add additional row below the row -- still not shure how to solve this
    " one
    " autocmd FileType markdown :nnoremap <Leader>h yypVr-<CR>
    " autocmd FileType markdown :nnoremap <Leader>H yypVr=<CR>

    " Adding shortcuts for underlying
    if has("gui_running")
      autocmd FileType markdown let &showbreak='↳'
    else
      autocmd FileType markdown let &showbreak='↳ '
    endif
augroup END


  " }}}


" }}}


" GUI Settings {{{
  " GVIM- (here instead of .gvimrc)
  if has('gui_running')
    set guioptions-=T           " Remove the toolbar
    set lines=40                " 40 lines of text instead of 24
    if !exists("g:spf13_no_big_font")
      if has("gui_running")
        set guifont=Andale\ Mono\ Regular:h12,Menlo\ Regular:h11,Consolas\ Regular:h12,Courier\ New\ Regular:h14
      endif
    endif
  else
    if &term == 'xterm' || &term == 'screen'
      set t_Co=256            " Enable 256 colors to stop the CSApprox warning and make xterm vim shine
    endif
    "set term=builtin_ansi       " Make arrow and other keys work
  endif
" }}}

" Initialize directories {{{
  function! InitializeDirectories()
    let parent = $HOME
    let prefix = 'vim'
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
