" vim: tw=78 foldmarker={{{,}}} foldlevel=0 foldmethod=marker spell:

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
  NeoBundle 'tpope/vim-sensible'

  " Vim Arpeggio for key chords
  NeoBundle 'kana/vim-arpeggio'

  " Vim TMUX navigator
  NeoBundle 'christoomey/vim-tmux-navigator'

  " Vim Airline and Vim Bufferline
  NeoBundle 'bling/vim-airline'
  NeoBundle 'bling/vim-bufferline'

  " Solarized color scheme
  NeoBundle 'altercation/vim-colors-solarized'

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
" }}}

" Plugin Configuration {{{

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
" }}}
" {{{
" }}}
