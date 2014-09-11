" Basic NeoVim configuration file
" For now let's try to use pathogen

" NeoBundle configuration from the README.md

" Setting up the leader and localleader
let mapleader = "\<Space>"

if has('vim_starting')
  set runtimepath+=~/.nvim/bundle/neobundle.vim/
endif

" Start NeoBundle
call neobundle#begin(expand('~/.nvim/bundle'))

NeoBundleFetch 'Shougo/neobundle.vim'

" Plugins go here
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

" Add vimproc
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

call neobundle#end()

" Set that just to be safe
filetype plugin indent on

" Check for uninstalled bundles
NeoBundleCheck

" Normal config goes here

" jk is esc, always
inoremap jk <esc>

" Some binding for fugitive
nnoremap <leader>gs :Gstatus<CR>

" Ensure that the background is set properly
if filereadable(expand("~/.vimrc.background"))
  source ~/.vimrc.background
endif

" Setting up NVim UI
" Fixing the colorscheme
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

" Setting up solarized color scheme"
if filereadable(expand("~/.nvim/bundle/vim-colors-solarized/colors/solarized.vim"))
  let g:solarized_termcolors=256
  let g:solarized_termtrans=1
  let g:solarized_contrast="normal"
  let g:solarized_visibility="normal"
  color solarized
endif

