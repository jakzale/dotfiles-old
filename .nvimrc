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
