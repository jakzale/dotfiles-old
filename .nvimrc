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
NeoBundle 'tpope/vim-sensible'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'christoomey/vim-tmux-navigator'

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
