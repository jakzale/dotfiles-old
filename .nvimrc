" Basic NeoVim configuration file
" For now let's try to use pathogen

" NeoBundle configuration from the README.md

if has('vim_starting')
  set runtimepath+=~/.nvim/bundle/neobundle.vim/
endif

" Start NeoBundle
call neobundle#begin(expand('~/.nvim/bundle'))

NeoBundleFetch 'Shougo/neobundle.vim'

" Plugins go here
NeoBundle 'tpope/vim-sensible'
NeoBundle 'christoomey/vim-tmux-navigator'

call neobundle#end()

" Set that just to be safe
filetype plugin indent on

" Check for uninstalled bundles
NeoBundleCheck

" Normal config goes here
