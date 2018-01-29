inoremap jk <esc>

filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab

" Some terminal settings
tnoremap jk <C-\><C-n> 
tnoremap <Esc> <C-\><C-n>

" Set up python3 for neovim
let g:python3_host_prog = '/Users/jakub/.virtualenvs/neovim3/bin/python'
let g:python_host_prog = '/Users/jakub/.virtualenvs/neovim2/bin/python'


" Setting up dein
set runtimepath+=~/.config/nvim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state(expand('~/.config/nvim/dein'))
    call dein#begin(expand('~/.config/nvim/dein'))

    call dein#add('Shougo/dein.vim')
    call dein#add('tpope/vim-surround')
    " call dein#add('tpope/vim-sensible')
    call dein#add('tpope/vim-fugitive')

    call dein#end()
    call dein#save_state()
endif

" Not sure if these two need to be set
filetype plugin indent on
syntax enable
