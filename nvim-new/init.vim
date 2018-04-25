" Setting up dein for package management
set runtimepath+=~/.config/nvim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state(expand('~/.config/nvim/dein'))
    call dein#begin(expand('~/.config/nvim/dein'))

    call dein#add('Shougo/dein.vim')
    call dein#add('tpope/vim-surround')
    call dein#add('tpope/vim-sensible')
    call dein#add('tpope/vim-fugitive')
    call dein#add('tpope/vim-commentary')
    call dein#add('tpope/vim-repeat')
    call dein#add('vim-airline/vim-airline')
    call dein#add('vim-airline/vim-airline-themes')
    call dein#add('sjl/badwolf')
    call dein#add('airblade/vim-gitgutter')
    call dein#add('cespare/vim-toml')
    call dein#add('Shougo/denite.nvim')
    call dein#add('mileszs/ack.vim')
    " For some reason this does not work...
    " call dein#add('fsharp/vim-fsharp')
    call dein#add('w0rp/ale')
    call dein#add('lervag/vimtex')

    call dein#end()
    call dein#save_state()
endif

" Remap jk to <esc> everywhere 
inoremap jk <esc>

" Remap j and k to gj and gk
noremap j gj
noremap k gk
noremap gj j
noremap gk k

filetype plugin indent on
syntax enable

" Some basic settings
set tabstop=4
set shiftwidth=4
set expandtab

" Using the badwolf color scheme
colorscheme badwolf

" Disable space and use it as mapleader
nnoremap <space> <nop>
let mapleader = "\<space>"
" For now lets set local leader to space as well
let maplocalleader = "\<space>"

" Some visual mode keybindigs
vnoremap > >gv
vnoremap < <gv

" Terminal key bindings
tnoremap jk <C-\><C-n>
tnoremap <esc> <C-\><C-n>

" Set up python3 for neovim
let g:python3_host_prog = '/Users/jakub/.virtualenvs/neovim3/bin/python'
let g:python_host_prog = '/Users/jakub/.virtualenvs/neovim2/bin/python'

" Settings for vim-fugitive
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gr :Gread<cr>

" Settings for Ack.vim
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

