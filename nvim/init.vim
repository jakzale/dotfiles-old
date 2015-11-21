" vim: tw=78 foldmarker={{{,}}} foldlevel=0 foldmethod=marker spell et sw=2 sts=2 :

" A simple config file for neovim

" not sure if this is needed
set t_Co=256
" try using badwolf for now
colorscheme badwolf

filetype indent on
set wildmenu

inoremap jk <esc>
let mapleader="\<Space>"
nnoremap <space> <nop>

nnoremap <silent> <leader>/ :nohlsearch<CR>

set tabstop=2
set softtabstop=2
" not sure if this part is relevant
set shiftwidth=2
set expandtab

set number
set showcmd
set cursorline
set showmatch


" Save two key strokes on formatting paragraphs
nnoremap <leader>q gqap

augroup myGroup
  autocmd!

  " Markdown specific settings
  autocmd FileType markdown nnoremap <silent> <leader>o :!open -a Marked\ 2 %<CR>
augroup END
