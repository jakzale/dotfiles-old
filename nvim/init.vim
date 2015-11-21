" vim: tw=78 foldmarker={{{,}}} foldlevel=0 foldmethod=marker spell et sw=2 sts=2 :

" NeoVim {{{
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
" let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" Wait for a new release of iterm
" }}}

" Syntax highlighting {{{
set t_Co=256
set background=dark
syntax on
colorscheme badwolf
" }}}

" Mapleader {{{
let mapleader="\<Space>"
nnoremap <space> <nop>
" }}}

" Esc {{{
" Consider using arpeggio
inoremap jk <esc>
" }}}


filetype indent on
set wildmenu


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

" Do not display the current mode
set noshowmode

" Save two key strokes on formatting paragraphs
nnoremap <leader>q gqap

" Show up git status
nnoremap <leader>gs :Gstatus<CR>

augroup myGroup
  autocmd!

  " Markdown specific settings
  autocmd FileType markdown nnoremap <silent> <leader>o :!open -a Marked\ 2 %<CR>
augroup END

" Airline.vim {{{
augroup airline_config
  autocmd!

  " disable showing separators
  let g:airline_left_sep = ''
  let g:airline_right_sep = ''

  " use airline tabline extension
  let g:airline#extensions#tabline#enabled = 1

  " TODO: Analyse that .nvimrc further
augroup END
" }}}


" path to plugged hardcoded for now
call plug#begin('~/.config/nvim/plugged')
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
call plug#end()
