" vim: tw=78 foldmarker={{{,}}} foldlevel=0 foldmethod=marker spell et sw=2 sts=2 :

" TODO: consider using vim-pandoc plugin
" TODO: consider writing my own syntax for my own dialect of markdown
" TODO: play around with the ctrlp plugin
" TODO: consider adding the py-matcher plugin
" TODO: consider adding the `ag` plugin
" TODO: Read about fugitive
"         https://www.reddit.com/r/vim/comments/21f4gm/best_workflow_when_using_fugitive/
" TODO: Read about writing actual text in vim
"         https://github.com/reedes/vim-pencil

" NeoVim {{{
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
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

" Smartcase when searching
set ignorecase
set smartcase

" Save two key strokes on formatting paragraphs
nnoremap <leader>q gqap

" Show up git status
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gc :Gcommit<CR>

" Set up the dictionary
set spell
" Jolly good!
set spelllang=en_gb

" Toggle show tabs and trailing spaces (,c) {{{
set lcs=tab:›\ ,trail:·,eol:¬,nbsp:_
set fcs=fold:-
nnoremap <silent> <leader>c :set nolist!<CR>
" }}}

" EasyAlign {{{
augroup easyalign_config
  autocmd!

  xmap ga <Plug>(EasyAlign)
  nmap ga <Plug>(EasyAlign)
augroup END
" }}}

" Markdown {{{
augroup markdown_config
  autocmd!
  if !exists("g:marked_command")
    let g:marked_command = "open -a Marked\\ 2"
  endif

  function! OpenInMarked()
    execute "silent !" . g:marked_command . " " . bufname("%")
  endfunction

  if !exists("g:pandoc_command")
    let g:pandoc_command = "pandoc"
  endif

  if !exists("g:pandoc_flags")
    let g:pandoc_flags = "-f markdown -t beamer --latex-engine=xelatex"
  endif

  function! CompilePresentationWithPandoc()
    let infile = bufname("%")
    let outfile = fnamemodify(infile, ":r") . ".pdf"
    execute "!" . g:pandoc_command . " " . g:pandoc_flags . " ". infile
              \ . " -o " . outfile
  endfunction

  autocmd FileType markdown nnoremap <buffer> <leader>o :call OpenInMarked()<cr>
  autocmd FileType markdown nnoremap <buffer> <leader>p :call CompilePresentationWithPandoc()<cr>
augroup END
" }}}

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

" Latex {{{
augroup latex_config
  autocmd!

  let g:neomake_tex_enabled_makers = ['chktex', 'xelatex']

  let g:neomake_tex_xelatex_maker = {
        \ 'exe': 'latexmk',
        \ 'args': ['-xelatex', '-interaction=nonstopmode'],
        \ 'cwd': '%:p:h'
        \ }

  " autocmd FileType tex set makeprg=latexmk\ -xelatex\ %
augroup END
" }}}

" Plugins {{{
" path to plugged hardcoded for now
call plug#begin('~/.config/nvim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'benekastah/neomake'
Plug 'bling/vim-airline'
Plug 'ianks/gruvbox' " Plug 'morhetz/gruvbox'
Plug 'idris-hackers/idris-vim'
Plug 'junegunn/vim-easy-align'
Plug 'kien/ctrlp.vim'
Plug 'leafgarland/typescript-vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
call plug#end()
" }}}

" Syntax highlighting {{{
set t_Co=256
set background=dark
syntax on
colorscheme gruvbox
" }}}
