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

" Check if we are using mosh
if strlen($MOSH) == 0
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  set t_Co=256
endif

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

" Set split directions
" set splitbelow
set splitright

" Use deoplete.
let g:deoplete#enable_at_startup = 1

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

" Terminal {{{
" Settings to handle terminal
augroup terminal_config
  autocmd!

  " Terminal cursor setting
  " hard coded for gruvbox for now
  au TermOpen * highlight TermCursor ctermfg=9 guifg=#fb4934
  " disable spelling in terminal
  au TermOpen * setlocal nospell
  " disable deoplete for terminal
  au TermOpen * let b:deoplete_disable_auto_complete=1

  tnoremap <leader><ESC> <C-\><C-n>
  " Let's try this for now
  tnoremap jk <C-\><C-n>

  " <Leader>T opens a new terminal in horizontal split
  nnoremap <leader>T :split term://zsh<CR>
  " <Leader>t opens a new terminal in vertical split
  nnoremap <leader>t :vsplit term://zsh<CR>

  " Switch to insert mode when entering a terminal buffer
  " NOTE: Currently breaks with tabline
  " au BufEnter * if &buftype == 'terminal' | :startinsert | endif

  tnoremap <silent> <C-w>h <C-\><C-n><C-w>h
  tnoremap <silent> <C-w>j <C-\><C-n><C-w>j
  tnoremap <silent> <C-w>k <C-\><C-n><C-w>k
  tnoremap <silent> <C-w>l <C-\><C-n><C-w>l

  " This will not work, cause neovim cannot handle <C-h>
  " nnoremap <silent> <C-h> :wincmd h<CR>
  " nnoremap <silent> <C-j> :wincmd j<CR>
  " nnoremap <silent> <C-k> :wincmd k<CR>
  " nnoremap <silent> <C-l> :wincmd l<CR>
" }}}

" Haskell {{{
augroup haskell_config
  autocmd!

  " Disable haskellmode-vim
  let g:haskellmode_completion_ghc=0

  " Set up proper omnifunc
  autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

  " Disable those pesky spellings
  autocmd FileType haskell setlocal nospell

  " Haskell specific key bindings
  nnoremap <leader>hw :GhcModTypeInsert<CR>
  nnoremap <leader>hs :GhcModSplitFunCase<CR>
  nnoremap <leader>ht :GhcModType<CR>
  nnoremap <leader>hd :GhcModTypeClear<CR>
augroup END
" }}}

" Plugins {{{
" path to plugged hardcoded for now
" Previously used:
" Plug 'morhetz/gruvbox'
call plug#begin('~/.config/nvim/plugged')
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/vimproc.vim', {'do': 'make'}
Plug 'airblade/vim-gitgutter'
Plug 'benekastah/neomake'
Plug 'bling/vim-airline'
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'
Plug 'ianks/gruvbox'
Plug 'idris-hackers/idris-vim'
Plug 'junegunn/vim-easy-align'
Plug 'kien/ctrlp.vim'
Plug 'leafgarland/typescript-vim'
Plug 'neovimhaskell/haskell-vim'
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
