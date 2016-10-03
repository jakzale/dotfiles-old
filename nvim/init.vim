" vim: tw=78 foldmarker={{{,}}} foldlevel=0 foldmethod=marker spell et sw=2 sts=2 :

" TODO: consider using vim-pandoc plugin
" TODO: consider writing my own syntax for my own dialect of markdown
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

" Allow to switching out of modified buffer
set hidden

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

" Custom setting during marking
function! s:marker_setup()
  set tabstop=8
  set softtabstop=8
  set shiftwidth=8
  set noexpandtab
  set list
endfunction

command! Marker call s:marker_setup()
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

  " autocmd FileType markdown nnoremap <buffer> <leader>o :call OpenInMarked()<cr>
  " autocmd FileType markdown nnoremap <buffer> <leader>p :call CompilePresentationWithPandoc()<cr>
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

  let g:neomake_tex_enabled_makers = ['chktex']

  " Copied from neomake
  " Decided to disable warning 1 (commands terminated with space), and warning
  " 26 (whitespace before punctuation).
  let g:neomake_tex_chktex_maker = {
        \ 'exe': 'chktex',
        \ 'args': ['--nowarn', '1', '--nowarn', '26'],
        \ 'errorformat':
            \ '%EError %n in %f line %l: %m,' .
            \ '%WWarning %n in %f line %l: %m,' .
            \ '%WMessage %n in %f line %l: %m,' .
            \ '%Z%p^,' .
            \ '%-G%.%#'
        \ }

  " Open neomake list when stuff is added
  let g:neomake_open_list = 1

  function! s:cclose_and_neomake()
    " Close the clist window
    cclose
    Neomake!
  endfunction

  command! CCNeomake call s:cclose_and_neomake()

  function! s:latex_setup()

    " Switch to use my own compiler for now
    compiler latex-simple

    nnoremap <buffer> <leader>p :CCNeomake<CR>

    " Set up local autocmd that will run neomake
    autocmd BufWritePost <buffer> Neomake

    " Set up tex counter
    nnoremap <buffer> <leader>lc :NeomakeSh make count<CR>
  endfunction

  autocmd FileType tex call s:latex_setup()

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

  " Set up proper compiler
  autocmd FileType haskell setlocal makeprg=stack\ build

  " Haskell specific key bindings
  " nnoremap <leader>hw :GhcModTypeInsert<CR>
  " nnoremap <leader>hs :GhcModSplitFunCase<CR>
  " nnoremap <leader>ht :GhcModType<CR>
  " nnoremap <leader>hd :GhcModTypeClear<CR>
augroup END
" }}}

" CTRLP.VIM {{{
augroup ctrlp_config
  autocmd!

  " This one should work, but the other is more general
  " let g:ctrlp_user_command = {
  "       \ 'types' : {
  "         \ 1 : ['.git', 'cd %s && git ls-files . -co --exclude-standard']
  "         \ },
  "       \ 'fallback': 'ag %s -l --nocolor'
  "       \ }
  if executable('ag')
    let g:ctrlp_user_command = 'cd %s && ag --nocolor --hidden --ignore .git --nopager -l -g .\*'
  endif
augroup END
" }}}

" Ack.vim {{{
augroup ack_config
  if executable('ag')
    let g:ackprg = 'ag --vimgrep'
  endif
augroup END
" }}}

" Plugins {{{
" path to plugged hardcoded for now
" Previously used:
" Plug 'Shougo/vimproc.vim', {'do': 'make'}
" Plug 'eagletmt/ghcmod-vim'
" Plug 'eagletmt/neco-ghc'
" Plug 'idris-hackers/idris-vim'
" Plug 'lambdatoast/elm.vim'
" Plug 'leafgarland/typescript-vim'
" Plug 'raichoo/purescript-vim'
" This one is a bit too heavyweight
" Plug 'LaTeX-Box-Team/LaTeX-Box'
" This one still defines its own compiler
" Plug 'lervag/vimtex'
" This one is very cool, but way too slooow!
"Plug 'jaxbot/github-issues.vim'

call plug#begin('~/.config/nvim/plugged')
Plug 'Shougo/deoplete.nvim'
Plug 'airblade/vim-gitgutter'
Plug 'benekastah/neomake'
Plug 'bling/vim-airline'
Plug 'junegunn/vim-easy-align'
Plug 'kien/ctrlp.vim'
Plug 'mileszs/ack.vim'
Plug 'morhetz/gruvbox'
Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
" Load private vim config
Plug '~/.config/nvim/plugged/private'
call plug#end()
" }}}

" Syntax highlighting {{{
set t_Co=256
set background=dark
syntax on
colorscheme gruvbox
" }}}
