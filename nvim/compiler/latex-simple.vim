if exists('current_compiler') | finish | endif
let current_compiler = 'latex-simple'

" Use make
CompilerSet makeprg=""
" Use a simple error format
CompilerSet errorformat=%f:%l:\ %m
