set guifont=Courier\ New\ 12
syntax on
set fileencodings=utf-8,latin2
set hlsearch
set nobackup
set autoindent
set nocompatible
set linebreak

set shiftwidth=2 " wcięcie [>]
set cindent shiftwidth=2
set tabstop=8
set softtabstop=2
autocmd BufNewFile,BufRead *.java,*.py setlocal shiftwidth=4 " wcięcie [>]
autocmd BufNewFile,BufRead *.java,*.py setlocal cindent shiftwidth=4
autocmd BufNewFile,BufRead *.java,*.py setlocal tabstop=4
autocmd BufNewFile,BufRead *.java,*.py setlocal softtabstop=4
autocmd BufNewFile,BufRead *.py setlocal textwidth=80
autocmd BufNewFile,BufRead *.py setlocal smarttab
autocmd BufNewFile,BufRead *.py setlocal expandtab
autocmd BufNewFile,BufRead *.py setlocal smartindent

filetype plugin indent on " automatyczne wcięcia dla programistów
" jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal g'\"" | endif
endif
" wyłączanie podświetlania szukania przez Ctrl+N
nmap <C-N> :noh <CR>
