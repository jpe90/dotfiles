" Turn on syntax highlighting.
syntax on

" Show line numbers.
set number

" This enables relative line numbering mode. With both number and
" relativenumber enabled, the current line shows the true line number, while
" all other lines (above and below) are numbered relative to the current line.
set relativenumber

" Always show the status line at the bottom, even if you only have one window open.
set laststatus=2

" backspace over anything.
set backspace=indent,eol,start

" This setting makes search case-insensitive when all characters in the string
" being searched are lowercase. However, the search becomes case-sensitive if
" it contains any capital letters. This makes searching more convenient.
set ignorecase
set smartcase

" Enable searching as you type, rather than waiting till you press enter.
set incsearch

" Unbind some useless/annoying default key bindings.
nmap Q <Nop> " 'Q' in normal mode enters Ex mode. You almost never want this.

" Disable audible bell because it's annoying.
set noerrorbells visualbell t_vb=

" Enable mouse support. You should avoid relying on this too much, but it can
" sometimes be convenient.
set mouse+=a

call plug#begin('~/.vim/plugged')

Plug 'ziglang/zig.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-eunuch'
Plug 'dart-lang/dart-vim-plugin'
Plug 'neovimhaskell/haskell-vim'
Plug 'dag/vim-fish'

call plug#end()

" ############################ MISC COMMANDS ######################
set tabstop=4
set shiftwidth=4

" colorscheme minimalist
" hi Normal guibg=NONE ctermbg=NONE

" ######################### BLOCK CURSOR #########################

let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" Optionally reset the cursor on start:
augroup myCmds
au!
autocmd VimEnter * silent !echo -ne "\e[2 q"
augroup END


" ################### FZF ####################

" nnoremap <silent> <C-p> :Files<CR>
" nnoremap <silent> <C-g> :GFiles<CR>
" nnoremap <silent> <C-t> :Tags<CR>
" nnoremap <C-f> :Rg! 
" let g:fzf_buffers_jump = 1
" " [[B]Commits] Customize the options used by 'git log':
" let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
