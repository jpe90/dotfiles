syntax on
set termguicolors
set hidden
set wildmenu
set number
set relativenumber
set laststatus=2
set backspace=indent,eol,start
set ruler
set ignorecase
set smartcase
set incsearch
nmap Q <Nop> " 'Q' in normal mode enters Ex mode. You almost never want this.
set noerrorbells visualbell t_vb=
set mouse+=a
set splitbelow
set tabstop=4
set shiftwidth=4
set softtabstop=-1
set shiftwidth=0
set shiftround
set expandtab
set autoindent
set smartindent
set ignorecase
set modeline
filetype plugin indent on


call plug#begin('~/.vim/plugged')

Plug 'ziglang/zig.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-eunuch'
Plug 'dart-lang/dart-vim-plugin'
Plug 'neovimhaskell/haskell-vim'
" Plug 'sdiehl/vim-ormolu'
Plug 'dag/vim-fish'
Plug 'wojciechkepka/vim-github-dark'

call plug#end()

:colorscheme ghdark
" ################# VIM-FUGITIVE ##############

nmap <leader>gh :diffget //2<CR>
nmap <leader>gl :diffget //3<CR>
nmap <leader>gs :G<CR>
