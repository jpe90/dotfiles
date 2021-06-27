syntax on
set termguicolors
set background=dark
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
set tabstop=2
set shiftwidth=2
set softtabstop=-1
set shiftwidth=0
set shiftround
set expandtab
set autoindent
set smartindent
set ignorecase
set modeline
set clipboard+=unnamedplus
filetype plugin indent on


call plug#begin('~/.vim/plugged')

Plug 'ziglang/zig.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-eunuch'
Plug 'dart-lang/dart-vim-plugin'
Plug 'neovimhaskell/haskell-vim'
Plug 'sdiehl/vim-ormolu'
Plug 'dag/vim-fish'
Plug 'morhetz/gruvbox'
Plug 'tbastos/vim-lua'
Plug 'mechatroner/rainbow_csv'
Plug 'rust-lang/rust.vim'
Plug 'rhysd/conflict-marker.vim'

call plug#end()

:colorscheme gruvbox
" ################# VIM-FUGITIVE ##############

nmap <leader>gh :diffget //2<CR>
nmap <leader>gl :diffget //3<CR>
nmap <leader>gs :G<CR>
