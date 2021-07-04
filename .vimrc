syntax on
" set syntax
set undofile
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
set termguicolors
filetype on
"filetype plugin indent on

call plug#begin('~/.vim/plugged')

Plug 'ziglang/zig.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'dart-lang/dart-vim-plugin'
Plug 'neovimhaskell/haskell-vim'
Plug 'sdiehl/vim-ormolu'
Plug 'dag/vim-fish'
Plug 'morhetz/gruvbox'
Plug 'sainnhe/sonokai'
Plug 'tbastos/vim-lua'
Plug 'mechatroner/rainbow_csv'
Plug 'rust-lang/rust.vim'
Plug 'rhysd/conflict-marker.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'



call plug#end()

"let g:gruvbox_contrast_dark = 'hard'

:colorscheme gruvbox

let g:mapleader = "\<Space>"

" ################# FZF #######################

  nnoremap <silent> <leader><space> :GFiles<CR>
  nnoremap <silent> <leader>f :Files<CR>
  nnoremap <silent> <leader>b :Buffers<CR>
  nnoremap <silent> <leader>h :History<CR>

" ################# VIM-FUGITIVE ##############

nmap <leader>gh :diffget //2<CR>
nmap <leader>gl :diffget //3<CR>
nmap <leader>gs :G<CR>

" ################ HASKELL

let g:haskell_indent_disable = 1


" ############## syntax highlighting fix for vim

if $TERM =~# '256color' && ( $TERM =~# '^screen'  || $TERM =~# '^tmux' )
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
