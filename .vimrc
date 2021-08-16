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
Plug 'dag/vim-fish'
Plug 'morhetz/gruvbox'
Plug 'tbastos/vim-lua'
Plug 'rust-lang/rust.vim'
Plug 'rhysd/conflict-marker.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-sneak'
Plug 'preservim/nerdtree'
Plug 'LnL7/vim-nix'
Plug 'jiangmiao/auto-pairs'

call plug#end()

"let g:gruvbox_contrast_dark = 'hard'

colorscheme gruvbox
syntax on
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
set clipboard=unnamedplus
hi Normal guibg=NONE ctermbg=NONE
" filetype on
filetype plugin indent on

let g:mapleader = "\<Space>"

" ################# FZF #######################

nnoremap <silent> <C-p> :GFiles<CR>
nnoremap <silent> <leader><space> :Files<CR>
nnoremap <silent> <C-f> :Rg<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>h :History<CR>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt

" ################# VIM-FUGITIVE ##############

nmap <leader>gh :diffget //2<CR>
nmap <leader>gl :diffget //3<CR>
nmap <leader>gs :G<CR>

" ################ HASKELL ####################

" let g:haskell_indent_disable = 1

" ############## new term in current dir

:command Nu :silent ! nu

" ############### NERDTREE

nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-b> :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFind<CR>

" ############## syntax highlighting fix for tmux

if $TERM =~# '256color' && ( $TERM =~# '^screen'  || $TERM =~# '^tmux' )
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif

" vim hardcodes background color erase even if the terminfo file does
" not contain bce (not to mention that libvte based terminals
" incorrectly contain bce in their terminfo files). This causes
" incorrect background rendering when using a color theme with a
" background color.
let &t_ut=''

nnoremap Y y$

nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ`z

inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ? ?<c-g>u
inoremap ! !<c-g>u
