" ========================
" General Settings
" ========================
syntax on
set nowrap
set wildmenu
set wildoptions+=fuzzy
set wildignore=*.o,*~,*.pyc,*.pdf,*.pyo,*.zip,*.so,*.swp,*.dll,*.DS_Store,*.obj,*.bak,*.exe,*.jpg,*.gif,*.png,*.a,.git\*,.hg\*,.svn\*
set undodir=~/.vim/undo-dir
set undofile
set noswapfile
set hidden
set number
set relativenumber
set et
set guicursor+=a:blinkon0
set background=dark
set laststatus=2
set backspace=indent,eol,start
set ruler
set smartcase
set hlsearch
set incsearch
set noerrorbells
set visualbell
set t_vb=
set mouse+=a
set ignorecase
set modeline
set nohls
set textwidth=0
set clipboard=unnamed
filetype plugin indent on
set foldmethod=marker
let c_no_curly_error=1
let g:mapleader = " "

" ========================
" Mappings
" ========================
nmap Q <Nop>
" Clear search highlight
map <silent> <leader><cr> :noh<cr>

nnoremap <silent> <C-p> :Files<CR>
nnoremap <leader>f :Rg<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>xr :History<CR>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
vnoremap Y "+y
nnoremap yY ^"+y$
vnoremap S "_dP

" ========================
" Commands
" ========================
:command W w
:command Q q
command! -nargs=0 Gofmt :!gofmt -s -w %

" ========================
" Autocommands
" ========================
autocmd FileType css setlocal et tw=80 ts=2 sw=2 sts=2
autocmd FileType c,cpp,h,hpp setlocal et tw=80 ts=4 sw=4 sts=4
autocmd FileType js setlocal et tw=80 ts=2 sw=2 sts=2
autocmd FileType python setlocal et tw=80 ts=4 sw=4 sts=4
autocmd FileType go setlocal noexpandtab tw=80 ts=8 sw=8 sts=8
autocmd BufWritePre * :%s/\s\+$//e
au BufRead,BufNewFile *.gohtml set filetype=gotexttmpl
autocmd FileType go setlocal formatprg=gofmt
if &diff
    map <leader>1 :diffget LOCAL<CR>
    map <leader>2 :diffget BASE<CR>
    map <leader>3 :diffget REMOTE<CR>
endif

" ========================
" Other Configurations
" ========================
set encoding=utf-8
set nobackup
set nowritebackup

" Cursor settings
let &t_SI.="\e[5 q" "SI = INSERT mode
let &t_SR.="\e[4 q" "SR = REPLACE mode
let &t_EI.="\e[1 q" "EI = NORMAL mode (ELSE)

