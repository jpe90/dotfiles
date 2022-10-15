syntax on
set wildmenu
if has('nvim')
	set undodir=~/.config/nvim/undo-dir
else
	set undodir=~/.vim/undo-dir
	set wildoptions+=fuzzy
endif
set hidden
set undofile
set background=dark
set number
set relativenumber
set laststatus=2
set backspace=indent,eol,start
set ruler
set smartcase
set hlsearch
set incsearch
nmap Q <Nop> " 'Q' in normal mode enters Ex mode. You almost never want this.
set noerrorbells visualbell t_vb=
set mouse+=a
set splitbelow
set autoindent
set ignorecase
set modeline
filetype plugin indent on
set foldmethod=marker
let g:coc_user_config="/home/solaire/.vim/coc-settings.json"
" colorscheme torte
highlight ColorColumn ctermbg=darkgrey guibg=lightgrey
highlight RedundantSpaces ctermbg=red guibg=red
match RedundantSpaces /\s\+$/
let c_no_curly_error=1

" 80 chars/line
set textwidth=0

if exists('&colorcolumn')
  set colorcolumn=80
endif

:command W w
:command Q q

" nnoremap <SPACE> <Nop>
let g:mapleader = ","

map <silent> <leader><cr> :noh<cr>

" ################# EasyAlgin #######################
 " Start interactive EasyAlign in visual mode (e.g. vipga)
 xmap ga <Plug>(EasyAlign)

 " Start interactive EasyAlign for a motion/text object (e.g. gaip)
 nmap ga <Plug>(EasyAlign)

" ################# FZF #######################

nnoremap <silent> <C-p> :Files<CR>
nnoremap <silent> <C-f> :Ag<CR>
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
nnoremap <silent> <leader>cd :cd %:p:h<cr>

autocmd FileType css setlocal et tw=80 ts=2 sw=2 sts=2

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

