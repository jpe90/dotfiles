call plug#begin("~/.config/nvim/plugged")
Plug 'tpope/vim-fugitive'
Plug 'dart-lang/dart-vim-plugin'
Plug 'junegunn/fzf.vim'
Plug 'leafgarland/typescript-vim'
Plug 'ziglang/zig.vim'
Plug 'tpope/vim-eunuch'
Plug 'neovimhaskell/haskell-vim'

call plug#end()


" ############## MISC #####################
set showmatch
set mat=0
set laststatus=2
set nowrap
set history=1000
set backspace=indent,eol,start
syntax enable
set number
set relativenumber
set mouse+=a
set title

autocmd Filetype typescript setlocal shiftwidth=2 tabstop=2
" ############### COLOR ######################
" set termguicolors
" set background=dark


" ############### INDENTATION ###############
"
"
" length of an actual \t character:
set tabstop=4
" length to use when editing text (eg. TAB and BS keys)
" (0 for ‘tabstop’, -1 for ‘shiftwidth’):
set softtabstop=-1
" length to use when shifting text (eg. <<, >> and == commands)
" (0 for ‘tabstop’):
set shiftwidth=0
" round indentation to multiples of 'shiftwidth' when shifting text
" (so that it behaves like Ctrl-D / Ctrl-T):
set shiftround

" if set, only insert spaces; otherwise insert \t and complete with spaces:
set expandtab

" reproduce the indentation of the previous line:
set autoindent
" keep indentation produced by 'autoindent' if leaving the line blank:
"set cpoptions+=I
" try to be smart (increase the indenting level after ‘{’,
" decrease it after ‘}’, and so on):
set smartindent
" a stricter alternative which works better for the C language:
"set cindent
" use language‐specific plugins for indenting (better):
filetype plugin indent on


" ############ DART INDENT ##################

let g:dart_style_guide = 2
let g:dart_format_on_save = 1

" ################### BUGFIX #################

if &encoding != 'utf-8'
	set encoding=utf-8
endif

" ################# VIM-FUGITIVE ##############

nmap <leader>gh :diffget //2<CR>
nmap <leader>gl :diffget //3<CR>
nmap <leader>gs :G<CR>


" ################### FZF ####################

nnoremap <silent> <C-p> :Files<CR>
nnoremap <silent> <C-g> :GFiles<CR>
nnoremap <silent> <C-t> :Tags<CR>
nnoremap <C-f> :Rg! 
let g:fzf_buffers_jump = 1
" [[B]Commits] Customize the options used by 'git log':
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
