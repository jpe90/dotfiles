syntax on
set wildmenu
set undodir=~/.vim/undo-dir
set wildoptions+=fuzzy
set wildignore=*.o,*~,*.pyc
set wildignore+=*.pdf,*.pyo,*.pyc,*.zip,*.so,*.swp,*.dll,*.o,*.DS_Store,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.a
set wildignore+=.git\*,.hg\*,.svn\*.o,*~,*pyc
set hidden
set undofile
set background=dark
set number
set relativenumber
set et

" set statusline=%F%m%r%h%w%=\ [%Y]\ [%{&ff}]\ [%04l,%04v]\ [%p%%]\ [%L]
set laststatus=2
set backspace=indent,eol,start
set ruler
set smartcase
set hlsearch
set incsearch
" set termguicolors
nmap Q <Nop> " 'Q' in normal mode enters Ex mode. You almost never want this.
set noerrorbells visualbell t_vb=
set mouse+=a
set autoindent
set ignorecase
set modeline
set nohls
" don't indent function return type
:set cino+=t0
filetype plugin indent on
set foldmethod=marker
" let g:coc_user_config="/home/solaire/.vim/coc-settings.json"
colorscheme torte
" highlight MatchParen ctermbg=grey ctermfg=white
highlight ColorColumn ctermbg=darkgrey guibg=lightgrey
highlight RedundantSpaces ctermbg=red guibg=red
highlight Normal ctermfg=white guifg=white
match RedundantSpaces /\s\+$/
let c_no_curly_error=1

" 80 chars/line
set textwidth=0

:command W w
:command Q q

" shortcuts for copying to clipboard
nnoremap Y "+y
vnoremap Y "+y
nnoremap yY ^"+y$

" nnoremap <SPACE> <Nop>
let g:mapleader = ","

map <silent> <leader><cr> :noh<cr>

" ################# EasyAlign #######################
 " Start interactive EasyAlign in visual mode (e.g. vipga)
 " xmap ga <Plug>(EasyAlign)

 " Start interactive EasyAlign for a motion/text object (e.g. gaip)
 " nmap ga <Plug>(EasyAlign)

" ################# FZF #######################

" nnoremap <silent> <C-p> :Files<CR>
" nnoremap <silent> <C-f> :Ag<CR>
" nnoremap <leader>b :Buffers<CR>
" nnoremap <leader>xr :History<CR>

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
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR> " Trim trailing spaces

autocmd FileType css setlocal et tw=80 ts=2 sw=2 sts=2
autocmd FileType c,cpp,h,hpp setlocal et tw=80 ts=4 sw=4 sts=4
autocmd FileType js setlocal et tw=80 ts=2 sw=2 sts=2
autocmd FileType python setlocal et tw=80 ts=4 sw=4 sts=4

" ############## syntax highlighting fix for tmux

" let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
" let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" set termguicolors

" vim hardcodes background color erase even if the terminfo file does
" not contain bce (not to mention that libvte based terminals
" incorrectly contain bce in their terminfo files). This causes
" incorrect background rendering when using a color theme with a
" background color.
" let &t_ut=''

highlight StatusLine cterm=none ctermbg=none ctermfg=darkgrey
highlight StatusLineNC cterm=none ctermbg=none ctermfg=darkgrey

" vimscript
" let g:copilot_node_command = "~/.nvm/versions/node/v16.18.1/bin/node"
" disable copilot
let g:copilot_enabled = 0

" command to toggle copilot
command! -nargs=0 ToggleCopilot :let g:copilot_enabled = !g:copilot_enabled

" bind toggle copilot to <leader>tc
nnoremap <leader>tc :ToggleCopilot<CR>
