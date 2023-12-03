" General settings
set wildmenu                           " Enables a visual interactive menu for command completion.
set undodir=~/.vim/undo-dir            " Sets the directory where undo history files will be stored.
set wildoptions+=fuzzy                 " Adds fuzzy matching to wildmenu completion.
set wildignore=*.o,*~,*.pyc            " Specifies file patterns to ignore in file navigation.
set hidden                             " Allows switching buffers without saving changes.
set undofile                           " Enables persistent undo.
set noswapfile                         " Disables creating swap files.
set number                             " Displays line numbers.
set relativenumber                     " Displays the relative line number for each line.
set et                                 " Expands tabs into spaces.
set background=dark                    " Sets a dark background for color schemes.
set laststatus=2                       " Always displays the status line.
set backspace=indent,eol,start         " Configures backspace to work on indent, EOL, and insert start.
set ruler                              " Shows the cursor position in the status line.
set smartcase                          " Searches are case-insensitive unless containing uppercase.
set hlsearch                           " Highlights all search results.
set incsearch                          " Shows search results as you type.
set noerrorbells visualbell t_vb=      " Disables error bells and visual bells.
set mouse+=a                           " Enables mouse support in all modes.
set ignorecase                         " Makes searches case-insensitive.
set modeline                           " Allows custom per-file settings at the end of files.
set nohls                              " Disables highlighting search matches.
set guifont=Iosevka\ Fixed:h14         " Sets GUI font to Iosevka Fixed size 14.
filetype plugin indent on              " Enables filetype detection and indentation plugins.
set foldmethod=marker                  " Sets folding method to use markers in the text.
syntax on                              " Enables syntax highlighting.
colorscheme retrobox                   " Sets the color scheme to Retrobox.
set textwidth=0                        " Sets maximum text width (0 for no limit).
set clipboard=unnamed                  " Uses the system clipboard for all yank, delete, change, and put operations.

" Key mappings
nmap Q <Nop>
vnoremap Y "+y
nnoremap yY ^"+y$
map <silent> <leader><cr> :noh<cr>
let g:mapleader = " "
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
vnoremap S "_dP
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

" Diff mode mappings
if &diff
	map <leader>1 :diffget LOCAL<CR>
	map <leader>2 :diffget BASE<CR>
	map <leader>3 :diffget REMOTE<CR>
endif

" Cursor style changes
let &t_SI.="\e[5 q" " Insert mode
let &t_SR.="\e[4 q" " Replace mode
let &t_EI.="\e[1 q" " Normal mode

" Encoding and backup settings
set encoding=utf-8
set nobackup
set nowritebackup

" Autocommands
autocmd FileType css setlocal et tw=80 ts=2 sw=2 sts=2
autocmd FileType c,cpp,h,hpp setlocal et tw=80 ts=4 sw=4 sts=4
autocmd FileType js setlocal et tw=80 ts=2 sw=2 sts=2
autocmd FileType python setlocal et tw=80 ts=4 sw=4 sts=4
autocmd FileType go setlocal noexpandtab tw=80 ts=8 sw=8 sts=8
autocmd BufWritePre * :%s/\s\+$//e

" Plugins

" FZF Mappings https://github.com/junegunn/fzf.vim
nnoremap <leader>p :GitFiles<CR>
nnoremap <leader>f :Rg<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>xr :History<CR>

" Copilot settings https://github.com/github/copilot.vim
let g:copilot_enabled = 0
command! -nargs=0 ToggleCopilot :let g:copilot_enabled = !g:copilot_enabled
nnoremap <leader>tc :ToggleCopilot<CR>

" Go-specific settings
command! -nargs=0 Gofmt :!gofmt -s -w %
autocmd FileType go setlocal formatprg=gofmt
