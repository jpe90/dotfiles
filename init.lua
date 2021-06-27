-- Install packer
local execute = vim.api.nvim_command

local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '.. install_path)
end
vim.api.nvim_exec([[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]], false)

local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim'       -- Package manager
  use 'tpope/vim-fugitive'           -- Git commands in nvim
  use 'tpope/vim-rhubarb'            -- Fugitive-companion to interact with github
  use 'tpope/vim-commentary'         -- "gc" to comment visual regions/lines
  use 'tpope/vim-surround'         -- "gc" to comment visual regions/lines
  use 'tpope/vim-eunuch'
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    require('telescope').setup {
      defaults = {
        generic_sorter =  require'telescope.sorters'.get_fzy_sorter,
        file_sorter =  require'telescope.sorters'.get_fzy_sorter,
      }
    }
  }
  use { 'lukas-reineke/indent-blankline.nvim', branch="lua" }
  use 'neovimhaskell/haskell-vim'
  use 'ziglang/zig.vim'
  use 'dart-lang/dart-vim-plugin'
  use 'morhetz/gruvbox'
  use 'Mofiqul/vscode.nvim'
  use 'dag/vim-fish'
  use 'wojciechkepka/vim-github-dark'
  use 'mechatroner/rainbow_csv'
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' ,
    require'nvim-treesitter.configs'.setup {
      ensure_installed = "all", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
      highlight = {
        enable = true,              -- false will disable the whole extension
      },
    }
  }
  use 'sdiehl/vim-ormolu'
  use 'rust-lang/rust.vim'
  use { 'kyazdani42/nvim-tree.lua'  }
  use 'sainnhe/sonokai'
  use 'arcticicestudio/nord-vim'
  use "akinsho/nvim-toggleterm.lua"
  ------------------------ heavy
  --use 'hrsh7th/vim-vsnip'
  --use 'hrsh7th/vim-vsnip-integ'
  --
  --use 'hrsh7th/nvim-compe'
  --
  --
  --use 'neovim/nvim-lspconfig'        -- Collection of configurations for built-in LSP client
  --use "folke/lua-dev.nvim"
  --
  --use 'Neevash/awesome-flutter-snippets'
  --use "rafamadriz/friendly-snippets"
  --use  'itchyny/lightline.vim' 
  --

end)

-- require('leftovers')

-- nvim-tree
vim.api.nvim_set_keymap('n', '<C-n>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})


vim.cmd [[
set termguicolors
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
set smartindent
set ignorecase
set background=dark
colorscheme gruvbox
set undofile
set clipboard+=unnamedplus
]]

-- filetype plugin indent on

--Incremental live completion
vim.o.inccommand = "nosplit"

--Set highlight on search
vim.o.hlsearch = false
vim.o.incsearch = true

--Make line numbers default
vim.wo.number = true
vim.wo.relativenumber = true

--Do not save when switching buffers
vim.o.hidden = true

--Enable mouse mode
vim.o.mouse = "a"

--Enable break indent
vim.o.breakindent = true

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

--Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn="yes"

-- gruvbox contrast
-- vim.g.gruvbox_contrast_dark="hard"

--vim.o.completeopt = "menuone,noselect,noinsert"
-- tab navigation
vim.api.nvim_set_keymap('n', '<A-1>', '1gt', { noremap = true, silent=true})
vim.api.nvim_set_keymap('n', '<A-2>', '2gt', { noremap = true, silent=true})
vim.api.nvim_set_keymap('n', '<A-3>', '3gt', { noremap = true, silent=true})
vim.api.nvim_set_keymap('n', '<A-4>', '4gt', { noremap = true, silent=true})
vim.api.nvim_set_keymap('n', '<A-5>', '5gt', { noremap = true, silent=true})
vim.api.nvim_set_keymap('n', '<A-6>', '6gt', { noremap = true, silent=true})
vim.api.nvim_set_keymap('n', '<A-7>', '7gt', { noremap = true, silent=true})
vim.api.nvim_set_keymap('n', '<A-8>', '8gt', { noremap = true, silent=true})

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent=true})
vim.g.mapleader = " "
vim.g.maplocalleader = " "

--Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap=true, expr = true, silent = true})
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", {noremap= true, expr = true, silent = true})

-- --Remap escape to leave terminal mode
vim.api.nvim_exec([[
  augroup Terminal
    autocmd!
    au TermOpen * tnoremap <buffer> <Esc> <c-\><c-n>
    au TermOpen * set nonu
  augroup end
]], false)

--Add map to enter paste mode
vim.o.pastetoggle="<F3>"

--Map blankline
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'

-- toggleterm

require("toggleterm").setup{
  open_mapping = [[<M-`>]],
}

-- haskell

vim.g.haskell_enable_quantification=1
vim.g.haskell_enable_recursivedo=1
vim.g.haskell_enable_arrowsyntax=1
vim.g.haskell_enable_pattern_synonyms=1
vim.g.haskell_enable_typeroles=1
vim.g.haskell_enable_static_pointers=1

--Set statusbar

vim.o.completeopt = "menuone,noselect"

-- Toggle to disable mouse mode and indentlines for easier paste
ToggleMouse = function()
  if vim.o.mouse == 'a' then
    vim.cmd[[IndentBlanklineDisable]]
    vim.wo.signcolumn='no'
    vim.o.mouse = 'v'
    vim.wo.number = false
    print("Mouse disabled")
  else
    vim.cmd[[IndentBlanklineEnable]]
    vim.wo.signcolumn='yes'
    vim.o.mouse = 'a'
    vim.wo.number = true
    print("Mouse enabled")
  end
end

vim.api.nvim_set_keymap('n', '<F10>', '<cmd>lua ToggleMouse()<cr>', { noremap = true })

-- Telescope
--Add leader shortcuts
vim.api.nvim_set_keymap('n', '<leader>p', [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<C-p>', [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true, silent = true, search_dirs = "~"})
vim.api.nvim_set_keymap('n', '<leader>b', [[<cmd>lua require('telescope.builtin').buffers()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader><leader>', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>tg', [[<cmd>lua require('telescope.builtin').tags()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>ts', [[<cmd>lua require('telescope.builtin').treesitter()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>?', [[<cmd>lua require('telescope.builtin').oldfiles()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>fd', [[<cmd>lua require('telescope.builtin').grep_string()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>fp', [[<cmd>lua require('telescope.builtin').live_grep()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>o', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gc', [[<cmd>lua require('telescope.builtin').git_commits()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gb', [[<cmd>lua require('telescope.builtin').git_branches()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gs', [[<cmd>lua require('telescope.builtin').git_status()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gp', [[<cmd>lua require('telescope.builtin').git_bcommits()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>lr', [[<cmd>lua require('telescope.builtin').registers()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>col', [[<cmd>lua require('telescope.builtin').colorscheme()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>ky', [[<cmd>lua require('telescope.builtin').keymaps()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>mk', [[<cmd>lua require('telescope.builtin').marks()<cr>]], { noremap = true, silent = true})


vim.api.nvim_set_keymap('n', '<leader>tm', '<cmd>! $TERMINAL . & disown<cr><cr>', { noremap = true, silent=true})
-- vim.api.nvim_set_keymap('n', '<leader>od', 'yi(<cmd>yi(<cr>', { noremap = true, silent=true})
-- Change preview window location
vim.g.splitbelow = true

-- Highlight on yank
vim.api.nvim_exec([[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]], false)


