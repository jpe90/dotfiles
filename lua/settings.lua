vim.o.inccommand = "nosplit"
--Set highlight on search
vim.o.hlsearch = false
vim.o.incsearch = true
vim.o.ignorecase = true
vim.o.smartcase = true
-- vim.o.termguicolors = true
vim.o.termguicolors = false
vim.o.synmaxcol = 0
-- expand tabs to spaces
vim.o.expandtab = true
vim.o.hidden = true
--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true--Enable mouse mode
vim.o.mouse = "a"
--Decrease update time
vim.o.updatetime = 250
--Do not save when switching buffers
--Enable break indent
vim.o.breakindent = true
--vim.o.completeopt = "menuone,noselect,noinsert"


-- Change preview window location
vim.g.splitbelow = true
vim.g.mapleader = " "
vim.g.maplocalleader = " "

--Make line numbers default
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.signcolumn="yes"

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

-- Highlight on yank
vim.api.nvim_exec([[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]], false)

vim.cmd [[
set tabstop=2
set shiftwidth=2
set softtabstop=-1
set shiftwidth=0
set shiftround
set expandtab
set undofile
set clipboard+=unnamedplus
]]
