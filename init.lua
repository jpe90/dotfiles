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
  use 'tpope/vim-unimpaired'
  use 'tpope/vim-repeat'
  use 'justinmk/vim-sneak'
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    require('telescope').setup {
      defaults = {
        generic_sorter =  require'telescope.sorters'.get_fzy_sorter,
        file_sorter =  require'telescope.sorters'.get_fzy_sorter,
      }
    }
  }
  use { 'lukas-reineke/indent-blankline.nvim', branch="master" }
  use 'neovimhaskell/haskell-vim'
  use 'ziglang/zig.vim'
  use 'dart-lang/dart-vim-plugin'
  -- use 'sainnhe/gruvbox-material'
  use {"npxbr/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  use 'dag/vim-fish'
  -- use 'mechatroner/rainbow_csv'
  use 'sdiehl/vim-ormolu'
  use 'rust-lang/rust.vim'
  use "akinsho/nvim-toggleterm.lua"
  -- use 'sainnhe/sonokai'
  -- use 'folke/tokyonight.nvim'
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' ,
    require'nvim-treesitter.configs'.setup {
      ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
      highlight = {
        enable = true,              -- false will disable the whole extension
      },
    }
  }
  use 'hrsh7th/vim-vsnip'
  use 'hrsh7th/vim-vsnip-integ'
  use 'neovim/nvim-lspconfig'        -- Collection of configurations for built-in LSP client
  use 'hrsh7th/nvim-compe'           -- Autocompletion plugin
  use "rafamadriz/friendly-snippets"
  use 'Neevash/awesome-flutter-snippets'
  -- Add git related info in the signs columns and popups
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    config = function()
      require('gitsigns').setup()
    end
  }
  use "folke/lua-dev.nvim"
  use 'itchyny/lightline.vim'        -- Fancier statusline
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
  use 'elixir-editors/vim-elixir'
  use 'tomasiser/vim-code-dark'
end)

vim.cmd [[
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
set undofile
set clipboard+=unnamedplus
set termguicolors
colorscheme gruvbox
]]

vim.o.background = "dark" -- or "light" for light mode

vim.g.lightline = { colorscheme = 'gruvbox';
      active = { left = { { 'mode', 'paste' }, { 'gitbranch', 'readonly', 'filename', 'modified' } } };
      component_function = { gitbranch = 'fugitive#head', };
}

-- vim.g.sonokai_style = 'andromeda'
-- vim.g.sonokai_enable_italic=1


vim.g.nvim_tree_show_icons = { git = 0 }
vim.g.nvim_tree_auto_close = 1
vim.g.nvim_tree_hijack_netrw = 1

vim.api.nvim_set_keymap('n', '<C-n>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})

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

--Map blankline
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'
-- vim.g.indent_blankline_space_char = "."

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

-- Telescope
--Add leader shortcuts
vim.api.nvim_set_keymap('n', '<leader><leader>', [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>b', [[<cmd>lua require('telescope.builtin').buffers()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>cb', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>tg', [[<cmd>lua require('telescope.builtin').tags()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>ts', [[<cmd>lua require('telescope.builtin').treesitter()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>?', [[<cmd>lua require('telescope.builtin').oldfiles()<cr>]], { noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>fd', [[<cmd>lua require('telescope.builtin').grep_string()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>p', [[<cmd>lua require('telescope.builtin').live_grep()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>o', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gc', [[<cmd>lua require('telescope.builtin').git_commits()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gb', [[<cmd>lua require('telescope.builtin').git_branches()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gs', [[<cmd>lua require('telescope.builtin').git_status()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>gp', [[<cmd>lua require('telescope.builtin').git_bcommits()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>reg', [[<cmd>lua require('telescope.builtin').registers()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>col', [[<cmd>lua require('telescope.builtin').colorscheme()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>ky', [[<cmd>lua require('telescope.builtin').keymaps()<cr>]], { noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>mk', [[<cmd>lua require('telescope.builtin').marks()<cr>]], { noremap = true, silent = true})


-- janky snippet shit
vim.api.nvim_set_keymap("i" , "<C-e>"      , "compe#confirm()" , { noremap = true , expr = true , silent = true })
vim.api.nvim_set_keymap("i" , "<C-l>"     , "vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'" , { noremap = false , expr = true })  -- Ctrl-L to jump on placeholders.
vim.api.nvim_set_keymap("s" , "<C-l>"     , "vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'" , { noremap = false , expr = true })
-- attempt bugfix for autocomplete when i don't want it
vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

-- LSP settings
local nvim_lsp = require('lspconfig')
local on_attach = function(_client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  vim.api.nvim_command("au BufWritePre <buffer> lua vim.lsp.buf.formatting_sync(nil, 1000)")

  local opts = { noremap=true, silent=true }
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'ff', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_set_keymap('n', '<leader>ca', [[<cmd>lua require('telescope.builtin').lsp_code_actions()<cr>]], { noremap = true, silent = true})
  vim.api.nvim_set_keymap('n', '<leader>gr', [[<cmd>lua require('telescope.builtin').lsp_references()<cr>]], { noremap = true, silent = true})
  vim.api.nvim_set_keymap('n', '<leader>wd', [[<cmd>lua require('telescope.builtin').lsp_workspace_diagnostics()<cr>]], { noremap = true, silent = true})

 --
end

local root_pattern = nvim_lsp.util.root_pattern
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

-- Enable the following language servers
-- local servers = { 'clangd', 'rust_analyzer', 'pyright', 'tsserver', 'dartls', 'sumneko_lua', 'hls', 'elixirls'}
local servers = { 'clangd',  'pyright',  'dartls', 'hls' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { 
    on_attach = on_attach,
    --root_dir = root_pattern(".git"),
    capabilities = capabilities,
  }
end

-- require'lspconfig'.elixirls.setup{
--     capabilities = capabilities,
--     on_attach = on_attach,
--     cmd = { "/usr/lib/elixir-ls/language_server.sh" };
-- }


local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif vim.fn.call("vsnip#available", {1}) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end

-- Compe setup
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    nvim_lsp = true;
    vsnip = true;
  };
}


-- vim.api.nvim_set_keymap('n', '<leader>tm', '<cmd>! $TERMINAL . & disown<cr><cr>', { noremap = true, silent=true})
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


