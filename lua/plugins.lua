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
  -- use 'ggandor/lightspeed.nvim'
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
  use {"npxbr/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  use 'dag/vim-fish'
  use 'rust-lang/rust.vim'
  use "akinsho/nvim-toggleterm.lua"
  use 'folke/tokyonight.nvim'
  use 'Th3Whit3Wolf/one-nvim'
  use 'axvr/photon.vim'
  -- use 'jpe90/monokai.nvim'
  -- use 'tanvirtin/monokai.nvim'
  use 'sainnhe/sonokai'
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' ,
    require'nvim-treesitter.configs'.setup {
      ensure_installed = "all", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
      highlight = {
        enable = true,              -- false will disable the whole extension
      },
    }
  }
  use {'neovim/nvim-lspconfig'}        -- Collection of configurations for built-in LSP client
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
  use {"folke/lua-dev.nvim", opt = true}
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
  use 'elixir-editors/vim-elixir'
  use 'windwp/nvim-autopairs'
  use 'LnL7/vim-nix'
  use 'mg979/vim-visual-multi'
  use {
    'hrsh7th/nvim-compe',
    requires = {
      { 'hrsh7th/vim-vsnip' },
      { 'hrsh7th/vim-vsnip-integ'},
      { "rafamadriz/friendly-snippets"}
    }
  }


end)

require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  numhl = false,
  linehl = false,
  keymaps = {
    -- Default keymap options
    noremap = true,

    ['n ]c'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'"},
    ['n [c'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'"},

    ['n <leader>hS'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['v <leader>hS'] = '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <leader>hu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['v <leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <leader>hR'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n <leader>hp'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <leader>hb'] = '<cmd>lua require"gitsigns".blame_line(true)<CR>',

    -- Text objects
    ['o ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>'
  },
  watch_index = {
    interval = 1000,
    follow_files = true
  },
  current_line_blame = false,
  current_line_blame_delay = 1000,
  current_line_blame_position = 'eol',
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  word_diff = false,
  use_decoration_api = true,
  use_internal_diff = true,  -- If luajit is present
}

require("toggleterm").setup{
  open_mapping = [[<M-`>]],
}


require('nvim-autopairs').setup()
vim.g.sonokai_style = 'espresso'


-- tree
vim.g.nvim_tree_auto_close = 1
vim.g.nvim_tree_hijack_netrw = 1
vim.api.nvim_set_keymap('n', '<C-b>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})

--colors
-- require('monokai')
vim.cmd('colorscheme sonokai')
-- vim.o.background = "dark" -- or "light" for light mode
-- vim.g.sonokai_enable_italic=1



-- haskell

vim.g.haskell_enable_quantification=1
vim.g.haskell_enable_recursivedo=1
vim.g.haskell_enable_arrowsyntax=1
vim.g.haskell_enable_pattern_synonyms=1
vim.g.haskell_enable_typeroles=1
vim.g.haskell_enable_static_pointers=1

--Map blankline

vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'
