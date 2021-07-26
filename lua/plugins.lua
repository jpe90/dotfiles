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
  use {"npxbr/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  -- use "akinsho/nvim-toggleterm.lua"
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
  -- use 'kyazdani42/nvim-web-devicons'
  -- use 'kyazdani42/nvim-tree.lua'
  use 'windwp/nvim-autopairs'
  use {
    'hrsh7th/nvim-compe',
    requires = {
      { 'hrsh7th/vim-vsnip' },
      { 'hrsh7th/vim-vsnip-integ'},
      { "rafamadriz/friendly-snippets"}
    }
  }


end)

require('gitsigns').setup {}

-- require("toggleterm").setup{
--   open_mapping = [[<M-`>]],
-- }


require('nvim-autopairs').setup()

-- tree
-- vim.g.nvim_tree_auto_close = 1
-- vim.g.nvim_tree_hijack_netrw = 1
-- vim.api.nvim_set_keymap('n', '<C-b>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})

--colors
vim.cmd('colorscheme gruvbox')

--Map blankline

vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'
