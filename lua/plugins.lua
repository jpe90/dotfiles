
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
  use {"junegunn/fzf",
        run = function()
          vim.fn["fzf#install"]()
        end}
  use "junegunn/fzf.vim"
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
  use 'sainnhe/gruvbox-material'
  use {"npxbr/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  use 'dag/vim-fish'
  -- use 'mechatroner/rainbow_csv'
  use 'rust-lang/rust.vim'
  use "akinsho/nvim-toggleterm.lua"
  use 'sainnhe/sonokai'
  use 'folke/tokyonight.nvim'
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
  -- use 'hrsh7th/nvim-compe'           
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
  use 'windwp/nvim-autopairs'
  use 'LnL7/vim-nix'
end)

require('nvim-autopairs').setup()
-- vim.g.sonokai_style = 'andromeda'
-- vim.g.nvim_tree_show_icons = { git = 0 }


-- tree
vim.g.nvim_tree_auto_close = 1
vim.g.nvim_tree_hijack_netrw = 1
vim.api.nvim_set_keymap('n', '<C-n>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})

--colors
-- vim.g.sonokai_enable_italic=1
vim.g.lightline = { colorscheme = 'tokyonight';
      active = { left = { { 'mode', 'paste' }, { 'gitbranch', 'readonly', 'filename', 'modified' } } };
      component_function = { gitbranch = 'fugitive#head', };
}

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

--Map blankline
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'
