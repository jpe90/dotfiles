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
  use {"npxbr/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  -- use {"/home/solaire/git/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  use 'dag/vim-fish'
  use 'rust-lang/rust.vim'
  use "akinsho/nvim-toggleterm.lua"
  use 'sainnhe/sonokai'
  use 'folke/tokyonight.nvim'
  use 'Th3Whit3Wolf/one-nvim'
  use 'axvr/photon.vim'
  -- use 'jpe90/monokai.nvim'
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' ,
    require'nvim-treesitter.configs'.setup {
      ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
      highlight = {
        enable = true,              -- false will disable the whole extension
      },
    }
  }
  -- use 'neovim/nvim-lspconfig'        -- Collection of configurations for built-in LSP client
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
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
  use 'elixir-editors/vim-elixir'
  use 'tomasiser/vim-code-dark'
  -- use 'bluz71/vim-nightfly-guicolors'
  use 'windwp/nvim-autopairs'
  use 'LnL7/vim-nix'
  -- use 'nvim-treesitter/playground'
  use 'mg979/vim-visual-multi'
end)

-- require "nvim-treesitter.configs".setup {
--   playground = {
--     enable = true,
--     disable = {},
--     updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
--     persist_queries = false, -- Whether the query persists across vim sessions
--     keybindings = {
--       toggle_query_editor = 'o',
--       toggle_hl_groups = 'i',
--       toggle_injected_languages = 't',
--       toggle_anonymous_nodes = 'a',
--       toggle_language_display = 'I',
--       focus_language = 'f',
--       unfocus_language = 'F',
--       update = 'R',
--       goto_node = '<cr>',
--       show_help = '?',
--     },
--   }
-- }


require('nvim-autopairs').setup()
-- vim.g.sonokai_style = 'andromeda'
-- vim.g.nvim_tree_show_icons = { git = 0 }


-- tree
vim.g.nvim_tree_auto_close = 1
vim.g.nvim_tree_hijack_netrw = 1
vim.api.nvim_set_keymap('n', '<C-b>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})

--colors
-- require('monokai')
-- vim.cmd('colorscheme monokai')
vim.cmd('colorscheme gruvbox')
vim.o.background = "dark" -- or "light" for light mode
-- vim.g.sonokai_enable_italic=1

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
