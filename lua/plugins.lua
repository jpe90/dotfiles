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

  -- languages
  use 'lervag/vimtex'
  use 'neovimhaskell/haskell-vim'

  -- THE POPE
  use 'tpope/vim-fugitive'           -- Git commands in nvim
  use 'tpope/vim-rhubarb'            -- Fugitive-companion to interact with github
  -- use 'tpope/vim-commentary'         -- "gc" to comment visual regions/lines
  use 'tpope/vim-surround'         -- "gc" to comment visual regions/lines
  use 'tpope/vim-unimpaired'
  use 'tpope/vim-repeat'

  use { 'b3nj5m1n/kommentary',}
  -- visuals
  use { 'lukas-reineke/indent-blankline.nvim', branch="master" }
  use 'maksimr/vim-jsbeautify'

  -- utilities
  use 'kyazdani42/nvim-web-devicons'
  use 'kyazdani42/nvim-tree.lua'
  use 'justinmk/vim-sneak'
  use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    require('telescope').setup {
      defaults = {
        generic_sorter =  require'telescope.sorters'.get_fzy_sorter,
        file_sorter =  require'telescope.sorters'.get_fzy_sorter,
      }
    }
  }
  use {
    'hrsh7th/nvim-compe',
    requires = {
      { 'hrsh7th/vim-vsnip' },
      { 'hrsh7th/vim-vsnip-integ'},
      { "rafamadriz/friendly-snippets"}
    }
  }
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    config = function()
      require('gitsigns').setup()
    end
  }
  use 'steelsojka/pears.nvim'
  -- use {'steelsojka/pears.nvim',
  --   config=function()
  --     require "pears".setup(function(conf)
  --       conf.on_enter(function(pears_handle)
  --         if vim.fn.pumvisible() == 1 and vim.fn.complete_info().selected ~= -1 then
  --           return vim.fn["compe#confirm"]("<CR>")
  --         else
  --           pears_handle()
  --         end
  --       end)
  --     end)
  --   end}
  -- use {'windwp/nvim-autopairs',
  --   config = function()
  --     require('nvim-autopairs').setup()
  --   end
  --   }
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' ,
    require'nvim-treesitter.configs'.setup {
      ensure_installed = "all", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
      highlight = {
        enable = true,              -- false will disable the whole extension
      },
      indent = {
        enable = true,
      }
    }
  }
  use {'neovim/nvim-lspconfig'}        -- Collection of configurations for built-in LSP client

  -- colors
  -- use {"jpe90/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  use {"/home/solaire/git/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  use 'projekt0n/github-nvim-theme'
  use 'jpe90/onedark.nvim'
  use 'ray-x/material_plus.nvim'
  use 'nvim-treesitter/playground'
  use 'folke/tokyonight.nvim'
  use {"folke/lua-dev.nvim", opt = true}
  use 'mizlan/iswap.nvim'


end)
require('kommentary.config').configure_language("default", {
    prefer_single_line_comments = true,
})
require('kommentary.config').configure_language("dart", {
    single_line_comment_string = "//",
    multi_line_comment_strings = {"/*", "*/"},
})
require "pears".setup(function(conf)
  conf.on_enter(function(pears_handle)
    if vim.fn.pumvisible() == 1 and vim.fn.complete_info().selected ~= -1 then
      return vim.fn["compe#confirm"]("<CR>")
    else
      pears_handle()
    end
  end)
end)


-- require("toggleterm").setup{
--   open_mapping = [[<M-`>]],
-- }


-- tree
vim.g.nvim_tree_auto_close = 1
vim.g.nvim_tree_hijack_netrw = 1
vim.g.nvim_tree_quit_on_open = 1
vim.g.nvim_tree_lsp_diagnostics = 1
vim.api.nvim_set_keymap('n', '<C-b>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})
-- vim.cmd[[set rtp+=/home/solaire/git/nvim-highlite]]

--colors
-- vim.g.onedark_style='warm'
-- vim.cmd('colorscheme onedark')
-- vim.g.tokyonight_style = "storm"
-- vim.g.tokyonight_italic_functions = true
vim.g.tokyonight_transparent = true
vim.g.tokyonight_transparent_sidebar = true
vim.g.tokyonight_italic_comments = true
vim.cmd[[colorscheme tokyonight]]
--
-- if (tonumber(os.date('%I'))<8)
--   then vim.g.background='dark'
--   else
--     vim.g.background='light'
-- end
-- vim.g.gruvbox_italic = 0
-- vim.g.gruvbox_italicize_comments = 0
vim.g.gruvbox_contrast_light = "hard"
vim.g.gruvbox_contrast_dark = "hard"
-- vim.cmd('colorscheme gruvbox')
-- vim.cmd([[hi Normal guibg=NONE ctermbg=NONE]])

-- require('monokai')
-- vim.cmd('colorscheme monokai')

-- -- Lua
-- require("github-theme").setup({
--   themeStyle = "dark",
--   -- ... your github-theme config
-- })


--Map blankline

vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'


