 -- Install packer
local execute = vim.api.nvim_command

local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
local function t(str)
    -- Adjust boolean arguments as needed
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end


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

  -- use {'karb94/neoscroll.nvim',
  --   config = function()
  --     require('neoscroll').setup()
  --   end
  -- }

  -- THE POPE
  use 'tpope/vim-fugitive'           -- Git commands in nvim
  use 'tpope/vim-rhubarb'            -- Fugitive-companion to interact with github
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

  -- use { 'ibhagwan/fzf-lua',
  --   requires = {
  --     'vijaymarupudi/nvim-fzf',
  --     'kyazdani42/nvim-web-devicons' } -- optional for icons
  -- }

  use {
    'ms-jpq/coq_nvim',
    requires = {
      'ms-jpq/coq.artifacts'
    },
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
  use {'neovim/nvim-lspconfig'}        -- Collection of configurations for built-in LSP client

  -- colors
  -- use {"jpe90/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  use 'rakr/vim-one'
  use 'dracula/vim'
  -- use '/home/solaire/git/vim'
  use 'gruvbox-community/gruvbox'
  -- use '~/git/gruvbox'
  use 'rafamadriz/neon'
  use 'projekt0n/github-nvim-theme'
  use 'jpe90/onedark.nvim'
  use 'ray-x/material_plus.nvim'
  use 'folke/tokyonight.nvim'
  use {"folke/lua-dev.nvim", opt = true}


end)
require('kommentary.config').configure_language("default", {
    prefer_single_line_comments = true,
})
require('kommentary.config').configure_language("dart", {
    single_line_comment_string = "//",
    multi_line_comment_strings = {"/*", "*/"},
})
-- require "pears".setup()
vim.g.coq_settings = {
  ["auto_start"] = true,
  ["keymap.jump_to_mark"] = "<c-n>",
  ["keymap.bigger_preview"] = "<c-b>",
  ["clients.tmux.enabled"] = false,
  ["clients.tree_sitter.enabled"] = false,
}

-- autopair with coq
require "pears".setup(function(conf)
  conf.on_enter(function(pears_handle)
    if vim.fn.pumvisible() == 1 and vim.fn.complete_info().selected ~= -1 then
      return (t'<C-y>')
    else
      pears_handle()
    end
  end)
end)

--[[ vim.g.coq_settings = {
  coq_settings.keymap.recommended = true,
} ]]
-- vim.g.coq_settings.keymap.recommended=true

-- tree
vim.g.nvim_tree_auto_close = 1
vim.g.nvim_tree_hijack_netrw = 1
vim.g.nvim_tree_quit_on_open = 1
vim.g.nvim_tree_lsp_diagnostics = 1
vim.api.nvim_set_keymap('n', '<C-b>', '<cmd>NvimTreeToggle<cr>', { noremap = true, silent=true})

--colors
-- vim.g.onedark_style='warm'
-- vim.cmd('colorscheme onedark')
-- vim.g.tokyonight_style = "storm"

-- vim.g.tokyonight_italic_functions = true
-- vim.g.tokyonight_transparent = true
-- vim.g.tokyonight_transparent_sidebar = true
-- vim.g.tokyonight_italic_comments = true
-- vim.cmd[[colorscheme tokyonight]]

-- -- if (tonumber(os.date('%I'))<8)
-- --   then vim.g.background='dark'
-- --   else
-- --     vim.g.background='light'
-- -- end

-- vim.g.gruvbox_italic = 0
-- vim.g.gruvbox_italicize_comments = 0
-- vim.g.gruvbox_contrast_light = "hard"
vim.g.gruvbox_contrast_dark = "hard"
vim.cmd('colorscheme gruvbox')

-- vim.g.dracula_italic=0
-- vim.cmd('colorscheme dracula')


-- vim.g.neon_style = "default"
-- vim.g.neon_italic_keyword = true
-- vim.g.neon_italic_function = true
-- vim.g.neon_transparent = true
-- vim.cmd[[colorscheme neon]]

vim.cmd([[hi Normal guibg=NONE ctermbg=NONE]])

-- require("github-theme").setup({
--   hideInactiveStatusline = true,
--   darkSidebar = false,
--   darkFloat = true,
--   transparent = true,
--   colors = {bg_statusline = "#000000"},
-- })

--Map blankline
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile'}
vim.g.indent_blankline_char_highlight = 'LineNr'

-- treesitter INTHE DUMPSTER
  -- use { '/home/solaire/git/nvim-treesitter', run = ':TSUpdate' ,
  -- -- use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' ,
  --   require'nvim-treesitter.configs'.setup {
  --     ensure_installed = "all", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  --     highlight = {
  --       enable = true,              -- false will disable the whole extension
  --       custom_captures = {
  --         -- ["_string_fragment"] = "HsString",
  --         ["escape"] = "Character",
  --       }
  --     },
  --     indent = {
  --       enable = true,
  --     }
  --   }
  -- }
  -- use { 'nvim-treesitter/nvim-treesitter-refactor',}

-- local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
-- parser_config.porcupine = {
--   install_info = {
--     url = "/home/solaire/.config/nvim/tree-sitter/tree-sitter-porcupine",
--     files = {"src/parser.c"}
--   }
-- }
  -- use 'nvim-treesitter/playground'
--
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
--   },
--   query_linter = {
--     enable = true,
--     use_virtual_text = true,
--     lint_events = {"BufWrite", "CursorHold"},
--   },
--     refactor = {
--     smart_rename = {
--       enable = true,
--       keymaps = {
--         smart_rename = "<leader>zz",
--       },
--     },
--   },
-- }
  -- use 'mizlan/iswap.nvim'
