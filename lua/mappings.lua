function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end
--Incremental live completion

-- vim.cmd(':command! Nu :silent ! nu')
-- -- Telescope
-- --Add leader shortcuts
-- map('n', '<C-p>', [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>b', [[<cmd>lua require('telescope.builtin').buffers()<cr>]], { noremap = true, silent = true})
-- map('n', '<C-f>', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>f', [[<cmd>lua require('telescope.builtin').live_grep()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>o', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>ga', [[<cmd>lua require('telescope.builtin').git_commits()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>gp', [[<cmd>lua require('telescope.builtin').git_branches()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>gs', [[<cmd>lua require('telescope.builtin').git_status()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>gb', [[<cmd>lua require('telescope.builtin').git_bcommits()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>reg', [[<cmd>lua require('telescope.builtin').registers()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>col', [[<cmd>lua require('telescope.builtin').colorscheme()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>ky', [[<cmd>lua require('telescope.builtin').keymaps()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>mk', [[<cmd>lua require('telescope.builtin').marks()<cr>]], { noremap = true, silent = true})
-- map('n', '<C-b>', '<cmd>NnnPicker<cr>', { noremap = true, silent=true})


map('n', '<leader>reg', [[<cmd>lua require('fzf-lua').registers()<cr>]], { noremap = true, silent = true})
map('n', '<leader>ky', [[<cmd>lua require('fzf-lua').keymaps()<cr>]], { noremap = true, silent = true})
map('n', '<leader>col', [[<cmd>lua require('fzf-lua').colorschemes()<cr>]], { noremap = true, silent = true})
map('n', '<leader>ga', [[<cmd>lua require('fzf-lua').git_commits()<cr>]], { noremap = true, silent = true})
map('n', '<leader>?', [[<cmd>lua require('fzf-lua').oldfiles()<cr>]], { noremap = true, silent = true})
map('n', '<leader>p', [[<cmd>lua require('fzf-lua').files()<cr>]], { noremap = true, silent = true})
map('n', '<leader>b', [[<cmd>lua require('fzf-lua').buffers()<cr>]], { noremap = true, silent = true})
map('n', '<C-f>', [[<cmd>lua require('fzf-lua').grep_curbuf()<cr>]], { noremap = true, silent = true})
map('n', '<C-p>', [[<cmd>lua require('fzf-lua').git_files()<cr>]], { noremap = true, silent = true})
map('n', '<leader>f', [[<cmd>lua require('fzf-lua').live_grep()<cr>]], { noremap = true, silent = true})
-- give C-c the same hook behavior as esc
map('i', '<C-c>', '<Esc>', { noremap = true })

-- make Y behave like other capital letters
map('n', 'Y', 'y$', { noremap = true })

map('n', 'n', 'nzzzv', { noremap = true })
map('n', 'N', 'Nzzzv', { noremap = true })
map('n', 'J', 'mzJ`z', { noremap = true })

map('i', ',', ',<c-g>u', { noremap = true })
map('i', '.', '.<c-g>u', { noremap = true })
map('i', '!', '!<c-g>u', { noremap = true })
map('i', '?', '?<c-g>u', { noremap = true })


vim.api.nvim_set_keymap('i', '<CR>', 'pumvisible() ? (complete_info().selected == -1 ? "\\<C-e><CR>" : "\\<C-y>" ) : "\\<CR>"', {expr = true})
