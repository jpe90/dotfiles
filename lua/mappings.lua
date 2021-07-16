function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end
--Incremental live completion


map("n", "<C-p>", ":Files<cr>", {silent = true, noremap = true})
map("n", "<leader>ff", ":GFiles<cr>", {silent = true, noremap = true})
map("n", "<leader>rg", ":Rg<cr>", {silent = true, noremap = true})
-- Telescope
--Add leader shortcuts
map('n', '<C-p>', [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true, silent = true})
map('n', '<leader>b', [[<cmd>lua require('telescope.builtin').buffers()<cr>]], { noremap = true, silent = true})
map('n', '<C-f>', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>]], { noremap = true, silent = true})
map('n', '<leader>tg', [[<cmd>lua require('telescope.builtin').tags()<cr>]], { noremap = true, silent = true})
map('n', '<leader>ts', [[<cmd>lua require('telescope.builtin').treesitter()<cr>]], { noremap = true, silent = true})
map('n', '<leader>?', [[<cmd>lua require('telescope.builtin').oldfiles()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>fd', [[<cmd>lua require('telescope.builtin').grep_string()<cr>]], { noremap = true, silent = true})
-- map('n', '<leader>p', [[<cmd>lua require('telescope.builtin').live_grep()<cr>]], { noremap = true, silent = true})
map('n', '<leader>o', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<cr>]], { noremap = true, silent = true})
map('n', '<leader>gc', [[<cmd>lua require('telescope.builtin').git_commits()<cr>]], { noremap = true, silent = true})
map('n', '<leader>gb', [[<cmd>lua require('telescope.builtin').git_branches()<cr>]], { noremap = true, silent = true})
map('n', '<leader>gs', [[<cmd>lua require('telescope.builtin').git_status()<cr>]], { noremap = true, silent = true})
map('n', '<leader>gp', [[<cmd>lua require('telescope.builtin').git_bcommits()<cr>]], { noremap = true, silent = true})
map('n', '<leader>reg', [[<cmd>lua require('telescope.builtin').registers()<cr>]], { noremap = true, silent = true})
map('n', '<leader>col', [[<cmd>lua require('telescope.builtin').colorscheme()<cr>]], { noremap = true, silent = true})
map('n', '<leader>ky', [[<cmd>lua require('telescope.builtin').keymaps()<cr>]], { noremap = true, silent = true})
map('n', '<leader>mk', [[<cmd>lua require('telescope.builtin').marks()<cr>]], { noremap = true, silent = true})
