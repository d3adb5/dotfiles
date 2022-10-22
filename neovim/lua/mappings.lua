local map = vim.keymap.set

map("n", "<Space>", "za")
map("t", "<Esc>", "<C-\\><C-n>")

map("n", "'", "`")
map("n", "`", "'")

map("n", "<C-k>", vim.diagnostic.goto_prev)
map("n", "<C-j>", vim.diagnostic.goto_next)
