local map = vim.keymap.set

map("n", "<Space>", "za")
map("t", "<Esc>", "<C-\\><C-n>")

map("n", "'", "`")
map("n", "`", "'")

map("n", "<C-f>", "<cmd>Files<cr>")
map("n", "<C-l>", "<cmd>Lines<cr>")
