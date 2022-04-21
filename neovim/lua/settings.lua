local global = vim.o
local window = vim.wo
local buffer = vim.bo

global.ttimeoutlen = 0
global.updatetime  = 100
global.laststatus  = 2

global.ignorecase = true
global.smartcase  = true
global.incsearch  = true
global.ruler      = true
global.hlsearch   = false
global.splitbelow = true
global.splitright = true

global.fillchars  = "vert: ,fold: "
global.listchars  = "tab:⁞ ,trail:·"
global.wildignore = "*.o,*.ho,*.hi,*.pdf,*.png,*.obj,*.jpg,*.png"
global.diffopt    = "internal,filler,closeoff,foldcolumn:0"

global.scrolloff     = 15
global.sidescrolloff = 5

global.completeopt = "menu,menuone,noinsert,noselect"
global.shortmess   = "filnxtToOFc"

window.list       = true
window.number     = true
window.cursorline = true
window.foldmethod = "marker"

buffer.smartindent = true
buffer.expandtab   = true
buffer.tabstop     = 2
buffer.shiftwidth  = 2
buffer.textwidth   = 80

buffer.formatoptions = "tcroqnj"

global.statusline = "%#PmenuSel# %{b:gitbranch} %#FileName# %<%f %m %="
  .. "%#PmenuSel# %y %6(L%l%) %-6(C%v%) %P "
