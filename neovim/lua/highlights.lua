function hl(name, val)
  vim.api.nvim_set_hl(0, name, val)
end

-- There's a new default colorscheme for Neovim >=0.10.
vim.cmd.colorscheme("vim")

hl("CursorLine",   { ctermbg =  0 })
hl("CursorLineNr", { ctermfg = 11, ctermbg = 0})
hl("NormalFloat",  {})
hl("Pmenu",        { ctermfg =  7, ctermbg = 0 })
hl("PmenuSel",     { ctermfg = 15, ctermbg = 8 })
hl("Todo",         { ctermfg = 15, ctermbg = 7 })
hl("TabLineFill",  {})
hl("TabLine",      {})
hl("VertSplit",    { ctermfg = 7, cterm = { bold = true } })
hl("StatusLine",   { ctermfg = 0, ctermbg = 7, cterm = { bold = true } })
hl("StatusLineNC", { ctermbg = 0 })
hl("Folded",       { ctermfg = 15, ctermbg = 0 })
hl("SignColumn",   { ctermfg = 14 })
hl("FileName",     { ctermfg = 12, ctermbg = 0 })

hl("DiffAdd",    { ctermbg = 22 })
hl("DiffDelete", { ctermbg = 88 })
hl("DiffText",   { ctermbg = 91 })
hl("DiffChange", { ctermbg = 24 })
