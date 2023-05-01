local cmp = require("cmp")
local lsp = require("lspconfig")
local cap = require("cmp_nvim_lsp").default_capabilities()

local on_attach = function (client, bufnr)
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set("n", "K",     vim.lsp.buf.hover, bufopts)
  vim.keymap.set("n", "gd",    vim.lsp.buf.definition, bufopts)
  vim.keymap.set("n", "gi",    vim.lsp.buf.implementation, bufopts)
  vim.keymap.set("n", "gr",    vim.lsp.buf.references, bufopts)
  vim.keymap.set("n", "gD",    vim.lsp.buf.declaration, bufopts)
  vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, bufopts)

  vim.keymap.set("n", "<leader>f",  vim.lsp.buf.formatting, bufopts)
  vim.keymap.set("n", "<leader>D",  vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts)
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, bufopts)
end

cmp.setup({
  snippet = { expand = function (a) vim.fn["UltiSnips#Anon"](a.body) end },
  mapping = cmp.mapping.preset.insert({}),
  sources = {
    { name = "nvim_lsp"}, { name = "ultisnips" }, { name = "buffer" },
    { name = "path" }
  }
})

lsp.groovyls.setup({ on_attach = on_attach, capabilities = cap })
lsp.pyright.setup({ on_attach = on_attach, capabilities = cap })
lsp.terraformls.setup({ on_attach = on_attach, capabilities = cap })
lsp.hls.setup({ on_attach = on_attach, capabilities = cap })
lsp.lua_ls.setup({ on_attach = on_attach, capabilities = cap })
lsp.gopls.setup({ on_attach = on_attach, capabilities = cap })
lsp.clangd.setup({ on_attach = on_attach, capabilities = cap })
lsp.helm_ls.setup({ on_attach = on_attach, capabilities = cap })
