local cmp = require("cmp")
local cap = require("cmp_nvim_lsp").default_capabilities()

local on_attach = function (client, bufnr)
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set("n", "K",     vim.lsp.buf.hover, bufopts)
  vim.keymap.set("n", "gd",    vim.lsp.buf.definition, bufopts)
  vim.keymap.set("n", "gi",    vim.lsp.buf.implementation, bufopts)
  vim.keymap.set("n", "gr",    vim.lsp.buf.references, bufopts)
  vim.keymap.set("n", "gD",    vim.lsp.buf.declaration, bufopts)
  vim.keymap.set("n", "<C-s>", vim.lsp.buf.signature_help, bufopts)

  vim.keymap.set("n", "<leader>f",  vim.lsp.buf.format, bufopts)
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

local servers = {
  "groovyls", "pyright", "terraformls", "hls", "lua_ls",
  "gopls", "clangd", "helm_ls", "bashls"
}

for _, lsp in ipairs(servers) do
  vim.lsp.config(lsp, { on_attach = on_attach, capabilities = cap })
  vim.lsp.enable(lsp)
end
