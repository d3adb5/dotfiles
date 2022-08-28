local cmp = require("cmp")
local lsp = require("lspconfig")
local cap = require("cmp_nvim_lsp")
  .update_capabilities(vim.lsp.protocol.make_client_capabilities())

cmp.setup({
  snippet = { expand = function (a) vim.fn["UltiSnips#Anon"](a.body) end },
  mapping = cmp.mapping.preset.insert({}),
  sources = {
    { name = "nvim_lsp"}, { name = "ultisnips" }, { name = "buffer" },
    { name = "path" }
  }
})

lsp.groovyls.setup({ capabilities = cap })
lsp.pyright.setup({ capabilities = cap })
lsp.terraformls.setup({ capabilities = cap })
lsp.hls.setup({ capabilities = cap })
lsp.sumneko_lua.setup({ capabilities = cap })
