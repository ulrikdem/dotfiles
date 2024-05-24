-- Setup {{{1

-- vim: foldmethod=marker

-- Allow omitting vim. and vim.api.nvim_ prefixes
setmetatable(_G, {__index = vim})
nvim = {}
for k, v in pairs(api) do
    nvim[k:sub(6)] = v
end

local augroup = nvim.create_augroup("init.lua", {})

cmd.runtime("old_init.vim")

-- Mappings {{{1

-- Delete default mappings that set a new undo point
if fn.maparg("<C-w>", "i") ~= "" then
    keymap.del("i", "<C-w>")
end
if fn.maparg("<C-u>", "i") ~= "" then
    keymap.del("i", "<C-u>")
end

keymap.set("n", "gcu", "gcgc", {remap = true})

-- Completion {{{1

local cmp = require("cmp")
cmp.setup({
    sources = {
        {name = "nvim_lsp"},
        {name = "path"},
        {name = "buffer", group_index = 1, option = {get_bufnrs = nvim.list_bufs}},
    },
    mapping = cmp.mapping.preset.insert({
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<Tab>"] = cmp.mapping.select_next_item(),
        ["<S-Tab>"] = cmp.mapping.select_prev_item(),
        ["<PageDown>"] = cmp.mapping.scroll_docs(4),
        ["<PageUp>"] = cmp.mapping.scroll_docs(-4),
    }),
    preselect = cmp.PreselectMode.None,
    formatting = {expandable_indicator = false},
})

-- LSP {{{1

-- Must be included when configuring servers
lsp_client_capabilities = tbl_deep_extend(
    "force",
    lsp.protocol.make_client_capabilities(),
    require("cmp_nvim_lsp").default_capabilities({snippetSupport = false})
)

-- Mappings use the proposed gr prefix: https://github.com/neovim/neovim/pull/28650
keymap.set("n", "grn", lsp.buf.rename)
keymap.set({"n", "x"}, "gra", lsp.buf.code_action)
keymap.set("n", "grr", lsp.buf.references)
keymap.set("n", "grq", lsp.buf.format)

keymap.set("n", "<M-LeftMouse>", "<LeftMouse><Cmd>lua lsp.buf.hover()<CR>", {remap = true})
keymap.set("n", "<M-RightMouse>", "<LeftMouse><C-w>d", {remap = true})

diagnostic.config({
    severity_sort = true,
    signs = false,
    float = {header = ""},
})

keymap.set("n", "yoe", function()
    diagnostic.enable(not diagnostic.is_enabled({bufnr = 0}), {bufnr = 0})
end)
keymap.set("n", "yok", function()
    lsp.inlay_hint.enable(not lsp.inlay_hint.is_enabled({bufnr = 0}), {bufnr = 0})
end)

nvim.create_autocmd("LspAttach", {
    group = augroup,
    callback = function(ev)
        local client = lsp.get_client_by_id(ev.data.client_id)
        local signature_triggers = tbl_get(client.server_capabilities, 'signatureHelpProvider', 'triggerCharacters')
        if signature_triggers then
            nvim.create_autocmd("InsertCharPre", {
                buffer = ev.buf,
                callback = function()
                    if list_contains(signature_triggers, v.char) then
                        nvim.feedkeys(keycode("<Cmd>lua lsp.buf.signature_help()<CR>"), "", false)
                    end
                end,
            })
        end
        if client.server_capabilities.documentHighlightProvider then
            nvim.create_autocmd({"CursorHold", "CursorHoldI"}, {
                buffer = ev.buf,
                callback = lsp.buf.document_highlight,
            })
        end
    end,
})

on_document_highlight = on_document_highlight or lsp.handlers["textDocument/documentHighlight"]
lsp.handlers["textDocument/documentHighlight"] = function(err, result, ctx, config)
    lsp.util.buf_clear_references(ctx.bufnr)
    on_document_highlight(err, result, ctx, config)
end
