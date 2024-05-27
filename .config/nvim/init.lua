-- Setup {{{1

-- vim: foldmethod=marker

local augroup = vim.api.nvim_create_augroup("init", {})

vim.cmd.runtime("old_init.vim")

-- Mappings {{{1

-- Delete default mappings that set a new undo point
if vim.fn.maparg("<C-w>", "i") ~= "" then vim.keymap.del("i", "<C-w>") end
if vim.fn.maparg("<C-u>", "i") ~= "" then vim.keymap.del("i", "<C-u>") end

vim.keymap.set("n", "gcu", "gcgc", {remap = true})

-- Remap do and dp/dx to use a motion
_G.diff_bufnr = 0
function _G.diffget_operator() vim.cmd.diffget({diff_bufnr, range = {vim.fn.line("'["), vim.fn.line("']")}}) end
function _G.diffput_operator() vim.cmd.diffput({diff_bufnr, range = {vim.fn.line("'["), vim.fn.line("']")}}) end
-- The use of <Cmd> clears the count so that it doesn't affect the motion
vim.keymap.set("n", "do", "<Cmd>set operatorfunc=v:lua.diffget_operator | lua diff_bufnr = vim.v.count<CR>g@")
vim.keymap.set("n", "dp", "<Cmd>set operatorfunc=v:lua.diffput_operator | lua diff_bufnr = vim.v.count<CR>g@")
vim.keymap.set("n", "dx", "dp", {remap = true})
-- Map doo and dpp/dxx to the original behavior
vim.keymap.set("n", "doo", "do")
vim.keymap.set("n", "dpp", "dp")
vim.keymap.set("n", "dpx", "dp")

-- Completion {{{1

local cmp = require("cmp")

local function maybe_complete(fallback)
    if vim.api.nvim_get_current_line():sub(1, vim.api.nvim_win_get_cursor(0)[2]):match("[^%s]$") then
        cmp.complete()
    else
        fallback()
    end
end

cmp.setup({
    sources = {
        {name = "nvim_lsp"},
        {name = "path"},
        {name = "buffer", group_index = 1, option = {get_bufnrs = vim.api.nvim_list_bufs}},
    },
    mapping = cmp.mapping.preset.insert({
        ["<Tab>"] = cmp.mapping(function(fallback)
            if not cmp.select_next_item() then maybe_complete(fallback) end
        end),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if not cmp.select_prev_item() then maybe_complete(fallback) end
        end),
        ["<PageDown>"] = cmp.mapping.scroll_docs(4),
        ["<PageUp>"] = cmp.mapping.scroll_docs(-4),
    }),
    preselect = cmp.PreselectMode.None,
    formatting = {expandable_indicator = false}, --- @diagnostic disable-line: missing-fields
})

-- LSP {{{1

-- Must be included when configuring servers
_G.lsp_client_capabilities = vim.tbl_deep_extend(
    "force",
    vim.lsp.protocol.make_client_capabilities(),
    require("cmp_nvim_lsp").default_capabilities({snippetSupport = false})
)

-- Mappings use the proposed gr prefix: https://github.com/neovim/neovim/pull/28650
vim.keymap.set("n", "grn", vim.lsp.buf.rename)
vim.keymap.set({"n", "x"}, "gra", vim.lsp.buf.code_action)
vim.keymap.set("n", "grr", vim.lsp.buf.references)
vim.keymap.set("n", "grq", vim.lsp.buf.format)

vim.keymap.set("n", "<M-LeftMouse>", "<LeftMouse><Cmd>lua vim.lsp.buf.hover()<CR>")
vim.keymap.set("n", "<M-RightMouse>", "<LeftMouse><Cmd>lua vim.diagnostic.open_float()<CR>")

vim.diagnostic.config({
    severity_sort = true,
    signs = false,
    float = {header = "", prefix = ""},
    virtual_text = {format = function(d) return d.message:match("[^\n]*") end},
})

vim.keymap.set("n", "yoe", function()
    vim.diagnostic.enable(not vim.diagnostic.is_enabled({bufnr = 0}), {bufnr = 0})
end)
vim.keymap.set("n", "yok", function()
    vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({bufnr = 0}), {bufnr = 0})
end)

vim.api.nvim_create_autocmd("LspAttach", {
    group = augroup,
    callback = function(ev)
        local augroup = vim.api.nvim_create_augroup("init_lsp_" .. ev.buf, {})
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if not client then return end
        local signature_triggers = vim.tbl_get(client.server_capabilities, 'signatureHelpProvider', 'triggerCharacters')
        if signature_triggers then
            vim.api.nvim_create_autocmd("InsertCharPre", {
                buffer = ev.buf,
                group = augroup,
                callback = function()
                    if vim.list_contains(signature_triggers, vim.v.char) then
                        vim.api.nvim_feedkeys(vim.keycode("<Cmd>lua vim.lsp.buf.signature_help()<CR>"), "", false)
                    end
                end,
            })
        end
        if client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
            vim.api.nvim_create_autocmd({"CursorHold", "CursorHoldI"}, {
                buffer = ev.buf,
                group = augroup,
                callback = vim.lsp.buf.document_highlight
            })
        end
    end,
})

_G.on_document_highlight = on_document_highlight or vim.lsp.handlers[vim.lsp.protocol.Methods.textDocument_documentHighlight]
vim.lsp.handlers[vim.lsp.protocol.Methods.textDocument_documentHighlight] = function(err, result, ctx, config)
    vim.lsp.util.buf_clear_references(ctx.bufnr)
    on_document_highlight(err, result, ctx, config)
end
