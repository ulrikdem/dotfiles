-- Setup {{{1

-- vim: foldmethod=marker

-- Allow omitting vim. and vim.api.nvim_ prefixes
setmetatable(_G, {__index = vim})
nvim = setmetatable({}, {__index = function(_, key)
    return api["nvim_" .. key]
end})

local augroup = nvim.create_augroup("init.lua", {})

cmd.runtime("old_init.vim")

-- Completion {{{1

nvim.create_autocmd("InsertCharPre", {
    group = augroup,
    callback = function()
        if fn.pumvisible() == 0 and fn.state("m") == "" -- Skip when input is pending, e.g. during . repeat
                and (v.char:match("%a") or list_contains(b.completion_triggers or {}, v.char)) then
            -- Require b.completion_triggers to opt in for omni completion
            nvim.feedkeys(keycode(b.completion_triggers and "<C-x><C-o>" or "<C-n>"), "n", false)
        end
    end,
})

local function set_tab_mapping(tab, complete_key)
    keymap.set("i", tab, function()
        if fn.pumvisible() ~= 0 then
            return fn.reg_recording() == "" and complete_key
        elseif nvim.get_current_line():sub(1, nvim.win_get_cursor(0)[2]):match("[^%s]$") then
            return b.completion_triggers and "<C-x><C-o>" or complete_key
        else
            return tab
        end
    end, {expr = true})
end

set_tab_mapping("<Tab>", "<C-n>")
set_tab_mapping("<S-Tab>", "<C-p>")

-- LSP {{{1

-- Mappings use the proposed gl prefix: https://github.com/neovim/neovim/pull/28650
keymap.set("n", "gln", lsp.buf.rename)
keymap.set({"n", "x"}, "gll", lsp.buf.code_action)
keymap.set("n", "glr", lsp.buf.references)
keymap.set("n", "glq", lsp.buf.format)

keymap.set("n", "<M-LeftMouse>", "<LeftMouse><Cmd>lua lsp.buf.hover()<CR>", {remap = true})
keymap.set("n", "<M-RightMouse>", "<LeftMouse><C-w>d", {remap = true})

nvim.create_autocmd("LspAttach", {
    group = augroup,
    callback = function(ev)
        local client = lsp.get_client_by_id(ev.data.client_id)
        if client.server_capabilities.completionProvider then
            b[ev.buf].completion_triggers = client.server_capabilities.completionProvider.triggerCharacters or {}
        end
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

keymap.set("n", "yok", function()
    lsp.inlay_hint.enable(not lsp.inlay_hint.is_enabled({bufnr = 0}), {bufnr = 0})
end)
keymap.set("n", "yoe", function()
    diagnostic.enable(not diagnostic.is_enabled({bufnr = 0}), {bufnr = 0})
end)

diagnostic.config({
    severity_sort = true,
    signs = false,
    float = {header = ""},
})
