-- Setup {{{1

-- vim: foldmethod=marker

local api = vim.api
local augroup = api.nvim_create_augroup("init", {})

vim.cmd.runtime("old_init.vim")

-- Options {{{1

local o = vim.o

-- Mappings {{{1

local map = vim.keymap.set

-- Delete default mappings that set a new undo point
if vim.fn.maparg("<C-w>", "i") ~= "" then vim.keymap.del("i", "<C-w>") end
if vim.fn.maparg("<C-u>", "i") ~= "" then vim.keymap.del("i", "<C-u>") end

map("n", "gcu", "gcgc", {remap = true})

-- Remap do and dp/dx to use a motion
_G.diff_bufnr = 0
function _G.diffget_operator() vim.cmd.diffget({diff_bufnr, range = {vim.fn.line("'["), vim.fn.line("']")}}) end
function _G.diffput_operator() vim.cmd.diffput({diff_bufnr, range = {vim.fn.line("'["), vim.fn.line("']")}}) end
-- The use of <Cmd> clears the count so that it doesn't affect the motion
map("n", "do", "<Cmd>set operatorfunc=v:lua.diffget_operator | lua diff_bufnr = vim.v.count<CR>g@")
map("n", "dp", "<Cmd>set operatorfunc=v:lua.diffput_operator | lua diff_bufnr = vim.v.count<CR>g@")
map("n", "dx", "dp", {remap = true})
-- Map doo and dpp/dxx to the original behavior
map("n", "doo", "do")
map("n", "dpp", "dp")
map("n", "dpx", "dp")

-- The mappings above don't work in visual mode. As alternative, allow gv in operator-pending mode
map("o", "gv", "<Cmd>normal! gv<CR>")

map("n", "<C-Tab>", "gt")
map("n", "<C-S-Tab>", "gT")

-- Statusline etc {{{1

o.statusline = " %{v:lua.statusline_git()}%<%{v:lua.statusline_path(0)} %{v:lua.statusline_modified()}"
    .. "%=%{v:lua.statusline_diagnostics()}%l,%c%V %P "
vim.g.qf_disable_statusline = true -- Don't let quickfix ftplugin override statusline

o.title = true
o.titlestring = "%{v:lua.statusline_path(0, v:true)} - nvim"
o.titlelen = 0

o.tabline = "%!v:lua.tabline()"

function _G.tabline()
    local s = "%T" -- For some reason the first item has no effect, so add a useless one
    for i, tab in ipairs(api.nvim_list_tabpages()) do
        s = s .. "%" .. i .. "T"
            .. (tab == api.nvim_get_current_tabpage() and "%#TabLineSel#" or "%#TabLine#")
            .. " %.40(%{v:lua.statusline_path(nvim_tabpage_get_win(" .. tab .. "))}%) "
    end
    return s .. "%T%#TabLineFill#"
end

function _G.statusline_path(winid, absolute)
    local name = api.nvim_buf_get_name(api.nvim_win_get_buf(winid))
    if name:match("^term://") then
        return name:gsub("^term://.-//%d+:", "term://")
    elseif name:match("^fugitive://") then
        return vim.fn.fnamemodify(vim.fn.FugitiveReal(name), absolute and ":~" or ":~:.")
    end
    return api.nvim_eval_statusline(absolute and "%F" or "%f", {winid = winid}).str
end

function _G.statusline_git()
    local s = vim.fn.FugitiveStatusline()
    local match = s:match("^%[Git%((.*)%)%]$") or s:match("^%[Git:(.-)%(.*%)%]$")
    return match and match .. "  " or ""
end

function _G.statusline_modified()
    return o.modified and "+ " or ""
end

function _G.statusline_diagnostics()
    local count = 0
    for _, v in pairs(vim.diagnostic.count(0)) do count = count + v end
    return count > 0 and count .. "⚠ " or ""
end

api.nvim_create_autocmd("DiagnosticChanged", {command = "redrawstatus!", group = augroup})

-- Autocommands {{{1

api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "sh,zsh",
    callback = function(ev)
        -- The default :ShKeywordPrg and :ZshKeywordPrg are broken in nvim
        vim.bo[ev.buf].keywordprg = ":Man"
    end
})

api.nvim_create_autocmd("TextYankPost", {
    group = augroup,
    callback = function()
        vim.highlight.on_yank({higroup = "Visual", on_visual = false})
    end,
})

-- Completion {{{1

local cmp = require("cmp")

local function maybe_complete(fallback)
    if api.nvim_get_current_line():sub(1, api.nvim_win_get_cursor(0)[2]):match("[^%s]$") then
        cmp.complete()
    else
        fallback()
    end
end

cmp.setup({
    sources = {
        {name = "nvim_lsp"},
        {name = "path"},
        {name = "buffer", group_index = 1, option = {get_bufnrs = api.nvim_list_bufs}},
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

local lsp = vim.lsp

-- Mappings use the proposed gr prefix: https://github.com/neovim/neovim/pull/28650
map("n", "grn", lsp.buf.rename)
map({"n", "x"}, "gra", lsp.buf.code_action)
map("n", "grr", lsp.buf.references)
map("n", "grq", lsp.buf.format)

map("n", "<M-LeftMouse>", "<LeftMouse><Cmd>lua vim.lsp.buf.hover()<CR>")
map("n", "<M-RightMouse>", "<LeftMouse><Cmd>lua vim.diagnostic.open_float()<CR>")

vim.diagnostic.config({
    severity_sort = true,
    signs = false,
    float = {header = "", prefix = ""},
    virtual_text = {format = function(d) return d.message:match("[^\n]*") end},
})

map("n", "yoe", function()
    vim.diagnostic.enable(not vim.diagnostic.is_enabled())
end)
map("n", "yok", function()
    lsp.inlay_hint.enable(not lsp.inlay_hint.is_enabled({}))
end)

--- @param ... string | string[] | fun(name: string, path: string): boolean
--- @return string?
function _G.find_root(...)
    return vim.iter({...}):fold(nil, function(root, marker) return root or vim.fs.root(0, marker) end)
end

--- @class LspConfig: vim.lsp.ClientConfig
--- @field sandbox? {args?: string[], read?: string[], write?: string[]}

--- @param config LspConfig
function _G.start_lsp(config)
    if fn.executable(config.cmd[1]) ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
        config.name = config.cmd[1]
        if config.sandbox then
            config.cmd = vim.iter({
                -- LSP servers should check that the parent is still alive, else exit, so share the pid namspace
                "sandbox", "-spid",
                config.sandbox.args or {},
                -- Use tbl_values to filter out nils
                vim.tbl_map(function(p) return "-r" .. p end, vim.tbl_values(config.sandbox.read or {})),
                vim.tbl_map(function(p) return "-w" .. p end, vim.tbl_values(config.sandbox.write or {})),
                "--", config.cmd,
            }):flatten():totable()
            for _, dir in pairs(config.sandbox.write or {}) do fn.mkdir(dir, "p") end
        end
        config.capabilities = vim.tbl_deep_extend("force",
            lsp.protocol.make_client_capabilities(),
            require("cmp_nvim_lsp").default_capabilities({snippetSupport = false}))
        vim.lsp.start(config, {
            bufnr = 0,
            reuse_client = function(client, config)
                -- The default considers nil root_dirs to be distinct, resulting in a new client for each buffer
                return client.name == config.name and client.root_dir == config.root_dir
            end,
        })
    end
end

api.nvim_create_autocmd("LspAttach", {
    group = augroup,
    callback = function(ev)
        local augroup = api.nvim_create_augroup("init_lsp_" .. ev.buf, {})
        local client = lsp.get_client_by_id(ev.data.client_id)
        if not client then return end
        local signature_triggers = vim.tbl_get(client.server_capabilities, "signatureHelpProvider", "triggerCharacters")
        if signature_triggers then
            api.nvim_create_autocmd("InsertCharPre", {
                buffer = ev.buf,
                group = augroup,
                callback = function()
                    if vim.list_contains(signature_triggers, vim.v.char) then
                        api.nvim_feedkeys(vim.keycode("<Cmd>lua vim.lsp.buf.signature_help()<CR>"), "", false)
                    end
                end,
            })
        end
        if client.supports_method(lsp.protocol.Methods.textDocument_documentHighlight) then
            api.nvim_create_autocmd({"CursorHold", "CursorHoldI"}, {
                buffer = ev.buf,
                group = augroup,
                callback = lsp.buf.document_highlight
            })
        end
    end,
})

_G.on_document_highlight = on_document_highlight or lsp.handlers[lsp.protocol.Methods.textDocument_documentHighlight]
lsp.handlers[lsp.protocol.Methods.textDocument_documentHighlight] = function(err, result, ctx, config)
    lsp.util.buf_clear_references(ctx.bufnr)
    on_document_highlight(err, result, ctx, config)
end
