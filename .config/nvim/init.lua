-- Setup {{{1

-- vim: foldmethod=marker

local api = vim.api
local fn = vim.fn
local lsp = vim.lsp
local map = vim.keymap.set
local o = vim.o
local uv = vim.uv --- @type table

vim.cmd.runtime("old_init.vim")
vim.cmd.colorscheme("ulrikdem")

-- Options {{{1

o.ignorecase = true
o.smartcase = true

o.wildignorecase = true
o.wildmode = "longest:full,full"

o.completeopt = "menuone,noselect,popup"
o.dictionary = "/usr/share/dict/words"

vim.opt.shortmess:append("Ic")

o.cursorline = true
o.cursorlineopt = "number"

o.relativenumber = true
o.numberwidth = 3

o.scrolloff = 4
o.smoothscroll = true

o.linebreak = true
o.breakindent = true

o.tabstop = 4
o.shiftwidth = 0
o.expandtab = true

o.list = true
o.listchars = "tab:→ ,trail:·,nbsp:·"

vim.opt.diffopt:append("vertical,foldcolumn:1,algorithm:histogram,linematch:60,hiddenoff")

o.fillchars = "foldopen:▾,foldclose:▸"
o.foldcolumn = "auto:9"
o.foldtext = "v:lua.foldtext()"
function _G.foldtext()
    return ("%s %s (%d lines) "):format(
        fn["repeat"]("·", vim.v.foldlevel * 2),
        vim.trim(fn.foldtext():gsub("^.-:", "")),
        vim.v.foldend - vim.v.foldstart + 1)
end

o.clipboard = "unnamed"
if not (vim.env.DISPLAY or vim.env.WAYLAND_DISPLAY) then
    local osc52 = require("vim.ui.clipboard.osc52")
    local function paste()
        -- Use unnamed register, because many terminals don't allow paste via OSC 52
        local r = fn.getreginfo('"')
        return {r.regcontents, r.regtype}
    end
    vim.g.clipboard = {
        copy = {["*"] = osc52.copy("*"), ["+"] = osc52.copy("+")},
        paste = {["*"] = paste, ["+"] = paste},
    }
end

o.mouse = "a"
o.mousemodel = "extend"

o.timeout = false

-- Mappings {{{1

map("n", "<C-s>", "<Cmd>write<CR>")

map({"!", "t"}, "<C-BS>", "<C-w>")

-- Delete default mappings that set a new undo point
if fn.maparg("<C-w>", "i") ~= "" then vim.keymap.del("i", "<C-w>") end
if fn.maparg("<C-u>", "i") ~= "" then vim.keymap.del("i", "<C-u>") end

map("n", "gcu", "gcgc", {remap = true})

map("n", "<C-RightMouse>", "<C-o>")

map("c", "/", function()
    return fn.pumvisible() ~= 0 and fn.getcmdline():sub(1, fn.getcmdpos() - 1):match("/$")
        and "<C-y>" or "/"
end, {expr = true})

for _, dir in ipairs({"Left", "Down", "Up", "Right"}) do
    map("n", ("<M-%s>"):format(dir), ("<C-w><%s>"):format(dir))
    -- Terminal mappings are separate, because counts don't work with <C-\><C-n>
    map("t", ("<M-%s>"):format(dir), ("<C-\\><C-n><C-w><%s>"):format(dir))
end

map("n", "<C-Tab>", "gt")
map("n", "<C-S-Tab>", "gT")
map("t", "<C-Tab>", "<C-\\><C-n>gt")
map("t", "<C-S-Tab>", "<C-\\><C-n>gT")

for i = 1, 10 do
    map({"n", "t"}, ("<M-%d>"):format(i % 10), ("<C-\\><C-n>%dgt"):format(i))
end

map("n", "ZT", "<Cmd>silent only | quit<CR>") -- Close tab

-- Remap do and dp to use a motion
_G.diff_bufnr = 0
function _G.diffget_operator() vim.cmd.diffget({diff_bufnr, range = {fn.line("'["), fn.line("']")}}) end
function _G.diffput_operator() vim.cmd.diffput({diff_bufnr, range = {fn.line("'["), fn.line("']")}}) end
-- The use of <Cmd> clears the count so that it doesn't affect the motion
map("n", "do", "<Cmd>set operatorfunc=v:lua.diffget_operator | lua diff_bufnr = vim.v.count<CR>g@")
map("n", "dp", "<Cmd>set operatorfunc=v:lua.diffput_operator | lua diff_bufnr = vim.v.count<CR>g@")
-- Map doo and dpp to the original behavior
map("n", "doo", "do")
map("n", "dpp", "dp")

-- The mappings above don't work in visual mode. As alternative, allow gv in operator-pending mode
map("o", "gv", "<Cmd>normal! gv<CR>")

-- Remap to something more convenient in my keymap
map("n", "dx", "dp", {remap = true})
map("n", "dpx", "dp")
map("", "[h", "[c", {remap = true})
map("", "]h", "]c", {remap = true})

-- Map <M-[> and <M-]> to enter a "mode" that prefixes every keypress with [ or ], respectively
for _, bracket in ipairs({"[", "]"}) do
    -- The intermediate mapping is only to improve the command shown in the bottom right
    local intermediate = ("<lt>M-%s>"):format(bracket)
    map("n", ("<M-%s>"):format(bracket), intermediate, {remap = true})
    local function repeat_bracket()
        local c = fn.getcharstr()
        if c == vim.keycode("<M-[>") or c == vim.keycode("<M-]>") then
            return c
        elseif c:match("^%d$") then
            return c .. vim.keycode(intermediate)
        else
            return bracket .. c .. "zz" .. vim.keycode(intermediate)
        end
    end
    map("n", intermediate, repeat_bracket, {expr = true, replace_keycodes = false, remap = true})
    map("n", intermediate .. "<Esc>", "")
end

-- Autocommands {{{1

local augroup = api.nvim_create_augroup("init", {})

api.nvim_create_autocmd("TextYankPost", {
    group = augroup,
    callback = function()
        vim.highlight.on_yank({higroup = "Visual", on_visual = false})
    end,
})

api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "sh,zsh",
    callback = function(ev)
        -- The default :ShKeywordPrg and :ZshKeywordPrg are broken in nvim
        vim.bo[ev.buf].keywordprg = ":Man"
    end
})

-- Statusline {{{1

o.statusline = " %{v:lua.statusline_git()}%<%{v:lua.statusline_path(0)} %{v:lua.statusline_modified()}"
    .. "%=%{v:lua.statusline_diagnostics()}%l/%L:%c%V "
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
    if name == "" then
        return api.nvim_eval_statusline("%f", {winid = winid}).str
    elseif name:match("^term://") then
        return name:gsub("^term://.-//%d+:", "term://")
    else
        return fn.fnamemodify(fn.FugitiveReal(name), absolute and ":~" or ":~:.")
    end
end

function _G.statusline_git()
    local s = fn.FugitiveStatusline()
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
    window = {
        documentation = {winhighlight = "Normal:NormalFloat,Error:NormalFloat"},
    },
})

-- LSP {{{1

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
        local bufnr = ev.buf
        local augroup = api.nvim_create_augroup("init_lsp_" .. bufnr, {})
        local client = lsp.get_client_by_id(ev.data.client_id)
        if not client then return end

        local signature_triggers = vim.tbl_get(client.server_capabilities, "signatureHelpProvider", "triggerCharacters")
        if signature_triggers then
            local pattern = "[" .. vim.pesc(table.concat(signature_triggers)) .. "][%s]*$"
            api.nvim_create_autocmd("TextChangedI", {
                buffer = bufnr,
                group = augroup,
                callback = function()
                    local lnum, col = unpack(api.nvim_win_get_cursor(0))
                    local line = api.nvim_get_current_line():sub(1, col)
                    while lnum > 1 and line:match("^%s*$") do
                        lnum = lnum - 1
                        line = api.nvim_buf_get_lines(bufnr, lnum - 1, lnum, true)[1]
                    end
                    if line and line:match(pattern) then
                        client.request(
                            lsp.protocol.Methods.textDocument_signatureHelp,
                            lsp.util.make_position_params(0, client.offset_encoding),
                            lsp.with(lsp.handlers.signature_help, {silent = true, focusable = false}))
                    end
                end,
            })
        end

        if client.supports_method(lsp.protocol.Methods.textDocument_documentHighlight) then
            local timer = uv.new_timer()
            api.nvim_create_autocmd({"CursorMoved", "ModeChanged", "BufLeave"}, {
                buffer = bufnr,
                group = augroup,
                callback = function(ev)
                    if ev.event == "BufLeave" or fn.mode():match("[^nc]") then
                        lsp.util.buf_clear_references(bufnr)
                    elseif fn.mode() == "n" then
                        timer:start(100, 0, vim.schedule_wrap(function()
                            if api.nvim_get_current_buf() ~= bufnr or fn.mode() ~= "n" then return end
                            client.request(
                                lsp.protocol.Methods.textDocument_documentHighlight,
                                lsp.util.make_position_params(0, client.offset_encoding),
                                function(err, refs)
                                    lsp.util.buf_clear_references(bufnr)
                                    if refs and api.nvim_get_current_buf() == bufnr and fn.mode() == "n" then
                                        lsp.util.buf_highlight_references(bufnr, refs, client.offset_encoding)
                                    elseif err then
                                        lsp.log.error(client.name, tostring(err))
                                    end
                                end)
                        end))
                    end
                end,
            })
        end

        api.nvim_create_autocmd("LspDetach", {
            buffer = bufnr,
            group = augroup,
            callback = function()
                lsp.util.buf_clear_references(bufnr)
                api.nvim_del_augroup_by_id(augroup)
            end,
        })
    end,
})
