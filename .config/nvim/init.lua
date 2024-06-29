-- Setup {{{1

-- vim: foldmethod=marker

local api = vim.api
local cmd = vim.cmd
local fn = vim.fn
local lsp = vim.lsp
local map = vim.keymap.set
local o = vim.o

cmd.runtime("old_init.vim")
cmd.colorscheme("ulrikdem")

-- Options {{{1

local defaults = setmetatable({}, {
    __newindex = function(_, k, v)
        -- Don't override local options that have been changed from the global value
        (o[k] == vim.go[k] and o or vim.go)[k] = v
    end,
})
-- Convince language server this has the same type as vim.o
if false then defaults = o end

defaults.inccommand = "split"

defaults.ignorecase = true
defaults.smartcase = true

defaults.wildignorecase = true
defaults.wildmode = "longest:full,full"

defaults.completeopt = "menuone,noselect,popup"
defaults.dictionary = "/usr/share/dict/words"

vim.opt.shortmess:append("Ic")

defaults.cursorline = true
defaults.cursorlineopt = "number"

defaults.relativenumber = true
defaults.numberwidth = 3

defaults.scrolloff = 4
defaults.sidescrolloff = 8
defaults.smoothscroll = true

defaults.linebreak = true
defaults.breakindent = true

defaults.tabstop = 4
defaults.shiftwidth = 0
defaults.expandtab = true

defaults.list = true
defaults.listchars = "tab:→ ,trail:·,nbsp:·"

vim.opt.diffopt:append("vertical,foldcolumn:1,algorithm:histogram,linematch:60,hiddenoff")

defaults.fillchars = "foldopen:▾,foldclose:▸"
defaults.foldtext = "v:lua.foldtext()"
function _G.foldtext()
    return ("%s %s (%d lines) "):format(
        fn["repeat"]("·", vim.v.foldlevel * 2),
        vim.trim(fn.foldtext():gsub("^.-:", "")),
        vim.v.foldend - vim.v.foldstart + 1)
end

-- This is usually autodetected from the COLORTERM variable,
-- but ssh doesn't propagate it without configuration on the client and server
if vim.env.SSH_TTY then defaults.termguicolors = true end

defaults.clipboard = "unnamed"
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

defaults.mouse = "a"
defaults.mousemodel = "extend"

defaults.timeout = false

-- Mappings {{{1

map("n", "<C-s>", "<Cmd>write<CR>")

map({"!", "t"}, "<C-BS>", "<C-w>")

-- Delete default mappings that set a new undo point
if fn.maparg("<C-w>", "i") ~= "" then vim.keymap.del("i", "<C-w>") end
if fn.maparg("<C-u>", "i") ~= "" then vim.keymap.del("i", "<C-u>") end

map("n", "gcu", "gcgc", {remap = true})

map("n", "<C-RightMouse>", "<C-o>")

map({"i", "s"}, "<C-l>", function() vim.snippet.jump(1) end)
map({"i", "s"}, "<C-h>", function() vim.snippet.jump(-1) end)

map("c", "/", function()
    return fn.pumvisible() ~= 0 and fn.getcmdline():sub(1, fn.getcmdpos() - 1):match("/$")
        and "<C-y><Tab>" or "/"
end, {expr = true})
defaults.wildcharm = 9

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
function _G.diffget_operator() cmd.diffget({diff_bufnr, range = {fn.line("'["), fn.line("']")}}) end
function _G.diffput_operator() cmd.diffput({diff_bufnr, range = {fn.line("'["), fn.line("']")}}) end
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

local augroup = api.nvim_create_augroup("init.lua", {})

api.nvim_create_autocmd("FileType", {
    group = augroup,
    callback = function() pcall(vim.treesitter.start) end
})
vim.treesitter.language.register("bash", "sh")

api.nvim_create_autocmd("TextYankPost", {
    group = augroup,
    callback = function()
        vim.highlight.on_yank({higroup = "Visual", on_visual = false})
    end,
})

api.nvim_create_autocmd("TermOpen", {
    group = augroup,
    callback = function()
        vim.wo[0][0].number = false
        vim.wo[0][0].relativenumber = false
        vim.bo.matchpairs = ""
        cmd.startinsert()
        api.nvim_create_autocmd("BufEnter", {buffer = 0, command = "startinsert"})
    end,
})

api.nvim_create_autocmd("VimResized", {group = augroup, command = "wincmd ="})

local sidebar_width = 78
local function make_sidebar()
    if o.columns <= sidebar_width * 2 or o.winfixwidth then return end
    cmd.wincmd("H")
    api.nvim_win_set_width(0, sidebar_width)
    o.winfixwidth = true
    cmd.wincmd("=")
end

api.nvim_create_autocmd("BufWinEnter", {
    group = augroup,
    callback = function()
        if o.buftype == "help" then make_sidebar() end
    end,
})
api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = {"man", "fugitive"},
    callback = function() make_sidebar() end,
})
vim.env.MANWIDTH = tostring(sidebar_width + 1)

api.nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = {"sh", "zsh"},
    callback = function(args)
        -- The default :ShKeywordPrg and :ZshKeywordPrg are broken in nvim
        vim.bo[args.buf].keywordprg = ":Man"
    end
})

-- Statusline {{{1

defaults.statusline = " %{v:lua.statusline_git()}%<%{v:lua.statusline_path(0)}%( %m%)"
    .. "%= %{v:lua.statusline_lsp_progress()}%{v:lua.statusline_diagnostics()}%c%V %l/%L "
vim.g.qf_disable_statusline = true -- Don't let quickfix ftplugin override statusline

defaults.title = true
defaults.titlestring = "%{v:lua.statusline_path(0, v:true)} - nvim"
defaults.titlelen = 0

defaults.tabline = "%!v:lua.tabline()"

function _G.tabline()
    local tabs = api.nvim_list_tabpages()
    local current = api.nvim_get_current_tabpage()
    local max_width = math.max(math.floor(o.columns / #tabs) - 3, 1)
    local s = "%T" -- For some reason the first item has no effect, so add a useless one
    for i, tab in ipairs(tabs) do
        s = s .. (tab == current and "%#TabLineSel#" or "%#TabLine#") .. "%" .. i .. "T "
            .. "%." .. max_width .. "(%{v:lua.statusline_path(" .. api.nvim_tabpage_get_win(tab) .. ")}%)"
            .. " %T%#TabLineFill#" .. (tab == current and "▌" or tabs[i + 1] == current and "▐" or "│")
    end
    return s
end

--- @param winid integer
--- @param absolute? boolean
function _G.statusline_path(winid, absolute)
    local name = api.nvim_buf_get_name(api.nvim_win_get_buf(winid))
    if name == "" or o.buftype == "nofile" then
        return api.nvim_eval_statusline("%f", {winid = winid}).str
    elseif o.buftype == "terminal" then
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

function _G.statusline_diagnostics()
    local count = 0
    for _, v in pairs(vim.diagnostic.count(0)) do count = count + v end
    return count > 0 and count .. "⚠ " or ""
end

api.nvim_create_autocmd("DiagnosticChanged", {command = "redrawstatus!", group = augroup})

local lsp_progress = {} --- @type table<integer, string?>
function _G.statusline_lsp_progress()
    local s = ""
    for _, client in ipairs(lsp.get_clients({bufnr = 0})) do
        if lsp_progress[client.id] then s = s .. lsp_progress[client.id] end
    end
    return s
end

local lsp_progress_timer = vim.uv.new_timer()
api.nvim_create_autocmd("LspProgress", {
    group = augroup,
    callback = function(args)
        local t = args.data.params.value
        lsp_progress[args.data.client_id] = t.kind ~= "end"
            and ("[%s%s] "):format(t.percentage and t.percentage .. "% " or "", t.title)
            or nil
        if not lsp_progress_timer:is_active() then
            lsp_progress_timer:start(16, 0, vim.schedule_wrap(function()
                cmd.redrawstatus({bang = true})
            end))
        end
    end,
})

-- Completion {{{1

local cmp = require("cmp")

--- @param fallback fun()
local function maybe_complete(fallback)
    if api.nvim_get_current_line():sub(1, api.nvim_win_get_cursor(0)[2]):match("[^%s]") then
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
        ["<C-y>"] = cmp.mapping.confirm({select = true}),
    }),
    preselect = cmp.PreselectMode.None,
    formatting = { --- @diagnostic disable-line: missing-fields
        expandable_indicator = false,
        format = function(entry, item)
            if entry.source.name == "buffer" or entry.source.name == "omni" then
                item.kind = item.menu -- Remove uninformative "Text" kind
            end
            item.menu = nil
            return item
        end,
    },
    window = {
        documentation = {winhighlight = "Normal:NormalFloat,Error:NormalFloat"},
    },
})

cmp.setup.cmdline({"/", "?"}, {
    sources = {{name = "buffer"}},
    mapping = cmp.mapping.preset.cmdline(),
    completion = {autocomplete = false},
    formatting = { --- @diagnostic disable-line: missing-fields
        format = function(_, item)
            item.kind = nil
            return item
        end,
    },
})

-- LSP {{{1

-- Mappings use the proposed gr prefix: https://github.com/neovim/neovim/pull/28650
map("n", "grn", lsp.buf.rename)
map({"n", "x"}, "gra", lsp.buf.code_action)
map("n", "grr", lsp.buf.references)
map("n", "grq", lsp.buf.format)

for k, v in pairs({k = {lsp.inlay_hint, "inlay hints"}, e = {vim.diagnostic, "diagnostics"}}) do
    map("n", "yo" .. k, function()
        v[1].enable(not v[1].is_enabled())
        api.nvim_echo({{v[2] .. ": " .. (v[1].is_enabled() and "enabled" or "disabled")}}, false, {})
    end)
end

map("n", "<M-LeftMouse>", "<LeftMouse><Cmd>lua vim.lsp.buf.hover()<CR>")
map("n", "<M-RightMouse>", "<LeftMouse><Cmd>lua vim.diagnostic.open_float()<CR>")

vim.diagnostic.config({
    severity_sort = true,
    signs = false,
    float = {header = "", prefix = ""},
    virtual_text = {format = function(d) return d.message:match("[^\n]*") end},
})

api.nvim_create_autocmd("InsertEnter", {
    group = augroup,
    callback = function()
        if lsp.inlay_hint.is_enabled({}) then
            lsp.inlay_hint.enable(false)
            api.nvim_create_autocmd("InsertLeave", {
                once = true,
                callback = function() lsp.inlay_hint.enable() end,
            })
        end
    end,
})

--- @param ... string | string[] | fun(name: string, path: string): boolean
--- @return string?
function _G.find_root(...)
    return vim.iter({...}):fold(nil, function(root, marker) return root or vim.fs.root(0, marker) end)
end

--- @class LspConfig: vim.lsp.ClientConfig
--- @field sandbox? {args?: string[], read?: string[], write?: string[]}
--- @field on_detach? fun(client: vim.lsp.Client, bufnr: integer)

--- @param config LspConfig
function _G.start_lsp(config)
    if fn.executable(config.cmd[1]) ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
        config.name = config.cmd[1]

        if config.sandbox then
            config.cmd = vim.iter({
                -- LSP servers should check that the parent is still alive, else exit, so share the pid namspace
                "sandbox", "-spid",
                -- Use tbl_values to filter out nils
                vim.tbl_map(function(p) return "-r" .. p end, vim.tbl_values(config.sandbox.read or {})),
                vim.tbl_map(function(p) return "-w" .. p end, vim.tbl_values(config.sandbox.write or {})),
                config.sandbox.args or {},
                config.cmd,
            }):flatten():totable()
            for _, dir in pairs(config.sandbox.write or {}) do fn.mkdir(dir, "p") end
        end

        config.capabilities = vim.tbl_deep_extend("force",
            lsp.protocol.make_client_capabilities(),
            require("cmp_nvim_lsp").default_capabilities({snippetSupport = false}),
            config.capabilities or {})

        lsp.start(config, {
            bufnr = 0,
            reuse_client = function(client, config)
                -- The default considers nil root_dirs to be distinct, resulting in a new client for each buffer
                return client.name == config.name and client.root_dir == config.root_dir
            end,
        })
    end
end

--- @param client_id integer
function _G.lsp_augroup(client_id)
    return api.nvim_create_augroup("lsp_client_" .. client_id, {clear = false})
end

api.nvim_create_autocmd("LspAttach", {
    group = augroup,
    callback = function(args)
        local bufnr = args.buf
        local client = lsp.get_client_by_id(args.data.client_id)
        if not client then return end

        local augroup = lsp_augroup(client.id)
        api.nvim_clear_autocmds({buffer = bufnr, group = augroup})

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
                    if line:match(pattern) then
                        client.request(
                            lsp.protocol.Methods.textDocument_signatureHelp,
                            lsp.util.make_position_params(0, client.offset_encoding),
                            lsp.with(lsp.handlers.signature_help, {silent = true, focusable = false}))
                    end
                end,
            })
        end

        if client.supports_method(lsp.protocol.Methods.textDocument_documentHighlight) then
            local timer = vim.uv.new_timer()
            api.nvim_create_autocmd({"CursorMoved", "ModeChanged", "BufLeave"}, {
                buffer = bufnr,
                group = augroup,
                callback = function(args)
                    if args.event == "BufLeave" or fn.mode():match("[^nc]") then
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
            callback = function(args)
                if args.data.client_id ~= client.id then return end
                if client.supports_method(lsp.protocol.Methods.textDocument_documentHighlight) then
                    lsp.util.buf_clear_references(bufnr)
                end
                local config = client.config --[[ @as LspConfig ]]
                if config.on_detach then config.on_detach(client, bufnr) end
                api.nvim_clear_autocmds({buffer = bufnr, group = augroup})
            end,
        })
    end,
})
