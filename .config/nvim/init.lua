-- Setup {{{1

-- vim: foldmethod=marker

setmetatable(_G, {__index = vim.api})

local fn = vim.fn
local lsp = vim.lsp
local map = vim.keymap.set

vim.cmd.runtime("old_init.vim")
vim.cmd.colorscheme("ulrikdem")

-- Options {{{1

local defaults = setmetatable({}, {
    __newindex = function(_, k, v)
        -- Don't override local options that have been changed from the global value
        (vim.o[k] == vim.go[k] and vim.o or vim.go)[k] = v
    end,
})
-- Convince language server this has the same type as vim.o
if false then defaults = vim.o end

defaults.splitbelow = true
defaults.splitright = true

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

map("n", "<C-s>", vim.cmd.write)

map({"!", "t"}, "<C-BS>", "<C-w>")

-- Delete default mappings that set a new undo point
if fn.maparg("<C-w>", "i") ~= "" then vim.keymap.del("i", "<C-w>") end
if fn.maparg("<C-u>", "i") ~= "" then vim.keymap.del("i", "<C-u>") end

map("n", "gcu", "gcgc", {remap = true})

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
_G.diff_cmd, _G.diff_bufnr = "", 0
function _G.diff_operator()
    vim.cmd[diff_cmd]({
        diff_bufnr ~= 0 and diff_bufnr or nil,
        range = {fn.line("'["), fn.line("']")},
    })
end
-- The use of <Cmd> clears the count so that it doesn't affect the motion
map("n", "do", "<Cmd>set operatorfunc=v:lua.diff_operator | lua diff_cmd, diff_bufnr = 'diffget', vim.v.count<CR>g@")
map("n", "dp", "<Cmd>set operatorfunc=v:lua.diff_operator | lua diff_cmd, diff_bufnr = 'diffput', vim.v.count<CR>g@")
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

vim.g.mapleader = " "

-- Autocommands {{{1

local augroup = nvim_create_augroup("init.lua", {})

nvim_create_autocmd("FileType", {
    group = augroup,
    callback = function() pcall(vim.treesitter.start) end
})
vim.treesitter.language.register("bash", "sh")

nvim_create_autocmd("TextYankPost", {
    group = augroup,
    callback = function()
        vim.highlight.on_yank({higroup = "Visual", on_visual = false})
    end,
})

nvim_create_autocmd("BufWritePre", {
    group = augroup,
    callback = function(args)
        local dir = vim.fs.dirname(args.match)
        if args.match:match("^/") and not vim.uv.fs_stat(dir) then
            fn.mkdir(dir, "p")
            print("created directory " .. dir .. "\n")
        end
    end,
})

nvim_create_autocmd("TermOpen", {
    group = augroup,
    callback = function()
        vim.wo[0][0].number = false
        vim.wo[0][0].relativenumber = false
        vim.bo.matchpairs = ""
        vim.cmd.startinsert()
        nvim_create_autocmd("BufEnter", {buffer = 0, command = "startinsert"})
    end,
})

nvim_create_autocmd("VimResized", {group = augroup, command = "wincmd ="})

local sidebar_width = 80
local function make_sidebar()
    if vim.o.columns <= sidebar_width * 2 or vim.o.winfixwidth then return end
    vim.cmd.wincmd("L")
    nvim_win_set_width(0, sidebar_width)
    vim.o.winfixwidth = true
    vim.cmd.wincmd("=")
end

nvim_create_autocmd("BufWinEnter", {
    group = augroup,
    callback = function()
        if vim.o.buftype == "help" then make_sidebar() end
    end,
})
nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = {"man", "fugitive"},
    callback = function() make_sidebar() end,
})
vim.env.MANWIDTH = tostring(sidebar_width + 1)

nvim_create_autocmd("FileType", {
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

defaults.rulerformat = "%l/%L"

defaults.title = true
defaults.titlestring = "%{v:lua.statusline_path(0, v:true)} - nvim"
defaults.titlelen = 0

defaults.tabline = "%!v:lua.tabline()"

function _G.tabline()
    local tabs = nvim_list_tabpages()
    local current = nvim_get_current_tabpage()
    local max_width = math.max(math.floor(vim.o.columns / #tabs) - 3, 1)
    local s = "%T" -- For some reason the first item has no effect, so add a useless one
    for i, tab in ipairs(tabs) do
        s = s .. (tab == current and "%#TabLineSel#" or "%#TabLine#") .. "%" .. i .. "T "
            .. "%." .. max_width .. "(%{v:lua.statusline_path(" .. nvim_tabpage_get_win(tab) .. ")}%)"
            .. " %T%#TabLineFill#" .. (tab == current and "▌" or tabs[i + 1] == current and "▐" or "│")
    end
    return s
end

--- @param winid integer
--- @param absolute? boolean
function _G.statusline_path(winid, absolute)
    local bufnr = nvim_win_get_buf(winid)
    local name = nvim_buf_get_name(bufnr)
    local buftype = vim.bo[bufnr].buftype
    if buftype == "quickfix" then
        return nvim_eval_statusline("%q", {winid = winid}).str
            .. (vim.w[winid].quickfix_title and " " .. vim.w[winid].quickfix_title or "")
    elseif buftype == "nofile" or name == "" then
        return nvim_eval_statusline("%f", {winid = winid}).str
    elseif buftype == "terminal" then
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

nvim_create_autocmd("DiagnosticChanged", {command = "redrawstatus!", group = augroup})

local lsp_progress = {} --- @type table<integer, string?>
function _G.statusline_lsp_progress()
    local s = ""
    for _, client in ipairs(lsp.get_clients({bufnr = 0})) do
        if lsp_progress[client.id] then s = s .. lsp_progress[client.id] end
    end
    return s
end

local lsp_progress_timer = vim.uv.new_timer()
nvim_create_autocmd("LspProgress", {
    group = augroup,
    callback = function(args)
        local t = args.data.params.value
        lsp_progress[args.data.client_id] = t.kind ~= "end"
            and ("[%s%s] "):format(t.percentage and t.percentage .. "% " or "", t.title)
            or nil
        if not lsp_progress_timer:is_active() then
            lsp_progress_timer:start(16, 0, vim.schedule_wrap(function()
                vim.cmd.redrawstatus({bang = true})
            end))
        end
    end,
})

-- Quickfix {{{1

nvim_create_user_command("Grep", function(opts)
    vim.system({"rg", "--json", unpack(opts.fargs)}, {}, function(result)
        if result.code == 2 then
            return vim.schedule_wrap(nvim_err_write)(result.stderr)
        elseif result.signal ~= 0 then
            return vim.schedule_wrap(nvim_err_writeln)(("rg exited with signal %d"):format(result.signal))
        end
        local items = vim.iter(vim.gsplit(result.stdout, "\n", {trimempty = true}))
            :map(vim.json.decode)
            :filter(function(message) return message.type == "match" end)
            :map(function(match)
                return {
                    filename = match.data.path.text or vim.base64.decode(match.data.path.bytes),
                    lnum = match.data.line_number,
                    col = match.data.submatches[1] and match.data.submatches[1].start + 1,
                    text = vim.re.gsub(
                        (match.lines.text or vim.base64.decode(match.lines.bytes)):gsub("\n$", ""),
                        vim.lpeg.P("\0"),
                        "\n"),
                    user_data = {
                        highlight_ranges = vim.tbl_map(function(submatch)
                            return {submatch.start, submatch["end"]}
                        end, match.data.submatches),
                    },
                }
            end)
            :totable()
        vim.schedule(function()
            fn.setqflist({}, " ", {title = "Grep " .. opts.args, items = items})
            vim.cmd("botright copen")
        end)
    end)
end, {nargs = "+", complete = "file"})
map("n", "grg", "<Cmd>Grep -Fwe <cword><CR>")

-- Close the window before running cwindow/lwindow, because the focus depends on whether it is already open
nvim_create_autocmd("QuickFixCmdPost", {group = augroup, pattern = "[^l]*", command = "cclose | botright cwindow"})
nvim_create_autocmd("QuickFixCmdPost", {group = augroup, pattern = "l*", command = "lclose | lwindow"})

map("n", "<Leader>tq", function()
    vim.cmd(fn.getqflist({winid = true}).winid ~= 0 and "cclose" or "botright copen")
end)
map("n", "<Leader>tl", function()
    vim.cmd(fn.getloclist(0, {winid = true}).winid ~= 0 and "lclose" or "lopen")
end)

local function after_jump()
    vim.cmd("normal! zv")
    local list, i = unpack(fn.getjumplist())
    fn.settagstack(nvim_get_current_win(), {
        items = {{from = {list[i].bufnr, list[i].lnum, list[i].col + 1, list[i].coladd}, tagname = "quickfix"}},
    }, "t")
end

nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "qf",
    callback = function()
        map("n", "<CR>", function()
            vim.cmd(fn.getwininfo(nvim_get_current_win())[1].loclist == 0 and ".cc | cclose" or ".ll | lclose")
            after_jump()
        end, {buffer = 0})
        map("n", "<M-CR>", function()
            vim.cmd.normal({vim.keycode("<CR>"), bang = true})
            after_jump()
        end, {buffer = 0})

        local wo = vim.wo[0][0]
        wo.list = false
        wo.wrap = false
        wo.foldmethod = "expr"
        wo.foldexpr = "v:lua.quickfix_foldexpr()"
        wo.foldtext = "v:lua.quickfix_foldtext()"
        wo.foldlevel = 99
    end,
})

--- @type table<integer, {foldlevel: table<integer, integer | string>, foldtext: table<integer, string>}>
_G.quickfix_data = quickfix_data or {}

local quickfix_types = {
    E = {"error", "DiagnosticError"},
    W = {"warning", "DiagnosticWarn"},
    I = {"info", "DiagnosticInfo"},
    N = {"note", "DiagnosticHint"},
}

defaults.quickfixtextfunc = "v:lua.quickfix_textfunc"

--- @param args quickfixtextfunc_args
function _G.quickfix_textfunc(args)
    local what = {id = args.id, qfbufnr = true, title = true, items = true}
    local list = args.quickfix == 1 and fn.getqflist(what) or fn.getloclist(args.winid, what)
    local bufnr = list.qfbufnr
    local is_toc = vim.endswith(list.title, "TOC")

    local lines = {} --- @type string[]
    local highlights = {}
    local namespace = nvim_create_namespace("quickfix")

    if args.start_idx == 1 then
        nvim_buf_clear_namespace(bufnr, namespace, 0, -1)
        quickfix_data[bufnr] = {foldlevel = {}, foldtext = {}}
    end
    local data = quickfix_data[bufnr]

    for i = args.start_idx, args.end_idx do
        local item = list.items[i]
        local leading_space = is_toc and 0 or #item.text:match("%s*")
        local line = ""

        if item.bufnr ~= 0 then
            local name = nvim_buf_get_name(item.bufnr)
            line = name ~= "" and fn.fnamemodify(name, ":~:.") or "[No Name]"
            -- Dim path for all but the first (consecutive) item for the same buffer
            if item.bufnr == vim.tbl_get(list.items, i - 1, "bufnr") then
                table.insert(highlights, {i - 1, 0, #line, "NonText"})
                data.foldlevel[i] = 1
            else
                data.foldlevel[i] = ">1"
                data.foldtext[i] = line
            end
        end

        line = line .. "|"
        if item.lnum ~= 0 then
            line = line .. ("%4d"):format(item.lnum)
        end
        line = line .. "| "

        local type = quickfix_types[item.type:upper()]
        if type then
            local name, group = unpack(type)
            table.insert(highlights, {i - 1, #line, #line + #name + 1, group})
            line = line .. name .. ": "
        end

        local highlight_ranges = vim.tbl_get(item, "user_data", "highlight_ranges")
        for _, range in ipairs(highlight_ranges or {}) do
            table.insert(highlights, {
                i - 1,
                #line + math.max(range[1] - leading_space, 0),
                #line + math.max(range[2] - leading_space, 0),
                "String",
            })
        end

        local text = item.text:sub(leading_space + 1)
        if not highlight_ranges then text = text:gsub("\n%s*", " ") end
        line = line .. text

        local foldlevel = vim.tbl_get(item, "user_data", "foldlevel")
        if foldlevel then
            data.foldlevel[i] = foldlevel
            data.foldtext[i] = text
        end

        table.insert(lines, line)
    end

    vim.schedule(function()
        -- Cancel if the list has been replaced since this was scheduled
        -- This can happen when updating the list from a DiagnosticChanged autocmd
        if quickfix_data[bufnr] ~= data then return end

        for _, highlight in ipairs(highlights) do
            local line, col, end_col, group = unpack(highlight)
            nvim_buf_set_extmark(bufnr, namespace, line, col, {end_col = end_col, hl_group = group})
        end
    end)

    return lines
end

function _G.quickfix_foldexpr()
    return quickfix_data[nvim_get_current_buf()].foldlevel[vim.v.lnum] or 0
end

function _G.quickfix_foldtext()
    return ("%s (%d lines) "):format(
        quickfix_data[nvim_get_current_buf()].foldtext[vim.v.foldstart],
        vim.v.foldend - vim.v.foldstart + 1)
end

-- Fuzzy finder {{{1

--- @class fzf_opts
--- @field args? string[]
--- @field cwd? string
--- @field input? string[]
--- @field on_output? fun(lines: string[])
--- @field to_quickfix fun(line: string): vim.quickfix.entry
--- @field title string
--- @field loclist_winid? integer

--- @param opts fzf_opts
local function run_fzf(opts)
    local bufnr = nvim_create_buf(false, true)
    nvim_open_win(bufnr, true, {
        relative = "editor",
        anchor = "SW",
        row = vim.o.lines - 1,
        col = 0,
        height = 11,
        width = vim.o.columns,
        border = {"", "─", "", "", "", "", "", ""},
    })
    vim.wo[0][0].winhighlight = "NormalFloat:Normal,FloatBorder:WinSeparator"

    local output = fn.tempname()
    local cmd = ("fzf --layout=reverse-list -m %s >%s"):format(
        vim.iter(opts.args or {}):map(fn.shellescape):join(" "),
        fn.shellescape(output))
    local input
    if opts.input then
        input = fn.tempname()
        cmd = cmd .. " <" .. fn.shellescape(input)
        fn.writefile(opts.input, input)
    end

    fn.termopen(cmd, {
        cwd = opts.cwd,
        on_exit = function()
            nvim_buf_delete(bufnr, {})
            local lines = fn.readfile(output)
            vim.uv.fs_unlink(output)
            if input then vim.uv.fs_unlink(input) end

            if opts.on_output then opts.on_output(lines) end
            if #lines == 1 then
                local item = opts.to_quickfix(lines[1])
                if not (item.bufnr or item.filename) then return end
                local bufnr = item.bufnr or fn.bufadd(item.filename)
                vim.cmd("normal! m'")
                nvim_win_set_buf(0, bufnr)
                vim.bo.buflisted = true
                if item.lnum and item.lnum > 0 then
                    local set_cursor = item.vcol and item.vcol ~= 0 and fn.setcursorcharpos or fn.cursor
                    set_cursor(item.lnum, item.col and item.col > 0 and item.col or 1)
                    after_jump()
                end
            elseif #lines > 1 then
                local what = {title = opts.title, items = vim.tbl_map(opts.to_quickfix, lines)}
                if opts.loclist_winid then
                    fn.setloclist(opts.loclist_winid, {}, " ", what)
                    vim.cmd("lopen")
                else
                    fn.setqflist({}, " ", what)
                    vim.cmd("botright copen")
                end
            end
        end,
    })
end

map("n", "<Leader>ff", vim.cmd.Fzf)
nvim_create_user_command("Fzf", function(opts)
    local cwd = vim.fs.normalize(opts.args)
    run_fzf({
        args = {("--prompt=%s/"):format(fn.fnamemodify(cwd, ":p:~"):gsub("/$", ""))},
        cwd = cwd,
        to_quickfix = function(line) return {filename = line, valid = true} end,
        title = "Files",
    })
end, {nargs = "?", complete = "file"})

--- @return string[]
local function list_buffers()
    return vim.tbl_map(function(info)
        return ("%d %s%s"):format(
            info.bufnr,
            info.name ~= "" and fn.fnamemodify(info.name, ":~:.") or "[No Name]",
            vim.bo[info.bufnr].modified and " [+]" or "")
    end, fn.getbufinfo({buflisted = 1}))
end

--- @param bufnr integer
function _G.delete_buffer(bufnr)
    if not vim.bo[bufnr].modified then
        nvim_buf_delete(bufnr, {})
    end
    return fn.join(list_buffers(), "\n")
end

map("n", "<Leader>fb", function()
    run_fzf({
        args = {
            "--prompt=buffer: ",
            "--with-nth=2..",
            ([[--bind=ctrl-d:reload:nvim --server %s --remote-expr v:lua.delete_buffer\({1}\)]])
                :format(fn.shellescape(vim.v.servername)),
        },
        input = list_buffers(),
        to_quickfix = function(line)
            return {bufnr = tonumber(vim.gsplit(line, " ")()), valid = true}
        end,
        title = "Buffers",
    })
end)

map("n", "<Leader>fg", vim.cmd.IGrep)
nvim_create_user_command("IGrep", function(opts)
    run_fzf({
        args = {
            "--prompt=grep: ",
            ("--bind=change:top+reload:rg --column --color ansi -H0Se {q} %s | igrep-format %d")
                :format(opts.args, vim.o.columns),
            "--with-nth=-1",
            "--delimiter=\\0",
            "--ansi",
            "--disabled",
        },
        input = {},
        to_quickfix = function(line)
            local parts = vim.split(line, "\n")
            local filename, lnum, col = unpack(parts)
            local text = vim.iter(parts):slice(4, #parts - 1):join("\x1b")
            return {filename = filename, lnum = tonumber(lnum), col = tonumber(col), text = text}
        end,
        title = "Grep",
    })
end, {nargs = "*", complete = "file"})

-- @param items vim.quickfix.entry[]
local function quickfix_to_fzf(items)
    --- @param i integer
    --- @param item vim.quickfix.entry
    return vim.iter(items):enumerate():map(function(i, item)
        local location = ""
        if item.bufnr and item.bufnr ~= 0 then
            local name = nvim_buf_get_name(item.bufnr)
            location = name ~= "" and fn.fnamemodify(name, ":~:.") or "[No Name]"
        elseif item.filename then
            location = fn.fnamemodify(item.filename, ":~:.")
        end
        if item.lnum and item.lnum ~= 0 then location = location .. ":" .. item.lnum end

        local text = vim.trim(item.text or ""):gsub("\n%s*", " "):gsub("\t", " ")
        local type = quickfix_types[(item.type or ""):upper()]
        if type then text = type[1] .. ": " .. text end
        local container_names = vim.tbl_get(item, "user_data", "container_names")
        if container_names then
            text = ("%s\x1b[90m%s\x1b[0m"):format(
                text,
                vim.iter(container_names):map(function(s) return " < " .. s end):join(""))
        end

        if text ~= "" then
            local clean_text = text:gsub("\x1b%[%d*m", "")
            local pad = vim.o.columns - fn.strwidth(clean_text) - fn.strwidth(location) - 3
            return ("%d %s%s\x1b[90m%s\x1b[0m"):format(i, text, (" "):rep(math.max(pad, 1)), location)
        else
            return ("%d %s"):format(i, location)
        end
    end):totable()
end

nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "qf",
    callback = function()
        map("n", "s", function()
            local list = fn.getwininfo(nvim_get_current_win())[1].loclist == 0
                and fn.getqflist({title = true, items = true, winid = true})
                or fn.getloclist(0, {title = true, items = true, winid = true, filewinid = true})
            run_fzf({
                args = {"--with-nth=2..", "--ansi", "--tiebreak=begin"},
                input = quickfix_to_fzf(list.items),
                on_output = function(lines)
                    -- Focus isn't automatically returned to the quickfix window if the fzf window is focused when closed
                    nvim_set_current_win(list.winid)
                    if #lines == 1 then nvim_win_close(list.winid, false) end
                end,
                to_quickfix = function(line)
                    return list.items[tonumber(vim.gsplit(line, " ")())]
                end,
                title = list.title,
                loclist_winid = list.filewinid,
            })
        end, {buffer = 0})
    end,
})

map("n", "<Leader>fs", function()
    local bufnr = nvim_get_current_buf()
    local last_query = ""
    local items = {}
    function _G.workspace_symbols(query)
        query = vim.gsplit(query, " ")() or ""
        if query ~= last_query then
            last_query = query
            local results = lsp.buf_request_sync(bufnr, lsp.protocol.Methods.workspace_symbol, {query = query})
            items = {}
            for _, result in pairs(results or {}) do
                if result.error then lsp.log.error(tostring(result.error)) end
                lsp.util.symbols_to_items(result.result or {}, bufnr, items)
            end
        end
        return fn.join(quickfix_to_fzf(items), "\n")
    end
    run_fzf({
        args = {
            "--prompt=symbol: ",
            ([[--bind=change:top+reload:nvim --server %s --remote-expr "v:lua.workspace_symbols('$(printf %%s {q} | sed "s/'/''/g")')"]])
                :format(fn.shellescape(vim.v.servername)),
            "--with-nth=2..",
            "--ansi",
            "--tiebreak=begin",
        },
        input = {},
        to_quickfix = function(line)
            return items[tonumber(vim.gsplit(line, " ")())]
        end,
        title = "Symbols",
    })
end)

-- Completion {{{1

local cmp = require("cmp")

function _G.get_listed_bufnrs()
    return vim.tbl_map(function(info) return info.bufnr end, fn.getbufinfo({buflisted = 1}))
end

--- @param fallback fun()
local function maybe_complete(fallback)
    if nvim_get_current_line():sub(1, nvim_win_get_cursor(0)[2]):match("[^%s]") then
        cmp.complete()
    else
        fallback()
    end
end

cmp.setup({
    sources = {
        {name = "nvim_lsp"},
        {name = "path"},
        {name = "buffer", group_index = 1, option = {get_bufnrs = get_listed_bufnrs}},
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

-- LSP {{{1

map("n", "grn", lsp.buf.rename)
map({"n", "x"}, "gra", lsp.buf.code_action)
map("n", "grr", lsp.buf.references)
map("n", "gri", lsp.buf.implementation)
map("n", "grd", lsp.buf.declaration)
map("n", "grt", lsp.buf.type_definition)
map("n", "grq", lsp.buf.format)
map("n", "grl", lsp.codelens.run)

map("n", "gO", function()
    local bufnr = nvim_get_current_buf()
    local winid = nvim_get_current_win()
    lsp.buf_request_all(
        bufnr,
        lsp.protocol.Methods.textDocument_documentSymbol,
        {textDocument = lsp.util.make_text_document_params()},
        function(results)
            local items = {}
            for _, result in pairs(results) do
                if result.error then return nvim_err_writeln(tostring(result.error)) end
                lsp.util.symbols_to_items(result.result, bufnr, items)
            end
            -- The filename and line numbers are concealed when the title ends in TOC
            fn.setloclist(winid, {}, " ", {title = "Symbols TOC", items = items})
            nvim_set_current_win(winid)
            vim.cmd.lopen()
        end)
end)

for k, v in pairs({k = {lsp.inlay_hint, "inlay hints"}, e = {vim.diagnostic, "diagnostics"}}) do
    map("n", "yo" .. k, function()
        v[1].enable(not v[1].is_enabled())
        nvim_echo({{v[2] .. ": " .. (v[1].is_enabled() and "enabled" or "disabled")}}, false, {})
    end)
end

map("n", "<M-LeftMouse>", "<LeftMouse><Cmd>lua vim.lsp.buf.hover()<CR>")
map("n", "<M-RightMouse>", "<LeftMouse><Cmd>lua vim.diagnostic.open_float()<CR>")

vim.diagnostic.config({
    severity_sort = true,
    signs = false,
    virtual_text = {format = function(d) return d.message:match("[^\n]*") end},
})

nvim_create_autocmd("InsertEnter", {
    group = augroup,
    callback = function()
        if lsp.inlay_hint.is_enabled({}) then
            lsp.inlay_hint.enable(false)
            nvim_create_autocmd("InsertLeave", {
                once = true,
                callback = function() lsp.inlay_hint.enable() end,
            })
        end
    end,
})

--- @param ... string | string[] | fun(name: string, path: string): boolean
--- @return string?
function _G.find_root(...)
    return vim.fs.root(0, ".lsp_root")
        or vim.iter({...}):fold(nil, function(root, marker) return root or vim.fs.root(0, marker) end)
end

nvim_create_user_command("CdRoot", function()
    local root_dir = find_root(".git") or vim.fs.dirname(nvim_buf_get_name(0))
    for _, client in pairs(lsp.get_clients({bufnr = 0})) do
        root_dir = client.root_dir or root_dir
    end
    print(root_dir)
    nvim_set_current_dir(root_dir)
end, {})

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
                vim.tbl_map(function(p) return "-r:" .. p end, vim.tbl_values(config.sandbox.read or {})),
                vim.tbl_map(function(p) return "-w:" .. p end, vim.tbl_values(config.sandbox.write or {})),
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

_G.old_locations_to_items = old_locations_to_items or lsp.util.locations_to_items
function lsp.util.locations_to_items(locations, offset_encoding)
    --- @type vim.quickfix.entry[]
    local items = old_locations_to_items(locations, offset_encoding)
    for _, item in ipairs(items) do
        --- @type lsp.Range
        local range = item.user_data.range or item.user_data.targetSelectionRange
        item.user_data = {
            highlight_ranges = {
                range.start.line == range["end"].line and {
                    lsp.util._str_byteindex_enc(item.text, range.start.character, offset_encoding),
                    lsp.util._str_byteindex_enc(item.text, range["end"].character, offset_encoding),
                } or nil,
            },
        }
    end
    return items
end

--- @param symbols lsp.DocumentSymbol[] | lsp.WorkspaceSymbol[] | lsp.SymbolInformation[]
--- @param bufnr integer
--- @param items? vim.quickfix.entry[]
--- @param depth? integer
--- @param container_names? string[]
function lsp.util.symbols_to_items(symbols, bufnr, items, depth, container_names)
    items = items or {}
    depth = depth or 0
    for _, symbol in ipairs(symbols) do
        local range = symbol.selectionRange or symbol.location.range
        local kind = lsp.protocol.SymbolKind[symbol.kind] or "Unknown"
        local text = ("%s[%s] %s"):format(("  "):rep(depth), kind, symbol.name)
        table.insert(items, {
            filename = symbol.location and vim.uri_to_fname(symbol.location.uri),
            bufnr = not symbol.location and bufnr or nil,
            lnum = range.start.line + 1,
            col = range.start.character + 1, -- This neglects to take into account offset_encoding
            text = symbol.detail and symbol.detail ~= "" and text .. ": " .. symbol.detail or text,
            user_data = {
                highlight_ranges = {{#text - #symbol.name, #text}},
                container_names = symbol.containerName and {symbol.containerName} or container_names,
                -- Only override foldlevel for DocumentSymbols, which have a hierarchy
                foldlevel = symbol.range and (next(symbol.children or {}) and ">" .. depth + 1 or depth),
            },
        })
        lsp.util.symbols_to_items(
            symbol.children or {},
            bufnr,
            items,
            depth + 1,
            {symbol.name, unpack(container_names or {})})
    end
    return items
end

--- @param client_id integer
function _G.lsp_augroup(client_id)
    return nvim_create_augroup("lsp_client_" .. client_id, {clear = false})
end

nvim_create_autocmd("LspAttach", {
    group = augroup,
    callback = function(args)
        local bufnr = args.buf
        local client = lsp.get_client_by_id(args.data.client_id)
        if not client then return end

        local augroup = lsp_augroup(client.id)
        nvim_clear_autocmds({buffer = bufnr, group = augroup})

        local signature_triggers = vim.tbl_get(client.server_capabilities, "signatureHelpProvider", "triggerCharacters")
        if signature_triggers then
            local pattern = "[" .. vim.pesc(table.concat(signature_triggers)) .. "][%s]*$"
            nvim_create_autocmd("TextChangedI", {
                buffer = bufnr,
                group = augroup,
                callback = function()
                    local lnum, col = unpack(nvim_win_get_cursor(0))
                    local line = nvim_get_current_line():sub(1, col)
                    while lnum > 1 and line:match("^%s*$") do
                        lnum = lnum - 1
                        line = nvim_buf_get_lines(bufnr, lnum - 1, lnum, true)[1]
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
            nvim_create_autocmd({"CursorMoved", "ModeChanged", "BufLeave"}, {
                buffer = bufnr,
                group = augroup,
                callback = function(args)
                    if args.event == "BufLeave" or fn.mode():match("[^nc]") then
                        lsp.util.buf_clear_references(bufnr)
                    elseif fn.mode() == "n" then
                        timer:start(100, 0, vim.schedule_wrap(function()
                            if nvim_get_current_buf() ~= bufnr or fn.mode() ~= "n" then return end
                            client.request(
                                lsp.protocol.Methods.textDocument_documentHighlight,
                                lsp.util.make_position_params(0, client.offset_encoding),
                                function(err, refs)
                                    lsp.util.buf_clear_references(bufnr)
                                    if refs and nvim_get_current_buf() == bufnr and fn.mode() == "n" then
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

        nvim_create_autocmd("LspDetach", {
            buffer = bufnr,
            group = augroup,
            callback = function(args)
                if args.data.client_id ~= client.id then return end
                if client.supports_method(lsp.protocol.Methods.textDocument_documentHighlight) then
                    lsp.util.buf_clear_references(bufnr)
                end
                local config = client.config --[[ @as LspConfig ]]
                if config.on_detach then config.on_detach(client, bufnr) end
                nvim_clear_autocmds({buffer = bufnr, group = augroup})
            end,
        })
    end,
})
