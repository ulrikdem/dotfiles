-- Setup {{{1

-- vim: foldmethod=marker

setmetatable(_G, {__index = vim.api})

local fn = vim.fn
local lsp = vim.lsp
local map = vim.keymap.set

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

defaults.guicursor = "i-ci-ve:ver25,r-cr:hor20"

defaults.cursorline = true
defaults.cursorlineopt = "number"

defaults.relativenumber = true
defaults.numberwidth = 3

defaults.scrolloff = 4
defaults.sidescrolloff = 8

defaults.linebreak = true
defaults.breakindent = true

defaults.tabstop = 4
defaults.shiftwidth = 0
defaults.expandtab = true

defaults.list = true
defaults.listchars = "tab:→ ,trail:·,nbsp:·"

defaults.fillchars = "foldopen:▼,foldclose:▶"
defaults.foldtext = "v:lua.foldtext()"
function _G.foldtext()
    return ("%s %s (%d lines) "):format(
        fn["repeat"]("·", vim.v.foldlevel * 2),
        vim.trim(fn.foldtext():gsub("^.-:", "")),
        vim.v.foldend - vim.v.foldstart + 1)
end

defaults.nrformats = "hex,bin,blank"

vim.opt.suffixes:remove(".h")

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

vim.g.mapleader = " "

map("n", "<C-s>", vim.cmd.write)

map({"!", "t"}, "<C-BS>", "<C-w>")

-- Delete default mappings that set a new undo point
pcall(vim.keymap.del, "i", "<C-w>")
pcall(vim.keymap.del, "i", "<C-u>")

map("n", "gcu", "gcgc", {remap = true})

map({"i", "s"}, "<C-l>", function() vim.snippet.jump(1) end)
map({"i", "s"}, "<C-h>", function() vim.snippet.jump(-1) end)

map("c", "/", function()
    return fn.pumvisible() ~= 0 and vim.endswith(fn.getcmdline():sub(1, fn.getcmdpos() - 1), "/")
        and "<Down>" or "/"
end, {expr = true})

map("t", "<Esc>", "<C-\\><C-n>")
map("t", "<C-Esc>", "<Esc>")

for _, dir in ipairs({"Left", "Down", "Up", "Right"}) do
    map("n", ("<M-%s>"):format(dir), ("<C-w><%s>"):format(dir))
end

map("n", "<C-Tab>", "gt")
map("n", "<C-S-Tab>", "gT")
for i = 1, 10 do
    map("n", ("<M-%d>"):format(i % 10), ("%dgt"):format(i))
end

map("n", "ZT", "<Cmd>silent only | quit<CR>") -- Close tab

if vim.env.RANGER_LEVEL then
    map("n", "-", function()
        if vim.v.count == 0 and #fn.getbufinfo({buflisted = 1}) * fn.winnr("$") * fn.tabpagenr("$") == 1 then
            return "<C-w>q"
        else
            return "<Plug>(dirvish_up)"
        end
    end, {expr = true})
end

-- Map <M-[> and <M-]> to enter a "mode" that prefixes every keypress with [ or ], respectively
for _, bracket in ipairs({"[", "]"}) do
    -- The intermediate mapping is only to improve the command shown in the bottom right
    local intermediate = ("<lt>M-%s>"):format(bracket)
    map("n", ("<M-%s>"):format(bracket), intermediate, {remap = true})
    map("n", intermediate, function()
        local c = fn.getcharstr()
        if c == vim.keycode("<M-[>") or c == vim.keycode("<M-]>") then
            return c
        elseif c:match("^%d$") then
            return c .. vim.keycode(intermediate)
        else
            return bracket .. c .. "zz" .. vim.keycode(intermediate)
        end
    end, {expr = true, replace_keycodes = false, remap = true})
    map("n", intermediate .. "<Esc>", "")
end

-- Autocommands {{{1

local augroup = nvim_create_augroup("init.lua", {})

nvim_create_autocmd("BufReadPost", {group = augroup, command = "DetectIndent"})
nvim_create_user_command("DetectIndent", function()
    local shiftwidth_votes = vim.defaulttable(function() return 0 end)
    local expandtab_votes = vim.o.expandtab and 1 or 0
    local mixed, prev_indent
    for _, line in ipairs(nvim_buf_get_lines(0, 0, -1, true)) do
        local indent = line:match("^[ \t]*")
        if indent:find("\t") then
            expandtab_votes = expandtab_votes - 1
            mixed = mixed or indent:find(" ")
            prev_indent = nil
        else
            if #indent > 0 then expandtab_votes = expandtab_votes + 1 end
            if prev_indent and #indent ~= prev_indent then
                local diff = math.abs(#indent - prev_indent)
                shiftwidth_votes[diff] = shiftwidth_votes[diff] + 1
            end
            prev_indent = #indent
        end
    end
    local shiftwidth = fn.shiftwidth()
    for _, n in ipairs({2, 4, 8}) do
        if shiftwidth_votes[n] > shiftwidth_votes[shiftwidth] then shiftwidth = n end
    end
    vim.bo.shiftwidth = shiftwidth
    vim.bo.tabstop = shiftwidth
    vim.bo.softtabstop = 0
    vim.bo.expandtab = expandtab_votes > 0
    if mixed then -- If any lines contain mixed indentation, default to traditional values
        vim.bo.tabstop = 8
        vim.bo.expandtab = false
    end
end, {bar = true})

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
        if vim.startswith(args.match, "/") and not vim.uv.fs_stat(dir) then
            fn.mkdir(dir, "p")
            vim.notify("created directory " .. dir .. "\n")
        end
    end,
})

nvim_create_autocmd("TermOpen", {
    group = augroup,
    callback = function()
        -- The MatchParen highlight isn't updated in terminal mode, so disable it
        vim.bo.matchpairs = ""
    end,
})

nvim_create_autocmd("VimResized", {group = augroup, command = "wincmd ="})

local make_sidebar --- @type fun()
do
    local sidebar_width = 80
    vim.env.MANWIDTH = tostring(sidebar_width + 1)

    --- @param size? integer
    local function toggle_side(size)
        if vim.o.winfixwidth then
            vim.cmd.wincmd("J")
            size = size or vim.o.buftype == "quickfix" and 10 or nil
            if size then
                nvim_win_set_height(0, size)
                vim.o.winfixheight = true
            end
            vim.o.winfixwidth = false
        else
            vim.cmd.wincmd("L")
            nvim_win_set_width(0, size or sidebar_width)
            vim.o.winfixwidth = true
            vim.o.winfixheight = false
        end
        vim.cmd.wincmd("=")
    end

    function make_sidebar()
        if vim.o.columns > sidebar_width * 2 and not vim.o.winfixwidth then
            toggle_side()
        end
    end

    nvim_create_autocmd("BufWinEnter", {
        group = augroup,
        callback = function()
            if vim.o.buftype == "help" or vim.o.filetype == "man" or vim.o.filetype == "fugitive" then
                make_sidebar()
            end
        end,
    })

    map("n", "<Leader>ts", function()
        toggle_side(vim.v.count ~= 0 and vim.v.count or nil)
    end)
end

-- Statusline {{{1

defaults.statusline = " %{v:lua.statusline_git()}%<%{v:lua.statusline_path(0, v:false, v:true)}%( %m%)"
    .. "%= %{v:lua.statusline_lsp_progress()}%{v:lua.statusline_diagnostics()}%c%V %l/%L "
vim.g.qf_disable_statusline = true -- Don't let quickfix ftplugin override statusline

defaults.rulerformat = "%l/%L"

defaults.title = true
defaults.titlestring = "%{v:lua.statusline_path(0, v:true)} - nvim"
defaults.titlelen = 0

defaults.tabline = "%!v:lua.tabline()"

--- @return string
function _G.tabline()
    local tabs = nvim_list_tabpages()
    local current = nvim_get_current_tabpage()
    local max_width = math.max(math.floor(vim.o.columns / #tabs) - 3, 1)
    local s = ""
    for i, tab in ipairs(tabs) do
        s = s .. (tab == current and "%#TabLineSel#" or "%#TabLine#") .. "%" .. i .. "T "
            .. "%." .. max_width .. "(%{v:lua.statusline_path(" .. nvim_tabpage_get_win(tab) .. ")}%)"
            .. " %T%#TabLineFill#" .. (tab == current and "▌" or tabs[i + 1] == current and "▐" or "│")
    end
    return s
end

--- @param winid integer
--- @param absolute? boolean
--- @param title? boolean
--- @return string
function _G.statusline_path(winid, absolute, title)
    local bufnr = nvim_win_get_buf(winid)
    local name = nvim_buf_get_name(bufnr)
    local buftype = vim.bo[bufnr].buftype
    if buftype == "terminal" then
        return "[Terminal]"
            .. (title and vim.b[bufnr].term_title ~= name and " " .. vim.b[bufnr].term_title or "")
    elseif buftype == "quickfix" then
        return nvim_eval_statusline("%q", {winid = winid}).str
            .. (title and vim.w[winid].quickfix_title and " " .. vim.w[winid].quickfix_title or "")
    elseif buftype == "nofile" and vim.bo[bufnr].filetype ~= "dirvish" or name == "" then
        return nvim_eval_statusline("%f", {winid = winid}).str
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

do
    local progress = {} --- @type table<integer, string?>
    function _G.statusline_lsp_progress()
        local s = ""
        for _, client in ipairs(lsp.get_clients({bufnr = 0})) do
            if progress[client.id] then s = s .. progress[client.id] end
        end
        return s
    end

    local timer = vim.uv.new_timer()
    nvim_create_autocmd("LspProgress", {
        group = augroup,
        callback = function(args)
            local t = args.data.params.value
            progress[args.data.client_id] = t.kind ~= "end"
                and ("[%s%s] "):format(t.percentage and t.percentage .. "% " or "", t.title)
                or nil
            if not timer:is_active() then
                timer:start(16, 0, vim.schedule_wrap(function()
                    vim.cmd.redrawstatus({bang = true})
                end))
            end
        end,
    })
end

-- Git and diff {{{1

nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "GV",
    callback = function() vim.wo[0][0].list = false end,
})

map("n", "<Leader>tg", function()
    for _, winid in ipairs(nvim_tabpage_list_wins(0)) do
        if vim.b[nvim_win_get_buf(winid)].fugitive_type == "index" then
            nvim_win_hide(winid)
            return
        end
    end
    vim.cmd.Git()
end)

map("n", "<Leader>td", function()
    if vim.o.diff then
        for _, winid in ipairs(nvim_tabpage_list_wins(0)) do
            if vim.wo[winid].diff and winid ~= nvim_get_current_win() then
                nvim_win_hide(winid)
            end
        end
    else
        vim.cmd("keepalt Gdiffsplit!")
    end
end)

vim.opt.diffopt:append("vertical,foldcolumn:1,algorithm:histogram,hiddenoff")

-- Remap do and dp to use a motion
_G.diff_bufnr = 0
for key, cmd in pairs({o = "diffget", p = "diffput"}) do
    _G[cmd .. "_operator"] = function()
        vim.cmd[cmd]({
            diff_bufnr ~= 0 and diff_bufnr or nil,
            range = {fn.line("'["), fn.line("']")},
        })
    end
    -- The use of <Cmd> clears the count so that it doesn't affect the motion
    map("n", "d" .. key, ("<Cmd>set operatorfunc=v:lua.%s_operator | lua diff_bufnr = vim.v.count<CR>g@"):format(cmd))
    map("n", "d" .. key .. key, function()
        local lnum = nvim_win_get_cursor(0)[1]
        vim.cmd[cmd]({range = {lnum, lnum + vim.v.count1 - 1}})
    end)
    -- Make the original behavior available under a different mapping
    map("n", "d" .. key .. "c", "d" .. key)
end

-- The mappings above don't work in visual mode. As an alternative, allow gv in operator-pending mode
map("o", "gv", "<Cmd>normal! gv<CR>")

-- Remap to something more convenient in my keymap
map("n", "dx", "dp", {remap = true})
map("n", "dxx", "dpp", {remap = true})
map("", "[h", "[c", {remap = true})
map("", "]h", "]c", {remap = true})

-- REPL {{{1

--- @class repl_config
--- @field cmd? string[]
--- @field cwd? string
--- @field load_file? fun(path: string): string
--- @field format? fun(code: string): string
--- @field eval? fun(arg: {code: string} | {file: string})

do
    --- @type table<string, integer>
    _G.repl_bufnrs = repl_bufnrs or {}

    local default_cmd = {"zsh"}

    local function get_key()
        local repl = vim.b.repl or {} --- @type repl_config
        return table.concat(repl.cmd or default_cmd, "\0") .. "\0" .. (repl.cwd or "")
    end

    --- @param toggle? boolean
    local function open_repl(toggle)
        local repl = vim.b.repl --- @type repl_config?
        local key = get_key()
        local bufnr = repl_bufnrs[key]

        if not bufnr then
            vim.cmd("keepalt new")
            make_sidebar()
            bufnr = nvim_get_current_buf()
            repl_bufnrs[key] = bufnr
            fn.jobstart(repl and repl.cmd or default_cmd, {
                cwd = repl and repl.cwd,
                term = true,
                on_stdout = function()
                    for _, winid in ipairs(fn.win_findbuf(bufnr)) do
                        nvim_win_set_cursor(winid, {nvim_buf_line_count(bufnr), 0})
                    end
                end,
                on_exit = function()
                    repl_bufnrs[key] = nil
                    pcall(nvim_buf_delete, bufnr, {})
                end,
            })
            vim.b.repl = repl -- Allow using toggle mapping (and others) from REPL buffer
            -- Clear screen at first prompt, since the text is echoed before the REPL is ready
            nvim_chan_send(vim.o.channel, vim.keycode("<C-l>"))
            return bufnr
        end

        for _, winid in ipairs(nvim_tabpage_list_wins(0)) do
            if nvim_win_get_buf(winid) == bufnr then
                if toggle then
                    nvim_win_hide(winid)
                    return
                end
                return bufnr
            end
        end

        vim.cmd("keepalt split #" .. bufnr)
        make_sidebar()
        return bufnr
    end

    map("n", "<Leader>tt", function()
        if open_repl(true) then vim.cmd.startinsert() end
    end)

    --- @param s string
    function _G.bracketed_paste(s)
        return "\27[200~" .. s .. "\27[201~"
    end

    --- @type [integer, integer]
    local saved_cursor

    --- @param type "buffer" | "visual" | "char" | "line" | "block"
    function _G.repl_eval(type)
        local lines = {}
        if type == "visual" then
            lines = fn.getregion(fn.getpos("'<"), fn.getpos("'>"), {type = fn.visualmode()})
        elseif type ~= "buffer" then -- Used as normal mode operator
            lines = fn.getregion(fn.getpos("'["), fn.getpos("']"), {
                type = ({char = "v", line = "V", block = vim.keycode("<C-V>")})[type],
                exclusive = false,
            })
            nvim_win_set_cursor(0, saved_cursor)
        end
        local code = vim.iter(lines):map(function(s)
            s = s:gsub("\n", "\0")
            return s
        end):join("\n")

        local repl = vim.b.repl or {} --- @type repl_config
        if repl.eval then
            return repl.eval(type == "buffer" and {file = nvim_buf_get_name(0)} or {code = code})
        end

        local winid = nvim_get_current_win()
        local bufnr = open_repl()
        nvim_set_current_win(winid)

        if type == "buffer" then
            code = repl.load_file and repl.load_file(nvim_buf_get_name(0))
                or table.concat(nvim_buf_get_lines(0, 0, -1, true), "\n")
        end
        code = repl.format and repl.format(code) or vim.keycode("<C-e><C-u>") .. bracketed_paste(code) .. "\n"
        nvim_chan_send(vim.bo[bufnr].channel, code)
    end

    map("n", "<Leader>e", function()
        saved_cursor = nvim_win_get_cursor(0)
        vim.o.operatorfunc = "v:lua.repl_eval"
        return "g@"
    end, {expr = true})

    map("n", "<Leader>ee", "<Leader>e_", {remap = true})

    map("x", "<Leader>e", "<Esc><Cmd>lua repl_eval('visual')<CR>")

    map("n", "<Leader>es", function()
        vim.cmd("silent update")
        repl_eval("buffer")
    end)

    map("n", "<Leader>ec", function()
        local bufnr = repl_bufnrs[get_key()]
        if bufnr then nvim_chan_send(vim.bo[bufnr].channel, vim.keycode("<C-c>")) end
    end)
end

-- Combines the functions of :lua {chunk}, :[lua]= {expr} and :luafile/source {file}, preserving variables across uses
nvim_create_user_command("Lua", function(opts)
    local load, args
    if opts.bang then -- Reset environment, and load file if given
        _G.lua_env = nil
        if opts.args == "" then return end
        load, args = loadfile, {opts.args}
    else
        -- Attempt to parse as an expression, falling back to a chunk
        -- As far as I know the only ambiguous case is a function call, where we want to print the return values
        load, args = loadstring, {"return " .. opts.args, opts.args}
    end
    _G.lua_env = lua_env or setmetatable({}, {__index = _G})

    local func, err
    for _, arg in ipairs(args) do
        func, err = load(arg)
        if func then
            debug.sethook(function(event)
                -- Add top-level locals to environment. Doesn't work when the chunk has a tail call
                if event == "return" and debug.getinfo(2, "f").func == func then
                    local i, name, value = 1, debug.getlocal(2, 1)
                    while name do
                        if not vim.startswith(name, "(") then lua_env[name] = value end
                        i = i + 1
                        name, value = debug.getlocal(2, i)
                    end
                end
            end, "r");
            (function(ok, ...)
                if ok then
                    local results = {}
                    for i = 1, select("#", ...) do
                        table.insert(results, vim.inspect(select(i, ...), nil))
                    end
                    vim.notify(table.concat(results, "\n"))
                else
                    err = select(1, ...)
                end
            end)(pcall(setfenv(func, lua_env)))
            debug.sethook()
            break
        end
    end
    if err then vim.notify(err, vim.log.levels.ERROR) end
end, {nargs = "?", complete = "lua", bang = true})

-- Quickfix {{{1

package.loaded.quickfix = nil -- Reload when sourcing init.lua
local quickfix = require("quickfix")

defaults.quickfixtextfunc = "v:lua.require'quickfix'.textfunc"

nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "qf",
    callback = function()
        vim.wo[0][0].wrap = false
        vim.wo[0][0].list = false

        map("n", "<CR>", function()
            vim.cmd.normal({vim.keycode("<CR>"), bang = true})
            quickfix.after_jump()
        end, {buffer = 0})
        map("n", "<M-CR>", function()
            vim.cmd(fn.win_gettype() == "loclist" and ".ll | lclose" or ".cc | cclose")
            quickfix.after_jump()
        end, {buffer = 0})
    end,
})

map("n", "<Leader>tq", function()
    vim.cmd(fn.getqflist({winid = true}).winid ~= 0 and "cclose" or "botright copen")
end)
map("n", "<Leader>tl", function()
    vim.cmd(fn.getloclist(0, {winid = true}).winid ~= 0 and "lclose" or "lopen")
end)

local killable_process
do
    --- @type table<vim.SystemObj, string>
    local running_processes = {}

    --- @param cmd string
    --- @param on_exit fun(result: vim.SystemCompleted)
    function killable_process(cmd, on_exit)
        local proc
        proc = vim.system({"sh", "-c", cmd}, {}, function(result)
            running_processes[proc] = nil
            on_exit(result)
        end)
        running_processes[proc] = cmd
    end

    map("n", "<C-c>", function()
        for proc, _ in pairs(running_processes) do
            proc:kill("sigterm")
        end
        return "<C-c>"
    end, {expr = true})

    nvim_create_user_command("Processes", function()
        for proc, cmd in pairs(running_processes) do
            vim.notify(proc.pid .. "\t" .. cmd)
        end
    end, {bar = true})
end

nvim_create_user_command("Make", function(opts)
    -- Expand cmdline-special characters like %, #, <cword>, etc.
    -- Unlike vim.fn.expand, they can be anywhere in the string
    local function expand(str) --- @param str string
        local result --- @type string
        nvim_create_user_command("EXPAND", function(opts) result = opts.args end, {
            -- Expansion only takes place for "file" and "dir"
            complete = "file",
            -- If it takes at most one argument, then environment variables, command substitution and globs are also
            -- expanded, and some backslashes are removed, which we don't want, so that the shell can do all of it
            -- This is also why :Make and :Grep are declared to take multiple arguments
            nargs = "*",
            -- An unsolved issue is that the contents of expansions are backslash-escaped, which is convenient for
            -- interactive use since it handles some (but not all) shell special characters, but it's an incompatibility
            -- here when expanding makeprg, and %:S, which should be the safe way to expand it, gives the wrong result
        })
        vim.cmd.EXPAND("[" .. str:gsub("\\|", "|") .. "]") -- Add brackets to preserve leading and trailing whitespace
        nvim_del_user_command("EXPAND")
        return result:sub(2, -2)
    end

    local cmd, errorformat
    if opts.bang then
        cmd, errorformat = opts.args, vim.go.errorformat
    else
        cmd, errorformat = vim.o.makeprg, vim.o.errorformat
        if not cmd:find("$*", 1, true) then cmd = cmd .. " $*" end
        cmd = vim.trim(vim.iter(vim.gsplit(cmd, "$*", {plain = true})):map(expand):join(opts.args))
    end
    vim.notify(cmd)
    killable_process(cmd .. " 2>&1", vim.schedule_wrap(function(result) --- @param result vim.SystemCompleted
        if result.signal ~= 0 then
            vim.notify(("%s: exited with signal %d"):format(cmd, result.signal), vim.log.levels.ERROR)
        elseif result.code ~= 0 then
            vim.notify(("%s: exited with status %d"):format(cmd, result.code), vim.log.levels.WARN)
        else
            vim.notify(cmd .. ": finished")
        end
        local lines = vim.split(result.stdout, "\n")
        if lines[#lines] == "" then table.remove(lines) end
        fn.setqflist({}, " ", {title = cmd, lines = lines, efm = errorformat})
        -- Don't open quickfix window without valid entries (because unlike other quickfix
        -- actions the primary purpose is running the command, not seeing its results),
        -- and preserve focus (because it may be long-running and finish at any time)
        local winid = nvim_get_current_win()
        vim.cmd("botright cwindow")
        pcall(nvim_set_current_win, winid)
    end))
end, {nargs = "*", complete = "file", bang = true})

map("n", "<Leader>mm", "<Cmd>silent update | Make<CR>")
map("n", "<Leader>mc", "<Cmd>silent update | Make clean<CR>")

nvim_create_user_command("Grep", function(opts)
    killable_process("rg --json " .. opts.args, function(result)
        if result.signal ~= 0 then
            return vim.schedule_wrap(vim.notify)("rg: exited with signal " .. result.signal, vim.log.levels.ERROR)
        end
        local items = {}
        for line in vim.gsplit(result.stdout, "\n", {trimempty = true}) do
            -- Unlike table.insert, this doesn't skip indices when from_ripgrep returns nil
            items[#items + 1] = quickfix.from_ripgrep(line)
        end
        for line in vim.gsplit(result.stderr, "\n", {trimempty = true}) do
            items[#items + 1] = {type = "E", text = line}
        end
        vim.schedule_wrap(quickfix.set_list)({title = "rg " .. opts.args, items = items})
    end)
end, {nargs = "+", complete = "file"})

do
    local function grep(args, pattern)
        vim.cmd.Grep({args, fn.shellescape(pattern), magic = {file = false}})
    end
    map("n", "grg", function() grep("-Fwe", fn.expand("<cword>")) end)
    map("n", "grG", function() grep("-Fwe", fn.expand("<cWORD>")) end)
    map("x", "grg", function()
        local lines = fn.getregion(fn.getpos("v"), fn.getpos("."), {type = fn.mode()})
        grep("-FUe", vim.iter(lines):map(function(s) s = s:gsub("\n", "\0"); return s end):join("\n"))
        return "<Esc>"
    end, {expr = true})
end

nvim_create_user_command("HelpGrep", function(opts)
    vim.cmd.helpgrep(opts.args)
    local items = fn.getqflist()
    for _, item in ipairs(items) do
        item.user_data = {
            highlight_ranges = {{item.col - 1, item.end_col - 1}},
        }
    end
    quickfix.set_list({action = "r", items = items})
end, {nargs = 1})

nvim_create_user_command("Diagnostics", function()
    quickfix.set_list({title = "Diagnostics", items = vim.diagnostic.toqflist(vim.diagnostic.get())})
    _G.diagnostic_qf_id = fn.getqflist({id = 0}).id
end, {bar = true})
nvim_create_autocmd("DiagnosticChanged", {
    group = augroup,
    callback = function()
        if diagnostic_qf_id and fn.getqflist({id = diagnostic_qf_id}).id ~= 0 then
            fn.setqflist({}, "u", {id = diagnostic_qf_id, items = vim.diagnostic.toqflist(vim.diagnostic.get())})
        end
    end,
})

-- Fuzzy finder {{{1

--- @class fzf_opts
--- @field args? string[]
--- @field bind? table<string, string | {[1]: string, args: string, [2]: fun(...: string): string?}>
--- @field cwd? string
--- @field input? string[]
--- @field on_output fun(lines: string[])

--- @type {next_id: integer, [string]: fun(args: string): string?}
_G.fzf_bound_functions = {next_id = 0}

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

    opts.args = opts.args or {}
    local function_ids = {}
    for key, action in pairs(opts.bind or {}) do
        if type(action) == "table" then
            local func = action[2]
            local id = "_" .. fzf_bound_functions.next_id
            fzf_bound_functions.next_id = fzf_bound_functions.next_id + 1
            fzf_bound_functions[id] = function(args)
                return func(unpack(vim.split(args, "\n")))
            end
            table.insert(function_ids, id)
            action = ([[%s:nvim --server %s --remote-expr "v:lua.fzf_bound_functions.%s('$(printf '%%s\n' %s | sed "s/'/''/")')"]])
                :format(action[1], fn.shellescape(vim.v.servername), id, action.args)
        end
        table.insert(opts.args, ("--bind=%s:%s"):format(key, action))
    end

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

    fn.jobstart(cmd, {
        cwd = opts.cwd,
        term = true,
        on_exit = function()
            nvim_buf_delete(bufnr, {})
            local lines = fn.readfile(output)
            vim.uv.fs_unlink(output)
            if input then vim.uv.fs_unlink(input) end
            for _, id in ipairs(function_ids) do fzf_bound_functions[id] = nil end
            opts.on_output(lines)
        end,
    })
    vim.cmd.startinsert()
    map("t", "<Esc>", "<Esc>", {buffer = true})
end

--- @param title string
--- @param parse fun(line: string): vim.quickfix.entry
--- @return fun(lines: string[])
local function jump_or_setqflist(title, parse)
    return function(lines)
        if #lines == 1 then
            local item = parse(lines[1])
            if not (item.bufnr or item.filename) then return end
            local bufnr = item.bufnr or fn.bufadd(item.filename)
            -- Unlike nvim_win_set_buf, :buffer adds an entry to the jumplist and executes the SwapExists autocmd
            vim.cmd.buffer(bufnr)
            vim.bo.buflisted = true
            if item.lnum and item.lnum > 0 then
                local set_cursor = item.vcol and item.vcol ~= 0 and fn.setcursorcharpos or fn.cursor
                set_cursor(item.lnum, item.col and item.col > 0 and item.col or 1)
                quickfix.after_jump()
            end
        elseif #lines > 1 then
            quickfix.set_list({title = title, items = vim.tbl_map(parse, lines)})
        end
    end
end

map("n", "<Leader>ff", vim.cmd.Fzf)
nvim_create_user_command("Fzf", function(opts)
    local cwd = vim.fs.normalize(opts.args)
    run_fzf({
        args = {("--prompt=%s/"):format(fn.fnamemodify(cwd, ":p:~"):gsub("/$", ""))},
        cwd = cwd,
        on_output = jump_or_setqflist("Files", function(line)
            return {filename = line, valid = true}
        end),
    })
end, {nargs = "?", complete = "dir"})

map("n", "<Leader>fb", function()
    local function buffers()
        return vim.tbl_map(function(info)
            return ("%d %s%s"):format(
                info.bufnr,
                info.name ~= "" and fn.fnamemodify(info.name, ":~:.") or "[No Name]",
                vim.bo[info.bufnr].modified and " [+]" or "")
        end, fn.getbufinfo({buflisted = 1}))
    end
    run_fzf({
        args = {"--prompt=buffer: ", "--with-nth=2.."},
        input = buffers(),
        bind = {
            ["ctrl-d"] = {"reload", args = "{1}", function(bufnr)
                bufnr = tonumber(bufnr)
                if bufnr and not vim.bo[bufnr].modified then
                    nvim_buf_delete(bufnr, {})
                end
                return table.concat(buffers(), "\n")
            end},
        },
        on_output = jump_or_setqflist("Buffers", function(line)
            return {bufnr = tonumber(vim.gsplit(line, " ")()), valid = true}
        end),
    })
end)

map("n", "<Leader>fg", function()
    local pattern = ""
    local args = "-S"
    local editing_args = false
    local script = nvim_get_runtime_file("scripts/igrep_format.lua", false)[1]
    run_fzf({
        args = {"--prompt=grep: ", "--with-nth=2..", "--delimiter=\t", "--ansi", "--disabled"},
        bind = {
            change = {"top+transform", args = "{q}", function(query)
                if editing_args then args = query else pattern = query end
                return ("reload:rg --json -e %s %s | nvim -l %s %d")
                    :format(fn.shellescape(pattern), args, fn.shellescape(script), vim.o.columns)
            end},
            ["ctrl-o"] = {"transform", args = "", function()
                editing_args = not editing_args
                if editing_args then
                    return "change-prompt(args: )+change-query:" .. args
                else
                    return "change-prompt(grep: )+change-query:" .. pattern
                end
            end},
        },
        input = {},
        on_output = function(lines)
            return jump_or_setqflist(("rg -e %s %s"):format(fn.shellescape(pattern), args), function(line)
                return quickfix.from_ripgrep(vim.gsplit(line, "\t")() or "") or {}
            end)(lines)
        end,
    })
end)

nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = "qf",
    callback = function()
        map("n", "s", function()
            local list = quickfix.get_list({
                loclist_winid = fn.win_gettype() == "loclist" and 0 or nil,
                title = true, items = true, winid = true, filewinid = true,
            })
            run_fzf({
                args = {"--prompt=quickfix: ", "--with-nth=2..", "--ansi", "--tiebreak=begin"},
                input = vim.iter(ipairs(list.items)):map(function(i, item)
                    return item.valid ~= 0 and i .. " " .. quickfix.to_fzf(item) or nil
                end):totable(),
                on_output = function(lines)
                    -- Focus isn't automatically returned to the quickfix window when the fzf window is closed
                    nvim_set_current_win(list.winid)
                    if #lines == 1 then
                        vim.cmd(vim.gsplit(lines[1], " ")() .. (list.filewinid and "ll" or "cc"))
                        nvim_win_hide(list.winid)
                        quickfix.after_jump()
                    elseif #lines > 1 then
                        quickfix.set_list({
                            loclist_winid = list.filewinid,
                            title = list.title,
                            items = vim.tbl_map(function(line)
                                return list.items[tonumber(vim.gsplit(line, " ")())]
                            end, lines),
                        })
                    end
                end,
            })
        end, {buffer = 0})
    end,
})

map("n", "<Leader>fs", function()
    local bufnr = nvim_get_current_buf()
    local last_query = ""
    local items = {}
    run_fzf({
        args = {"--prompt=symbol: ", "--with-nth=2..", "--ansi", "--tiebreak=begin"},
        bind = {
            change = {"top+reload", args = "{q}", function(query)
                query = vim.gsplit(query, " ")() or ""
                if query ~= last_query then
                    last_query = query
                    local results = lsp.buf_request_sync(bufnr, lsp.protocol.Methods.workspace_symbol, {query = query})
                    items = {}
                    for _, result in pairs(results or {}) do
                        if result.error then lsp.log.error(tostring(result.error)) end
                        quickfix.from_lsp_symbols(result.result or {}, items)
                    end
                end
                return vim.iter(ipairs(items)):map(function(i, item)
                    return i .. " " .. quickfix.to_fzf(item)
                end):join("\n")
            end},
        },
        input = {},
        on_output = jump_or_setqflist("Symbols", function(line)
            return items[tonumber(vim.gsplit(line, " ")())]
        end),
    })
end)

--- @param opts? {prompt?: string, format_item?: fun(any): string}
function vim.ui.select(items, opts, on_choice)
    opts = opts or {}
    run_fzf({
        args = {("--prompt=%s "):format((opts.prompt or "select:"):lower()), "--with-nth=2..", "+m"},
        input = vim.iter(ipairs(items)):map(function(i, item)
            return i .. " " .. (opts.format_item or tostring)(item)
        end):totable(),
        on_output = function(lines)
            if lines[1] then
                local i = tonumber(vim.gsplit(lines[1], " ")())
                on_choice(items[i], i)
            else
                on_choice(nil, nil)
            end
        end,
    })
end

-- Completion {{{1

local cmp = require("cmp")

--- @return integer[]
function _G.get_listed_bufnrs()
    return vim.tbl_filter(function(bufnr) return vim.bo[bufnr].buflisted end, nvim_list_bufs())
end

--- @param fallback fun()
local function maybe_complete(fallback)
    if nvim_get_current_line():sub(1, nvim_win_get_cursor(0)[2]):find("[^%s]") then
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

map("n", "grd", lsp.buf.declaration)
map("n", "grt", lsp.buf.type_definition)
map("n", "grq", lsp.buf.format)
map("n", "grl", lsp.codelens.run)

map("n", "<M-LeftMouse>", "<LeftMouse><Cmd>lua vim.lsp.buf.hover()<CR>")
map("n", "<M-RightMouse>", "<LeftMouse><Cmd>lua vim.diagnostic.open_float()<CR>")

vim.diagnostic.config({
    severity_sort = true,
    signs = false,
    virtual_text = {format = function(d) return d.message:match("[^\n]*") end},
})

for k, v in pairs({e = {vim.diagnostic, "diagnostics"}, k = {lsp.inlay_hint, "inlay hints"}}) do
    map("n", "yo" .. k, function()
        v[1].enable(not v[1].is_enabled())
        nvim_echo({{v[2] .. ": " .. (v[1].is_enabled() and "enabled" or "disabled")}}, false, {})
    end)
end

nvim_create_autocmd("InsertEnter", {
    group = augroup,
    callback = function()
        if lsp.inlay_hint.is_enabled() then
            lsp.inlay_hint.enable(false)
            nvim_create_autocmd("InsertLeave", {
                once = true,
                callback = function() lsp.inlay_hint.enable() end,
            })
        end
    end,
})

_G.old_locations_to_items = old_locations_to_items or lsp.util.locations_to_items
function lsp.util.locations_to_items(locations, position_encoding)
    --- @type vim.quickfix.entry[]
    local items = old_locations_to_items(locations, position_encoding)
    for _, item in ipairs(items) do
        item.user_data = {
            highlight_ranges = {
                item.lnum == item.end_lnum and {item.col - 1, item.end_col - 1} or nil,
            },
        }
    end
    return items
end

map("n", "gO", function()
    local bufnr = nvim_get_current_buf()
    local winid = nvim_get_current_win()
    local cursor = nvim_win_get_cursor(0)
    local line = nvim_buf_get_lines(0, cursor[1] - 1, cursor[1], true)[1]
    lsp.buf_request_all(
        bufnr,
        lsp.protocol.Methods.textDocument_documentSymbol,
        {textDocument = lsp.util.make_text_document_params()},
        function(results)
            local items = {}
            local cursor_index = nil
            for client_id, result in pairs(results) do
                if result.err then
                    return vim.notify(tostring(result.err), vim.log.levels.ERROR)
                end
                cursor_index = quickfix.from_lsp_symbols(result.result, items, bufnr, {
                    line = cursor[1] - 1,
                    character = vim.str_utfindex(line, lsp.get_client_by_id(client_id).offset_encoding, cursor[2]),
                }) or cursor_index
            end
            quickfix.set_list({
                loclist_winid = winid,
                title = "Symbols",
                items = items,
                idx = cursor_index,
                context = {tree_foldlevel = 0},
            })
        end)
end)

for name, params in pairs({
    IncomingCalls = {lsp.protocol.Methods.textDocument_prepareCallHierarchy, lsp.protocol.Methods.callHierarchy_incomingCalls, function(r) return r.from end},
    OutgoingCalls = {lsp.protocol.Methods.textDocument_prepareCallHierarchy, lsp.protocol.Methods.callHierarchy_outgoingCalls, function(r) return r.to end},
    Supertypes = {lsp.protocol.Methods.textDocument_prepareTypeHierarchy, lsp.protocol.Methods.typeHierarchy_supertypes, function(r) return r end},
    Subtypes = {lsp.protocol.Methods.textDocument_prepareTypeHierarchy, lsp.protocol.Methods.typeHierarchy_subtypes, function(r) return r end},
}) do
    nvim_create_user_command(name, function()
        local bufnr = nvim_get_current_buf()
        local winid = nvim_get_current_win()
        vim.lsp.buf_request_all(bufnr, params[1], function(client)
            return lsp.util.make_position_params(winid, client.offset_encoding)
        end, function(results)
            --- @class hierarchy_item: lsp.CallHierarchyItem
            --- @field children? hierarchy_item[]
            local symbols = {} --- @type hierarchy_item[]
            local pending_requests = 0
            for client_id, result in pairs(results) do
                if result.err then
                    return vim.notify(tostring(result.err), vim.log.levels.ERROR)
                end
                local client = assert(lsp.get_client_by_id(client_id))
                local cache = {} --- @type table<string, hierarchy_item>
                local function process_symbol(symbol) --- @param symbol hierarchy_item
                    -- Assume symbols can be uniquely identified by their start positions
                    local id = ("%s:%d:%d")
                        :format(symbol.uri, symbol.selectionRange.start.line, symbol.selectionRange.start.character)
                    -- Prevent duplicate work and infinite recursion
                    if cache[id] then return cache[id] end
                    cache[id] = symbol

                    pending_requests = pending_requests + 1
                    client:request(params[2], {item = symbol}, function(err, result)
                        if err then vim.notify(tostring(err), vim.log.levels.WARN) end
                        symbol.children = vim.iter(result or {}):map(params[3]):map(process_symbol):totable()
                        pending_requests = pending_requests - 1
                        if pending_requests == 0 then
                            local items = {}
                            quickfix.from_lsp_symbols(symbols, items)
                            quickfix.set_list({title = name, items = items, context = {tree_foldlevel = 1}})
                        end
                    end, bufnr)
                    return symbol
                end
                vim.list_extend(symbols, vim.tbl_map(process_symbol, result.result))
            end
        end)
    end, {bar = true})
end

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
    vim.notify(root_dir)
    nvim_set_current_dir(root_dir)
end, {bar = true})

--- @class LspConfig: vim.lsp.ClientConfig
--- @field sandbox? {args?: string[], read?: string[], write?: string[]}
--- @field on_detach? fun(client: vim.lsp.Client, bufnr: integer)

--- @param config LspConfig
function _G.start_lsp(config)
    if fn.executable(config.cmd[1]) ~= 0 and vim.startswith(vim.uri_from_bufnr(0), "file:") then
        config.name = config.cmd[1]

        if config.sandbox then
            config.cmd = vim.iter({
                -- LSP servers should check that the parent is still alive, else exit, so share the pid namspace
                "sb", "-spid",
                -- Use tbl_values to filter out nils
                vim.tbl_map(function(p) return "-r:" .. p end, vim.tbl_values(config.sandbox.read or {})),
                vim.tbl_map(function(p) return "-w:" .. p end, vim.tbl_values(config.sandbox.write or {})),
                config.sandbox.args or {},
                config.cmd,
            }):flatten():totable()
            for _, dir in pairs(config.sandbox.write or {}) do fn.mkdir(dir, "p") end
        end

        config.capabilities = vim.tbl_deep_extend("force",
            require("cmp_nvim_lsp").default_capabilities({snippetSupport = false}),
            config.capabilities or {})

        lsp.start(config)
    end
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

        if client:supports_method(lsp.protocol.Methods.textDocument_documentHighlight) then
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
                            client:request(
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
                if client:supports_method(lsp.protocol.Methods.textDocument_documentHighlight) then
                    lsp.util.buf_clear_references(bufnr)
                end
                local config = client.config --[[ @as LspConfig ]]
                if config.on_detach then config.on_detach(client, bufnr) end
                nvim_clear_autocmds({buffer = bufnr, group = augroup})
            end,
        })
    end,
})
