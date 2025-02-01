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
        if vim.startswith(args.match, "/") and not vim.uv.fs_stat(dir) then
            fn.mkdir(dir, "p")
            vim.notify("created directory " .. dir .. "\n")
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

    nvim_create_autocmd("BufWinEnter", {
        group = augroup,
        callback = function()
            if (vim.o.buftype == "help" or vim.o.filetype == "man" or vim.o.filetype == "fugitive")
                    and vim.o.columns > sidebar_width * 2 and not vim.o.winfixwidth then
                toggle_side()
            end
        end,
    })

    map("n", "<Leader>ts", function()
        toggle_side(vim.v.count ~= 0 and vim.v.count or nil)
    end)
end

nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = {"c", "cpp"},
    callback = function(args)
        -- This will be the default in nvim 0.11
        vim.bo[args.buf].commentstring = "// %s"
    end,
})

nvim_create_autocmd("FileType", {
    group = augroup,
    pattern = {"sh", "zsh"},
    callback = function(args)
        -- The default :ShKeywordPrg and :ZshKeywordPrg are broken in nvim
        vim.bo[args.buf].keywordprg = ":Man"
    end
})

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
    for _, bufnr in ipairs(nvim_list_bufs()) do
        if vim.b[bufnr].fugitive_type == "index" then
            nvim_buf_delete(bufnr, {})
            return
        end
    end
    vim.cmd.Git()
end)

map("n", "<Leader>td", function()
    if vim.o.diff then
        for _, winid in ipairs(nvim_tabpage_list_wins(nvim_get_current_tabpage())) do
            if vim.wo[winid].diff and winid ~= nvim_get_current_win() then
                nvim_win_close(winid, false)
            end
        end
    else
        vim.cmd.Gdiffsplit({bang = true})
    end
end)

vim.opt.diffopt:append("vertical,foldcolumn:1,algorithm:histogram,linematch:60,hiddenoff")

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
            vim.cmd(fn.getwininfo(nvim_get_current_win())[1].loclist == 0 and ".cc | cclose" or ".ll | lclose")
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

--- @param args string
local function ripgrep(args)
    killable_process("rg --json " .. args, function(result)
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
        vim.schedule_wrap(quickfix.set_list)({title = "rg " .. args, items = items})
    end)
end

map("n", "grg", function() ripgrep("-Fwe " .. fn.shellescape(fn.expand("<cword>"))) end)
map("n", "grG", function() ripgrep("-Fwe " .. fn.shellescape(fn.expand("<cWORD>"))) end)
map("x", "grg", function()
    nvim_feedkeys(vim.keycode("<Esc>"), "nix", false)
    local l1, c1 = unpack(nvim_buf_get_mark(0, "<"))
    local l2, c2 = unpack(nvim_buf_get_mark(0, ">"))
    local text = nvim_buf_get_text(0, l1 - 1, c1, l2 - 1, c2 + 1, {})
    ripgrep("-FUe " .. fn.shellescape(table.concat(text, "\n")))
end)

nvim_create_user_command("Grep", function(opts)
    ripgrep(opts.args)
end, {nargs = "+", complete = "file"})

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

    fn.termopen(cmd, {
        cwd = opts.cwd,
        on_exit = function()
            nvim_buf_delete(bufnr, {})
            local lines = fn.readfile(output)
            vim.uv.fs_unlink(output)
            if input then vim.uv.fs_unlink(input) end
            for _, id in ipairs(function_ids) do fzf_bound_functions[id] = nil end
            opts.on_output(lines)
        end,
    })
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
                loclist_winid = fn.getwininfo(nvim_get_current_win())[1].loclist ~= 0 and 0 or nil,
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
                        nvim_win_close(list.winid, false)
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
                if result.error then
                    return vim.notify(tostring(result.error), vim.log.levels.ERROR)
                end
                cursor_index = quickfix.from_lsp_symbols(result.result, items, bufnr, {
                    line = cursor[1] - 1,
                    character = lsp.util._str_utfindex_enc(line, cursor[2], lsp.get_client_by_id(client_id).offset_encoding),
                }) or cursor_index
            end
            quickfix.set_list({
                loclist_winid = winid,
                title = "Symbols in " .. fn.fnamemodify(nvim_buf_get_name(bufnr), ":~:."),
                items = items,
                idx = cursor_index,
                context = {tree_foldlevel = 0},
            })
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
