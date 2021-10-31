-- Unique instance {{{1

local display = os.getenv("DISPLAY")
local instance_suffix = display and "."..display:gsub("[^%w_]", "_") or ""

if luakit.unique then
    local new = luakit.unique.new
    function luakit.unique.new(id)
        new(id..instance_suffix)
    end
end

local unique_instance = require("unique_instance")

-- Modules {{{1

local lousy = require("lousy")
lousy.theme.init(lousy.util.find_config("theme.lua"))
local theme = lousy.theme.get()

local adblock = require("adblock")
local adblock_chrome = require("adblock_chrome")
local binds_chrome = require("binds_chrome")
local clear_data = require("clear_data")
local cmdhist = require("cmdhist")
local downloads = require("downloads")
local downloads_chrome = require("downloads_chrome")
local editor = require("editor")
local follow = require("follow")
local follow_selected = require("follow_selected")
local go_input = require("go_input")
local go_next_prev = require("go_next_prev")
local go_up = require("go_up")
local help_chrome = require("help_chrome")
local history = require("history")
local history_chrome = require("history_chrome")
local image_css = require("image_css")
local log_chrome = require("log_chrome")
local modes = require("modes")
local newtab_chrome = require("newtab_chrome")
local open_editor = require("open_editor")
local quickmarks = require("quickmarks")
local referer_control_wm = require_web_module("referer_control_wm")
local search = require("search")
local select = require("select")
local session = require("session")
local settings = require("settings")
local settings_chrome = require("settings_chrome")
local styles = require("styles")
local tabhistory = require("tabhistory")
local undoclose = require("undoclose")
local userscripts = require("userscripts")
local view_source = require("view_source")
local webinspector = require("webinspector")
local webview = require("webview")
local window = require("window")

local completion_mtime = lfs.attributes(luakit.config_dir.."/completion_patched.lua", "modification")
if not completion_mtime
        or completion_mtime < lfs.attributes(luakit.install_paths.install_dir.."/lib/completion.lua", "modification")
        or completion_mtime < lfs.attributes(luakit.config_dir.."/completion.patch", "modification") then
    luakit.spawn_sync(string.format("patch -o %q %q %q",
        luakit.config_dir.."/completion_patched.lua",
        luakit.install_paths.install_dir.."/lib/completion.lua",
        luakit.config_dir.."/completion.patch"))
end
local completion = require("completion_patched")

-- Settings {{{1

settings.window.home_page = settings.window.new_tab_page
settings.window.search_engines = {
    ["!ddg"] = "https://duckduckgo.com/?q=%s",
    ["!man"] = "https://man.archlinux.org/man/%s",
}
settings.window.default_search_engine = "!ddg"
settings.window.check_filepath = false
settings.window.close_with_last_tab = true

unique_instance.open_links_in_new_window = true

settings.completion.history.order = "last_visit"

luakit.enable_spell_checking = true

settings.webview.default_charset = "utf-8"
settings.webview.hardware_acceleration_policy = "always"
settings.webview.enable_accelerated_2d_canvas = true
settings.webview.enable_webgl = true
settings.webview.enable_webaudio = true
settings.webview.enable_mediasource = true
settings.webview.enable_plugins = false
settings.webview.enable_java = false

soup.cookies_storage = luakit.data_dir.."/cookies.db"

session.recovery_file = luakit.data_dir.."/recovery"..instance_suffix

function select.label_maker()
    return trim(reverse(charset("ntesiroahdufyw")))
end
follow.pattern_maker = follow.pattern_styles.match_label

image_css.stylesheet.source = [[
    @media screen {
        body {
            background-color: black;
            height: 100%;
            display: grid;
        }
    }
    :root, img {
        filter: none !important;
    }
]]

-- Bindings {{{1

modes.remap_binds({"all", "passthrough"}, {
    {"<control-[>", "<Escape>", true},
})
modes.remove_binds("passthrough", {"<Escape>"})

modes.add_binds("normal", {
    {"!", "Start bang search.", function(win)
        win:enter_cmd(":open !")
    end},
    {"<control-!>", "Start bang search in a new tab.", function(win)
        win:enter_cmd(":tabopen !")
    end},

    {"<control-shift-c>", "Copy the selected text.", function()
        luakit.selection.clipboard = luakit.selection.primary
    end},
})

modes.remap_binds("normal", {
    {"<Mouse8>", "<Back>", true},
    {"<Mouse9>", "<Forward>", true},
})

settings.window.act_on_synthetic_keys = true
modes.add_binds({"normal", "insert"}, {
    {"<control-q>", "Send the next keypress directly to the webpage.", function(win)
        function win:hit(mods, key)
            win.hit = nil
            win.view:send_key(key, mods)
            return true
        end
    end},
})

editor.editor_cmd = "termite --name xmonad-custom-float -e 'nvim {file} +{line}'"
modes.remap_binds("insert", {
    {"<mod1-e>", "<control-e>"},
})

modes.add_binds("ex-follow", {
    {"<control-y>", "Hint all elements and yank the text of the matched element.", function(win)
        win:set_mode("follow", {
            prompt = "yank text",
            selector_func = "*",
            evaluator = function(element)
                return element.text_content
            end,
            func = function(text)
                luakit.selection.primary = text
            end,
        })
    end},

    {"d", "Hint all elements and delete the matched element.", function(win)
        win:set_mode("follow", {
            prompt = "delete",
            selector_func = "*",
            evaluator = function(element)
                element:remove()
            end,
        })
    end},
})

cmdhist.history_prev = "<control-p>"
cmdhist.history_next = "<control-n>"

modes.get_mode("command").reset_on_navigation = false
modes.get_mode("completion").reset_on_navigation = false

-- Tabs {{{1

modes.add_binds("normal", {
    {"b", "Choose and switch to a tab.", function(win)
        win:enter_cmd(":tabswitch ")
        win:set_mode("completion")
    end},
})

local function match_tabs(query, win, prefix, results)
    query = query or ""
    local index = tonumber(query:match("^%d+$"))
    local words = lousy.util.string.split(query)

    prefix = prefix or ""
    results = results or {}
    for i, view in ipairs(win.tabs.children) do
        local id = prefix..i
        local title = id..": "..(view.title or i)

        local match = true
        if index then
            match = i == index
        else
            local text = (title.." "..view.uri):lower()
            for _, word in ipairs(words) do
                if not text:find(word:lower(), 1, true) then
                    match = false
                    break
                end
            end
        end

        if match then
            table.insert(results, {
                lousy.util.escape(title), lousy.util.escape(view.uri),
                format = {{lit = id}}, buf = id,
                win = win, view = view, index = i,
            })
        end
    end
    return results
end

local function match_other_tabs(query, current_win)
    query = query or ""
    local win_id, index = query:match("^(%d+)%.?(%d*)$")
    win_id = tonumber(win_id)
    query = index and index or query

    local wins = lousy.util.table.values(window.bywidget)
    table.sort(wins, function(a, b)
        return a.win.id < b.win.id
    end)

    local results = {}
    for _, win in ipairs(wins) do
        if win ~= current_win and win.private == current_win.private
                and (not win_id or win.win.id == win_id) then
            match_tabs(query, win, win.win.id..".", results)
        end
    end
    return results
end

completion.completers.tab = {
    header = {"Tab", "URI"},
    func = match_tabs,
}
completion.completers.othertab = {
    header = {"Tab", "URI"},
    func = match_other_tabs,
}

modes.add_cmds{
    {":tabs[witch]", "Switch to the matching tab.", {
        format = "{tab}",
        func = function(win, opts)
            local tabs = match_tabs(opts.arg, win)
            if #tabs ~= 1 then
                win:error("No unique matching tab")
                return
            end
            win:goto_tab(tabs[1].index)
        end,
    }},

    {":tabde[tach]", "Move the current tab into a new window.", {
        func = function(win)
            if win.tabs:count() > 1 then
                window.new{win.view}
            end
        end,
    }},

    {":taba[ttach]", "Move a tab into the current window.", {
        format = "{othertab}",
        func = function(win, opts)
            local tabs = match_other_tabs(opts.arg, win)
            if #tabs ~= 1 then
                win:error("No unique matching tab")
                return
            end
            local tab = tabs[1]

            local close = tab.win.tabs:count() == 1
            settings.window.close_with_last_tab = false
            tab.win:detach_tab(tab.view)
            win:attach_tab(tab.view)
            win:goto_tab(-1)
            settings.window.close_with_last_tab = true
            if close then
                tab.win:close_win()
            end
        end,
    }},
}

lousy.widget.tab.label_format = "<span foreground='{index_fg}'>{index}</span>: {title}"

-- Widgets {{{1

window.add_signal("build", function(win)
    win.sbar.l.layout:pack(lousy.widget.uri())
    win.sbar.l.layout:pack(lousy.widget.progress())
    win.sbar.r.layout:pack(lousy.widget.buf())
    win.sbar.r.layout:pack(lousy.widget.scroll())
end)

webview.add_signal("init", function(view)
    luakit.idle_add(function()
        local function update_uri()
            local win = webview.window(view)
            if not win then
                return
            end

            local widget = win.sbar.l.layout.children[1]
            if widget.text:match("^<span color=") then
                return
            end

            local scheme = widget.text:match("^%a[%w+%-.]*:") or ""
            local color = scheme == "Link:" and theme.scheme_fg
                or win.view:ssl_trusted() and theme.trust_scheme_fg
                or (win.view:ssl_trusted() == false or scheme == "http:") and theme.notrust_scheme_fg
                or theme.scheme_fg

            widget.text = string.format("<span color=%q>%s</span>%s",
                color, scheme, widget.text:sub(#scheme + 1))
        end

        for _, signal in ipairs{"property::uri", "switched-page", "link-hover", "link-unhover"} do
            view:add_signal(signal, update_uri)
        end
        view:add_signal("load-status", function()
            view:emit_signal("link-unhover")
        end)
    end)
end)

function window.methods.update_win_title(win)
    win.win.title = ((win.view.title or "") == "" and "" or win.view.title.." - ").."luakit"
end

-- Smooth scroll {{{1

local function setup_smooth_scroll(view)
    local scroll_targets = {x = {}, y = {}}

    local index = getmetatable(view).__index
    getmetatable(view).__index = function(view, key)
        local value = index(view, key)
        if key ~= "scroll" then
            return value
        end

        return setmetatable({}, {
            __index = function(_, key)
                return scroll_targets[key] and scroll_targets[key][view] or value[key]
            end,

            __newindex = function(_, axis, target)
                local targets = scroll_targets[axis]
                target = math.max(target, 0)

                local direction = axis == "x" and "Width" or "Height"
                view:eval_js(string.format([[
                    document.documentElement.scroll%s - window.inner%s
                ]], direction, direction), {callback = function(max)
                    if targets[view] then
                        targets[view] = math.min(targets[view], math.max(max, 0))
                    end
                end})

                if targets[view] then
                    targets[view] = target
                    return
                end
                targets[view] = target

                local position = value[axis]
                local velocity = 0

                local scroll_timer = timer{interval = 16}
                local delta_time = scroll_timer.interval / 1000
                local undamped_freq = 30
                local decay = math.exp(-undamped_freq * delta_time)

                scroll_timer:add_signal("timeout", function()
                    local target = targets[view]
                    local rel_position = position - target
                    local temp = (undamped_freq * rel_position + velocity) * delta_time
                    velocity = (velocity - undamped_freq * temp) * decay
                    rel_position = (rel_position + temp) * decay
                    position = target + rel_position

                    local success = pcall(function()
                        value[axis] = math.floor(position + 0.5)
                    end)
                    if not success or math.abs(rel_position) < 0.5 then
                        targets[view] = nil
                        scroll_timer:stop()
                    end
                end)
                scroll_timer:start()
            end,
        })
    end

    webview.remove_signal("init", setup_smooth_scroll)
end
webview.add_signal("init", setup_smooth_scroll)

-- Private mode {{{1

local new_window = window.new
function window.new(args)
    local private = false
    args = lousy.util.table.filter_array(args, function(_, arg)
        private = private or arg.private or arg == "--private"
        return arg ~= "--private"
    end)

    local function set_private(win)
        if private then
            win.private = true
            win.sbar.ebox.bg = theme.private_bg
            win.sbar.l.ebox.bg = theme.private_bg
            win.sbar.sep.bg = theme.private_bg
            win.sbar.r.ebox.bg = theme.private_bg
        end
    end

    window.add_signal("init", set_private)
    win = new_window(args)
    window.remove_signal("init", set_private)
    return win
end

local new_tab = window.methods.new_tab
function window.methods.new_tab(win, arg, opts)
    opts = opts or {}
    opts.private = win.private
    return new_tab(win, arg, opts)
end

local set_prompt = window.methods.set_prompt
function window.methods.set_prompt(win, text, opts)
    opts = opts or {}
    set_prompt(win, text, {fg = opts.fg, bg = win.private and theme.private_bg or opts.bg})
end

local set_ibar_theme = window.methods.set_ibar_theme
function window.methods.set_ibar_theme(win, name)
    set_ibar_theme(win, name)
    if win.private then
        win.ibar.layout.bg = theme.private_bg
    end
end

modes.remove_binds("command", {":priv-t[abopen]"})

luakit.idle_add(function()
    undoclose.remove_signals("save")
    undoclose.add_signal("save", function(view)
        if (view.uri == "about:blank" or view.uri == settings.window.new_tab_page)
                and #view.history.items == 1 then
            return false
        end
    end)
end)

local save_session = session.save
function session.save(...)
    local wins = window.bywidget
    window.bywidget = lousy.util.table.filter_array(lousy.util.table.values(wins), function(_, win)
        return not win.private
    end)
    save_session(...)
    window.bywidget = wins
end

-- Sessions {{{1

completion.completers.session = {
    header = {"Saved Session"},
    func = function(prefix)
        local results = {}
        for file in lfs.dir(luakit.data_dir) do
            local name = file:match("^session%.(.+)")
            if name and name:find(prefix, 1, true) == 1 then
                table.insert(results, {lousy.util.escape(name), format = {{lit = name}}, buf = name})
            end
        end
        return results
    end,
}

local function session_path(name)
    return name and luakit.data_dir.."/session."..name:gsub("/", "_") or session.session_file
end

modes.add_cmds{
    {":write", "Save current session.", {
        format = "{session}",
        func = function(win, opts)
            session.save(session_path(opts.arg))
        end,
    }},

    {":restore", "Restore a saved session.", {
        format = "{session}",
        func = function(win, opts)
            local session_file = session.session_file
            local recovery_file = session.recovery_file
            session.session_file = session_path(opts.arg)
            session.recovery_file = nil
            if not session.restore(false) then
                win:error("Could not restore session")
            end
            session.session_file = session_file
            session.recovery_file = recovery_file
        end,
    }},

    {":delete", "Delete a saved session.", {
        format = "{session}",
        func = function(win, opts)
            if not os.remove(session_path(opts.arg)) then
                win:error("Could not delete session")
            end
        end,
    }},
}

-- Dark mode {{{1

settings.application.prefer_dark_mode = true

local dark_style = stylesheet{
    source = [[
        :root:not(.luakit-no-invert),
        :root:not(.luakit-no-invert) #luakit_select_overlay .hint_label,
        :root:not(.luakit-no-invert).luakit-invert-images
            :matches(img, video):not(:-webkit-full-screen, :root :-webkit-full-screen *),
        :matches(frame, iframe):not(:-webkit-full-screen) {
            filter: invert(1) hue-rotate(180deg);
        }
    ]],
}

webview.add_signal("init", function(view)
    view.stylesheets[dark_style] = true
end)

local check_dark_wm = require_web_module("check_dark_wm")

local function change_class(view, action, class)
    view:eval_js(string.format("document.documentElement.classList.%s(%q)", action, class),
        {no_return = true})
end

modes.add_binds("normal", {
    {"<control-v>", "Toggle dark mode for the current tab.", function(win)
        win.view.stylesheets[dark_style] = not win.view.stylesheets[dark_style]
        check_dark_wm:emit_signal(win.view, "check")
    end},

    {"V", "Toggle whether the current page is inverted.", function(win)
        if win.view.stylesheets[dark_style] then
            change_class(win.view, "toggle", "luakit-no-invert")
        else
            win.view.stylesheets[dark_style] = true
            change_class(win.view, "remove", "luakit-no-invert")
        end
    end},

    {"v", "Toggle whether images are inverted on the current page.", function(win)
        change_class(win.view, "toggle", "luakit-invert-images")
    end},
})

-- Downloads {{{1

local add_download = downloads.add
function downloads.add(uri, opts)
    local dl = type(uri) == "string" and download{uri = uri} or uri
    if dl.uri:match("file:") then
        dl:cancel()
    else
        add_download(dl, opts)
        dl:remove_signals("finished")
    end
end

function luakit.save_file(_, _, _, file)
    local cmd = string.format("%q %q", luakit.config_dir.."/download-prompt.sh", file)
    local status, file = luakit.spawn_sync(cmd)
    return status == 0 and file
end

downloads.add_signal("download::status", function(dl)
    if dl.status == "finished" and dl.destination:match("^/tmp/luakit%.") then
        downloads.do_open(dl)
    end
end)

local function add_torrent(torrent, win)
    luakit.spawn(string.format("transmission-remote -a %q", torrent), function(_, status)
        for _, win in pairs(win and {win} or window.bywidget) do
            if status == 0 then
                win:notify("Added torrent to Transmission")
            else
                win:error("Could not add torrent to Transmission")
            end
        end
    end)
end

downloads.add_signal("open-file", function(file, mime)
    if mime == "application/x-bittorrent"
            or mime == "application/octet-stream" and file:match("%.torrent$") then
        add_torrent(file)
    else
        luakit.spawn(string.format("xdg-open %q", file))
    end
    return true
end)

local close_tab = window.methods.close_tab
function window.methods.close_tab(win, view, ...)
    if view and view.uri == "about:blank" and #view.history.items == 1 and win.tabs:count() == 1 then
        view.uri = settings.window.new_tab_page
    else
        close_tab(win, view, ...)
    end
end

-- URIs {{{1

function lousy.uri.split(s)
    return {s}
end

local search_open = window.methods.search_open
function window.methods.search_open(win, s)
    if s:match("[%./]") and os.exists(s) then
        return s
    end
    return search_open(win, s)
end

webview.add_signal("init", function(view)
    view:add_signal("new-window-decision", function(_, uri)
        view.uri = uri
        return false
    end)

    view:add_signal("navigation-request", function(_, uri)
        if uri:match("^magnet:") then
            add_torrent(uri, webview.window(view))
            return false
        elseif uri:match("^mailto:") then
            luakit.spawn(string.format("termite -e 'alot compose %q'", uri))
            return false
        end
    end)
end)

history.add_signal("add", function(uri)
    if uri:match("^file:") then
        return false
    end
end)

local redirect_wm = require_web_module("redirect_wm")
modes.add_binds("normal", {
    {"gr", "Toggle host redirects in current tab.", function(win)
        redirect_wm:emit_signal(win.view, "toggle_host")
        win:reload()
    end},
    {"gs", "Toggle scheme redirects in current tab.", function(win)
        redirect_wm:emit_signal(win.view, "toggle_scheme")
        win:reload()
    end},
})

local function resolve_uri(uri, base)
    if not uri or uri:match("^%a[%w+%-.]*:") then
        return uri
    end
    local parts = soup.parse_uri(base)
    if uri:match("^//") then
        return parts.scheme..":"..uri
    end
    parts.fragment = uri:match("^#(.*)")
    if not parts.fragment then
        parts.query = uri:match("^%?(.*)")
        if uri:match("^/") then
            parts.path = uri
        elseif not parts.query then
            parts.path = parts.path:match(".*/")..uri
        end
    end
    return soup.uri_tostring(parts)
end

-- Media {{{1

local function play_media(uris, referrer, win)
    if #uris == 0 then
        win:error("Could not play media")
        return
    end

    local uri = table.remove(uris, 1)
    luakit.spawn(string.format("mpv --force-window --referrer=%q -- %q", referrer, uri), function(_, status)
        if status ~= 0 then
            msg.warn("Could not play media: "..uri)
            play_media(uris, referrer, win)
        end
    end)
end

modes.add_binds("ex-follow", {
    {"M", "Hint all audio and video elements and play the matched media with `mpv`.", function(win)
        win:set_mode("follow", {
            prompt = "mpv",
            selector_func = "audio, video",
            evaluator = function(element, page)
                local uris = {resolve_uri(element.attr.src, page.uri)}
                for _, source in ipairs(element:query("source")) do
                    table.insert(uris, resolve_uri(source.attr.src, page.uri))
                end
                if #uris ~= 0 then
                    element:remove()
                end
                return uris
            end,
            func = function(uris)
                table.insert(uris, win.view.uri)
                play_media(uris, win.view.uri, win)
            end,
        })
    end},

    {"m", "Hint all links and play the matched media with `mpv`.", function(win)
        win:set_mode("follow", {
            prompt = "mpv",
            selector = "uri",
            evaluator = "uri",
            func = function(uri)
                luakit.spawn(string.format("mpv --force-window -- %q", uri), function(_, status)
                    if status ~= 0 then
                        win:error("Could not play media")
                    end
                end)
            end,
        })
    end},
})

-- Initialization {{{1

luakit.spawn(string.format("%q/update-adblock.sh %q", luakit.config_dir, luakit.data_dir), function()
    adblock.load(true)
end)

if os.exists(luakit.config_dir.."/userconf.lua") then
    xpcall(function()
        require("userconf")
    end, msg.error)
end

if not session.restore() or #uris ~= 0 then
    window.new(uris)
end

-- vim: foldmethod=marker
