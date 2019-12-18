-- Modules {{{1

local unique_instance = require("unique_instance")

local lousy = require("lousy")
lousy.theme.init(lousy.util.find_config("theme.lua"))
local theme = lousy.theme.get()

local adblock = require("adblock")
local adblock_chrome = require("adblock_chrome")
local binds_chrome = require("binds_chrome")
local cmdhist = require("cmdhist")
local completion = require("completion")
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
local log_chrome = require("log_chrome")
local modes = require("modes")
local newtab_chrome = require("newtab_chrome")
local open_editor = require("open_editor")
local referer_control_wm = require_web_module("referer_control_wm")
local search = require("search")
local select = require("select")
local session = require("session")
local settings = require("settings")
local settings_chrome = require("settings_chrome")
local tabhistory = require("tabhistory")
local undoclose = require("undoclose")
local view_source = require("view_source")
local webinspector = require("webinspector")
local webview = require("webview")
local window = require("window")

-- Settings {{{1

settings.window.home_page = settings.window.new_tab_page
settings.window.search_engines = {[""] = "https://duckduckgo.com/?q=%s"}
settings.window.default_search_engine = ""
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

-- Bindings {{{1

modes.remap_binds({"all", "passthrough"}, {
    {"<control-[>", "<Escape>", true},
})
modes.remove_binds("passthrough", {"<Escape>"})

modes.add_binds("normal", {
    {"gs", "Change protocol to HTTPS.", function(win)
        win.view.uri = win.view.uri:gsub("^http:", "https:")
    end},
    {"<control-C>", "Copy the selected text.", function()
        luakit.selection.clipboard = luakit.selection.primary
    end},
})

settings.window.act_on_synthetic_keys = true
modes.add_binds({"normal", "insert"}, {
    {"<control-q>", "Send the next keypress directly to the webpage.", function(win)
        function win.hit(_, mods, key)
            win.hit = nil
            win.view:send_key(key, mods)
            return true
        end
    end},
})

editor.editor_cmd = "termite -e 'nvim {file} +{line}'"
modes.remap_binds("insert", {
    {"<mod1-e>", "<control-e>"},
})

cmdhist.history_prev = "<control-p>"
cmdhist.history_next = "<control-n>"

modes.get_mode("command").reset_on_navigation = false

local detach_target = nil
modes.add_cmds{
    {":tabde[tach]", "Move the current tab into a new window.", function(win)
        local close = win.tabs:count() == 1
        settings.window.close_with_last_tab = false
        if detach_target and detach_target.private == win.private then
            view = win.view
            win:detach_tab(view)
            detach_target:attach_tab(view)
            detach_target = nil
        else
            window.new{win.view}
        end
        settings.window.close_with_last_tab = true
        if close then
            win:close_win()
        end
    end},
    {":taba[ttach]", "Set the current window as the target for the next `:tabdetach`.", function(win)
        detach_target = win
    end},
}

-- Widgets {{{1

window.add_signal("build", function(win)
    win.sbar.l.layout:pack(lousy.widget.uri())
    win.sbar.l.layout:pack(lousy.widget.progress())
    win.sbar.r.layout:pack(lousy.widget.buf())
    win.sbar.r.layout:pack(log_chrome.widget())
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
            local scheme = widget.text:match("^%a[%a%d+%-.]*:") or ""
            local color = scheme == "Link:" and theme.scheme_fg
                or win.view:ssl_trusted() and theme.trust_scheme_fg
                or (win.view:ssl_trusted() == false or scheme == "http:") and theme.notrust_scheme_fg
                or theme.scheme_fg
            widget.text = string.format("<span color=%q>%s</span>%s", color,
                lousy.util.escape(scheme), lousy.util.escape(widget.text:sub(#scheme + 1)))
        end
        for _, signal in ipairs{"property::uri", "switched-page", "link-hover", "link-unhover"} do
            view:add_signal(signal, update_uri)
        end
        view:add_signal("load-status", function()
            view:emit_signal("link-unhover")
        end)
    end)
end)

log_chrome.widget_format = "{errors}{warnings}"
log_chrome.widget_error_format = "<span color='red'>%d✕</span>"
log_chrome.widget_warning_format = "<span color='orange'>%d⚠</span>"

lousy.widget.tab.label_format = "<span foreground='{index_fg}'>{index}</span>: {title}"

function window.methods.update_win_title(win)
    win.win.title = ((win.view.title or "") == "" and "" or win.view.title.." - ").."luakit"
end

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
            win.sbar.ebox.bg = theme.private_sbar_bg
            win.sbar.l.ebox.bg = theme.private_sbar_bg
            win.sbar.sep.bg = theme.private_sbar_bg
            win.sbar.r.ebox.bg = theme.private_sbar_bg
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

-- Dark mode {{{1

local dark_style = stylesheet{
    source = [[
        :root:not(.luakit-already-dark), iframe, frame {
            filter: invert(1) hue-rotate(180deg);
        }
    ]],
}

webview.add_signal("init", function(view)
    view.stylesheets[dark_style] = true
end)

local check_dark_wm = require_web_module("check_dark_wm")

modes.add_binds("normal", {
    {"cc", "Use normal colors in the current tab.", function(win)
        win.view.stylesheets[dark_style] = false
    end},
    {"cd", "Use dark colors by inverting light pages.", function(win)
        win.view.stylesheets[dark_style] = true
        check_dark_wm:emit_signal(win.view, "check")
    end},
    {"cf", "Force all pages to be inverted.", function(win)
        win.view.stylesheets[dark_style] = true
        check_dark_wm:emit_signal(win.view, "ignore")
    end},
})

-- Signals {{{1

local preserve_uri = false
webview.add_signal("init", function(view)
    if preserve_uri then
        preserve_uri = false
    else
        view.uri = settings.window.new_tab_page
    end

    view:remove_signals("create-web-view")
    view:add_signal("create-web-view", function()
        preserve_uri = true
        return webview.window(view):new_tab()
    end)

    view:add_signal("new-window-decision", function(_, uri)
        view.uri = uri
        return false
    end)

    view:add_signal("navigation-request", function(_, uri)
        if uri:match("^https://[^/]+%.m%.wikipedia%.org/") then
            view.uri = uri:gsub("^https://([^/]+)%.m%.wikipedia%.org/", "https://%1.wikipedia.org/")
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

webview.add_signal("init", function(view)
    view:add_signal("navigation-request", function(_, uri)
        if uri:match("^magnet:") then
            add_torrent(uri, webview.window(view))
            return false
        end
    end)
end)

local close_tab = window.methods.close_tab
function window.methods.close_tab(win, view, ...)
    if view and view.uri == "about:blank" and #view.history.items == 1 and win.tabs:count() == 1 then
        view.uri = settings.window.new_tab_page
    else
        close_tab(win, view, ...)
    end
end

-- Videos {{{1

local function play_video(uris, referrer, win)
    if #uris == 0 then
        win:error("Could not play video")
        return
    end
    local uri = table.remove(uris, 1)
    luakit.spawn(string.format("mpv --referrer %q -- %q", referrer, uri), function(_, status)
        if status ~= 0 then
            play_video(uris, referrer, win)
        end
    end)
end

modes.add_binds("ex-follow", {
    {"v", "Hint all videos and play it with `mpv`.", function(win)
        win:set_mode("follow", {
            prompt = "mpv",
            selector = "video",
            evaluator = function(element)
                local uris = {element.attr.src}
                for _, source in ipairs(element:query("source")) do
                    table.insert(uris, source.attr.src)
                end
                element:remove()
                return uris
            end,
            func = function(uris)
                table.insert(uris, win.view.uri)
                play_video(uris, win.view.uri, win)
            end,
        })
    end},
})
follow.selectors.video = "video"

-- Miscellaneous {{{1

function select.label_maker()
    return trim(sort(reverse(charset("asdfghjkl"))))
end
follow.pattern_maker = follow.pattern_styles.match_label

function lousy.uri.split(s)
    return {s}
end

local is_uri = lousy.uri.is_uri
function lousy.uri.is_uri(s)
    return s:match("[%./]") and os.exists(s) or is_uri(s)
end

local search_open = window.methods.search_open
function window.methods.search_open(...)
    local uri = search_open(...)
    if uri:match("^%a[%a%d+%-.]*:") or os.exists(uri) then
        return uri
    else
        return "https://"..uri
    end
end

local function pcal_eval_js_callback(view)
    local index = getmetatable(view).__index
    getmetatable(view).__index = function(view, key)
        local value = index(view, key)
        if key == "eval_js" then
            return function(view, js, opts)
                if opts.callback then
                    local cb = opts.callback
                    function opts.callback(...)
                        pcall(cb, ...)
                    end
                end
                value(view, js, opts)
            end
        end
        return value
    end
    webview.remove_signal("init", pcal_eval_js_callback)
end
webview.add_signal("init", pcal_eval_js_callback)

-- Initialization {{{1

luakit.spawn(string.format("%q/update-adblock.sh %q", luakit.config_dir, luakit.data_dir), function()
    adblock.load(true)
end)

if pcall(function() lousy.util.find_config("userconf.lua") end) then
    require("userconf")
end

local win = not luakit.nounique and session.restore()
if win then
    for i, uri in ipairs(uris) do
        win:new_tab(uri, {switch = i == 1})
    end
else
    window.new(uris)
end

-- vim: foldmethod=marker foldcolumn=2
