-- Imports {{{
local adblock = require("adblock")
local cmdhist = require("cmdhist")
local downloads = require("downloads")
local editor = require("editor")
local follow = require("follow")
local history = require("history")
local log_chrome = require("log_chrome")
local lousy = require("lousy")
local modes = require("modes")
local newtab_chrome = require("newtab_chrome")
local select = require("select")
local settings = require("settings")
local styles = require("styles")
local tablist = require("lousy.widget.tablist")
local unique_instance = require("unique_instance")
local webview = require("webview")
local window = require("window")
-- }}}

-- Settings {{{
settings.window.home_page = settings.window.new_tab_page
settings.window.search_engines = {[""] = "https://duckduckgo.com/?q=%s"}
settings.window.default_search_engine = ""
settings.window.check_filepath = false
settings.completion.history.order = "last_visit"

settings.window.max_title_len = math.huge
settings.tablist.always_visible = true
settings.window.close_with_last_tab = true
unique_instance.open_links_in_new_window = true

lousy.widget.hist.back_indicator = "&lt;"
lousy.widget.hist.forward_indicator = "&gt;"
log_chrome.widget_format = ""

follow.pattern_maker = follow.pattern_styles.match_label
select.label_maker = function()
    return trim(sort(reverse(charset("asdfghjkl"))))
end

settings.webview.enable_plugins = false
settings.webview.enable_java = false

settings.webview.hardware_acceleration_policy = "always"
settings.webview.enable_accelerated_2d_canvas = true
settings.webview.enable_webgl = true
settings.webview.enable_webaudio = true
settings.webview.enable_mediasource = true
-- }}}

-- Bindings {{{
modes.remap_binds({"all", "passthrough"}, {
    {"<control-[>", "<Escape>", true},
})
modes.remove_binds("passthrough", {"<Escape>"})

modes.add_binds("normal", {
    {"v", "Open one or more URLs in a new private window.", function(win)
        win:enter_cmd(":priv-tabopen ")
    end},
    {"V", "Open one or more URLs based on current location in a new private window.", function(win)
        win:enter_cmd(":priv-tabopen "..(win.view.uri or ""))
    end},
    {"gs", "Change protocol to HTTPS.", function(win)
        win.view.uri = win.view.uri:gsub("^http:", "https:")
    end},
    {"<control-C>", "Copy the selected text.", function()
        luakit.selection.clipboard = luakit.selection.primary
    end},
})

modes.add_binds({"normal", "insert"}, {
    {"<control-q>", "Send the next keypress directly to the webpage.", function(win)
        function win.hit(_, mods, key)
            win.hit = nil
            win.view:send_key(key, mods)
            return true
        end
    end},
})
settings.window.act_on_synthetic_keys = true

editor.editor_cmd = "termite -e 'nvim {file} +{line}'"
modes.remap_binds("insert", {
    {"<mod1-e>", "<control-e>"},
})

cmdhist.history_prev = "<control-p>"
cmdhist.history_next = "<control-n>"
-- }}}

-- Colors {{{
local theme = lousy.theme.get()
theme.ok = {fg = "white", bg = "gray"}
theme.notif_bg = theme.ok.bg
theme.warning_bg = theme.ok.bg
theme.menu_selected_bg = theme.ok.bg

for _, key in ipairs{
    "notif_fg", "ibar_fg",
    "menu_title_bg", "menu_fg", "menu_bg", "menu_selected_fg",
    "menu_active_bg", "menu_enabled_fg", "menu_enabled_bg", "menu_disabled_bg",
    "proxy_active_menu_fg", "proxy_active_menu_bg", "proxy_inactive_menu_bg",
} do
    theme[key] = nil
end

newtab_chrome.new_tab_src = "<title>New Tab</title>"

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
-- }}}

-- Signals {{{
webview.add_signal("init", function(view)
    view.uri = settings.window.new_tab_page

    view:add_signal("new-window-decision", function(_, uri)
        view.uri = uri
        return false
    end)

    view:add_signal("navigation-request", function(_, uri)
        if uri:match("^https://.+%.m%.wikipedia%.org/") then
            view.uri = uri:gsub("^https://(.+)%.m%.wikipedia%.org/", "https://%1.wikipedia.org/")
            return false
        elseif uri:match("^mailto:") then
            luakit.spawn(string.format("termite -e 'alot compose %q'", uri))
            return false
        end
        view.default_charset = uri:match("^file:") and "utf-8" or settings.webview.default_charset
    end)
end)

history.add_signal("add", function(uri)
    if uri:match("^file:") then
        return false
    end
end)
-- }}}

-- Downloads {{{
local add_download = downloads.add
function downloads.add(uri, opts)
    local dl = type(uri) == "string" and download{uri = uri} or uri
    add_download(dl, opts)
    dl:remove_signals("finished")
end

function luakit.save_file(title, win, dir, file)
    local cmd = string.format("%q %q", luakit.config_dir.."/download-prompt.sh", file)
    local status, file = luakit.spawn_sync(cmd)
    return status == 0 and file
end

downloads.remove_signals("download-location")
downloads.remove_signals("download::status")
downloads.remove_signals("open-file")

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
    if mime == "application/x-bittorrent" then
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
-- }}}

-- Videos {{{
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

follow.selectors.video = "video"

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
-- }}}

-- Adblock {{{
luakit.spawn(luakit.data_dir.."/adblock/update.sh", function()
    adblock.load(true)
end)
-- }}}

-- vim: foldmethod=marker foldcolumn=1
