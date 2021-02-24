local win = widget{type = "window"}
local view = widget{type = "webview", private = true}

view:add_signal("property::title", function()
    win.title = view.title
end)

view.uri = uris[1]
win.child = view
win:show()
