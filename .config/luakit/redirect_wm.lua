local ui = ipc_channel("redirect_wm")

local success, reverse_redirects = pcall(function()
    return require("redirects")
end)
if not success then
    return
end

local redirects = {}
for target, sources in pairs(reverse_redirects) do
    for _, source in ipairs(sources) do
        redirects[source] = {target}
    end
end

local reversed = setmetatable({}, {__mode = "k"})

luakit.add_signal("page-created", function(page)
    page:add_signal("send-request", function(_, uri)
        local redirects = reversed[page] and reverse_redirects or redirects
        uri = soup.parse_uri(uri)
        while uri.host and uri.host:match("%.") do
            if redirects[uri.host] then
                uri.host = redirects[uri.host][1]
                return soup.uri_tostring(uri)
            end
            uri.host = uri.host:match("%.(.+)")
        end
    end)
end)

ui:add_signal("reverse", function(_, page)
    reversed[page] = not reversed[page]
end)
