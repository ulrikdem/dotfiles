local ui = ipc_channel("redirect_wm")

local _, reverse_redirects = xpcall(function()
    return require("redirects")
end, function()
    return {}
end)

local redirects = {}
for target, sources in pairs(reverse_redirects) do
    for _, source in ipairs(sources) do
        redirects[source] = {target}
    end
end

local keep_host = setmetatable({}, {__mode = "k"})
local reverse_host = setmetatable({}, {__mode = "k"})
local keep_scheme = setmetatable({}, {__mode = "k"})
local reverse_scheme = setmetatable({}, {__mode = "k"})

luakit.add_signal("page-created", function(page)
    page:add_signal("send-request", function(_, old_uri)
        local uri = soup.parse_uri(old_uri)
        local changed = false

        if not keep_host[page] or reverse_host[page] then
            local redirects = reverse_host[page] and reverse_redirects or redirects
            local host = uri.host
            while host and host:match("%.") do
                if redirects[host] then
                    uri.host = redirects[host][1]
                    changed = true
                    break
                end
                host = host:match("%.(.+)")
            end
            reverse_host[page] = false
        end

        if not keep_scheme[page] and uri.scheme == "http" and uri.port == 80 then
            uri.scheme = "https"
            uri.port = 443
            changed = true
        elseif reverse_scheme[page] and uri.scheme == "https" and uri.port == 443 then
            uri.scheme = "http"
            uri.port = 80
            changed = true
            reverse_scheme[page] = false
        end

        if changed then
            uri = soup.uri_tostring(uri)
            msg.info("Redirecting %s to %s", old_uri, uri)
            return uri
        end
    end)
end)

ui:add_signal("toggle_host", function(_, page)
    keep_host[page] = not keep_host[page]
    reverse_host[page] = keep_host[page]
end)

ui:add_signal("toggle_scheme", function(_, page)
    keep_scheme[page] = not keep_scheme[page]
    reverse_scheme[page] = keep_scheme[page]
end)
