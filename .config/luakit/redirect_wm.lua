local ui = ipc_channel("redirect_wm")

local success, reverse_redirects = pcall(function()
    return require("redirects")
end)
if not success then
    reverse_redirects = {}
end

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
    page:add_signal("send-request", function(_, uri)
        uri = soup.parse_uri(uri)

        if not keep_host[page] or reverse_host[page] then
            local redirects = reverse_host[page] and reverse_redirects or redirects
            local host = uri.host
            while host and host:match("%.") do
                if redirects[host] then
                    uri.host = redirects[host][1]
                    break
                end
                host = host:match("%.(.+)")
            end
            reverse_host[page] = false
        end

        if not keep_scheme[page] and uri.scheme == "http" and uri.port == 80 then
            uri.scheme = "https"
            uri.port = 443
        elseif reverse_scheme[page] and uri.scheme == "https" and uri.port == 443 then
            uri.scheme = "http"
            uri.port = 80
            reverse_scheme[page] = false
        end

        return soup.uri_tostring(uri)
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
