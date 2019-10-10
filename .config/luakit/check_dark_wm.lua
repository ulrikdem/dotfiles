local ui = ipc_channel("check_dark_wm")

local ignored = setmetatable({}, {__mode = "k"})

local function check(page)
    if ignored[page] then
        return
    end
    page:eval_js([[
        {
            const get_color = element => {
                const color = getComputedStyle(element).backgroundColor
                const rgba = color.split(/[(,]/).slice(1).map(parseFloat).concat([1])
                return rgba[3] && rgba.slice(0, 3).map(x => x / 255 * rgba[3] + (1 - rgba[3]))
            }

            const color = get_color(document.documentElement)
                || document.body && get_color(document.body)
                || [1, 1, 1]

            const matrix = [
                [-0.574, 1.43, 0.144],
                [0.426, 0.43, 0.144],
                [0.426, 1.43, -0.856],
            ]

            const sum = a => a.reduce((x, y) => x + y)

            const inverted = matrix
                .map(row => sum([0, 1, 2].map(i => row[i] * color[i])))
                .map(x => Math.min(Math.max(1 - x, 0), 1))

            document.documentElement.classList.toggle('luakit-already-dark', sum(inverted) >= sum(color))
        }
    ]])
end

luakit.add_signal("page-created", function(page)
    page:add_signal("document-loaded", check)
end)

ui:add_signal("check", function(_, page)
    ignored[page] = nil
    check(page)
end)

ui:add_signal("ignore", function(_, page)
    ignored[page] = true
    page:eval_js("document.documentElement.classList.remove('luakit-already-dark')")
end)
