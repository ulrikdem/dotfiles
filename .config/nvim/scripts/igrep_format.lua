local quickfix = require("quickfix")

for line in io.lines() do
    local item = quickfix.from_ripgrep(line)
    if item then
        io.write(line .. "\t" .. quickfix.to_fzf(item, tonumber(arg[1]), true) .. "\n")
    end
end
