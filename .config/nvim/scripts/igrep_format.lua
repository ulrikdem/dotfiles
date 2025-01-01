local quickfix_utils = require("quickfix_utils")

for line in io.lines() do
    local item = quickfix_utils.from_ripgrep(line)
    if item then
        io.write(line .. "\t" .. quickfix_utils.to_fzf(item, tonumber(arg[1]), true) .. "\n")
    end
end
