setmetatable(getfenv(), {__index = vim})
nvim = setmetatable({}, {__index = function(_, key)
    return api["nvim_" .. key]
end})

cmd.runtime("old_init.vim")
