--- @type repl_config
vim.b.repl = {
    cmd = {"bash"},
    load_file = function(path)
        return "source " .. vim.fn.shellescape(path)
    end,
}
