--- @type repl_config
vim.b.repl = {
    cmd = {"zsh"},
    load_file = function(path)
        return "source " .. vim.fn.shellescape(path)
    end,
}
