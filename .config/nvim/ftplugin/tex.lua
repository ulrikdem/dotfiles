require("vimtex_config").init_buffer()

local cmp = require("cmp")
cmp.setup.buffer({
    sources = {
        {name = "omni", trigger_characters = {"\\", "{"}},
        {name = "buffer", group_index = 1, option = {get_bufnrs = get_listed_bufnrs}},
    },
})

vim.keymap.set("i", "<C-b>", "\\begin{", {buf = 0})
vim.keymap.set("i", "<C-z>", "<Plug>(vimtex-delim-close)", {buf = 0})
vim.keymap.set("i", "<C-/>", "\\frac", {buf = 0})

vim.cmd.AddTabularPattern({"tex", [[/&\|\\\\/]], bang = true})
vim.bo.formatexpr = "nvim_cmd(#{cmd: 'Tabularize', args: ['tex'], range: [v:lnum, v:lnum + v:count - 1]}, {})"

vim.wo[0][0].spell = true
