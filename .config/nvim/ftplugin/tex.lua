require("vimtex_config").init_buffer()

vim.keymap.set("i", "<C-b>", "\\begin{", {buf = 0})
vim.keymap.set("i", "<C-z>", "<Plug>(vimtex-delim-close)", {buf = 0})
vim.keymap.set("i", "<C-/>", "\\frac", {buf = 0})

vim.bo.formatexpr = [[nvim_cmd(#{cmd: 'Align', args: ['&\|\\\\'], range: [v:lnum, v:lnum + v:count - 1]}, {})]]

vim.wo[0][0].spell = true
