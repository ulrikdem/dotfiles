require("vimtex_config").init_buffer()

local cmp = require("cmp")
cmp.setup.buffer({
    sources = {
        {name = "omni", trigger_characters = {"\\", "{"}},
        {name = "buffer", group_index = 1, option = {get_bufnrs = get_listed_bufnrs}},
    },
})

for _, s in ipairs({"}", ","}) do
    vim.keymap.set("i", s, function()
        local col = nvim_win_get_cursor(0)[2]
        if cmp.get_active_entry() and nvim_get_current_line():sub(col, col) == "}" then
            return "<BS>" .. s
        else
            return s
        end
    end, {expr = true, remap = true, buffer = true})
end

vim.keymap.set("i", "<C-b>", "\\begin{", {buffer = true})
vim.keymap.set("i", "<C-z>", "<Plug>(vimtex-delim-close)", {buffer = true})

vim.cmd.AddTabularPattern({"tex", [[/&\|\\\\/]], bang = true})
vim.bo.formatexpr = "nvim_cmd(#{cmd: 'Tabularize', args: ['tex'], range: [v:lnum, v:lnum + v:count - 1]}, {})"

vim.wo[0][0].spell = true
