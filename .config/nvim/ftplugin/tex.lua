require("vimtex_config").init_buffer()

require("cmp").setup.buffer({
    sources = {
        {name = "omni", trigger_characters = {"\\", "{"}},
        {name = "buffer", group_index = 1, option = {get_bufnrs = get_listed_bufnrs}},
    },
})

vim.wo[0][0].spell = true

vim.cmd.AddTabularPattern({"tex", [[/&\|\\\\/]], bang = true})
vim.keymap.set("n", "grq", "vie:Tabularize tex<CR>", {remap = true, buffer = true, silent = true})
