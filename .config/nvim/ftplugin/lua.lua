vim.bo.keywordprg = ":help"
vim.bo.omnifunc = "v:lua.vim.lua_omnifunc"

require("cmp").setup.buffer({
    sources = {
        {name = "omni", trigger_characters = {"."}},
        {name = "path"},
        {name = "buffer", group_index = 1, option = {get_bufnrs = vim.api.nvim_list_bufs}},
    },
})
