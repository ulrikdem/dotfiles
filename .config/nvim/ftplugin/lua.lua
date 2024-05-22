bo.keywordprg = ":help"
bo.omnifunc = "v:lua.lua_omnifunc"

require("cmp").setup.buffer({
    sources = {
        {name = "omni", trigger_characters = {"."}},
        {name = "path"},
        {name = "buffer", group_index = 1, option = {get_bufnrs = nvim.list_bufs}},
    },
})
