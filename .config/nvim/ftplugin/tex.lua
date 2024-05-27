require("cmp").setup.buffer({
    sources = {
        {name = "omni", trigger_characters = {"\\", "{"}},
        {name = "buffer", group_index = 1, option = {get_bufnrs = vim.api.nvim_list_bufs}},
    },
})
