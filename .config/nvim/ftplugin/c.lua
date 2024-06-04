-- This file is also sourced from the built-in cpp ftplugin

local root_dir = vim.fs.root(0, {"compile_commands.json", ".ccls"}) or vim.fs.root(0, ".git")

start_lsp({
    name = "ccls",
    cmd = vim.iter({
        "sandbox",
        root_dir and {"-w", root_dir} or {},
        "ccls",
    }):flatten():totable(),
    -- Does not accept nil root_dir. Use filetype to enable separate servers with different clang args
    root_dir = root_dir or ("/" .. vim.bo.filetype),
    offset_encoding = "utf-32",

    -- https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
    init_options = vim.tbl_deep_extend("force", {
        completion = {detailedLabel = false},
    }, root_dir and {} or {
        cache = {directory = ""},
        index = {onChange = true},
        clang = {
            extraArgs = {
                vim.bo.filetype == "c" and "-std=c17" or "-std=c++20",
                "-Wall", "-Wextra", "-Wconversion", "-Wno-sign-conversion",
            },
        },
    }),

    on_attach = function(_, bufnr)
        local cmp = require("cmp")
        for _, c in ipairs({">", '"', "/"}) do
            vim.keymap.set("i", c, function()
                if cmp.visible() and vim.api.nvim_get_current_line():sub(1, vim.api.nvim_win_get_cursor(0)[2]):match(c .. "$") then
                    return "<C-y>"
                else
                    return c
                end
            end, {expr = true, remap = true, buffer = bufnr})
        end
    end,
})
