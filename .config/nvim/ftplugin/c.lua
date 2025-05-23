-- This file is also sourced from the built-in cpp ftplugin

for key, ext in pairs({h = "h", H = "hpp", c = "c", C = "cpp"}) do
    vim.keymap.set("n", "<Leader>o" .. key, "<Cmd>edit %:r." .. ext .. "<CR>", {buffer = true})
end

local root_dir = find_root({"compile_commands.json", ".ccls"}, ".git")
local completion_end_symbols = {">", '"', "/"}

start_lsp({
    cmd = {"ccls"},
    -- Does not accept nil root_dir. Use filetype to enable separate servers with different clang args
    root_dir = root_dir or ("/" .. vim.o.filetype),
    sandbox = {
        read = {root_dir},
        write = {root_dir and root_dir .. "/.ccls-cache"},
    },
    offset_encoding = "utf-32",

    -- https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
    init_options = vim.tbl_deep_extend("force", {
        completion = {detailedLabel = false},
    }, root_dir and {} or {
        cache = {directory = ""},
        index = {onChange = true},
        clang = {
            extraArgs = {
                vim.o.filetype == "c" and "-std=c17" or "-std=c++20",
                "-Wall", "-Wextra", "-Wconversion", "-Wno-sign-conversion",
            },
        },
    }),

    on_attach = function(_, bufnr)
        local cmp = require("cmp")
        for _, s in ipairs(completion_end_symbols) do
            vim.keymap.set("i", s, function()
                local col = nvim_win_get_cursor(0)[2]
                if cmp.get_active_entry() and nvim_get_current_line():sub(col, col) == s then
                    return "<C-y>"
                else
                    return s
                end
            end, {expr = true, remap = true, buffer = bufnr})
        end
    end,
    on_detach = function(_, bufnr)
        for _, s in ipairs(completion_end_symbols) do
            vim.keymap.del("i", s, {buffer = bufnr})
        end
    end,
})
