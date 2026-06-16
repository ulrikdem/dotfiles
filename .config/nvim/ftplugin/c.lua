-- This file is also sourced from the built-in cpp ftplugin

for key, ext in pairs({h = "h", H = "hpp", c = "c", C = "cpp"}) do
    vim.keymap.set("n", "<Leader>o" .. key, "<Cmd>edit %:r." .. ext .. "<CR>", {buf = 0})
end

local root_dir = find_root({"compile_commands.json", ".ccls"}, ".git")

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
    }, root_dir and {
        compilationDatabaseDirectory = vim.uv.fs_stat(root_dir .. "/build/compile_commands.json") and "build",
    } or {
        cache = {directory = ""},
        index = {onChange = true},
        clang = {
            extraArgs = {
                vim.o.filetype == "c" and "-std=c17" or "-std=c++20",
                "-Wall", "-Wextra", "-Wconversion", "-Wno-sign-conversion",
            },
        },
    }),

    convert_completion = function(item)
        return {
            word = item.textEdit.newText:gsub('[>"/]$', ""):gsub(" $", ""),
            abbr = item.label:gsub('[>"]$', ""),
            menu = "",
            info = ("```%s\n%s\n```\n%s"):format(vim.o.filetype, item.detail, item.documentation or ""),
        }
    end,
})
