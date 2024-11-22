local buf_path = vim.api.nvim_buf_get_name(0)
local runtime = vim.api.nvim_get_runtime_file("", true)
local in_runtime = vim.iter(runtime):any(function(path)
    return vim.startswith(buf_path, vim.fs.normalize(path) .. "/")
end)

if in_runtime then
    -- Allow opening help with K in visual mode
    vim.bo.keywordprg = ":help"
end

-- https://luals.github.io/wiki/settings/
local settings = {
    completion = {
        showParams = false,
        showWord = "Disable",
        keywordSnippet = "Disable",
    },
    diagnostics = {
        workspaceDelay = -1,
    },
    hint = {
        enable = true,
        setType = true,
        arrayIndex = "Disable",
    },
}

local root_dir
if in_runtime then
    settings = vim.tbl_deep_extend("force", settings, {
        workspace = {
            library = runtime,
        },
        runtime = {
            path = {"lua/?.lua", "lua/?/init.lua"},
            pathStrict = true,
            version = "LuaJIT",
        },
        diagnostics = {
            disable = {"duplicate-set-field", "redefined-local"},
        },
    })
    root_dir = runtime[1]
else
    root_dir = find_root({".luarc.json", ".luarc.jsonc"}, ".git")
end

start_lsp({
    cmd = {"lua-language-server"},
    root_dir = root_dir,
    sandbox = {read = {root_dir}},
    settings = {Lua = settings},
    on_attach = function(_, bufnr)
        -- This is not set by default because we set keywordprg above
        vim.keymap.set("n", "K", vim.lsp.buf.hover, {buffer = bufnr})
    end,
})
