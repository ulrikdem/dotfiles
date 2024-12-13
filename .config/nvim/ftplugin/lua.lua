local buf_path = vim.api.nvim_buf_get_name(0)
local runtime = vim.tbl_map(vim.uv.fs_realpath, vim.api.nvim_get_runtime_file("", true))
local in_runtime = vim.iter(runtime):any(function(path)
    return vim.startswith(buf_path, path .. "/")
end)

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

local read_paths
if in_runtime then
    read_paths = runtime
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
    -- Allow opening help with K in visual mode
    vim.bo.keywordprg = ":help"
else
    read_paths = {find_root({".luarc.json", ".luarc.jsonc"}, ".git")}
end

start_lsp({
    cmd = {"lua-language-server"},
    root_dir = read_paths[1],
    sandbox = {read = read_paths},
    settings = {Lua = settings},
    on_attach = function(_, bufnr)
        -- This is not set by default because we set keywordprg above
        vim.keymap.set("n", "K", vim.lsp.buf.hover, {buffer = bufnr})
    end,
})
