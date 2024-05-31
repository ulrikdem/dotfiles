local buf_path = vim.api.nvim_buf_get_name(0)
if buf_path:match("^fugitive:") then
    buf_path = vim.fn.FugitiveReal(buf_path)
end

local runtime = vim.api.nvim_get_runtime_file("", true)
local in_runtime = vim.iter(runtime):any(function(path)
    return vim.startswith(buf_path, vim.fs.normalize(path) .. "/")
end)

if in_runtime then
    -- Even with LSP, this allows opening help with K in visual mode, or
    -- <C-x><C-o> to complete modules the server doesn't know about (e.g. vim.uv)
    vim.bo.keywordprg = ":help"
    vim.bo.omnifunc = "v:lua.vim.lua_omnifunc"
end

if vim.fn.executable("lua-language-server") ~= 0 then
    -- https://luals.github.io/wiki/settings/
    local settings = {
        completion = {
            showParams = false,
            showWord = "Disable",
            keywordSnippet = "Disable",
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
        root_dir = vim.fs.root(0, {".luarc.json", ".luarc.jsonc", ".git"})
    end

    vim.lsp.start({
        name = "lua-language-server",
        cmd = vim.iter({{"sandbox"}, root_dir and {"-r", root_dir} or {}, {"lua-language-server"}}):flatten():totable(),
        root_dir = root_dir,
        capabilities = lsp_client_capabilities,
        settings = {Lua = settings},
        on_attach = function(_, bufnr)
            -- This is not set by default because we set keywordprg above
            vim.keymap.set("n", "K", vim.lsp.buf.hover, {buffer = bufnr})
        end,
    })
end
