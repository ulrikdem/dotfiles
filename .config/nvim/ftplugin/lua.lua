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
    end

    vim.lsp.start({
        name = "lua-language-server",
        cmd = {"lua-language-server"},
        capabilities = lsp_client_capabilities,
        root_dir = in_runtime and runtime[1]
            or vim.fs.root(0, {".luarc.json", ".luarc.jsonc"}) or vim.fs.root(0, ".git"),
        settings = {Lua = settings},
        on_attach = function(_, bufnr)
            -- This is not set by default because we set keywordprg above
            vim.keymap.set("n", "K", vim.lsp.buf.hover, {buffer = bufnr})
        end,
    })
end
