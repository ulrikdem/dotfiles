local buf_path = nvim_buf_get_name(0)
local runtime = vim.tbl_map(vim.uv.fs_realpath, nvim_get_runtime_file("", true))
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
    hint = {
        enable = true,
        setType = true,
        arrayIndex = "Disable",
    },
}

local read_paths
if in_runtime then
    if not api_lua_path then
        _G.api_lua_path = vim.fn.tempname() .. ".lua"
        local lines = {"--- @meta"}
        for k, _ in pairs(vim.api) do
            table.insert(lines, ("_G.%s = vim.api.%s"):format(k, k))
        end
        vim.fn.writefile(lines, api_lua_path)
    end
    table.insert(runtime, api_lua_path)

    read_paths = runtime
    settings = vim.tbl_deep_extend("force", settings, {
        workspace = {
            library = runtime,
            -- Plugins are library files (configured above), not part of the main workspace
            ignoreDir = {"pack"},
        },
        runtime = {
            path = {"lua/?.lua", "lua/?/init.lua"},
            pathStrict = true,
            version = "LuaJIT",
        },
        diagnostics = {
            disable = {
                "duplicate-set-field",
                "redefined-local",
                "unused-function",
                "empty-block",
                "lowercase-global",
            },
            neededFileStatus = {
                ["global-element"] = "Any",
            },
        },
    })

    -- Allow opening help with K in visual mode
    vim.bo.keywordprg = ":help"

    _G.lua_repl_env = lua_repl_env or setmetatable({}, {__index = _G})
    --- @type repl_config
    vim.b.repl = {
        eval = function(code)
            local func, err
            -- Attempt to parse as an expression, falling back to a chunk
            -- As far as I know the only ambiguous case is function calls, where we want to print the return values
            for _, code in ipairs({"return " .. code, code}) do
                func, err = loadstring(code)
                if func then
                    debug.sethook(function(event)
                        -- Add top-level locals to lua_repl_env. Doesn't work when func has a tail call
                        if event == "return" and debug.getinfo(2, "f").func == func then
                            local i, name, value = 1, debug.getlocal(2, 1)
                            while name do
                                if not vim.startswith(name, "(") then lua_repl_env[name] = value end
                                i = i + 1
                                name, value = debug.getlocal(2, i)
                            end
                        end
                    end, "r");
                    (function(ok, ...)
                        if ok then
                            local results = {}
                            for i = 1, select("#", ...) do
                                table.insert(results, vim.inspect(select(i, ...), nil))
                            end
                            vim.notify(table.concat(results, "\n"))
                        else
                            err = select(1, ...)
                        end
                    end)(pcall(setfenv(func, lua_repl_env)))
                    debug.sethook()
                    break
                end
            end
            if err then vim.notify(err, vim.log.levels.ERROR) end
        end,
    }
    -- This is different from :lua and := in that it can access REPL locals
    nvim_buf_create_user_command(0, "Lua", function(opts)
        vim.b.repl.eval(opts.args)
    end, {nargs = 1, complete = "lua"})
else
    read_paths = {find_root({".luarc.json", ".luarc.jsonc"}, ".git")}
    vim.b.repl = {cmd = "lua"} --- @type repl_config
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
