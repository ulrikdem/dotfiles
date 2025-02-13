local root_dir = find_root({"jsconfig.json", "tsconfig.json"}, "package.json", ".git")

start_lsp({
    cmd = {"typescript-language-server", "--stdio"},
    root_dir = root_dir,
    sandbox = {
        read = {root_dir},
        -- Allow downloading type definitions to cache
        args = {"-n"},
        write = {vim.fs.normalize("~/.cache/typescript")},
    },

    -- https://github.com/typescript-language-server/typescript-language-server/blob/master/docs/configuration.md
    init_options = {
        preferences = {
            includeInlayEnumMemberValueHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayParameterNameHints = "all",
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayVariableTypeHints = true,
        },
    },
    settings = {
        implicitProjectConfiguration = {checkJs = true},
    },
})

--- @type repl_config
vim.b.repl = {
    cmd = {"node"},
    cwd = root_dir,
    load_file = function(path)
        return ".load " .. path
    end,
    format = function(code)
        return code:find("\n") and ".editor\n" .. code .. "\4" or code .. "\n"
    end,
}
