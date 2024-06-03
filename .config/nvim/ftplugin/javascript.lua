if vim.fn.executable("typescript-language-server") ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
    local root_dir = vim.fs.root(0, {"jsconfig.json", "tsconfig.json"})
        or vim.fs.root(0, "package.json")
        or vim.fs.root(0, ".git")

    local cache_dir = vim.fs.normalize("~/.cache/typescript")
    vim.fn.mkdir(cache_dir, "p")

    vim.lsp.start({
        name = "typescript-language-server",
        cmd = vim.iter({
            "sandbox",
            "-s", "pid", -- Share pid namespace, because it periodically checks that client is running
            root_dir and {"-w", root_dir} or {},
            "-n", "-w", cache_dir, -- Allow downloading type definitions
            "typescript-language-server", "--stdio"
        }):flatten():totable(),
        root_dir = root_dir,
        capabilities = lsp_client_capabilities,

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
        settings = {implicitProjectConfiguration = {checkJs = true}},
    })
end
