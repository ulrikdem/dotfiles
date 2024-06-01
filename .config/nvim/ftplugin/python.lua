if vim.fn.executable("pyright-langserver") ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
    local root_dir = vim.fs.root(0, {"pyrightconfig.json", "pyproject.toml", ".git"})
    vim.lsp.start({
        name = "pyright",
        cmd = vim.iter({
            -- Share pid namespace, because it seems to periodically check that client is running
            {"sandbox", "-s", "pid"},
            root_dir and {"-r", root_dir} or {},
            vim.env.VIRTUAL_ENV and {"-r", vim.env.VIRTUAL_ENV} or {},
            {"pyright-langserver", "--stdio"}
        }):flatten():totable(),
        root_dir = root_dir,
        capabilities = lsp_client_capabilities,
        -- https://microsoft.github.io/pyright/#/settings
        settings = {},
    })
end
