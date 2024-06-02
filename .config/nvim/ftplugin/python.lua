if vim.fn.executable("pyright-langserver") ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
    local root_dir = vim.fs.root(0, {"pyrightconfig.json", "pyproject.toml"})
        or vim.fs.root(0, {"setup.cfg", "setup.py", "requirements.txt"})
        or vim.fs.root(0, ".git")

    vim.lsp.start({
        name = "pyright",
        cmd = vim.iter({
            {"sandbox", "-s", "pid"}, -- Share pid namespace, because it periodically checks that client is running
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
