local root_dir = vim.fs.root(0, "Cargo.toml")
if root_dir and vim.fn.executable("rust-analyzer") ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
    vim.lsp.start({
        name = "rust-analyzer",
        -- We could remove -n and use -r for .cargo, but would need to run cargo fetch (or build) first
        cmd = {"sandbox", "-n", "-w", root_dir, "-w", vim.fs.normalize("~/.cargo"), "rust-analyzer"},
        root_dir = root_dir,
        capabilities = lsp_client_capabilities,
    })
end
