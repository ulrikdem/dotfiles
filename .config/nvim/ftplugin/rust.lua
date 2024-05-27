local root = vim.fs.root(0, "Cargo.toml")
if root then
    vim.lsp.start({
        name = "rust-analyzer",
        cmd = {"rust-analyzer"},
        capabilities = lsp_client_capabilities,
        root_dir = root,
    })
end
