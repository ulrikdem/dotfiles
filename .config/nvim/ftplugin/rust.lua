local root = fs.root(0, "Cargo.toml")
if root then
    lsp.start{
        name = "rust-analyzer",
        cmd = {"rust-analyzer"},
        capabilities = lsp_client_capabilities,
        root_dir = root,
    }
end
