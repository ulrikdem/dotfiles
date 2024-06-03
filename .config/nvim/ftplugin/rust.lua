local root_dir = vim.fs.root(0, "Cargo.toml")
if root_dir and vim.fn.executable("rust-analyzer") ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
    local cargo_dir = vim.fs.normalize("~/.cargo")
    vim.fn.mkdir(cargo_dir, "p")

    vim.lsp.start({
        name = "rust-analyzer",
        cmd = {
            "sandbox",
            "-w", root_dir,
            "-n", "-w", cargo_dir, -- Allow downloading dependencies
            "rust-analyzer",
        },
        root_dir = root_dir,
        capabilities = lsp_client_capabilities,
    })
end
