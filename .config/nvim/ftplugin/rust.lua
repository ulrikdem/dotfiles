local root_dir = find_root({"Cargo.toml", "rust-project.json"})
if not root_dir then return end

start_lsp({
    cmd = {"rust-analyzer"},
    root_dir = root_dir,
    sandbox = {
        args = {"-n"},
        write = {root_dir, vim.fs.normalize("~/.cargo")},
    },
})
