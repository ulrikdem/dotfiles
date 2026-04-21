local root_dir = find_root({"Cargo.toml", "rust-project.json"})
if not root_dir then return end

vim.keymap.set("n", "<Leader>mm", "<Cmd>silent update | Make build<CR>", {buf = 0})
vim.keymap.set("n", "<Leader>mr", "<Cmd>silent update | Make build --release<CR>", {buf = 0})

start_lsp({
    cmd = {"rust-analyzer"},
    root_dir = root_dir,
    sandbox = {
        args = {"-n"},
        write = {root_dir, vim.fs.normalize("~/.cargo")},
    },
})
