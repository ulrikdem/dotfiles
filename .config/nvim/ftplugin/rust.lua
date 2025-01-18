local root_dir = find_root({"Cargo.toml", "rust-project.json"})
if not root_dir then return end

vim.keymap.set("n", "<Leader>mm", "<Cmd>silent update | Make build<CR>", {buffer = true})
vim.keymap.set("n", "<Leader>mr", "<Cmd>silent update | Make build --release<CR>", {buffer = true})

start_lsp({
    cmd = {"rust-analyzer"},
    root_dir = root_dir,
    sandbox = {
        args = {"-n"},
        write = {root_dir, vim.fs.normalize("~/.cargo")},
    },
})
