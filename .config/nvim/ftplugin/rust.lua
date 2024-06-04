local root_dir = vim.fs.root(0, "Cargo.toml")
if not root_dir then return end

local cargo_dir = vim.fs.normalize("~/.cargo")
vim.fn.mkdir(cargo_dir, "p")

start_lsp({
    name = "rust-analyzer",
    cmd = {
        "sandbox",
        "-w", root_dir,
        "-n", "-w", cargo_dir, -- Allow downloading dependencies
        "rust-analyzer",
    },
    root_dir = root_dir,
})
