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

    convert_completion = function(item)
        local use_decl = vim.tbl_get(item, "labelDetails", "detail")
        local doc = vim.tbl_get(item, "documentation", "value") or ""
        return {
            abbr = item.label,
            menu = "",
            info = (use_decl or item.detail)
                and ("```rust\n%s\n```\n%s"):format(vim.iter({use_decl, item.detail}):join("\n"), doc)
                or doc,
        }
    end,
})
