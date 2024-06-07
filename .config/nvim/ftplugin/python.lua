local root_dir = find_root(
    {"pyrightconfig.json", "pyproject.toml"},
    {"setup.cfg", "setup.py", "requirements.txt"},
    ".git")

start_lsp({
    cmd = {"pyright-langserver", "--stdio"},
    root_dir = root_dir,
    sandbox = {read = {root_dir, vim.env.VIRTUAL_ENV}},

    -- https://microsoft.github.io/pyright/#/settings
    settings = {
        -- Set something here so that didChangeConfiguration is sent, otherwise pyright only works without root_dir
        python = vim.empty_dict(),
    },
})
