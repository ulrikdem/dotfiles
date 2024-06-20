local root_dir = find_root(
    {"pyrightconfig.json", "pyproject.toml"},
    {"setup.cfg", "setup.py", "requirements.txt"},
    ".git")

start_lsp({
    cmd = {"pyright-langserver", "--stdio"},
    root_dir = root_dir,
    sandbox = {read = {root_dir, vim.env.VIRTUAL_ENV}},

    -- https://microsoft.github.io/pyright/#/settings
    settings = { -- This table can't be empty, otherwise pyright only works without root_dir
        python = {
            analysis = {diagnosticMode = "openFilesOnly"},
        },
    },
})
