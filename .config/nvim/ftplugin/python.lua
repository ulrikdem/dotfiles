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

    on_attach = function(client, bufnr)
        vim.api.nvim_buf_create_user_command(bufnr, "OrganizeImports", function()
            client.request(vim.lsp.protocol.Methods.workspace_executeCommand, {
                command = "pyright.organizeimports",
                arguments = {vim.uri_from_bufnr(bufnr)},
            })
        end, {})
    end,
    on_detach = function(_, bufnr)
        vim.api.nvim_buf_del_user_command(bufnr, "OrganizeImports")
    end,
})
