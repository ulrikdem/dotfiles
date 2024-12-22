local root_dir = find_root(
    {"pyrightconfig.json", "pyproject.toml", ".venv"},
    {"setup.cfg", "setup.py", "requirements.txt"},
    ".git")

local venv = vim.env.VIRTUAL_ENV
    -- Use the real path if .venv is a symlink, because venvs are not portable
    or root_dir and vim.uv.fs_realpath(root_dir .. "/.venv")

start_lsp({
    cmd = {"basedpyright-langserver", "--stdio"},
    root_dir = root_dir,
    sandbox = {
        read = {root_dir, venv},
        args = venv and {"-e", "PATH=" .. venv .. "/bin:" .. vim.env.PATH},
    },

    -- https://docs.basedpyright.com/latest/configuration/language-server-settings/
    settings = { -- This table can't be empty, otherwise pyright only works without root_dir
        basedpyright = {
            analysis = {
                typeCheckingMode = "standard",
            },
        },
    },

    on_attach = function(client, bufnr)
        nvim_buf_create_user_command(bufnr, "OrganizeImports", function()
            client.request(vim.lsp.protocol.Methods.workspace_executeCommand, {
                command = "basedpyright.organizeimports",
                arguments = {vim.uri_from_bufnr(bufnr)},
            })
        end, {})
    end,
    on_detach = function(_, bufnr)
        nvim_buf_del_user_command(bufnr, "OrganizeImports")
    end,
})
