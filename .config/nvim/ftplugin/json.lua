-- https://github.com/microsoft/vscode/tree/main/extensions/json-language-features/server#configuration
local settings = {
    json = {
        validate = {enable = true},
        format = {enable = true},
    },
}

local catalog_path = vim.fn.stdpath("data") .. "/schemastore.json"
local function read_catalog()
    settings.json.schemas = vim.json.decode(table.concat(vim.fn.readfile(catalog_path))).schemas
end
pcall(read_catalog)

start_lsp({
    cmd = {"vscode-json-language-server", "--stdio"},
    sandbox = {args = {"-n"}},
    settings = settings,
    capabilities = {textDocument = {completion = {completionItem = {snippetSupport = true}}}},

    on_attach = function(client, bufnr)
        nvim_buf_create_user_command(bufnr, "UpdateSchemaStore", function()
            vim.fn.mkdir(vim.fs.dirname(catalog_path), "p")
            vim.system({"wget", "-nv", "-O", catalog_path, "https://www.schemastore.org/api/json/catalog.json"}, {},
                vim.schedule_wrap(function(result) --- @param result vim.SystemCompleted
                    if result.code == 0 then
                        read_catalog()
                        client.notify(vim.lsp.protocol.Methods.workspace_didChangeConfiguration, {settings = settings})
                        vim.notify("schema store updated")
                    else
                        vim.notify(result.stderr:gsub("\n$", " "), vim.log.levels.ERROR)
                    end
                end))
        end, {})
    end,
    on_detach = function(_, bufnr)
        nvim_buf_del_user_command(bufnr, "UpdateSchemaStore")
    end
})
