if vim.fn.executable("haskell-language-server-wrapper") ~= 0 and vim.uri_from_bufnr(0):match("^file:") then
    vim.api.nvim_create_autocmd({"LspAttach", "TextChanged", "InsertLeave"}, {
        buffer = 0,
        group = vim.api.nvim_create_augroup("haskell_" .. vim.api.nvim_get_current_buf(), {}),
        callback = function(ev)
            local on_codelens = vim.lsp.codelens.on_codelens
            function vim.lsp.codelens.on_codelens(err, result, ctx, config)
                local count = #(result or {})
                if count == 0 then
                    on_codelens(err, result, ctx, config)
                    return
                end
                local client = vim.lsp.get_client_by_id(ctx.client_id)
                local function resolved(lens)
                    if lens.command then
                        lens.command.title = " " .. lens.command.title:gsub("^import%s[^(]*", ""):gsub("%s+", " ")
                    end
                    count = count - 1
                    if count == 0 then on_codelens(err, result, ctx, config) end
                end
                for i, lens in ipairs(result) do
                    if lens.command then
                        resolved(lens)
                    else
                        client.request(vim.lsp.protocol.Methods.codeLens_resolve, lens, function(_, lens)
                            result[i] = lens
                            resolved(lens)
                        end, ctx.bufnr)
                    end
                end
            end
            vim.lsp.codelens.refresh({bufnr = ev.buf})
            vim.lsp.codelens.on_codelens = on_codelens
        end,
    })

    vim.lsp.start({
        name = "haskell-language-server",
        cmd = {"haskell-language-server-wrapper", "--lsp"},
        capabilities = lsp_client_capabilities,
        root_dir = vim.fs.root(0, "hie.yaml")
            or vim.fs.root(0, {"cabal.project", "stack.yaml"})
            or vim.fs.root(0, function(name) return name == "package.yaml" or name:match("%.cabal$") end)
            or vim.fs.root(0, ".git"),
    })
end
