if fn.executable("haskell-language-server-wrapper") ~= 0 and uri_from_bufnr(0):match("^file:") then
    nvim.create_autocmd({"LspAttach", "TextChanged", "InsertLeave"}, {
        buffer = nvim.get_current_buf(),
        callback = function(ev)
            local on_codelens = lsp.codelens.on_codelens
            function lsp.codelens.on_codelens(err, result, ctx, config)
                local count = #(result or {})
                if count == 0 then
                    on_codelens(err, result, ctx, config)
                    return
                end
                local client = lsp.get_client_by_id(ctx.client_id)
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
                        client.request(lsp.protocol.Methods.codeLens_resolve, lens, function(_, lens)
                            result[i] = lens
                            resolved(lens)
                        end, ctx.bufnr)
                    end
                end
            end
            lsp.codelens.refresh({bufnr = ev.buf})
            lsp.codelens.on_codelens = on_codelens
        end,
    })

    lsp.start({
        name = "haskell-language-server",
        cmd = {"haskell-language-server-wrapper", "--lsp"},
        capabilities = lsp_client_capabilities,
        root_dir = fs.root(0, function(name)
            return ({
                ["hie.yaml"] = true,
                ["stack.yaml"] = true,
                ["package.yaml"] = true,
                ["cabal.project"] = true
            })[name] or name:match("%.cabal$")
        end),
    })
end
