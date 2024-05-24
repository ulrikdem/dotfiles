if fn.executable("haskell-language-server-wrapper") ~= 0 and uri_from_bufnr(0):match("^file:") then
    nvim.create_autocmd({"LspAttach", "CursorHold"}, {
        buffer = nvim.get_current_buf(),
        callback = function(ev)
            local on_codelens = lsp.codelens.on_codelens
            function lsp.codelens.on_codelens(err, result, ctx, config)
                for _, lens in ipairs(result) do
                    lens.command.title = " " .. lens.command.title:gsub("^import%s[^(]*", ""):gsub("%s+", " ")
                end
                on_codelens(err, result, ctx, config)
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
