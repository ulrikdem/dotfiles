if fn.executable("haskell-language-server-wrapper") ~= 0 and uri_from_bufnr(0):match("^file:") then
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
