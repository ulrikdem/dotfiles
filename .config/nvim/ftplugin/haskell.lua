local function refresh_codelens(ev)
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
            if not lens.command and client then
                client.request(vim.lsp.protocol.Methods.codeLens_resolve, lens, function(_, lens)
                    result[i] = lens
                    resolved(lens)
                end, ctx.bufnr)
            else
                resolved(lens)
            end
        end
    end
    vim.lsp.codelens.refresh({bufnr = ev.buf})
    vim.lsp.codelens.on_codelens = on_codelens
end

local root_dir = find_root(
    "hie.yaml",
    {"cabal.project", "stack.yaml"},
    function(n) return n == "package.yaml" or n:match(".%.cabal$") end,
    ".git")

local function existing_dirs(...)
    return vim.iter({...})
        :map(vim.fs.normalize)
        :filter(function(dir) return vim.fn.isdirectory(dir) ~= 0 end)
        :totable()
end

start_lsp({
    cmd = {"haskell-language-server-wrapper", "--lsp"},
    root_dir = root_dir,
    sandbox = root_dir and {
        args = {"-n"},
        write = {
            root_dir,
            vim.fs.normalize("~/.cache/hie-bios"),
            vim.fs.normalize("~/.cache/ghcide"),
            unpack(existing_dirs("~/.cabal", "~/.stack", "~/.ghcup")),
        },
    } or {
        read = existing_dirs("~/.ghcup"),
    },
    -- https://haskell-language-server.readthedocs.io/en/stable/configuration.html
    settings = {},

    on_attach = function(client, bufnr)
        refresh_codelens({buf = bufnr})
        vim.api.nvim_create_autocmd({"TextChanged", "InsertLeave"}, {
            buffer = bufnr,
            group = lsp_augroup(client.id),
            callback = refresh_codelens,
        })
    end,
})
