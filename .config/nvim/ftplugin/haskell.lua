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
    end,
})

local root_dir = vim.fs.root(0, "hie.yaml")
    or vim.fs.root(0, {"cabal.project", "stack.yaml"})
    or vim.fs.root(0, function(name) return name == "package.yaml" or name:match("%.cabal$") end)
    or vim.fs.root(0, ".git")

local tool_dirs = vim.tbl_map(vim.fs.normalize, {"~/.cabal", "~/.stack", "~/.ghcup"})
local cache_dirs = vim.tbl_map(vim.fs.normalize, {"~/.cache/hie-bios", "~/.cache/ghcide"})
for _, dir in ipairs(cache_dirs) do vim.fn.mkdir(dir, "p") end

start_lsp({
    name = "haskell-language-server-wrapper",
    cmd = vim.iter({
        "sandbox",
        root_dir and {"-w", root_dir, vim.tbl_map(function(dir) return {"-w", dir} end, cache_dirs), "-n"} or {},
        vim.tbl_map(function(dir) return vim.fn.isdirectory(dir) ~= 0 and {"-w", dir} or {} end, tool_dirs),
        "haskell-language-server-wrapper", "--lsp",
    }):flatten(3):totable(),
    root_dir = root_dir,
    -- https://haskell-language-server.readthedocs.io/en/stable/configuration.html
    settings = {},
})
