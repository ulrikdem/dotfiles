local lsp = vim.lsp
local pending_requests = {} --- @type table<integer, true?>

--- @param client vim.lsp.Client
--- @param bufnr integer
--- @param method string
--- @param params table
--- @param handler fun(result: any)
local function cancellable_request(client, bufnr, method, params, handler)
    local _, request_id
    _, request_id = client.request(method, params, function(err, result)
        if request_id and pending_requests[request_id] then
            pending_requests[request_id] = nil
            if err then
                lsp.log.error(client.name, tostring(err))
            else
                handler(result)
            end
        end
    end, bufnr)
    if request_id then pending_requests[request_id] = true end
end

--- @param client vim.lsp.Client
--- @param bufnr integer
local function refresh_codelens(client, bufnr)
    for request_id, _ in pairs(pending_requests) do client.cancel_request(request_id) end
    pending_requests = {}

    cancellable_request(client, bufnr, lsp.protocol.Methods.textDocument_codeLens, {
        textDocument = lsp.util.make_text_document_params(bufnr),
    }, function(lenses) --- @param lenses lsp.CodeLens[]
        local count = #lenses
        if count == 0 then
            lsp.codelens.clear(client.id, bufnr)
            return
        end

        --- @param lens lsp.CodeLens
        local function resolved(lens)
            if lens.command then
                lens.command.title = (" " .. lens.command.title):gsub("^ import%s[^(]*", "")
            end
            count = count - 1
            if count == 0 then
                lsp.codelens.save(lenses, bufnr, client.id)
                lsp.codelens.display(lenses, bufnr, client.id)
            end
        end

        for i, lens in ipairs(lenses) do
            if lens.command then
                resolved(lens)
            else
                cancellable_request(client, bufnr, lsp.protocol.Methods.codeLens_resolve, lens, function(lens)
                    lenses[i] = lens
                    resolved(lenses[i])
                end)
            end
        end
    end)
end

local root_dir = find_root(
    "hie.yaml",
    {"cabal.project", "stack.yaml"},
    function(n) return n == "package.yaml" or n:match(".%.cabal$") end,
    ".git")

--- @param ... string
--- @return string[]
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
    offset_encoding = "utf-32",

    -- https://haskell-language-server.readthedocs.io/en/stable/configuration.html
    settings = {},

    on_attach = function(client, bufnr)
        refresh_codelens(client, bufnr)
        local timer = vim.uv.new_timer()
        nvim_create_autocmd({"TextChanged", "InsertLeave"}, {
            buffer = bufnr,
            group = lsp_augroup(client.id),
            callback = function()
                timer:start(100, 0, vim.schedule_wrap(function()
                    refresh_codelens(client, bufnr)
                end))
            end,
        })
    end,
})
