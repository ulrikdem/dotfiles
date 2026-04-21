local function is_package(name)
    -- No need to check for package.yaml, since stack projects require stack.yaml
    return vim.endswith(name, ".cabal")
end
local root_dir = find_root(
    "hie.yaml",
    -- In the case of a multi-package project, choose the project root directory
    {"cabal.project", "stack.yaml"},
    is_package,
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
        function client:request(method, params, handler)
            if method == "codeLens/resolve" then
                local old_handler = handler
                function handler(err, result, ctx) --- @param result? lsp.CodeLens
                    if result and result.command then
                        result.command.title = result.command.title:gsub("\n", " "):gsub("  +", " ")
                    end
                    old_handler(err, result, ctx)
                end
            end
            return vim.lsp.client.request(self, method, params, handler)
        end
        vim.lsp.codelens.enable(true, {bufnr = bufnr})
    end,
})

--- @type repl_config
local repl = {
    cmd = {"ghci"},
    cwd = root_dir,
    load_file = function(path)
        return (':load "%s"'):format(vim.fn.escape(path, '"\\'))
    end,
    format = function(code)
        return vim.keycode("<C-e><C-u>") .. (code:find("\n") and ":{\n" .. code .. "\n:}" or code) .. "\n"
    end,
}
if root_dir then
    -- The order of these branches is important, since stack projects can include .cabal packages
    if vim.fn.executable("stack") and vim.uv.fs_stat(root_dir .. "/stack.yaml") then
        repl.cmd = {"stack", "ghci", "--no-load"}
    elseif vim.fn.executable("cabal") then
        local dir = vim.fs.root(0, is_package)
        if dir then
            -- cabal repl can be run at project level with "--enable-multi-repl all",
            -- but this has produced errors in all projects I tried, so run per package
            repl.cmd = {"cabal", "repl", "--repl-no-load"}
            repl.cwd = dir
        end
    end
end
vim.b.repl = repl
