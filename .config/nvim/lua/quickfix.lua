local M = {}

--- @class set_list_opts: vim.fn.setqflist.what
--- @field loclist_winid? integer

--- @param opts set_list_opts
function M.set_list(opts)
    if opts.loclist_winid then
        nvim_set_current_win(opts.loclist_winid)
        vim.fn.setloclist(0, {}, " ", opts)
        vim.cmd("lopen")
    else
        vim.fn.setqflist({}, " ", opts)
        vim.cmd("botright copen")
    end
end

--- @class get_list_opts: vim.fn.getloclist.what
--- @field loclist_winid? integer

--- @param opts get_list_opts
--- @return vim.fn.getqflist.ret | vim.fn.getloclist.ret
function M.get_list(opts)
    if opts.loclist_winid then
        return vim.fn.getloclist(opts.loclist_winid, opts)
    else
        return vim.fn.getqflist(opts)
    end
end

-- This is faster than string.gsub for plain patterns, since string.find has an optimization for those
--- @param str string
--- @param pattern string
--- @param replace string
local function gsub(str, pattern, replace)
    local new_str = ""
    local i = 1
    while true do
        local j, k = str:find(pattern, i)
        if not j or j < i then
            return new_str .. str:sub(i)
        end
        new_str = new_str .. str:sub(i, j - 1) .. replace
        if j <= k then
            i = k + 1
        else -- Empty match
            new_str = new_str .. str:sub(j, j)
            i = j + 1
        end
    end
end

local types = {
    E = {"error", "DiagnosticError"},
    W = {"warning", "DiagnosticWarn"},
    I = {"info", "DiagnosticInfo"},
    N = {"note", "DiagnosticHint"},
}

--- @type table<integer, {foldlevel: table<integer, integer | string>, foldtext: table<integer, string>}>
_G.quickfix_data = quickfix_data or {}

--- @param args quickfixtextfunc_args
function M.textfunc(args)
    local list = M.get_list({
        loclist_winid = args.quickfix == 0 and args.winid or nil,
        id = args.id,
        qfbufnr = true, title = true, items = true,
    })
    local bufnr = list.qfbufnr
    local is_toc = vim.endswith(list.title, "TOC")

    local lines = {} --- @type string[]
    local highlights = {}
    local namespace = nvim_create_namespace("quickfix")

    if args.start_idx == 1 then
        nvim_buf_clear_namespace(bufnr, namespace, 0, -1)
        quickfix_data[bufnr] = {foldlevel = {}, foldtext = {}}
    end
    local data = quickfix_data[bufnr]

    for i = args.start_idx, args.end_idx do
        local item = list.items[i]
        local leading_space = is_toc and 0 or #item.text:match("%s*")
        local line = ""

        if item.bufnr ~= 0 then
            local name = nvim_buf_get_name(item.bufnr)
            line = name ~= "" and vim.fn.fnamemodify(name, ":~:.") or "[No Name]"
            -- Dim path for all but the first (consecutive) item for the same buffer
            if item.bufnr == vim.tbl_get(list.items, i - 1, "bufnr") then
                table.insert(highlights, {i - 1, 0, #line, "NonText"})
                data.foldlevel[i] = 1
            else
                data.foldlevel[i] = ">1"
                data.foldtext[i] = line
            end
        end

        line = line .. "|"
        if item.lnum ~= 0 then
            line = line .. ("%4d"):format(item.lnum)
        end
        line = line .. "| "

        local type = types[item.type:upper()]
        if type then
            local name, group = unpack(type)
            table.insert(highlights, {i - 1, #line, #line + #name + 1, group})
            line = line .. name .. ": "
        end

        local highlight_ranges = vim.tbl_get(item, "user_data", "highlight_ranges")
        for _, range in ipairs(highlight_ranges or {}) do
            table.insert(highlights, {
                i - 1,
                #line + math.max(range[1] - leading_space, 0),
                #line + math.max(range[2] - leading_space, 0),
                "String",
            })
        end

        local text = item.text:sub(leading_space + 1)
        if not highlight_ranges then text = gsub(text, "\n%s*", " ") end
        line = line .. text

        local foldlevel = vim.tbl_get(item, "user_data", "foldlevel")
        if foldlevel then
            data.foldlevel[i] = foldlevel
            data.foldtext[i] = text
        end

        table.insert(lines, line)
    end

    vim.schedule(function()
        -- Cancel if the list has been replaced since this was scheduled
        -- This can happen when updating the list from a DiagnosticChanged autocmd
        if quickfix_data[bufnr] ~= data then return end

        for _, highlight in ipairs(highlights) do
            local line, col, end_col, group = unpack(highlight)
            nvim_buf_set_extmark(bufnr, namespace, line, col, {end_col = end_col, hl_group = group})
        end
    end)

    return lines
end

function M.foldexpr()
    return quickfix_data[nvim_get_current_buf()].foldlevel[vim.v.lnum] or 0
end

function M.foldtext()
    return ("%s (%d lines) "):format(
        quickfix_data[nvim_get_current_buf()].foldtext[vim.v.foldstart],
        vim.v.foldend - vim.v.foldstart + 1)
end

--- @param item vim.quickfix.entry
--- @param columns? integer
--- @param highlight? boolean
function M.to_fzf(item, columns, highlight)
    columns = columns or vim.o.columns

    local location = ""
    if item.bufnr and item.bufnr ~= 0 then
        local name = nvim_buf_get_name(item.bufnr)
        location = name ~= "" and vim.fn.fnamemodify(name, ":~:.") or "[No Name]"
    elseif item.filename then
        location = vim.fn.fnamemodify(item.filename, ":~:.")
    end
    if item.lnum and item.lnum ~= 0 then
        location = location .. ":" .. item.lnum
    elseif not item.text or item.text == "" then
        return location
    end

    local type = types[(item.type or ""):upper()]
    local type = type and type[1] .. ": " or ""

    local leading_space = #item.text:match("%s*")
    local text = item.text:sub(leading_space + 1)
    local highlight_ranges = highlight and vim.tbl_get(item, "user_data", "highlight_ranges") or {}
    text = gsub(text, next(highlight_ranges) and "\n" or "\n%s*", " ")
    text = gsub(text, "\t", " ")

    local container_names = vim.tbl_get(item, "user_data", "container_names")
    local container_names = container_names
        and vim.iter(container_names):map(function(s) return " < " .. s end):join("")
        or ""

    local strwidth = vim.fn.strwidth
    local pad = (" "):rep(math.max(1,
        columns - 3 - strwidth(type) - strwidth(text) - strwidth(container_names) - strwidth(location)))

    local new_text = ""
    local i = 0
    -- Assume highlight_ranges are sorted and non-overlapping
    for _, range in ipairs(highlight_ranges) do
        local j = math.max(range[1] - leading_space, 0)
        local k = math.max(range[2] - leading_space, 0)
        new_text = new_text .. text:sub(i + 1, j) .. "\x1b[32m" .. text:sub(j + 1, k) .. "\x1b[m"
        i = k
    end
    text = new_text .. text:sub(i + 1)

    return type .. text .. "\x1b[90m" .. container_names .. pad .. location .. "\x1b[m"
end

--- @param json string
--- @return vim.quickfix.entry?
function M.from_ripgrep(json)
    local obj = vim.json.decode(json)
    if obj.type == "match" then
        local match = obj.data
        local text = match.lines.text or vim.base64.decode(match.lines.bytes)
        if vim.endswith(text, "\n") then text = text:sub(1, -2) end
        return {
            filename = match.path.text or vim.base64.decode(match.path.bytes),
            lnum = match.line_number,
            col = match.submatches[1] and match.submatches[1].start + 1,
            text = gsub(text, "\0", "\n"),
            user_data = {
                highlight_ranges = vim.tbl_map(function(submatch)
                    return {submatch.start, submatch["end"]}
                end, match.submatches),
            },
        }
    end
end

--- @param symbols lsp.DocumentSymbol[] | lsp.WorkspaceSymbol[] | lsp.SymbolInformation[]
--- @param bufnr integer
function M.from_lsp_symbols(symbols, bufnr)
    local items = {} --- @type vim.quickfix.entry[]

    --- @param symbols lsp.DocumentSymbol[] | lsp.WorkspaceSymbol[] | lsp.SymbolInformation[]
    --- @param depth integer
    --- @param container_names string[]
    local function inner(symbols, depth, container_names)
        for _, symbol in ipairs(symbols) do
            local range = symbol.selectionRange or symbol.location.range
            local kind = vim.lsp.protocol.SymbolKind[symbol.kind] or "Unknown"
            local text = ("%s[%s] %s"):format(("  "):rep(depth), kind, symbol.name)
            table.insert(items, {
                filename = symbol.location and vim.uri_to_fname(symbol.location.uri),
                bufnr = not symbol.location and bufnr or nil,
                lnum = range.start.line + 1,
                col = range.start.character + 1, -- This neglects to take into account offset_encoding
                text = symbol.detail and symbol.detail ~= "" and text .. ": " .. symbol.detail or text,
                user_data = {
                    highlight_ranges = {{#text - #symbol.name, #text}},
                    container_names = symbol.containerName and {symbol.containerName} or container_names,
                    -- Only override foldlevel for DocumentSymbols, which have a hierarchy
                    foldlevel = symbol.range and (next(symbol.children or {}) and ">" .. depth + 1 or depth),
                },
            })
            if symbol.children then
                inner(symbol.children, depth + 1, {symbol.name, unpack(container_names)})
            end
        end
    end

    inner(symbols, 0, {})
    return items
end

return M
