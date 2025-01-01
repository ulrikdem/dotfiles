local M = {}

-- This is faster than string.gsub for plain patterns, since string.find has an optimization for those
--- @param str string
--- @param pattern string
--- @param replace string
local function gsub(str, pattern, replace)
    local new_str = ""
    local i = 1
    while true do
        local j, k = str:find(pattern, i)
        if not j then
            return new_str .. str:sub(i)
        end
        new_str = new_str .. str:sub(i, j - 1) .. replace
        i = k + 1
    end
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

M.types = {
    E = {"error", "DiagnosticError"},
    W = {"warning", "DiagnosticWarn"},
    I = {"info", "DiagnosticInfo"},
    N = {"note", "DiagnosticHint"},
}

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

    local type = M.types[(item.type or ""):upper()]
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

return M
