local M = {}
local segment_trees = {}

local function non_empty(value)
    if value ~= vim.NIL and value ~= "" then
        return value
    end
end

local position = {
    __eq = function(a, b)
        return a.line == b.line and a.character == b.character
    end,
    __lt = function(a, b)
        if a.line ~= b.line then
            return a.line < b.line
        else
            return a.character < b.character
        end
    end,
    __le = function(a, b)
        return not (a > b)
    end,
}
function position.new(t)
    return setmetatable(t, position)
end

local function insert(symbol, node, node_end)
    if symbol.start <= node.start and symbol.end_ >= node_end then
        table.insert(node.symbols, symbol)
        return
    end
    local split = node.right.start
    if symbol.start < split then
        insert(symbol, node.left, split)
    end
    if symbol.end_ > split then
        insert(symbol, node.right, node_end)
    end
end

function M.update(bufnr, symbols)
    if #symbols == 0 then
        segment_trees[bufnr] = nil
        return
    end

    if symbols[1].location then
        for i, symbol in ipairs(symbols) do
            symbols[i] = {
                name = symbol.name,
                start = symbol.location.range.start,
                end_ = symbol.location.range["end"],
                parent_name = non_empty(symbol.containerName),
            }
        end
    else
        for i, symbol in ipairs(symbols) do
            local children = non_empty(symbol.children)
            symbol = {
                name = symbol.name,
                start = symbol.range.start,
                end_ = symbol.range["end"],
                parent = symbol.parent,
            }
            symbols[i] = symbol
            if children then
                for _, child in ipairs(children) do
                    child.parent = symbol
                    table.insert(symbols, child)
                end
            end
        end
    end


    local positions = {position.new{line = 0, character = 0}}
    for _, symbol in ipairs(symbols) do
        table.insert(positions, position.new(symbol.start))
        table.insert(positions, position.new(symbol.end_))
    end
    table.sort(positions)

    local nodes = {}
    local last_position
    for _, position in ipairs(positions) do
        if position ~= last_position then
            table.insert(nodes, {start = position, symbols = {}})
            last_position = position
        end
    end
    last_position = position.new{line = last_position.line + 1}

    while #nodes > 1 do
        local new_nodes = {}
        for i = 1, #nodes, 2 do
            table.insert(new_nodes, i + 1 <= #nodes and {
                start = nodes[i].start,
                left = nodes[i],
                right = nodes[i + 1],
                symbols = {},
            } or nodes[i])
        end
        nodes = new_nodes
    end

    local tree = nodes[1]
    segment_trees[bufnr] = tree

    for _, symbol in ipairs(symbols) do
        if symbol.start < symbol.end_ then
            insert(symbol, tree, last_position)
        end
    end
end

function M.at_cursor()
    local node = segment_trees[vim.fn.bufnr()]
    if not node then
        return " "
    end

    local cursor = position.new(vim.lsp.util.make_position_params().position)

    local symbol
    while true do
        for _, s in ipairs(node.symbols) do
            if not symbol or s.start > symbol.start
                    or s.start == symbol.start and s.end_ < symbol.end_ then
                symbol = s
            end
        end
        if not node.right then
            break
        end
        node = cursor < node.right.start and node.left or node.right
    end
    if not symbol then
        return " "
    end

    local result = symbol.name
    local separator = " " .. vim.g.lightline.subseparator.left .. " "
    while symbol.parent do
        symbol = symbol.parent
        result = symbol.name .. separator .. result
    end
    if symbol.parent_name then
        result = symbol.parent_name .. separator .. result
    end
    return result
end

return M
