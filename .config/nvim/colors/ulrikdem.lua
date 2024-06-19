vim.cmd.highlight("clear")

vim.g.colors_name = "ulrikdem"

local bg, fg = "NvimDark", "NvimLight"
if vim.o.background == "light" then bg, fg = fg, bg end

--- @type table<string, vim.api.keyset.highlight>
local highlights = {
    StatusLine = {bg = fg .. "Gray4", fg = bg .. "Gray2"},
    StatusLineNC = {bg = bg .. "Gray4", fg = fg .. "Gray2"},
    TabLineSel = {link = "StatusLine"},

    CursorLineNr = {fg = fg .. "Gray4"},
    Folded = {fg = fg .. "Gray4"},
    QuickFixLine = {bg = bg .. "Gray3"},
    ColorColumn = {bg = bg .. "Gray3"},

    DiffAdd = {link = "DiffText"},
    DiffText = {bg = bg .. "Blue"},
    DiffChange = {bg = bg .. "Gray3"},
    DiffDelete = {bg = bg .. "Gray3", fg = bg .. "Gray3"},

    -- These links are defined in the diff syntax, but not in the git syntax
    diffAdded = {link = "Added"},
    diffRemoved = {link = "Removed"},

    NormalFloat = {link = "Pmenu"},
    CmpItemAbbrMatch = {fg = fg .. "Green"},
    CmpItemAbbrMatchFuzzy = {link = "CmpItemAbbrMatch"},

    LspReferenceText = {bg = bg .. "Gray3"},
    LspReferenceWrite = {bg = bg .. "Gray4"},

    DiagnosticUnderlineError = {undercurl = true, sp = fg .. "Red"},
    DiagnosticUnderlineWarn = {undercurl = true, sp = fg .. "Yellow"},
    DiagnosticUnderlineInfo = {undercurl = true, sp = fg .. "Cyan"},
    DiagnosticUnderlineHint = {undercurl = true, sp = fg .. "Blue"},
    DiagnosticUnderlineOk = {undercurl = true, sp = fg .. "Green"},
}

for k, v in pairs(highlights) do
    vim.api.nvim_set_hl(0, k, v)
end
