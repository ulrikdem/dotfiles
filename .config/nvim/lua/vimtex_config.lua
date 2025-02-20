local M = {}

local g = vim.g
g.vimtex_complete_close_braces = true
g.vimtex_view_method = "zathura"
g.vimtex_view_use_temp_files = true
g.vimtex_doc_handlers = {"vimtex#doc#handlers#texdoc"}
g.vimtex_ui_method = {confirm = "legacy", input = "legacy", select = "legacy"}
g.vimtex_toc_config = {split_pos = "vertical", show_help = false, layers = {"content"}, hide_line_numbers = false, fold_enable = true}
g.vimtex_fold_enabled = true
g.vimtex_indent_enabled = false
g.vimtex_indent_bib_enabled = false
g.vimtex_syntax_conceal = {cites = false, fancy = false, spacing = false, math_bounds = false, math_delimiters = false, math_fracs = false, math_super_sub = false, styles = false}
g.vimtex_imaps_enabled = false

local insert_maps = {} --- @type table<string, string>
local ucs_data = "/usr/share/texmf-dist/tex/latex/ucs/data"
for file in vim.fs.dir(ucs_data) do
    for line in io.lines(ucs_data .. "/" .. file) do
        local codepoint, rhs = line:match([[\uc@dclc{(%d+)}%b{}{\ensuremath(.*)}]])
        if codepoint then
            insert_maps[vim.fn.nr2char(tonumber(codepoint))] = rhs:match("^{(.*)}$") or rhs
        end
    end
end
insert_maps["√"] = "\\sqrt"
insert_maps["…"] = "\\dots"

function M.init_buffer()
    vim.wo[0][0].foldlevel = 99

    for lhs, rhs in pairs({
        ["<Leader>mm"] = "<Cmd>silent update | VimtexCompileSS<CR>",
        ["<Leader>mv"] = "<Cmd>VimtexView<CR>",
        ["<Leader>mc"] = "<Cmd>VimtexClean<CR>",
        ["<Leader>mC"] = "<Cmd>VimtexClean!<CR>",
        ["<C-c>"] = "<C-c><Cmd>VimtexStop<CR>",
        gO = "<Cmd>VimtexTocOpen<CR>",
        grc = "<Cmd>VimtexContextMenu<CR>",
    }) do
        vim.keymap.set("n", lhs, rhs, {buffer = true})
    end

    for lhs, rhs in pairs(insert_maps) do
        vim.keymap.set("i", lhs, function()
            local packages = vim.b.vimtex.packages
            -- Insert symbol directly if a package allows Unicode in math mode
            return (packages.ucs or packages["unicode-math"] or packages["unicode-math-input"]) and lhs or rhs
        end, {expr = true, buffer = true})
    end
end

return M
