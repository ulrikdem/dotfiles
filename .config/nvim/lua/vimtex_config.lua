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
end

return M
