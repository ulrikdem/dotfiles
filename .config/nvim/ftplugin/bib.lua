vim.cmd.runtime("ftplugin/tex.lua")

-- By default only format (including lowercasing keywords and consistently using curly braces)
-- and apply reliable small fixes (month abbreviations and dashes in page ranges)
local formatprg = "bibtex-tidy --blank-lines --no-align --no-remove-dupe-fields --no-escape --curly --months"
vim.bo.formatprg = formatprg

for cmd, args in pairs({
    Sort = " --sort",
    -- Don't sort, and overwrite the first entry with values from later ones, to more easily see the differences
    Deduplicate = " --remove-dupe-fields --duplicates=key,citation,doi --merge=overwrite",
}) do
    vim.api.nvim_buf_create_user_command(0, cmd, function(opts)
        vim.cmd["!"]({formatprg .. args, range = {opts.line1, opts.line2}})
    end, {range = "%"})
end
