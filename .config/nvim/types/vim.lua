--- @meta

--- @class vim.api.keyset.create_autocmd
-- The original definition types these fields as any
-- (which prevents inferring the type of the callback's parameter)
--- @diagnostic disable: duplicate-doc-field
--- @field group? string | integer
--- @field pattern? string | string[]
--- @field callback? fun(args: autocmd_args): boolean? | string
--- @diagnostic enable: duplicate-doc-field

--- @class autocmd_args
--- @field event string
--- @field match string
--- @field file string
--- @field buf integer
--- @field id integer
--- @field group? integer
--- @field data any

--- @type fun(cmd: string | vim.api.keyset.cmd) | table<string, fun(cmd: vim.api.keyset.cmd) | fun(...: any)>
vim.cmd = {}

--- @type table<string, string?>
vim.env = {}
