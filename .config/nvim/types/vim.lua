--- @meta

--- @type fun(cmd: string | vim.api.keyset.cmd) | table<string, fun(cmd: vim.api.keyset.cmd) | fun(...: any)>
vim.cmd = {}

--- @type table<string, string?>
vim.env = {}

--- @class quickfixtextfunc_args
--- @field quickfix 0 | 1
--- @field winid integer
--- @field id integer
--- @field start_idx integer
--- @field end_idx integer

--- @param list vim.quickfix.entry[]
--- @param action? " " | "a" | "r" | "u" | "f"
--- @param what? vim.fn.setqflist.what
--- @return 0 | -1
function vim.fn.setqflist(list, action, what) end

--- @param nr integer
--- @param list vim.quickfix.entry[]
--- @param action? " " | "a" | "r" | "u" | "f"
--- @param what? vim.fn.setqflist.what
--- @return 0 | -1
function vim.fn.setloclist(nr, list, action, what) end

--- @overload fun(): vim.fn.getqflist.ret.item[]
--- @overload fun(what: vim.fn.getqflist.what): vim.fn.getqflist.ret
function vim.fn.getqflist(what) end

--- @overload fun(nr: integer): vim.fn.getqflist.ret.item[]
--- @overload fun(nr: integer, what: vim.fn.getloclist.what): vim.fn.getloclist.ret
function vim.fn.getloclist(nr, what) end

--- @class vim.fn.getqflist.what
--- @field id? integer
--- @field nr? integer | "$"
--- @field idx? integer
--- @field items? true
--- @field size? true
--- @field title? true
--- @field changedtick? true
--- @field qfbufnr? true
--- @field winid? true
--- @field context? true
--- @field all? true
--- @field lines? string[]
--- @field efm? string
-- The fields typed as true? can actually be any value (only their presence matters)

--- @class vim.fn.getloclist.what: vim.fn.getqflist.what
--- @field filewinid? true

--- @class vim.fn.getqflist.ret
--- @field id integer
--- @field nr integer
--- @field idx integer
--- @field items vim.fn.getqflist.ret.item[]
--- @field size integer
--- @field title string
--- @field changedtick integer
--- @field qfbufnr integer
--- @field winid integer
--- @field context any
-- Technically all these fields should be marked optional, however I don't want to be forced to perform nil checks,
-- because you can ensure a field is present by setting the corresponding field of the what argument
-- I don't think this relationship can be expressed in the type system, except perhaps with an exponential number of overloads

--- @class vim.fn.getloclist.ret: vim.fn.getqflist.ret
--- @field filewinid integer

--- @class vim.fn.getqflist.ret.item: vim.quickfix.entry
--- @field bufnr integer
--- @field filename nil
--- @field module string
--- @field lnum integer
--- @field end_lnum integer
--- @field col integer
--- @field end_col integer
--- @field vcol 0 | 1
--- @field pattern string
--- @field type string
--- @field nr integer
--- @field text string
--- @field valid 0 | 1
--- @field user_data any
