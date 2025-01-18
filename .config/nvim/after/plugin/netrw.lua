-- Disable netrw directory browser, without disabling the entire plugin, so remote files can still be edited
-- Dirvish also does this, but only after netrw's VimEnter autocmd, which is too late for directories passed on the command line
nvim_del_augroup_by_name("FileExplorer")
