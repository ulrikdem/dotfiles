if exists('s:loaded')
  finish
endif
let s:loaded = 1

runtime! autoload/coc/util.vim

function! coc#util#get_float_mode(allow_selection, align_top, pum_align_top) abort
  let mode = mode()
  if pumvisible() && a:align_top == a:pum_align_top
    return v:null
  endif
  let checked = (mode == 's' && a:allow_selection) || index(['i', 'n', 'ic'], mode) != -1
  if !checked
    return v:null
  endif
  " if !s:is_vim && mode ==# 'i'
  "   " helps to fix undo issue, don't know why.
  "   call feedkeys("\<C-g>u", 'n')
  " endif
  let pos = coc#util#win_position()
  return [mode, bufnr('%'), pos, [line('.'), col('.')]]
endfunction
