let s:original = funcref('FugitiveWorkTree')

function! FugitiveWorkTree(...) abort
    return exists('$GIT_WORK_TREE') ? $GIT_WORK_TREE : call(s:original, a:000)
endfunction
