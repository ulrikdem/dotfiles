" Setup {{{1

" vim: foldmethod=marker

augroup vimrc
    autocmd!
augroup END

" Misc mappings {{{1

let g:mapleader = ' '

" Formatting {{{1

autocmd vimrc FileType tex AddTabularPattern! tex /&\|\\\\/
autocmd vimrc FileType tex nmap <buffer><silent> <Leader>gq vie:Tabularize tex<CR>

" Command execution {{{1

if executable('rg')
    set grepprg=rg\ --column\ --color=ansi
    set grepformat=[0m[35m%f[0m:[0m[32m%l[0m:[0m%c[0m:%m
else
    set grepprg=grep\ -rIn
endif

let s:match_start = "\e[31m"
let s:match_end = "\e[0m"

nnoremap <Leader>mm <Cmd>update \| make<CR>
nnoremap <Leader>mc <Cmd>make clean<CR>

command! -bang -nargs=+ -complete=file Grep silent grep<bang> <args>
nnoremap <Leader>gg <Cmd>Grep -Fwe '<cword>'<CR>
nnoremap <Leader>gG <Cmd>Grep -Fwe '<cword>' %:p:.:h:S<CR>

" File navigation {{{1

set suffixes-=.h
autocmd vimrc ColorScheme * highlight link DirvishSuffix Comment
autocmd vimrc ColorScheme * highlight link DirvishPathHead NonText
if $RANGER_LEVEL
    nmap <expr> - !v:count && len(getbufinfo(#{buflisted: v:true})) * winnr('$') * tabpagenr('$') == 1 ? '<C-W>q' : '<Plug>(dirvish_up)'
endif

" Quickfix {{{1

autocmd vimrc FileType qf call s:InitQuickfixBuffer()
function! s:InitQuickfixBuffer() abort
    if exists('w:added_qf_matches')
        return
    endif
    call matchadd('String', s:match_start.'.\{-}'.s:match_end)
    call matchadd('Conceal', '\e\[\d*m')
    setlocal conceallevel=2 concealcursor=nvc
    let w:added_qf_matches = v:true
endfunction

" Git {{{1

autocmd vimrc FileType GV setlocal nolist

nnoremap <Leader>tg <Cmd>call <SID>ToggleGitStatus()<CR>
function! s:ToggleGitStatus() abort
    let l:buf = filter(getbufinfo(), {i, b -> get(b.variables, 'fugitive_type', '') ==# 'index'})
    if !empty(l:buf)
        execute 'bdelete' l:buf[0].bufnr
        return
    endif
    Git
endfunction

nnoremap <Leader>td <Cmd>call <SID>ToggleDiff()<CR>
function! s:ToggleDiff() abort
    if &diff
        let l:current_win = win_getid()
        let l:wins = filter(gettabinfo(tabpagenr())[0].windows, {i, w -> getwinvar(w, '&diff')})
        diffoff!
        for l:win in l:wins
            call win_gotoid(l:win)
            if &foldmethod ==# 'manual'
                normal! zE
            endif
            if l:win != l:current_win
                close
            end
        endfor
        call win_gotoid(l:current_win)
    else
        Gdiffsplit!
    endif
endfunction

autocmd vimrc SourcePost fugitive.vim call s:OverrideWorkTree()
function! s:OverrideWorkTree() abort
    if !exists('*FugitiveWorkTree')
        return
    endif
    let l:WorkTree = funcref('FugitiveWorkTree')
    function! FugitiveWorkTree(...) abort closure
        if exists('$GIT_WORK_TREE')
            return $GIT_WORK_TREE
        else
            return call(l:WorkTree, a:000)
        endif
    endfunction
endfunction

" Filetypes {{{1

autocmd vimrc FileType c,cpp setlocal commentstring=//%s
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oh <Cmd>edit %:r.h<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oH <Cmd>edit %:r.hpp<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oc <Cmd>edit %:r.c<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oC <Cmd>edit %:r.cpp<CR>

if executable('cargo')
    autocmd vimrc FileType rust nnoremap <buffer> <Leader>mm <Cmd>update \| make build<CR>
    autocmd vimrc FileType rust nnoremap <buffer> <Leader>mr <Cmd>update \| make build --release<CR>
endif

autocmd vimrc FileType mail,markdown,tex setlocal spell

autocmd vimrc FileType dot setlocal commentstring=//%s
if executable('dot')
    autocmd vimrc FileType dot let &l:makeprg = 'dot -T$* -o'.expand('%:p:r:S').'.$* '.expand('%:p:S')
    autocmd vimrc FileType dot nnoremap <buffer> <Leader>mm <Cmd>update \| make png<CR>
    autocmd vimrc FileType dot nnoremap <buffer> <Leader>ms <Cmd>update \| make svg<CR>
endif
if executable('xdg-open')
    autocmd vimrc FileType dot nnoremap <buffer> <Leader>mv <Cmd>silent !xdg-open %:r:S.png &<CR>
endif

" Debugging {{{1

if executable('gdb')
    packadd termdebug
    let g:termdebug_wide = 1

    autocmd vimrc OptionSet signcolumn setglobal signcolumn&

    command! -nargs=* -complete=file Debug call s:Debug('gdb', <q-args>)
    if executable('rust-gdb')
        command! -nargs=* -complete=file DebugRust call s:Debug('rust-gdb', <q-args>)
    endif

    let s:gdb_buf = 0
    function! s:Debug(gdb, args) abort
        if s:gdb_buf
            return
        endif

        delcommand Source

        let g:termdebugger = a:gdb
        execute (empty(a:args) ? 'Termdebug' : 'TermdebugCommand') a:args
        let s:gdb_buf = bufnr()
        autocmd WinEnter <buffer> call feedkeys("\<M-0>\<C-L>", 'in')

        function! s:ToggleDebugWindow() abort closure
            let l:win = bufwinnr(s:gdb_buf)
            if l:win != -1
                execute l:win 'close'
                return
            endif
            Source
            15 split
            set winfixheight
            execute 'buffer' s:gdb_buf
            startinsert
        endfunction

        let l:maps = [
            \ ['<M-g>', '<Cmd>call <SID>ToggleDebugWindow()<CR>', 'nt'],
            \ ['<M-Q>', '<Cmd>call TermDebugSendCommand("quit\ny")<CR>', 'nt'],
            \ ['<M-b>', '<Cmd>Break<CR>', 'n'],
            \ ['<M-B>', '<Cmd>Clear<CR>', 'n'],
            \ ['<M-C-B>', '<Cmd>call TermDebugSendCommand("delete\ny")<CR>', 'nt'],
            \ ['<M-R>', '<Cmd>Run<CR>', 'nt'],
            \ ['<M-c>', '<Cmd>Continue<CR>', 'nt'],
            \ ['<M-C-C>', '<Cmd>Stop<CR>', 'nt'],
            \ ['<M-n>', '<Cmd>Over<CR>', 'nt'],
            \ ['<M-s>', '<Cmd>Step<CR>', 'nt'],
            \ ['<M-f>', '<Cmd>Finish<CR>', 'nt'],
            \ ['<M-u>', '<Cmd>call TermDebugSendCommand("up")<CR>', 'nt'],
            \ ['<M-d>', '<Cmd>call TermDebugSendCommand("down")<CR>', 'nt'],
            \ ['<M-e>', ':Evaluate<CR>', 'nx'],
        \ ]

        if filereadable('/usr/share/gdb-dashboard/.gdbinit') &&
            \ filereadable(expand('~/.config/gdb-dashboard'))
            enew
            let l:dashboard_buf = bufnr()
            let l:pty = nvim_get_chan_info(termopen('tail -f /dev/null # /dashboard')).pty
            call TermDebugSendCommand('source ~/.config/gdb-dashboard')
            call TermDebugSendCommand('dashboard -output '.l:pty)
            call extend(l:maps, [
                \ ['<M-w>', '<Cmd>call <SID>WatchExpression(0)<CR>', 'nx'],
                \ ['<M-W>', '<Cmd>call <SID>WatchExpression(1)<CR>', 'nx'],
                \ ['<M-C-W>', '<Cmd>call TermDebugSendCommand("dashboard expression clear")<CR>', 'nt'],
            \ ])
            command! -bang -nargs=+ Watch call s:WatchExpression(<bang>0, <q-args>)
        endif

        for [l:lhs, l:rhs, l:modes] in l:maps
            for l:mode in split(l:modes, '\zs')
                execute l:mode.'noremap <silent>' l:lhs l:rhs
            endfor
        endfor

        function! s:OnDebugExit(...) abort closure
            for [l:lhs, l:rhs, l:modes] in l:maps
                for l:mode in split(l:modes, '\zs')
                    execute l:mode.'unmap' l:lhs
                endfor
            endfor
            if exists('g:loaded_wordmotion')
                unlet g:loaded_wordmotion
                runtime plugin/wordmotion.vim
            endif

            execute 'bwipeout!' s:gdb_buf
            let s:gdb_buf = 0
            if exists('l:dashboard_buf')
                execute 'bwipeout!' l:dashboard_buf
            endif

            silent! delcommand Watch
        endfunction

        Program
        set nomodified
        let l:pty = nvim_get_chan_info(termopen('tail -f /dev/null # /io', #{
            \ on_stdout: function("\<SID>OnDebugStdout"),
            \ on_exit: funcref("\<SID>OnDebugExit"),
        \ })).pty
        call TermDebugSendCommand('tty '.l:pty)

        Source
        stopinsert
    endfunction

    function! s:WatchExpression(unwatch, ...) abort
        if a:0
            let l:expr = a:1
        elseif mode()[0] ==# 'n'
            let l:expr = expand('<cexpr>')
        else
            let l:reg = getreg('v', 1, v:true)
            let l:type = getregtype('v')
            normal! "vy
            let l:expr = @v
            call setreg('v', l:reg, l:type)
        endif
        call TermDebugSendCommand('dashboard expression '.(a:unwatch ? 'un' : '').'watch '.l:expr)
    endfunction

    function! s:OnDebugStdout(id, lines, event) abort
        if a:lines[-2:] ==# [
            \ "warning: GDB: Failed to set controlling terminal: Operation not permitted\r",
            \ '',
        \ ]
            silent execute "!clear >".(nvim_get_chan_info(a:id).pty)
        endif
    endfunction
endif
