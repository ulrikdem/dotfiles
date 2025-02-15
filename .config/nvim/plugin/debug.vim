if executable('gdb')
    packadd termdebug
    let g:termdebug_wide = 1

    augroup debug
        autocmd!
        autocmd OptionSet signcolumn setglobal signcolumn&
    augroup END

    command! -nargs=* -complete=file Debug call s:Debug('gdb', <q-args>)
    if executable('rust-gdb')
        command! -nargs=* -complete=file DebugRust call s:Debug('rust-gdb', <q-args>)
    endif

    let s:gdb_buf = 0
    function! s:Debug(gdb, args) abort
        if s:gdb_buf
            return
        endif

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
            let l:pty = nvim_get_chan_info(termopen('tail -f /dev/null')).pty
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

            execute 'bwipeout!' s:gdb_buf
            let s:gdb_buf = 0
            if exists('l:dashboard_buf')
                execute 'bwipeout!' l:dashboard_buf
            endif

            silent! delcommand Watch
        endfunction

        Program
        set nomodified
        let l:pty = nvim_get_chan_info(termopen('tail -f /dev/null', #{
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
