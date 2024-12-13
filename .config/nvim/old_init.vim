" Setup {{{1

" vim: foldmethod=marker

augroup vimrc
    autocmd!
augroup END

try
    call plug#begin(stdpath('data').'/plugged')

    function! s:InitPlugins() abort
        call plug#end()
        for l:plug in get(g:, 'plugs_order', [])
            if isdirectory(g:plugs[l:plug].dir)
                silent execute 'doautocmd vimrc User Plug_'.substitute(l:plug, '\W', '_', 'g')
            endif
        endfor
    endfunction
catch
    command! -nargs=+ Plug

    function! s:InitPlugins() abort
    endfunction
endtry

" Misc settings {{{1

autocmd vimrc FocusGained,BufEnter,QuickFixCmdPost * checktime

" Misc mappings {{{1

let g:mapleader = ' '

Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

Plug 'chaoren/vim-wordmotion'
let g:wordmotion_mappings = {
    \ 'w': '<M-w>',
    \ 'b': '<M-b>',
    \ 'e': '<M-e>',
    \ 'ge': 'g<M-e>',
    \ 'iw': 'i<M-w>',
    \ 'aw': 'a<M-w>',
    \ '<C-R><C-W>': '<C-R><M-w>',
\ }

" Formatting {{{1

Plug 'tpope/vim-sleuth'

Plug 'godlygeek/tabular'
autocmd vimrc User Plug_tabular autocmd vimrc FileType tex AddTabularPattern! tex /&\|\\\\/
autocmd vimrc User Plug_tabular autocmd vimrc FileType tex nmap <buffer><silent> <Leader>gq vie:Tabularize tex<CR>

" Command execution {{{1

if executable('rg')
    set grepprg=rg\ --column\ --color=ansi
    set grepformat=[0m[35m%f[0m:[0m[32m%l[0m:[0m%c[0m:%m
else
    set grepprg=grep\ -rIn
endif

let s:match_start = "\e[31m"
let s:match_end = "\e[0m"

Plug 'hauleth/asyncdo.vim'
autocmd vimrc User Plug_asyncdo_vim nnoremap <C-C> <Cmd>AsyncStop<CR>

autocmd vimrc User Plug_asyncdo_vim command! -bang -nargs=* -complete=file Make
    \ cclose | call asyncdo#run(<bang>0, substitute(&makeprg, '\\|', '|', 'g'), <q-args>)
autocmd vimrc User Plug_asyncdo_vim nnoremap <Leader>mm <Cmd>silent update \| Make<CR>
autocmd vimrc User Plug_asyncdo_vim nnoremap <Leader>mc <Cmd>Make clean<CR>

autocmd vimrc User Plug_asyncdo_vim command! -bang -nargs=+ -complete=file Grep
    \ cclose | call asyncdo#run(<bang>0, #{
        \ job: substitute(&grepprg, '\\|', '|', 'g'),
        \ errorformat: &grepformat,
    \ }, <q-args>)
autocmd vimrc User Plug_asyncdo_vim command! -bang -nargs=+ -complete=file RGrep
    \ Grep<bang> <args> %:p:.:h:S
autocmd vimrc User Plug_asyncdo_vim nnoremap <Leader>gg <Cmd>Grep -Fwe '<cword>'<CR>
autocmd vimrc User Plug_asyncdo_vim nnoremap <Leader>gG <Cmd>RGrep -Fwe '<cword>'<CR>

if executable('rg') && executable('igrep-format')
    autocmd vimrc User Plug_fzf nnoremap <Leader>fg <Cmd>IGrep<CR>
    autocmd vimrc User Plug_fzf nnoremap <Leader>fG <Cmd>IGrep -- %:p:.:h:S<CR>
    autocmd vimrc User Plug_fzf command! -nargs=* -complete=file IGrep call s:IGrep(<q-args>)
    function! s:IGrep(args) abort
        let l:cmd = 'rg --null --column --color=ansi --smart-case --regexp {q} '.a:args.
            \ ' | igrep-format '.&columns
        call s:CustomFzf(substitute(l:cmd, '{q}', "''", ''), [
            \ '--with-nth=-1..',
            \ '--delimiter=\0',
            \ '--ansi',
            \ '--layout=reverse-list',
            \ '--disabled',
            \ '--bind=change:top+reload:'.l:cmd,
        \ ], function("\<SID>ParseIGrep"))
    endfunction
    function! s:ParseIGrep(line) abort
        let [l:file, l:line, l:col; l:text] = split(a:line, '\n')[:-2]
        return #{
            \ filename: l:file,
            \ lnum: str2nr(l:line),
            \ col: str2nr(l:col),
            \ text: join(l:text, "\e"),
        \ }
    endfunction
endif

Plug 'tpope/vim-eunuch'

" File navigation {{{1

Plug 'justinmk/vim-dirvish'
set suffixes-=.h
autocmd vimrc ColorScheme * highlight link DirvishSuffix Comment
autocmd vimrc ColorScheme * highlight link DirvishPathHead NonText
if $RANGER_LEVEL
    autocmd vimrc User Plug_vim_dirvish nmap <expr> -
        \ !v:count && len(getbufinfo(#{buflisted: v:true})) * winnr('$') * tabpagenr('$') == 1 ? '<C-W>q' : '<Plug>(dirvish_up)'
endif

Plug 'junegunn/fzf', #{do: './install --bin'}
let g:fzf_action = {
    \ '': 'edit',
    \ 'ctrl-x': 'split',
    \ 'ctrl-v': 'vsplit',
    \ 'ctrl-t': 'tab split',
\ }
let g:fzf_layout = #{
    \ window: #{
        \ width: 1,
        \ height: 0.25,
        \ yoffset: 1,
        \ border: 'top',
    \ },
\ }

autocmd vimrc User Plug_fzf nnoremap <expr> <Leader>ff
    \ '<Cmd>FZF '.fnameescape(fnamemodify(getcwd(), ':~')).'<CR>'
autocmd vimrc User Plug_fzf nnoremap <expr> <Leader>fF
    \ '<Cmd>FZF '.fnameescape(expand('%:p:~:h')).'<CR>'
autocmd vimrc User Plug_fzf nnoremap <Leader>fb <Cmd>call <SID>CustomFzf(<SID>ListBuffers(), [
    \ '--prompt='.substitute(fnamemodify(getcwd(), ':~'), '/\?$', '/', ''),
    \ '--bind=ctrl-c:reload:nvim --server '.shellescape(v:servername).' --remote-expr "DeleteBuffer(''$(printf %s {} \| sed "s/''/''''/g")'')"',
\ ], {l -> #{filename: l}})<CR>
function! s:ListBuffers() abort
    return map(filter(getbufinfo(#{buflisted: v:true}),
        \ {i, b -> !empty(b.name)}), {i, b -> fnamemodify(b.name, ':~:.')})
endfunction
function! DeleteBuffer(name) abort
    let l:buf = bufadd(a:name)
    if bufwinnr(l:buf) == -1
        silent! execute 'bdelete' l:buf
    endif
    return join(s:ListBuffers(), "\n")
endfunction

" Quickfix {{{1

autocmd vimrc FileType qf call s:InitQuickfixBuffer()
function! s:InitQuickfixBuffer() abort
    if exists('w:added_qf_matches')
        return
    endif
    call matchadd('String', s:match_start.'.\{-}'.s:match_end)
    call matchadd('Conceal', '\e\[\d*m')
    setlocal conceallevel=2 concealcursor=nvc
    setlocal nolist
    let w:added_qf_matches = v:true
endfunction

autocmd vimrc QuickFixCmdPost [^l]* call s:OpenQuickfix('window')
function! s:OpenQuickfix(cmd) abort
    let l:win = win_getid()
    execute 'botright c'.a:cmd
    call win_gotoid(l:win)
endfunction

nnoremap <Leader>tq <Cmd>call <SID>ToggleQuickfix()<CR>
function! s:ToggleQuickfix() abort
    if empty(filter(getwininfo(), {i, w -> w.quickfix && !w.loclist && w.tabnr == tabpagenr()}))
        call s:OpenQuickfix('open')
    else
        cclose
    endif
endfunction

let g:fzf_action['ctrl-q'] = {l -> s:SetQuickfix(map(l, {i, l -> #{filename: l, valid: v:true}}))}

function! s:SetQuickfix(items) abort
    call setqflist(a:items)
    call s:OpenQuickfix('window')
    cfirst
endfunction

autocmd vimrc User Plug_fzf nnoremap <Leader>fq
    \ <Cmd>cclose \| call <SID>FzfFromQuickfix([], getqflist())<CR>
function! s:FzfFromQuickfix(options, items) abort
    let l:valid_items = []
    function! s:ProcessItems(items) abort closure
        let l:valid_items = []
        let l:lines = []
        for l:item in a:items
            if !get(l:item, 'valid', v:true)
                continue
            endif
            let l:left = trim(substitute(l:item.text, "\t", ' ', 'g'), ' ')
            if !empty(get(l:item, 'type', ''))
                let l:left = s:match_start.'['.(l:item.type).'] '.s:match_end.l:left
            endif
            let l:file = has_key(l:item, 'filename') ? l:item.filename : bufname(l:item.bufnr)
            let l:right = "\e[90m".fnamemodify(l:file, ':p:~:.').':'.(l:item.lnum)."\e[0m"
            let l:pad = &columns - 3 - strwidth(substitute(l:left.l:right, '\e\[\d*m', '', 'g'))
            call add(l:lines, len(l:valid_items).' '.l:left.repeat(' ', max([l:pad, 1])).l:right)
            call add(l:valid_items, l:item)
        endfor
        return l:lines
    endfunction
    call s:CustomFzf(s:ProcessItems(a:items), extend([
        \ '--with-nth=2..',
        \ '--delimiter= ',
        \ '--ansi',
        \ '--layout=reverse-list',
        \ '--tiebreak=begin',
    \ ], a:options), {l -> l:valid_items[split(l)[0]]})
    return funcref("\<SID>ProcessItems")
endfunction

function! s:CustomFzf(source, options, parse) abort
    function! s:FzfSink(lines) abort closure
        let l:key = remove(a:lines, 0)
        if l:key ==# 'ctrl-q'
            call s:SetQuickfix(map(a:lines, {i, l -> a:parse(l)}))
        else
            for l:line in a:lines
                if empty(l:line)
                    continue
                endif
                let l:item = a:parse(l:line)
                let l:buf = has_key(l:item, 'bufnr') ? l:item.bufnr : bufadd(l:item.filename)
                let l:lnum = get(l:item, 'lnum', 0)
                if empty(l:key) && l:buf == bufnr()
                    if l:lnum
                        normal! m'
                    endif
                else
                    execute g:fzf_action[l:key] '#'.l:buf
                endif
                if l:lnum
                    call cursor(l:lnum, get(l:item, 'col', 1))
                endif
            endfor
        endif
    endfunction
    call fzf#run(fzf#wrap(#{
        \ source: a:source,
        \ sinklist: funcref("\<SID>FzfSink"),
        \ options: extend([
            \ '--multi',
            \ '--expect=ctrl-x,ctrl-v,ctrl-t,ctrl-q',
        \ ], a:options),
    \ }))
endfunction

" Git {{{1

Plug 'tpope/vim-fugitive'

Plug 'junegunn/gv.vim'
autocmd vimrc FileType GV setlocal nolist

autocmd vimrc User Plug_vim_fugitive nnoremap <Leader>tg <Cmd>call <SID>ToggleGitStatus()<CR>
function! s:ToggleGitStatus() abort
    let l:buf = filter(getbufinfo(), {i, b -> get(b.variables, 'fugitive_type', '') ==# 'index'})
    if !empty(l:buf)
        execute 'bdelete' l:buf[0].bufnr
        return
    elseif !exists(':Git')
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
    elseif exists(':Gdiffsplit')
        Gdiffsplit!
    endif
endfunction

autocmd vimrc User Plug_vim_fugitive autocmd vimrc SourcePost fugitive.vim call s:OverrideWorkTree()
function! s:OverrideWorkTree() abort
    let l:WorkTree = funcref('FugitiveWorkTree')
    function! FugitiveWorkTree(...) abort closure
        if exists('$GIT_WORK_TREE')
            return $GIT_WORK_TREE
        else
            return call(l:WorkTree, a:000)
        endif
    endfunction
endfunction

" Search and completion {{{1

autocmd vimrc User Plug_fzf nnoremap <Leader>f/ <Cmd>call <SID>FzfFromQuickfix([],
    \ map(getbufline('%', 1, '$'), {i, l -> #{bufnr: bufnr(), lnum: i + 1, text: l}}))<CR>

" Filetypes {{{1

autocmd vimrc FileType c,cpp setlocal commentstring=//%s
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oh <Cmd>edit %:r.h<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oH <Cmd>edit %:r.hpp<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oc <Cmd>edit %:r.c<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>oC <Cmd>edit %:r.cpp<CR>

if executable('cargo')
    autocmd vimrc FileType rust compiler cargo
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType rust
        \ nnoremap <buffer> <Leader>mm <Cmd>silent update \| Make build<CR>
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType rust
        \ nnoremap <buffer> <Leader>mr <Cmd>silent update \| Make build --release<CR>
endif

autocmd vimrc FileType mail,markdown,tex setlocal spell

autocmd vimrc FileType dot setlocal commentstring=//%s
if executable('dot')
    autocmd vimrc FileType dot
        \ let &l:makeprg = 'dot -T$* -o'.expand('%:p:r:S').'.$* '.expand('%:p:S')
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType dot
        \ nnoremap <buffer> <Leader>mm <Cmd>silent update \| Make png<CR>
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType dot
        \ nnoremap <buffer> <Leader>ms <Cmd>silent update \| Make svg<CR>
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

call s:InitPlugins()
