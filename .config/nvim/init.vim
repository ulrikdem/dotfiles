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
        if exists('g:colors_name')
            silent execute 'doautocmd vimrc ColorScheme' g:colors_name
        endif
    endfunction
catch
    command! -nargs=+ Plug

    function! s:InitPlugins() abort
    endfunction
endtry

" Misc settings {{{1

set clipboard=unnamed
set cursorline
set linebreak breakindent showbreak=↪
set list listchars=tab:→\ ,trail:·,nbsp:·
set mouse=a
set scrolloff=4
set shortmess+=IA
set splitbelow splitright
set title titlestring=%F\ -\ nvim titlelen=0
set updatetime=100

set fillchars=foldopen:▾,foldclose:▸ foldcolumn=auto:9 foldtext=FoldText()
function! FoldText() abort
    return repeat('·', v:foldlevel * 2).substitute(trim(foldtext()), '.\{-}:', '', '').
        \ ' ('.(v:foldend - v:foldstart + 1).' lines) '
endfunction

command! -nargs=1 -complete=file Source -1 tabnew | source <args> | bwipeout

autocmd vimrc FocusGained,BufEnter,QuickFixCmdPost * checktime
autocmd vimrc VimResized * wincmd =

autocmd vimrc BufEnter * call s:ResizeHelp()
function! s:ResizeHelp() abort
    if exists('w:checked_help')
        return
    endif
    let w:checked_help = v:true
    if &buftype ==# 'help'
        wincmd L
        80 wincmd |
        set winfixwidth
    endif
endfunction

" Misc mappings {{{1

nnoremap <C-S> <Cmd>update<CR>
nnoremap ZT <Cmd>silent only \| quit<CR>

nnoremap <M-Left> <C-W>h
nnoremap <M-Down> <C-W>j
nnoremap <M-Up> <C-W>k
nnoremap <M-Right> <C-W>l

for s:i in range(1, 10)
    execute 'nnoremap <M-'.(s:i % 10).'>' s:i.'gt'
endfor

nmap <C-LeftMouse> <LeftMouse><C-]>
nnoremap <C-RightMouse> <C-O>

noremap! <C-BS> <C-W>
tnoremap <C-BS> <C-W>

let g:mapleader = ' '
set notimeout

Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
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

Plug 'psliwka/vim-smoothie'
let g:smoothie_no_default_mappings = v:true
autocmd vimrc User Plug_vim_smoothie call s:MapSmoothie()
function! s:MapSmoothie() abort
    nnoremap <C-D> <Cmd>call smoothie#downwards()<CR>
    nnoremap <C-U> <Cmd>call smoothie#upwards()<CR>
    xnoremap <C-D> <Cmd>call smoothie#downwards()<CR>
    xnoremap <C-U> <Cmd>call smoothie#upwards()<CR>
endfunction

map <expr> <M-r> <SID>ShowRelativeNumber()
function! s:ShowRelativeNumber() abort
    setlocal relativenumber
    redraw
    let l:char = getchar()
    setlocal norelativenumber
    return type(l:char) == v:t_number ? nr2char(l:char) : l:char
endfunction

nmap <expr> <M-[> <SID>StartBracketRepeat('[')
nmap <expr> <M-]> <SID>StartBracketRepeat(']')
function! s:StartBracketRepeat(bracket) abort
    let g:fake_mode = a:bracket.'-REPEAT'
    redrawstatus
    return a:bracket."\<Char-0xA0>"
endfunction

nnoremap [<Char-0xA0><Esc> <Cmd>unlet g:fake_mode \| redrawstatus<CR>
nnoremap ]<Char-0xA0><Esc> <Cmd>unlet g:fake_mode \| redrawstatus<CR>
nmap <expr> [<Char-0xA0> <SID>RepeatBracket('[', '<M-[>')
nmap <expr> ]<Char-0xA0> <SID>RepeatBracket(']', '<M-]>')
function! s:RepeatBracket(bracket, restart) abort
    let l:char = getchar()
    let l:char = type(l:char) == v:t_number ? nr2char(l:char) : l:char
    if l:char == "\<M-[>" || l:char == "\<M-]>"
        return l:char
    elseif l:char =~ '^\d$'
        return l:char.a:restart
    endif
    unlet g:fake_mode
    redrawstatus
    return a:bracket.l:char.'zz'.a:restart
endfunction

" Formatting {{{1

set expandtab tabstop=4 shiftwidth=0
Plug 'tpope/vim-sleuth'

if has('python3') && filereadable('/usr/share/clang/clang-format.py')
    command! -range=% ClangFormat call ClangFormat('<line1>:<line2>')
    function! ClangFormat(...) abort
        let l:lines = a:0 ? a:1 : v:lnum.':'.(v:lnum + v:count - 1)
        py3file /usr/share/clang/clang-format.py
    endfunction
endif

Plug 'godlygeek/tabular'

" Colorscheme {{{1

autocmd vimrc ColorScheme * call s:UpdateColorScheme()
function! s:UpdateColorScheme() abort
    highlight DiffDelete ctermfg=52 ctermbg=52 guifg=#5F0000 guibg=#5F0000
    highlight DiffAdd ctermfg=NONE ctermbg=22 guifg=NONE guibg=#005F00 guisp=#008700
    highlight DiffChange ctermfg=NONE ctermbg=17 guifg=NONE guibg=#00005F guisp=#0000AF
    highlight DiffText ctermfg=NONE ctermbg=19 guifg=NONE guibg=#0000AF guisp=#0000AF
    highlight manBold cterm=bold gui=bold
    highlight manItalic cterm=italic gui=italic
    highlight link manUnderline manItalic
    highlight link manTitle manSectionHeading
    highlight link manOptionDesc NONE
endfunction

Plug 'srcery-colors/srcery-vim'
let g:srcery_bg_passthrough = v:true
let g:srcery_italic = v:true
autocmd vimrc User Plug_srcery_vim colorscheme srcery

autocmd vimrc ColorScheme srcery call s:UpdateSrceryColorScheme()
function! s:UpdateSrceryColorScheme() abort
    execute 'highlight Normal guibg='.synIDattr(hlID('SrceryBlack'), 'fg', 'gui')
    call s:Highlight('StatusLine', #{bg: 'SrceryXgray5'})
    call s:Highlight('StatusLineNC', #{bg: 'SrceryXgray5', attr: 'NONE'})
    call s:Highlight('CursorLine', #{bg: 'SrceryXgray1'})
    call s:Highlight('CursorLineNr', #{bg: 'SrceryXgray1'})
    call s:Highlight('TermCursor', #{fg: 'SrceryBlack', bg: 'Normal', attr: 'NONE'})
    call s:Highlight('CocErrorVirtualText', #{bg: 'SrceryXgray1'})
    call s:Highlight('CocWarningVirtualText', #{bg: 'SrceryXgray1'})
    call s:Highlight('CocInfoVirtualText', #{bg: 'SrceryXgray1'})
    call s:Highlight('CocHintVirtualText', #{bg: 'SrceryXgray1'})
    call s:Highlight('Folded', #{attr: 'NONE'})
    call s:Highlight('Visual', #{bg: 'SrceryXgray4', attr: 'NONE'})
    call s:Highlight('Search', #{fg: 'SrceryBlack', bg: 'SrceryBrightYellow', attr: 'NONE'})
    call s:Highlight('IncSearch', #{fg: 'SrceryBlack', bg: 'SrceryYellow', attr: 'NONE'})
    call s:Highlight('PmenuThumb', #{bg: 'SrceryXgray5'})
    highlight! link PmenuSbar Pmenu
    highlight! link Operator SrceryBrightWhite
    highlight! link vimUserFunc SrceryBrightWhite
    highlight! link QuickFixLine Visual
    highlight! link Directory SrceryGreen
    highlight! link Error SrceryRedBold
    highlight! link ErrorMsg SrceryRedBold
    highlight! link WarningMsg SrceryRed
    highlight! link SpellBad CocErrorHighlight
    highlight! link SpellLocal CocWarningHighlight
    highlight! link SpellCap CocInfoHighlight
    highlight! link SpellRare CocHintHighlight
endfunction

function! s:Highlight(group, args) abort
    if has_key(a:args, 'fg')
        let l:id = synIDtrans(hlID(a:args.fg))
        execute 'highlight' a:group
            \ 'ctermfg='.synIDattr(l:id, 'fg', 'cterm') 'guifg='.synIDattr(l:id, 'fg', 'gui')
    endif
    if has_key(a:args, 'bg')
        let l:id = synIDtrans(hlID(a:args.bg))
        execute 'highlight' a:group
            \ 'ctermbg='.synIDattr(l:id, 'fg', 'cterm') 'guibg='.synIDattr(l:id, 'fg', 'gui')
    endif
    if has_key(a:args, 'attr')
        execute 'highlight' a:group 'cterm='.(a:args.attr) 'gui='.(a:args.attr)
    endif
endfunction

function! s:GetSrceryColors(fg, bg) abort
    return [
        \ synIDattr(hlID('Srcery'.a:fg), 'fg', 'gui'),
        \ synIDattr(hlID('Srcery'.a:bg), 'fg', 'gui'),
        \ synIDattr(hlID('Srcery'.a:fg), 'fg', 'cterm'),
        \ synIDattr(hlID('Srcery'.a:bg), 'fg', 'cterm'),
    \ ]
endfunction

" Statusline {{{1

Plug 'itchyny/lightline.vim'
let g:lightline = #{
    \ active: #{
        \ left: [['mode'], ['filename'], ['truncate']],
        \ right: [['ruler'], ['fileformat', 'fileencoding', 'filetype'], []],
    \ },
    \ inactive: #{
        \ left: [['filename'], ['truncate', 'space']],
        \ right: [['ruler']],
    \ },
    \ component: #{
        \ filename: '
            \%{substitute(expand("%:p:~"), ''\v(.*/)?(.*/)$|.*'', ''\2'', "")}%t
            \%{&modified ? " •" : ""}',
        \ truncate: '%<',
        \ space: ' ',
        \ ruler: '%p%% %l:%v',
    \ },
    \ component_visible_condition: #{
        \ truncate: 'v:false',
    \ },
    \ component_function: #{
        \ mode: 'StatusLineMode',
        \ fileformat: 'StatusLineFileFormat',
        \ fileencoding: 'StatusLineFileEncoding',
        \ filetype: 'StatusLineFileType',
    \ },
    \ component_expand: {},
    \ component_type: {},
    \ tabline: #{
        \ left: [['tabs']],
        \ right: [],
    \ },
    \ tab: #{
        \ active: ['tab'],
        \ inactive: ['tab'],
    \ },
    \ tab_component_function: #{
        \ tab: 'TabLabel',
    \ },
    \ _mode_: #{f: 'fake'},
\ }

if $TERM ==# 'linux' || exists('$NO_POWERLINE')
    let g:lightline.subseparator = #{left: '│', right: '│'}
else
    let g:lightline.separator = #{left: '', right: ''}
    let g:lightline.subseparator = #{left: '', right: ''}
endif

function! StatusLineMode() abort
    if exists('g:fake_mode')
        call lightline#link('f')
    endif
    let l:mode = get(g:, 'fake_mode', lightline#mode())
    return s:NarrowWindow() ? l:mode[0] : l:mode
endfunction

function! StatusLineFileFormat() abort
    return s:NarrowWindow() || &fileformat ==# 'unix' ? '' : &fileformat
endfunction
function! StatusLineFileEncoding() abort
    return s:NarrowWindow() || &fileencoding ==# 'utf-8' ? '' : &fileencoding
endfunction
function! StatusLineFileType() abort
    return s:NarrowWindow() ? '' : &filetype
endfunction

function! s:NarrowWindow() abort
    return winwidth(0) < 80
endfunction

function! TabLabel(tab) abort
    let l:win = tabpagewinnr(a:tab)
    let l:buf = tabpagebuflist(a:tab)[l:win - 1]
    let l:name = substitute(expand('#'.l:buf.':p:~'), '.*/\ze.', '', '')
    let l:type = getbufvar(l:buf, '&buftype')
    if l:type ==# 'terminal'
        let l:name = substitute(l:name, '.*#\ze.', '', '')
    elseif l:type ==# 'quickfix'
        let l:name = getwininfo(win_getid(l:win, a:tab))[0].loclist ?
            \ '[Location List]' : '[Quickfix List]'
    endif
    return a:tab.': '.(empty(l:name) ? '[No Name]' : l:name)
endfunction

autocmd vimrc User Plug_lightline_vim set noshowmode
autocmd vimrc User Plug_lightline_vim autocmd vimrc QuickFixCmdPost * call lightline#update()

autocmd vimrc User Plug_lightline_vim autocmd vimrc ColorScheme srcery call s:UpdateLightlineColors()
function! s:UpdateLightlineColors() abort
    let l:common = s:GetSrceryColors('BrightWhite', 'Xgray5')
    let l:palette = #{
        \ normal: #{
            \ middle: [s:GetSrceryColors('BrightWhite', 'Xgray3')],
            \ error: [s:GetSrceryColors('Black', 'Red')],
            \ warning: [s:GetSrceryColors('Black', 'BrightOrange')],
        \ },
        \ inactive: #{
            \ left: [l:common],
            \ right: [l:common],
        \ },
        \ tabline: #{
            \ left: [l:common],
            \ tabsel: [s:GetSrceryColors('Black', 'Blue')],
        \ },
    \ }
    for [l:mode, l:color] in [
        \ ['normal', 'Green'],
        \ ['insert', 'Blue'],
        \ ['replace', 'Red'],
        \ ['visual', 'Magenta'],
        \ ['command', 'Orange'],
        \ ['fake', 'Yellow'],
    \ ]
        let l:palette[l:mode] = get(l:palette, l:mode, {})
        let l:palette[l:mode].left = [s:GetSrceryColors('Black', l:color), l:common]
        let l:palette[l:mode].right = l:palette[l:mode].left
    endfor
    let g:lightline#colorscheme#custom#palette = l:palette
    let g:lightline.colorscheme = 'custom'
    call lightline#init()
    call lightline#colorscheme()
    call lightline#update()
endfunction

" Command execution {{{1

if executable('rg')
    set grepprg=rg\ --column\ --color=ansi
    set grepformat=[0m[35m%f[0m:[0m[32m%l[0m:[0m%c[0m:%m
else
    set grepprg=grep\ -rn
endif

let s:match_start = "\e[31m"
let s:match_end = "\e[0m"

Plug 'hauleth/asyncdo.vim'
autocmd vimrc User Plug_asyncdo_vim nnoremap <C-C> <Cmd>AsyncStop<CR>

call add(g:lightline.active.right[2], 'asyncdo')
let g:lightline.component.asyncdo = '%{exists("g:asyncdo") ? split(g:asyncdo.cmd)[0]."…" : ""}'
let g:lightline.component_visible_condition.asyncdo = 'exists("g:asyncdo")'

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
            \ '--phony',
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

Plug 'lambdalisue/suda.vim'

" Terminal {{{1

autocmd vimrc BufEnter * let &titlestring = (&buftype ==# 'terminal' ? 'terminal' : '%F').' - nvim'
autocmd vimrc TermOpen * set titlestring=terminal\ -\ nvim

autocmd vimrc TermOpen * setlocal matchpairs= nocursorline
autocmd vimrc TermOpen * startinsert
autocmd vimrc WinEnter * if &buftype ==# 'terminal' | startinsert | endif

tnoremap <M-Esc> <C-\><C-N>

tnoremap <M-Left> <Cmd>wincmd h<CR>
tnoremap <M-Down> <Cmd>wincmd j<CR>
tnoremap <M-Up> <Cmd>wincmd k<CR>
tnoremap <M-Right> <Cmd>wincmd l<CR>

for s:i in range(1, 10)
    execute 'tnoremap <M-'.(s:i % 10).'> <Cmd>tabnext' s:i.'<CR>'
endfor

nnoremap <Leader>ot <Cmd>terminal<CR>
nnoremap <expr> <Leader>oT '<Cmd>edit '.fnameescape('term://'.expand('%:p:h').'//'.&shell).'<CR>'

" File navigation {{{1

Plug 'justinmk/vim-dirvish'
set suffixes-=.h
autocmd vimrc ColorScheme srcery highlight link DirvishArg SrceryOrangeBold
autocmd vimrc ColorScheme srcery highlight link DirvishPathTail SrceryBlue
autocmd vimrc ColorScheme srcery highlight link DirvishSuffix SrceryBrightBlack
highlight link DirvishPathHead NonText
if $RANGER_LEVEL
    autocmd vimrc User Plug_vim_dirvish nmap <expr> -
        \ !v:count && bufnr('$') * winnr('$') * tabpagenr('$') == 1 ? '<C-W>q' : '<Plug>(dirvish_up)'
endif
if executable('xdg-open')
    autocmd vimrc FileType dirvish nnoremap <buffer> <M-CR>
        \ <Cmd>silent execute '!xdg-open' shellescape(getline('.'))<CR>
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
        \ highlight: 'VertSplit',
    \ },
\ }
autocmd vimrc FileType fzf mode

autocmd vimrc User Plug_fzf nnoremap <expr> <Leader>ff
    \ '<Cmd>FZF '.fnameescape(fnamemodify(getcwd(), ':~')).'<CR>'
autocmd vimrc User Plug_fzf nnoremap <expr> <Leader>fF
    \ '<Cmd>FZF '.fnameescape(expand('%:p:~:h')).'<CR>'
autocmd vimrc User Plug_fzf nnoremap <Leader>fb <Cmd>call <SID>CustomFzf(<SID>ListBuffers(), extend([
    \ '--prompt='.substitute(fnamemodify(getcwd(), ':~'), '/\?$', '/', ''),
\ ], executable('nvr') ? [
    \ '--bind=ctrl-c:reload:nvr --remote-expr "DeleteBuffer(''$(printf %s {} \| sed "s/''/''''/g")'')"',
\ ] : []), {l -> #{filename: l}})<CR>
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

nnoremap <M-o> <Cmd>call <SID>BufferJump(-1, '<C-O>')<CR>
nnoremap <M-i> <Cmd>call <SID>BufferJump(1, '<C-I>')<CR>
function! s:BufferJump(dir, key) abort
    let [l:jumps, l:current] = getjumplist()
    let l:i = l:current + a:dir
    let l:buf = bufnr()
    for l:j in range(v:count1)
        while get(l:jumps, l:i, #{bufnr: -1}).bufnr == l:buf
            let l:i += a:dir
        endwhile
        let l:buf = get(l:jumps, l:i, #{bufnr: -2}).bufnr
    endfor
    while get(l:jumps, l:i + 1, #{bufnr: -1}).bufnr == l:buf
        let l:i += 1
    endwhile
    if l:i >= 0 && l:i < len(l:jumps)
        execute 'normal' ((l:i - l:current) * a:dir).a:key
    endif
endfunction

" Quickfix {{{1

autocmd vimrc FileType qf call s:InitQuickfixBuffer()
function! s:InitQuickfixBuffer() abort
    if exists('w:added_qf_matches')
        return
    endif
    call matchadd('QuickFixMatch', s:match_start.'.\{-}'.s:match_end)
    call matchadd('Conceal', '\e\[\d*m')
    setlocal conceallevel=2 concealcursor=nvc
    setlocal nolist
    let w:added_qf_matches = v:true
endfunction
autocmd vimrc ColorScheme * highlight QuickFixMatch ctermfg=Red guifg=Red
autocmd vimrc ColorScheme srcery highlight! link QuickFixMatch SrceryRed

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
    wincmd L
    60 wincmd |
    set winfixwidth
    wincmd =
endfunction

autocmd vimrc User Plug_vim_fugitive autocmd vimrc FileType gitcommit wincmd L

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

set diffopt+=vertical,foldcolumn:1,algorithm:histogram,linematch:60,hiddenoff
nmap [h [c
nmap ]h ]c
nnoremap dx dp

call insert(g:lightline.active.left[1], 'git')
call insert(g:lightline.inactive.left[0], 'git')
let g:lightline.component_function.git = 'StatusLineGit'
function! StatusLineGit() abort
    if !exists('*FugitiveStatusline')
        return ''
    endif
    let l:prefix = ' '
    let l:status = FugitiveStatusline()
    if l:status =~# '\[Git:.*(.*)\]'
        return substitute(l:status, '\[Git:\(.*\)(.*)\]', l:prefix.'\1', '')
    else
        return substitute(l:status, '\[Git(\(.*\))\]', l:prefix.'\1', '')
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

set ignorecase smartcase
Plug 'pgdouyon/vim-evanesco'

set wildignorecase wildmode=longest:full,full wildcharm=<Tab>
cnoremap <expr> /
    \ pumvisible() && getcmdpos() > 1 && getcmdline()[getcmdpos() - 2] == '/' ? '<C-Y>' : '/'

autocmd vimrc User Plug_fzf nnoremap <Leader>f/ <Cmd>call <SID>FzfFromQuickfix([],
    \ map(getbufline('%', 1, '$'), {i, l -> #{bufnr: bufnr(), lnum: i + 1, text: l}}))<CR>

set dictionary=/usr/share/dict/words
set completeopt=menuone,noselect,noinsert shortmess+=c

set complete-=t
let s:start_completion = "\<C-N>"
inoremap <silent><expr> <Tab> <SID>TabMap('<Tab>', '<C-N>')
inoremap <silent><expr> <S-Tab> <SID>TabMap('<S-Tab>', '<C-P>')
function! s:TabMap(tab, key) abort
    return pumvisible() ? empty(reg_recording()) ? a:key : '' :
        \ col('.') <= 1 || getline('.')[col('.') - 2] =~ '\s' ? a:tab : s:start_completion
endfunction

function! s:CompletionFallback() abort
    if has('python3')
        Plug 'roxma/nvim-yarp'
        Plug 'ncm2/ncm2'
        Plug 'ncm2/ncm2-path'
        Plug 'ncm2/ncm2-bufword'
        Plug 'fgrsnau/ncm2-otherbuf'
        let g:ncm2#complete_length = 2
        autocmd vimrc User Plug_ncm2 autocmd vimrc BufEnter * call ncm2#enable_for_buffer()
        autocmd vimrc User Plug_ncm2 let s:start_completion = "\<C-R>=ncm2#force_trigger()\<CR>"
    else
        Plug 'lifepillar/vim-mucomplete'
        let g:mucomplete#enable_auto_at_startup = v:true
        let g:mucomplete#minimum_prefix_length = v:true
        let g:mucomplete#buffer_relative_paths = v:true
        let g:mucomplete#chains = #{
            \ default: ['path', 'omni', 'keyn'],
            \ vim: ['path', 'cmd', 'keyn'],
        \ }
        autocmd vimrc User Plug_vim_mucomplete iunmap <Tab>
        autocmd vimrc User Plug_vim_mucomplete iunmap <S-Tab>
    endif
    inoremap <expr> <CR> pumvisible() ? '<C-Y><CR>' : '<CR>'
endfunction

" Language client {{{1

if executable('node')
    function! PatchCoc(...) abort
        let l:dir = g:plugs['coc.nvim'].dir
        let l:float = '/autoload/coc/float.vim'
        call s:PatchFile(stdpath('config').l:float, readfile(l:dir.l:float), [
            \ ['call feedkeys("\\<C-g>u", ''n'')', ''],
            \ ['let bufnr = a:bufnr\zs', ' | call setbufvar(bufnr, "\&modifiable", v:true)'],
        \ ])
        call s:PatchFile(l:dir.'/bin/server.js', readfile(l:dir.'/build/index.js'), [
            \ ['score = \zs\l\+ == [a-z[\]]\+ ? \([0-9.]\+\) : [0-9.]\+', '\1'],
        \ ])
        call s:PatchFile(l:dir.'/lib/attach.js', [], [])
    endfunction
    Plug 'neoclide/coc.nvim', #{tag: 'v0.0.81', do: function('PatchCoc')}
else
    call s:CompletionFallback()
endif

function! s:PatchFile(path, lines, subs) abort
    let l:lines = a:lines
    for [l:pattern, l:sub] in a:subs
        let l:lines = map(l:lines, {i, s -> substitute(s, l:pattern, l:sub, '')})
    endfor
    call mkdir(fnamemodify(a:path, ':h'), 'p')
    call writefile(l:lines, a:path)
endfunction

let g:coc_user_config = #{
    \ coc: #{
        \ source: #{
            \ around: #{firstMatch: v:false, priority: 2},
            \ buffer: #{firstMatch: v:false},
        \ },
    \ },
    \ signature: #{
        \ floatMaxWidth: 80,
        \ maxWindowHeight: 100,
    \ },
    \ diagnostic: #{
        \ maxWindowHeight: 100,
        \ checkCurrentLine: v:true,
        \ format: '%message',
        \ enableMessage: 'never',
        \ virtualText: v:true,
        \ virtualTextLines: 1,
        \ separateRelatedInformationAsDiagnostics: v:true,
        \ enableHighlightLineNumber: v:false,
        \ errorSign: '✖',
        \ warningSign: '⚠',
        \ infoSign: 'ℹ',
        \ hintSign: 'ℹ',
    \ },
\ }
autocmd vimrc User CocNvimInit call coc#config('suggest', #{
    \ invalidInsertCharacters: split(' (/:;<', '\zs'),
    \ snippetIndicator: '',
    \ detailField: 'preview',
    \ maxCompleteItemCount: 1000,
\ })

autocmd vimrc ColorScheme * call s:UpdateCocColors()
function! s:UpdateCocColors() abort
    for l:level in ['Error', 'Warning', 'Info', 'Hint']
        call s:Highlight('Coc'.l:level.'VirtualText', #{fg: 'Coc'.l:level.'Float', attr: 'italic'})
    endfor
    highlight link CocHighlightText CocBold
    highlight link CocUnderline CocBold
    highlight link CocHoverRange NONE
endfunction

autocmd vimrc User Plug_coc_nvim call s:InitLsp()
function! s:InitLsp() abort
    let s:start_completion = coc#refresh()
    inoremap <CR> <C-G>u<CR>

    let s:lsp_filetypes = []
    for l:ls in values(g:coc_user_config.languageserver)
        call extend(s:lsp_filetypes, l:ls.filetypes)
    endfor
    autocmd vimrc FileType * call s:InitLspBuffer()
endfunction

function! s:InitLspBuffer() abort
    if index(s:lsp_filetypes, &filetype) == -1 || @% =~ '^\a\+://'
        return
    endif

    setlocal signcolumn=yes

    nnoremap <buffer> ]g <Cmd>call CocActionAsync('diagnosticNext')<CR>
    nnoremap <buffer> [g <Cmd>call CocActionAsync('diagnosticPrevious')<CR>
    nmap <buffer> <Leader>ge
        \ <Cmd>call CocActionAsync('diagnosticInfo', function('<SID>FocusFloat'))<CR>
    nnoremap <buffer> <Leader>gE
        \ <Cmd>call CocActionAsync('diagnosticList', {e, d -> <SID>SetQuickfix(map(d, {i, d -> #{
            \ filename: fnamemodify(d.file, ':p:~:.'),
            \ lnum: d.lnum,
            \ col: d.col,
            \ type: d.severity[0],
            \ text: d.message,
        \ }}))})<CR>

    nnoremap <buffer> <Leader>gh <Cmd>call CocActionAsync('doHover', function('<SID>FocusFloat'))<CR>
    nmap <buffer> <M-LeftMouse> <LeftMouse><Leader>gh

    nnoremap <buffer> <C-]> <Cmd>call CocActionAsync('jumpDefinition')<CR>
    nnoremap <buffer> <Leader>gd <Cmd>call CocActionAsync('jumpDeclaration')<CR>
    nnoremap <buffer> <Leader>gt <Cmd>call CocActionAsync('jumpTypeDefinition')<CR>
    nnoremap <buffer> <Leader>gi <Cmd>call CocActionAsync('jumpImplementation')<CR>
    nnoremap <buffer> <Leader>gr <Cmd>call CocActionAsync('jumpReferences')<CR>

    nnoremap <buffer> <Leader>gy <Cmd>call v:lua.lsp_symbols.jump(v:count1)<CR>

    onoremap <buffer> iy <Cmd>call v:lua.lsp_symbols.select('v', v:count1)<CR>
    onoremap <buffer> ay <Cmd>call v:lua.lsp_symbols.select('V', v:count1)<CR>
    xnoremap <buffer><silent> iy :<C-U>call v:lua.lsp_symbols.select('v', v:count1)<CR>
    xnoremap <buffer><silent> ay :<C-U>call v:lua.lsp_symbols.select('V', v:count1)<CR>

    if isdirectory(g:plugs.fzf.dir)
        nnoremap <buffer> <Leader>gs <Cmd>call CocActionAsync('documentSymbols',
            \ {e, s -> <SID>FzfFromQuickfix([], map(s, {i, s -> <SID>SymbolToQuickfix(s)}))})<CR>
        if executable('nvr')
            nnoremap <buffer> <Leader>gS <Cmd>call <SID>FzfFromWorkspaceSymbols()<CR>
        endif
    endif

    autocmd CursorHold <buffer> call CocActionAsync('highlight')

    nmap <buffer> <Leader>gR <Cmd>call CocActionAsync('rename')<CR>

    nmap <buffer> <Leader>gA <Cmd>call CocActionAsync('codeAction')<CR>
    nmap <buffer> <Leader>gaa <Cmd>call CocActionAsync('codeAction', 'n')<CR>
    nmap <buffer> <Leader>ga <Cmd>set operatorfunc=<SID>CodeActionOperatorFunc<CR>g@
    xmap <buffer> ga <Esc><Cmd>call CocActionAsync('codeAction', visualmode())<CR>

    nmap <buffer> <Leader>gq <Cmd>call CocActionAsync('format')<CR>
    setlocal formatexpr=CocActionAsync('formatSelected')
endfunction

function! s:FocusFloat(...) abort
    if !coc#float#has_float()
        return
    endif
    call coc#float#jump()
    setlocal nomodifiable concealcursor=nvc
    nnoremap <buffer> <Esc> <C-W>c
endfunction

function! s:CodeActionOperatorFunc(type) abort
    call CocActionAsync('codeAction', a:type)
endfunction

autocmd vimrc User Plug_fzf let g:coc_enable_locationlist = v:false
autocmd vimrc User Plug_fzf autocmd vimrc User CocLocationsChange ++nested
    \ call s:FzfFromQuickfix([], map(g:coc_jump_locations, function("\<SID>HighlightRange")))
function! s:HighlightRange(index, item) abort
    let l:start = v:lua.vim.str_byteindex(a:item.text, a:item.range.start.character, v:true)
    let l:end = v:lua.vim.str_byteindex(a:item.text, a:item.range.end.character, v:true)
    let a:item.text = (l:start ? a:item.text[:(l:start - 1)] : '').s:match_start.
        \ (a:item.text[(l:start):(l:end - 1)]).s:match_end.(a:item.text[(l:end):])
    return a:item
endfunction

function! s:SymbolToQuickfix(symbol) abort
    let l:line = getbufline(get(a:symbol, 'filename', '%'), a:symbol.lnum)
    return #{
        \ filename: get(a:symbol, 'filename', @%),
        \ lnum: a:symbol.lnum,
        \ col: empty(l:line) ? a:symbol.col :
            \ v:lua.vim.str_byteindex(l:line[0], a:symbol.col - 1, v:true) + 1,
        \ text: (a:symbol.text).s:match_start.' ['.(a:symbol.kind).']'.s:match_end,
    \ }
endfunction

function! s:FzfFromWorkspaceSymbols() abort
    let l:ProcessItems = s:FzfFromQuickfix(['--bind=change:top+reload:
        \nvr --remote-expr "WorkspaceSymbolQuery(''$(printf %s {q} | sed "s/''/''''/g")'')" | tail -c +2'], [])
    let l:last_query = ''
    let l:results = ''
    function! WorkspaceSymbolQuery(query) abort closure
        let l:words = split(a:query)
        if empty(l:words)
            return ''
        endif
        if l:words[0] ==# l:last_query
            return l:results
        endif
        let l:symbols = map(CocAction('getWorkspaceSymbols', l:words[0]), {i, s -> s:SymbolToQuickfix(#{
            \ filename: v:lua.vim.uri_to_fname(s.location.uri),
            \ lnum: s.location.range.start.line + 1,
            \ col: s.location.range.start.character + 1,
            \ text: s.name,
            \ kind: v:lua.vim.lsp.util._get_symbol_kind_name(s.kind),
        \ })})
        let l:last_query = l:words[0]
        let l:results = "\n".join(l:ProcessItems(l:symbols), "\n")."\n"
        return l:results
    endfunction
endfunction

lua lsp_symbols = require("lsp_symbols")
call add(g:lightline.active.left[2], 'lsp_symbol')
let g:lightline.component_function.lsp_symbol = 'StatusLineLspSymbol'
function! StatusLineLspSymbol() abort
    return mode() =~# '^i' || mode() =~# '^R' ? ' ' : v:lua.lsp_symbols.statusline()
endfunction

call insert(g:lightline.active.right[2], 'errors')
call insert(g:lightline.active.right[2], 'warnings')
let g:lightline.component_type.errors = 'error'
let g:lightline.component_type.warnings = 'warning'
let g:lightline.component_expand.errors = 'StatusLineLspErrors'
let g:lightline.component_expand.warnings = 'StatusLineLspWarnings'
function! StatusLineLspErrors() abort
    let l:count = get(b:, 'coc_diagnostic_info', #{error: 0}).error
    return l:count ? l:count.(g:coc_user_config.diagnostic.errorSign) : ''
endfunction
function! StatusLineLspWarnings() abort
    let l:count = get(b:, 'coc_diagnostic_info', #{warning: 0}).warning
    return l:count ? l:count.(g:coc_user_config.diagnostic.warningSign) : ''
endfunction
autocmd vimrc User Plug_lightline_vim autocmd vimrc User CocDiagnosticChange call lightline#update()

" Language server {{{1

let g:coc_user_config.languageserver = {}

if executable('ccls')
    let g:coc_user_config.languageserver.c = #{
        \ command: 'ccls',
        \ filetypes: ['c', 'cpp'],
        \ rootPatterns: ['compile_commands.json', '.ccls'],
        \ initializationOptions: #{
            \ completion: #{
                \ detailedLabel: v:false,
            \ },
        \ },
    \ }

    autocmd vimrc User Plug_coc_nvim autocmd vimrc FileType c,cpp call s:InitCclsBuffer()
    function! s:InitCclsBuffer() abort
        inoremap <buffer><expr> "
            \ pumvisible() && col('.') > 1 && getline('.')[col('.') - 2] == '"' ? '<C-Y>' : '"'
        inoremap <buffer><expr> >
            \ pumvisible() && col('.') > 1 && getline('.')[col('.') - 2] == '>' ? '<C-Y>' : '>'

        if exists('s:ccls_configured')
            return
        endif
        let s:ccls_configured = v:true
        for l:pattern in g:coc_user_config.languageserver.c.rootPatterns
            if !empty(findfile(l:pattern, escape(expand('%:p'), ' ,;').';'))
                return
            endif
        endfor
        call coc#config('languageserver.c.initializationOptions', #{
            \ cache: #{
                \ directory: '',
            \ },
            \ clang: #{
                \ extraArgs: [
                    \ &filetype ==# 'c' ? '-std=c17' : '-std=c++20',
                    \ '-Wall', '-Wextra', '-Wconversion', '-Wno-sign-conversion',
                \ ],
            \ },
        \ })
    endfunction
endif

if executable('pylsp')
    let g:coc_user_config.languageserver.python = #{
        \ command: 'pylsp',
        \ filetypes: ['python'],
    \ }
endif

if executable('rust-analyzer')
    let g:coc_user_config.languageserver.rust = #{
        \ command: 'rust-analyzer',
        \ filetypes: ['rust'],
        \ rootPatterns: ['Cargo.toml'],
        \ settings: #{
            \ rust-analyzer: #{
                \ checkOnSave: #{enable: v:false},
            \ },
        \ },
    \ }
endif

if executable('typescript-language-server')
    let g:coc_user_config.languageserver.typescript = #{
        \ command: 'typescript-language-server',
        \ args: ['--stdio'],
        \ filetypes: ['typescript', 'javascript'],
        \ rootPatterns: ['package.json'],
    \ }
endif

" Filetypes {{{1

Plug 'Shougo/neco-vim'
Plug 'neoclide/coc-neco'

Plug 'sheerun/vim-polyglot'
let g:polyglot_disabled = ['autoindent', 'sensible']

autocmd vimrc User Plug_vim_polyglot autocmd vimrc FileType c,cpp setlocal commentstring=//%s
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

Plug 'lervag/vimtex'
Plug 'neoclide/coc-vimtex'
let g:vimtex_indent_on_ampersands = v:false
let g:vimtex_indent_bib_enabled = v:false
if executable('nvr')
    let g:vimtex_compiler_progname = 'nvr'
endif
if executable('zathura')
    let g:vimtex_view_method = 'zathura'
endif
let g:vimtex_view_use_temp_files = v:true
let g:vimtex_view_forward_search_on_start = v:false
let g:vimtex_fold_enabled = v:true
let g:vimtex_fold_types = {}
for s:key in ['envs', 'env_options', 'items', 'cmd_single', 'cmd_single_opt', 'cmd_multi', 'cmd_addplot']
    let g:vimtex_fold_types[s:key] = #{enabled: v:false}
endfor
autocmd vimrc User Plug_vimtex autocmd vimrc FileType tex,bib call s:InitVimtexBuffer()
function! s:InitVimtexBuffer() abort
    nnoremap <buffer> <Leader>mm <Cmd>silent update \| VimtexCompileSS<CR>
    nnoremap <buffer> <Leader>mc <Cmd>VimtexClean<CR><Cmd>call <SID>CleanTexFiles()<CR>
    nnoremap <buffer> <Leader>mC <Cmd>VimtexClean!<CR><Cmd>call <SID>CleanTexFiles()<CR>
    nmap <buffer> <Leader>mv <Plug>(vimtex-view)
    nmap <buffer> <Leader>tc <Plug>(vimtex-toc-open)
    setlocal foldlevel=9
endfunction
function! s:CleanTexFiles() abort
    for l:ext in ['_vimtex.pdf', '_vimtex.synctex.gz', '.bbl', '.run.xml', '.nav', '.snm', '.vrb']
        call delete(fnamemodify(b:vimtex.tex, ':r').l:ext)
    endfor
endfunction
autocmd vimrc User Plug_tabular autocmd vimrc FileType tex AddTabularPattern! tex /&\|\\\\/
autocmd vimrc User Plug_tabular autocmd vimrc FileType tex nmap <buffer><silent> <Leader>gq vie:Tabularize tex<CR>

Plug 'iamcco/markdown-preview.nvim', #{do: {-> mkdp#util#install()}}
let g:mkdp_page_title = '${name}'
let g:mkdp_auto_close = v:false
autocmd vimrc User Plug_markdown_preview_nvim autocmd vimrc FileType markdown
    \ nnoremap <buffer> <Leader>mv <Cmd>MarkdownPreview<CR>

autocmd vimrc BufNewFile,BufRead *.gv set filetype=dot
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

    autocmd vimrc ColorScheme srcery highlight link debugPC DiffChange
    autocmd vimrc ColorScheme srcery highlight link debugBreakpoint SrceryRedBold
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
            command! -nargs=1 -complete=file Source -1 tabnew | source <args> | bwipeout
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

" Local configuration {{{1

runtime local_init.vim

call s:InitPlugins()
