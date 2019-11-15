" Setup {{{1

" vim: foldmethod=marker foldcolumn=1

augroup vimrc
    autocmd!
augroup END

try
    call plug#begin('~/.local/share/nvim/plugged')

    function! s:init_plugins() abort
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

    function! s:init_plugins() abort
    endfunction
endtry

" Misc settings {{{1

set clipboard=unnamed
set cursorline
set hidden
set linebreak breakindent breakindentopt=shift:8,sbr showbreak=↪
set list listchars=tab:→\ ,trail:␣
set mouse=a
set scrolloff=4
set shortmess+=IA
set splitbelow splitright
set title titlestring=%F\ -\ nvim titlelen=0
set updatetime=100

autocmd vimrc FocusGained,BufEnter,QuickFixCmdPost * checktime
autocmd vimrc VimResized * wincmd =

" Misc mappings {{{1

nnoremap <C-S> <Cmd>update<CR>
nnoremap ZT <Cmd>silent only \| quit<CR>

nnoremap <M-h> <C-W>h
nnoremap <M-j> <C-W>j
nnoremap <M-k> <C-W>k
nnoremap <M-l> <C-W>l

cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <C-F> <Right>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>

let g:mapleader = ' '

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
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

set expandtab tabstop=4 shiftwidth=0
Plug 'tpope/vim-sleuth'

set nojoinspaces

if has('python3') && filereadable('/usr/share/clang/clang-format.py')
    autocmd vimrc FileType c,cpp,java,javascript,typescript
        \ nnoremap <buffer> <Leader>gq <Cmd>call Clang_format('all')<CR>
    autocmd vimrc FileType c,cpp,java,javascript,typescript
        \ set formatexpr=Clang_format((v:lnum).':'.(v:lnum+v:count-1))
    function! Clang_format(lines) abort
        let l:lines = a:lines
        py3file /usr/share/clang/clang-format.py
    endfunction
endif

Plug 'godlygeek/tabular'

" Colorscheme {{{1

set termguicolors

Plug 'srcery-colors/srcery-vim'
let g:srcery_transparent_background = 1
let g:srcery_inverse = 0
let g:srcery_italic = 1
autocmd vimrc User Plug_srcery_vim colorscheme srcery

autocmd vimrc ColorScheme srcery call s:update_colorscheme()
function! s:update_colorscheme() abort
    highlight! link NonText SrceryXgray5
    highlight! link SpecialKey SrceryWhite
    highlight! link SpellBad ALEError
    highlight! link SpellLocal ALEWarning
    highlight! link SpellRare ALEInfo
    highlight! link SpellCap ALEInfo
    highlight link QuickFixLine Visual
    highlight link PmenuSbar Pmenu
    execute 'highlight PmenuThumb'
        \ 'ctermbg='.synIDattr(hlID('SrceryXgray3'), 'fg', 'cterm')
        \ 'guibg='.synIDattr(hlID('SrceryXgray3'), 'fg', 'gui')
    execute 'highlight CursorLineNr'
        \ 'ctermbg='.synIDattr(hlID('CursorLine'), 'bg', 'cterm')
        \ 'guibg='.synIDattr(hlID('CursorLine'), 'bg', 'gui')
    execute 'highlight Visual'
        \ 'cterm=NONE ctermbg='.synIDattr(hlID('SrceryXgray4'), 'fg', 'cterm')
        \ 'gui=NONE guibg='.synIDattr(hlID('SrceryXgray4'), 'fg', 'gui')
    for [l:group, l:color] in [
        \ ['Search', 'SrceryBrightYellow'], ['IncSearch', 'SrceryYellow'],
        \ ['DiffAdd', 'DiffAdd'], ['DiffDelete', 'DiffDelete'],
        \ ['DiffChange', 'DiffChange'], ['DiffText', 'DiffText'],
    \ ]
        execute 'highlight' l:group 'cterm=NONE gui=NONE'
            \ 'ctermfg='.synIDattr(hlID('SrceryBlack'), 'fg', 'cterm')
            \ 'ctermbg='.synIDattr(hlID(l:color), 'fg', 'cterm')
            \ 'guifg='.synIDattr(hlID('SrceryBlack'), 'fg', 'gui')
            \ 'guibg='.synIDattr(hlID(l:color), 'fg', 'gui')
    endfor
endfunction

function! s:get_srcery_colors(fg, bg) abort
    return [
        \ synIDattr(hlID('Srcery'.a:fg), 'fg', 'gui'),
        \ synIDattr(hlID('Srcery'.a:bg), 'fg', 'gui'),
        \ synIDattr(hlID('Srcery'.a:fg), 'fg', 'cterm'),
        \ synIDattr(hlID('Srcery'.a:bg), 'fg', 'cterm'),
    \ ]
endfunction

" Statusline {{{1

Plug 'itchyny/lightline.vim'
let g:lightline = {
    \ 'active': {
        \ 'left': [['mode'], ['git', 'filename']],
        \ 'right': [['ruler'], ['filetype'], ['warnings', 'errors', 'asyncdo']],
    \ },
    \ 'inactive': {
        \ 'left': [['git', 'filename']],
        \ 'right': [['ruler']],
    \ },
    \ 'component': {
        \ 'filename': '
            \%{substitute(expand("%:p:~"), "\\v^(/)$|^(\\~)/$|.*/([^/]+)/$|.*", "\\1\\2\\3", "")}%t
            \%{&modified ? " •" : ""}',
        \ 'asyncdo': '%{exists("g:asyncdo") ? split(g:asyncdo.cmd)[0]."…" : ""}',
        \ 'ruler': '%p%% %l:%v%<',
    \ },
    \ 'component_function': {
        \ 'git': 'Git_statusline',
    \ },
    \ 'component_expand': {
        \ 'errors': 'Coc_error_count',
        \ 'warnings': 'Coc_warning_count',
    \ },
    \ 'component_type': {
        \ 'errors': 'error',
        \ 'warnings': 'warning',
    \ },
    \ 'tabline': {
        \ 'left': [['tabs']],
        \ 'right': [],
    \ },
    \ 'tab': {
        \ 'active': ['tabnum', 'filename'],
        \ 'inactive': ['tabnum', 'filename'],
    \ },
    \ 'separator': {
        \ 'left': '',
        \ 'right': '',
    \ },
    \ 'subseparator': {
        \ 'left': '',
        \ 'right': '',
    \ },
\ }

autocmd vimrc User Plug_lightline_vim set noshowmode
autocmd vimrc User Plug_lightline_vim autocmd vimrc QuickFixCmdPost * call lightline#update()

autocmd vimrc User Plug_lightline_vim autocmd vimrc ColorScheme srcery
    \ call s:update_lightline_colors()
function! s:update_lightline_colors() abort
    let l:common = s:get_srcery_colors('BrightWhite', 'Xgray5')
    execute 'highlight StatusLine cterm=NONE ctermbg='.l:common[3].' gui=NONE guibg='.l:common[1]
    execute 'highlight StatusLineNC cterm=NONE ctermbg='.l:common[3].' gui=NONE guibg='.l:common[1]
    let l:palette = {
        \ 'normal': {
            \ 'middle': [s:get_srcery_colors('BrightWhite', 'Xgray3')],
            \ 'error': [s:get_srcery_colors('Black', 'Red')],
            \ 'warning': [s:get_srcery_colors('Black', 'BrightOrange')],
        \ },
        \ 'inactive': {
            \ 'left': [l:common],
            \ 'right': [l:common],
        \ },
        \ 'tabline': {
            \ 'left': [l:common],
            \ 'tabsel': [s:get_srcery_colors('Black', 'Blue')],
        \ },
    \ }
    for [l:mode, l:color] in [
        \ ['normal', 'Blue'],
        \ ['insert', 'Green'],
        \ ['replace', 'Red'],
        \ ['visual', 'Magenta'],
        \ ['command', 'Orange'],
    \ ]
        let l:palette[l:mode] = get(l:palette, l:mode, {})
        let l:palette[l:mode].left = [s:get_srcery_colors('Black', l:color), l:common]
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
    set grepprg=rg\ --vimgrep grepformat=%f:%l:%c:%m
endif

Plug 'hauleth/asyncdo.vim'

autocmd vimrc User Plug_asyncdo_vim command! -bang -nargs=+ -complete=file Grep
    \ cclose | call asyncdo#run(<bang>0, {
        \ 'job': substitute(&grepprg, '\\|', '|', 'g'),
        \ 'errorformat': &grepformat,
    \ }, <q-args>)
autocmd vimrc User Plug_asyncdo_vim command! -bang -nargs=+ -complete=file RGrep
    \ Grep<bang> <args> %:h:S
autocmd vimrc User Plug_asyncdo_vim command! -bang -nargs=* -complete=file Make
    \ cclose | call asyncdo#run(<bang>0, substitute(&makeprg, '\\|', '|', 'g'), <q-args>)
autocmd vimrc User Plug_asyncdo_vim nnoremap <Leader>mm <Cmd>silent update \| Make<CR>
autocmd vimrc User Plug_asyncdo_vim nnoremap <Leader>mc <Cmd>Make clean<CR>
autocmd vimrc User Plug_asyncdo_vim nnoremap <C-C> <Cmd>AsyncStop<CR>

Plug 'tpope/vim-eunuch'

" File navigation {{{1

Plug 'justinmk/vim-dirvish'
set suffixes-=.h
autocmd vimrc FileType dirvish nnoremap <buffer> C <Cmd>cd % \| pwd<CR>

Plug 'junegunn/fzf', {'do': './install --bin'}
let g:fzf_layout = {'down': 15}
let g:fzf_action = {
    \ '': 'edit',
    \ 'ctrl-x': 'split',
    \ 'ctrl-v': 'vsplit',
    \ 'ctrl-t': 'tab split',
    \ 'ctrl-z': 'silent! bdelete',
\ }

autocmd vimrc User Plug_fzf nnoremap <Leader>ff <Cmd>FZF<CR>
autocmd vimrc User Plug_fzf nnoremap <Leader>fr <Cmd>call fzf#run(fzf#wrap({
    \ 'dir': expand('%:p:h'),
    \ 'options': [
        \ '--prompt='.pathshorten(substitute(expand('%:p:~:h'), '/$', '', '')).'/',
        \ '--multi',
    \ ],
\ }))<CR>
autocmd vimrc User Plug_fzf nnoremap <Leader>fb <Cmd>call fzf#run(fzf#wrap({
    \ 'source': map(filter(getbufinfo({'buflisted': 1}),
        \ {i, b -> !empty(b.name)}), {i, b -> fnamemodify(b.name, ':~:.')}),
    \ 'options': [
        \ '--prompt='.pathshorten(substitute(fnamemodify(getcwd(), ':~'), '/$', '', '')).'/',
        \ '--multi',
    \ ],
\ }))<CR>

nnoremap <M-o> <Cmd>call <SID>jump_backward()<CR>
function! s:jump_backward() abort
    let [l:jumps, l:current] = getjumplist()
    let l:i = l:current - 1
    while l:i >= 0 && l:jumps[l:i].bufnr == bufnr()
        let l:i -= 1
    endwhile
    if l:i >= 0
        execute 'normal' (l:current - l:i)."\<C-O>"
    endif
endfunction

nnoremap <M-i> <Cmd>call <SID>jump_forward()<CR>
function! s:jump_forward() abort
    let [l:jumps, l:current] = getjumplist()
    let l:i = l:current + 1
    while l:i < len(l:jumps) && l:jumps[l:i].bufnr == bufnr() ||
        \ l:i + 1 < len(l:jumps) && l:jumps[l:i + 1].bufnr == l:jumps[l:i].bufnr
        let l:i += 1
    endwhile
    if l:i < len(l:jumps)
        execute 'normal' (l:i - l:current)."\<C-I>"
    endif
endfunction

" Quickfix {{{1

autocmd vimrc QuickFixCmdPost [^l]* call s:open_quickfix('window')
function! s:open_quickfix(cmd) abort
    let l:win = win_getid()
    execute 'botright c'.a:cmd
    call win_gotoid(l:win)
endfunction

nnoremap <Leader>tq <Cmd>call <SID>toggle_quickfix()<CR>
function! s:toggle_quickfix() abort
    if empty(filter(getwininfo(), {i, w -> w.quickfix && !w.loclist && w.tabnr == tabpagenr()}))
        call s:open_quickfix('open')
    else
        cclose
    endif
endfunction

let g:fzf_action['ctrl-q'] = {l -> s:set_quickfix(map(l, {i, l -> {'filename': l, 'valid': 1}}))}

function! s:set_quickfix(items) abort
    call setqflist(a:items)
    call s:open_quickfix('window')
    cfirst
endfunction

autocmd vimrc User Plug_fzf
    \ nnoremap <Leader>fq <Cmd>cclose \| call <SID>fzf_from_quickfix([], getqflist())<CR>
function! s:fzf_from_quickfix(options, ...) abort
    let l:all_items = []
    let l:seen = {}
    function! s:process_items(items) abort closure
        let l:lines = []
        for l:item in a:items
            let l:string = string(l:item)
            if !get(l:item, 'valid', 1) || has_key(l:seen, l:string)
                continue
            endif
            let l:seen[l:string] = 1
            let l:file = has_key(l:item, 'filename') ? l:item.filename : bufname(l:item.bufnr)
            let l:right = "\e[37m".fnamemodify(l:file, ':p:~:.').':'.(l:item.lnum)."\e[m"
            let l:left = substitute(l:item.text, "\t", ' ', 'g')
            if has_key(l:item, 'range')
                let l:start = l:item.range.start.character
                let l:end = l:item.range.end.character
                let l:left = (l:start ? l:left[:(l:start - 1)] : '')."\e[31m".
                    \ l:left[(l:start):(l:end - 1)]."\e[m".l:left[(l:end):]
            endif
            let l:left = trim(l:left, ' ')
            if !empty(get(l:item, 'type', ''))
                let l:left = "\e[31m".(l:item.type).":\e[m ".l:left
            endif
            let l:pad = &columns - 3 - strwidth(substitute(l:left.l:right, '\e\[[^m]*m', '', 'g'))
            call add(l:lines, len(l:all_items).' '.l:left.repeat(' ', max([l:pad, 1])).l:right)
            call add(l:all_items, l:item)
        endfor
        return l:lines
    endfunction
    if a:0
        let l:source = s:process_items(a:1)
        let l:Return = 0
    else
        let l:source_file = tempname()
        call writefile([], l:source_file)
        let l:source = 'tail -f '.shellescape(l:source_file)
        let l:Return = {i -> writefile(s:process_items(i), l:source_file, 'a')}
    endif
    function! s:fzf_sink(lines) abort closure
        if exists('l:source_file')
            call delete(l:source_file)
        endif
        let l:key = remove(a:lines, 0)
        if l:key ==# 'ctrl-q'
            call s:set_quickfix(map(a:lines, {i, l -> l:all_items[split(l)[0]]}))
        else
            for l:line in a:lines
                let l:item = l:all_items[split(l:line)[0]]
                let l:file = has_key(l:item, 'filename') ? l:item.filename : bufname(l:item.bufnr)
                if empty(l:key) && fnamemodify(l:file, ':p') ==# expand('%:p')
                    normal! m'
                else
                    execute g:fzf_action[l:key] fnameescape(l:file)
                endif
                call cursor(l:item.lnum, get(l:item, 'col', 1))
            endfor
        endif
    endfunction
    call fzf#run(fzf#wrap({
        \ 'source': l:source,
        \ 'sink*': funcref("\<SID>fzf_sink"),
        \ 'options': extend([
            \ '--with-nth=2..',
            \ '--delimiter= ',
            \ '--tiebreak=begin',
            \ '--ansi',
            \ '--layout=reverse-list',
            \ '--multi',
            \ '--expect=ctrl-x,ctrl-v,ctrl-t,ctrl-q',
        \ ], a:options),
    \ }))
    return l:Return
endfunction

" Terminal {{{1

autocmd vimrc TermOpen * execute 'file' fnameescape('[Terminal '.jobpid(&channel).']')
autocmd vimrc TermOpen * startinsert
autocmd vimrc BufEnter * if &buftype ==# 'terminal' | startinsert | endif

tnoremap <M-h> <C-\><C-N><C-W>h
tnoremap <M-j> <C-\><C-N><C-W>j
tnoremap <M-k> <C-\><C-N><C-W>k
tnoremap <M-l> <C-\><C-N><C-W>l

nnoremap <Leader>tt <Cmd>call <SID>toggle_terminal(getcwd())<CR>
nnoremap <Leader>tT <Cmd>call <SID>toggle_terminal(expand('%:p:h'))<CR>

let s:terminal_buffer = -1
function! s:toggle_terminal(cwd) abort
    if bufwinnr(s:terminal_buffer) != -1
        execute bufwinnr(s:terminal_buffer) 'hide'
        return
    endif
    botright 15 split
    set winfixheight
    if bufexists(s:terminal_buffer)
        execute 'buffer' s:terminal_buffer
    else
        execute 'edit' fnameescape('term://'.a:cwd.'//'.&shell)
        set nobuflisted
        let s:terminal_buffer = bufnr()
    endif
endfunction

" Git {{{1

Plug 'tpope/vim-fugitive'

autocmd vimrc User Plug_vim_fugitive nnoremap <Leader>tg <Cmd>call <SID>toggle_git_status()<CR>
function! s:toggle_git_status() abort
    let l:buf = filter(getbufinfo(), {i, b -> get(b.variables, 'fugitive_type', '') ==# 'index'})
    if !empty(l:buf)
        execute 'bdelete' l:buf[0].bufnr
        return
    elseif !exists(':Gstatus')
        return
    endif
    Gstatus
    wincmd H
    call s:resize_to_fit()
    set winfixwidth
    wincmd =
    autocmd TextChanged <buffer> call s:resize_to_fit()
endfunction

function! s:resize_to_fit() abort
    execute 'vertical resize'
        \ max(add(map(getbufline('%', 1, '$'), {i, l -> strdisplaywidth(l)}), 20))
endfunction

autocmd vimrc User Plug_vim_fugitive autocmd vimrc FileType gitcommit wincmd K

nnoremap <Leader>td <Cmd>call <SID>toggle_diff()<CR>
function! s:toggle_diff() abort
    if !&diff
        if exists(':Gdiffsplit')
            Gdiffsplit!
        endif
        return
    endif
    let l:wins = filter(gettabinfo(tabpagenr())[0].windows,
        \ {i, w -> w != win_getid() && getwinvar(w, '&diff')})
    diffoff!
    for l:win in l:wins
        execute win_id2win(l:win) 'close'
    endfor
endfunction

set diffopt+=vertical,foldcolumn:1,hiddenoff

function! Git_statusline() abort
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

" Search and completion {{{1

set inccommand=nosplit
set ignorecase smartcase
Plug 'pgdouyon/vim-evanesco'

set wildignorecase wildmode=longest:full,full
cnoremap <expr> <C-N> pumvisible() ? "\<C-N>" : "\<Down>"
cnoremap <expr> <C-P> pumvisible() ? "\<C-P>" : "\<Up>"

autocmd vimrc User Plug_fzf nnoremap <Leader>f/ <Cmd>call <SID>fzf_from_quickfix([],
    \ map(getbufline('%', 1, '$'), {i, l -> {'bufnr': bufnr(), 'lnum': i + 1, 'text': l}}))<CR>

set dictionary=/usr/share/dict/words
set completeopt=menuone,noselect,noinsert shortmess+=c
inoremap <expr> <Tab> pumvisible() ? "\<C-N>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-P>" : "\<S-Tab>"

function! s:completion_fallback() abort
    if has('python3')
        Plug 'roxma/nvim-yarp'
        Plug 'ncm2/ncm2'
        Plug 'ncm2/ncm2-path'
        Plug 'ncm2/ncm2-bufword'
        Plug 'fgrsnau/ncm2-otherbuf', {'branch': 'ncm2'}
        let g:ncm2#complete_length = 2
        autocmd vimrc User Plug_ncm2 autocmd BufEnter * call ncm2#enable_for_buffer()
    else
        Plug 'lifepillar/vim-mucomplete'
        let g:mucomplete#enable_auto_at_startup = 1
        let g:mucomplete#minimum_prefix_length = 1
        let g:mucomplete#buffer_relative_paths = 1
        autocmd vimrc User Plug_vim_mucomplete iunmap <Tab>
        autocmd vimrc User Plug_vim_mucomplete iunmap <S-Tab>
    endif
    inoremap <expr> <CR> pumvisible() ? "\<C-Y>\<CR>" : "\<CR>"
endfunction

" Language client {{{1

if executable('curl') && executable('node')
    function! Download_coc(options) abort
        let l:dir = (g:plugs['coc.nvim'].dir).'/build'
        call mkdir(l:dir, 'p')
        execute '!curl -f https://raw.githubusercontent.com/neoclide/coc.nvim/release/build/index.js'
            \ "| sed -E '".
            \ 's/( *)let sa = a\.sortText;/'.
                \ '\1if (a.priority \!= b.priority) return b.priority - a.priority;\n&/;'.
            \ 's/\\\\<C-g>u//;'.
            \ "' >".fnameescape(l:dir.'/index.js')
    endfunction
    Plug 'neoclide/coc.nvim', {'do': function('Download_coc')}
else
    call s:completion_fallback()
endif

autocmd vimrc User Plug_coc_nvim inoremap <silent> <expr> <Tab> pumvisible() ? "\<C-N>" :
    \ col('.') <= 1 \|\| getline('.')[col('.') - 2] =~# '\s' ? "\<Tab>" : coc#refresh()
autocmd vimrc User Plug_coc_nvim inoremap <CR> <C-G>u<CR>

let g:coc_user_config = {
    \ 'coc': {
        \ 'preferences': {
            \ 'extensionUpdateCheck': 'never',
        \ },
        \ 'source': {
            \ 'around': {
                \ 'priority': 2,
                \ 'firstMatch': v:false,
            \ },
            \ 'buffer': {
                \ 'firstMatch': v:false,
            \ },
        \ },
    \ },
    \ 'suggest': {
        \ 'maxCompleteItemCount': 1000,
        \ 'invalidInsertCharacters': split(' "(/:<>', '\zs'),
        \ 'snippetIndicator': '',
        \ 'detailField': 'preview',
    \ },
    \ 'signature': {
        \ 'preferShownAbove': v:false,
        \ 'hideOnTextChange': v:true,
    \ },
    \ 'diagnostic': {
        \ 'errorSign': '✕',
        \ 'warningSign': '⚠',
        \ 'infoSign': 'ℹ',
        \ 'hintSign': '➤',
        \ 'locationlist': v:false,
    \ },
\ }

autocmd vimrc User Plug_coc_nvim autocmd vimrc FileType * call s:init_lsp_buffer()
function! s:init_lsp_buffer() abort
    if index(s:lsp_filetypes, &filetype) == -1 || @% =~# '^fugitive:'
        return
    endif
    setlocal signcolumn=yes
    nnoremap <buffer> ]g <Cmd>call CocActionAsync('diagnosticNext')<CR>
    nnoremap <buffer> [g <Cmd>call CocActionAsync('diagnosticPrevious')<CR>
    nnoremap <buffer> <Leader>ge
        \ <Cmd>call CocActionAsync('diagnosticList', {e, d -> <SID>set_quickfix(map(d, {i, d -> {
            \ 'filename': fnamemodify(d.file, ':p:~:.'),
            \ 'lnum': d.lnum,
            \ 'col': d.col,
            \ 'type': d.severity[0],
            \ 'text': d.message,
        \ }}))})<CR>
    nnoremap <buffer> <Leader>gc
        \ <Cmd>call CocActionAsync('getCurrentFunctionSymbol', function('<SID>echo_result'))<CR>
    nnoremap <buffer> <Leader>gd <Cmd>call CocActionAsync('jumpDefinition')<CR>
    nnoremap <buffer> <Leader>gD <Cmd>call CocActionAsync('jumpDeclaration')<CR>
    nnoremap <buffer> <Leader>gt <Cmd>call CocActionAsync('jumpTypeDefinition')<CR>
    nnoremap <buffer> <Leader>gi <Cmd>call CocActionAsync('jumpImplementation')<CR>
    nnoremap <buffer> <Leader>gr <Cmd>call CocActionAsync('jumpReferences')<CR>
    nnoremap <buffer> <Leader>gR <Cmd>call CocActionAsync('rename')<CR>
    nnoremap <buffer> <Leader>ga <Cmd>call CocActionAsync('codeAction')<CR>
    xnoremap <buffer> ga <Esc><Cmd>call CocActionAsync('codeAction', visualmode())<CR>
    nnoremap <buffer> <Leader>gh <Cmd>call CocActionAsync('doHover')<CR>
    nnoremap <buffer> <Leader>gf <Cmd>call coc#util#float_jump()<CR>
    if isdirectory(g:plugs.fzf.dir)
        nnoremap <buffer> <Leader>gs <Cmd>call CocActionAsync('documentSymbols',
            \ {e, s -> <SID>fzf_from_quickfix([], <SID>map_symbols(s))})<CR>
        if executable('nvr')
            nnoremap <buffer> <Leader>gS <Cmd>call <SID>fzf_from_workspace_symbols()<CR>
        endif
    endif
    if &filetype !~# 'javascript\|typescript'
        nnoremap <buffer> <Leader>gq <Cmd>call CocActionAsync('format')<CR>
        set formatexpr=CocActionAsync('formatSelected')
        autocmd CursorHold <buffer> call CocActionAsync('highlight')
    endif
endfunction
function! s:echo_result(err, result) abort
    echo a:result
endfunction

autocmd vimrc User Plug_fzf let g:coc_enable_locationlist = 0
autocmd vimrc User Plug_fzf autocmd vimrc User CocLocationsChange ++nested
    \ call s:fzf_from_quickfix([], g:coc_jump_locations)

function! s:fzf_from_workspace_symbols() abort
    let l:ls = filter(keys(g:coc_user_config.languageserver),
        \ {i, l -> index(g:coc_user_config.languageserver[l].filetypes, &filetype) != -1})[0]
    let l:Add_items = s:fzf_from_quickfix(['--bind=change:top+execute-silent:'.
        \ 'nvr -c "call Workspace_symbol_query(''$(echo {q} | sed "s/''/''''/g")'')" &'])
    function! Workspace_symbol_query(query) abort closure
        for l:word in split(a:query)
            let l:word = substitute(substitute(l:word, '^[''^]', '', ''), '[$\\]$', '', '')
            if empty(l:word) || l:word[0] ==# '!' || l:word ==# '|'
                continue
            endif
            call CocRequestAsync(l:ls, 'workspace/symbol', {'query': l:word},
                \ {e, s -> l:Add_items(s:map_symbols(map(s, {i, s -> {
                    \ 'filepath': iconv(substitute(substitute(s.location.uri, '^file://', '', ''),
                        \ '%\(\x\x\)', {m -> nr2char('0x'.m[1])}, 'g'), 'utf-8', 'latin1'),
                    \ 'lnum': s.location.range.start.line + 1,
                    \ 'col': s.location.range.start.character + 1,
                    \ 'text': s.name,
                    \ 'kind': s.kind > 26 ? 'Unknown' : [
                        \ 'File', 'Module', 'Namespace', 'Package', 'Class', 'Method', 'Property',
                        \ 'Field', 'Constructor', 'Enum', 'Interface', 'Function', 'Variable',
                        \ 'Constant', 'String', 'Number', 'Boolean', 'Array', 'Object', 'Key',
                        \ 'Null', 'EnumMember', 'Struct', 'Event', 'Operator', 'TypeParameter',
                    \ ][s.kind - 1],
                \ }})))})
        endfor
    endfunction
endfunction
function! s:map_symbols(symbols) abort
    return map(a:symbols, {i, s -> {
        \ 'filename': get(s, 'filepath', @%),
        \ 'lnum': s.lnum,
        \ 'col': s.col,
        \ 'text': (s.text).' ['.(s.kind).']',
        \ 'range': {
            \ 'start': {
                \ 'character': strwidth(s.text) + 1,
            \ },
            \ 'end': {
                \ 'character': strwidth(s.text) + strwidth(s.kind) + 3,
            \ },
        \ },
    \ }})
endfunction

autocmd vimrc ColorScheme * highlight Bold cterm=bold gui=bold
highlight link CocHighlightText Bold
highlight link CocUnderline Bold
highlight link CocErrorSign ALEErrorSign
highlight link CocWarningSign ALEWarningSign
highlight link CocInfoSign ALEInfoSign
highlight link CocHintSign ALEInfoSign
highlight link CocErrorHighlight ALEError
highlight link CocWarningHighlight ALEWarning
highlight link CocInfoHighlight ALEInfo
highlight link CocHintHighlight ALEInfo

autocmd vimrc User Plug_lightline_vim autocmd vimrc User CocDiagnosticChange call lightline#update()
function! Coc_error_count() abort
    let l:count = get(b:, 'coc_diagnostic_info', {'error': 0}).error
    return l:count ? l:count.g:coc_user_config.diagnostic.errorSign : ''
endfunction
function! Coc_warning_count() abort
    let l:count = get(b:, 'coc_diagnostic_info', {'warning': 0}).warning
    return l:count ? l:count.g:coc_user_config.diagnostic.warningSign : ''
endfunction

" Language server {{{1

let g:coc_user_config.languageserver = {}

if executable('ccls')
    let g:coc_user_config.languageserver.ccls = {
        \ 'command': 'ccls',
        \ 'filetypes': ['c', 'cpp'],
        \ 'rootPatterns': ['compile_commands.json', '.ccls'],
        \ 'initializationOptions': {
            \ 'cache': {
                \ 'directory': '/tmp/ccls',
            \ },
            \ 'clang': {
                \ 'extraArgs': ['-std=c++17', '-Wall', '-Wextra'],
            \ },
            \ 'completion': {
                \ 'detailedLabel': v:false,
            \ },
        \ }
    \ }
endif

if executable('javascript-typescript-stdio')
    let g:coc_user_config.languageserver.jstsls = {
        \ 'command': 'javascript-typescript-stdio',
        \ 'filetypes': ['javascript', 'typescript'],
        \ 'rootPatterns': ['package.json'],
    \ }
endif

if executable('pyls')
    let g:coc_user_config.languageserver.pyls = {
        \ 'command': 'pyls',
        \ 'filetypes': ['python'],
    \ }
endif

if executable('rls')
    let g:coc_user_config.languageserver.rls = {
        \ 'command': 'rls',
        \ 'filetypes': ['rust'],
        \ 'rootPatterns': ['Cargo.toml'],
        \ 'requireRootPattern': v:true,
        \ 'settings': {
            \ 'rust': {
                \ 'clippy_preference': 'on',
            \ },
        \ },
    \ }
endif

let s:lsp_filetypes = []
for s:ls in values(g:coc_user_config.languageserver)
    call extend(s:lsp_filetypes, s:ls.filetypes)
endfor

" Filetypes {{{1

Plug 'sheerun/vim-polyglot'
let g:polyglot_disabled = ['latex']
let g:jsx_ext_required = 1
let g:python_highlight_space_errors = 0

autocmd vimrc FileType c,cpp set commentstring=//%s
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>eh <Cmd>edit %:r.h<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>eH <Cmd>edit %:r.hpp<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>ec <Cmd>edit %:r.c<CR>
autocmd vimrc FileType c,cpp nnoremap <buffer> <Leader>eC <Cmd>edit %:r.cpp<CR>

if executable('cargo')
    autocmd vimrc FileType rust compiler cargo
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType rust
        \ nnoremap <buffer> <Leader>mm <Cmd>silent update \| Make build<CR>
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType rust
        \ nnoremap <buffer> <Leader>mr <Cmd>silent update \| Make build --release<CR>
endif

autocmd vimrc FileType gitcommit,mail,markdown,tex setlocal spell
autocmd vimrc SourcePost init.vim autocmd vimrc FileType mail set formatoptions-=t

Plug 'lervag/vimtex'
Plug 'neoclide/coc-vimtex'
let g:tex_flavor = 'latex'
let g:vimtex_indent_on_ampersands = 0
let g:vimtex_indent_bib_enabled = 0
if executable('nvr')
    let g:vimtex_compiler_progname = 'nvr'
endif
if executable('zathura')
    let g:vimtex_view_method = 'zathura'
endif
let g:vimtex_fold_enabled = 1
let g:vimtex_fold_types = {}
for s:key in ['envs', 'env_options', 'cmd_single', 'cmd_single_opt', 'cmd_multi', 'cmd_addplot']
    let g:vimtex_fold_types[s:key] = {'enabled': 0}
endfor
autocmd vimrc User Plug_vimtex autocmd vimrc FileType tex call s:init_vimtex_buffer()
function! s:init_vimtex_buffer() abort
    setlocal foldcolumn=2
    nnoremap <buffer> <Leader>mm <Cmd>silent update \| VimtexCompileSS<CR>
    nnoremap <buffer> <Leader>mc <Cmd>VimtexClean<CR><Cmd>call <SID>clean_tex_files()<CR>
    nnoremap <buffer> <Leader>mC <Cmd>VimtexClean!<CR><Cmd>call <SID>clean_tex_files()<CR>
    nmap <buffer> <Leader>mv <Plug>(vimtex-view)
    nmap <buffer> <Leader>tc <Plug>(vimtex-toc-open)
endfunction
function! s:clean_tex_files() abort
    for l:ext in ['.synctex.gz', '.bbl', '.nav', '.snm']
        call delete(expand('%:r').l:ext)
    endfor
endfunction

Plug 'iamcco/markdown-preview.nvim', {'do': {-> mkdp#util#install()}}
let g:mkdp_page_title = '${name}'
let g:mkdp_auto_close = 0
autocmd vimrc User Plug_markdown_preview_nvim autocmd vimrc FileType markdown
    \ nnoremap <buffer> <Leader>mv <Cmd>MarkdownPreview<CR>

autocmd vimrc BufNewFile,BufRead *.gv set filetype=dot
autocmd vimrc FileType dot set commentstring=//%s
if executable('dot')
    autocmd vimrc FileType dot
        \ let &l:makeprg = 'dot -T$* -o'.expand('%:p:r:S').'.$* '.expand('%:p:S')
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType dot
        \ nnoremap <buffer> <Leader>mm <Cmd>silent update \| Make png<CR>
    autocmd vimrc User Plug_asyncdo_vim autocmd vimrc FileType dot
        \ nnoremap <buffer> <Leader>ms <Cmd>silent update \| Make svg<CR>
endif
if executable('xdg-open')
    autocmd vimrc FileType dot,plantuml
        \ nnoremap <buffer> <Leader>mv <Cmd>silent !xdg-open %:r:S.png &<CR>
endif

Plug 'Elzair/ifm-vim'
autocmd vimrc FileType ifm set commentstring=#%s
if executable('ifm')
    autocmd vimrc FileType ifm let &l:makeprg =
        \ 'ifm -m -o '.expand('%:p:r:S').'.ps '.expand('%:p:S')
    autocmd vimrc FileType ifm setlocal errorformat=ifm:\ error:\ %f\\,\ line\ %l:\ %m
endif
if executable('xdg-open')
    autocmd vimrc FileType ifm nnoremap <buffer> <Leader>mv <Cmd>silent !xdg-open %:r:S.ps &<CR>
endif

" Debugging {{{1

if executable('gdb')
    packadd termdebug
    let g:termdebug_wide = 1
    autocmd vimrc ColorScheme srcery highlight link debugPC DiffChange
    autocmd vimrc ColorScheme srcery highlight link debugBreakpoint SrceryRedBold
    nnoremap <Leader>gb <Cmd>Break<CR>
    nnoremap <Leader>gB <Cmd>Clear<CR>
    xnoremap K :Evaluate<CR>

    if executable('rust-gdb')
        command! -bang -nargs=* -complete=file TermdebugRust
            \ let g:termdebugger = 'rust-gdb' | Termdebug<bang> <args> | let g:termdebugger = 'gdb'
        command! -bang -nargs=+ -complete=file TermdebugRustCommand
            \ let g:termdebugger = 'rust-gdb' | TermdebugCommand<bang> <args> | let g:termdebugger = 'gdb'
    endif
endif

if executable('cfr')
    autocmd vimrc BufReadCmd *.class call s:decompile_java_class()
    function! s:decompile_java_class() abort
        setlocal undolevels=-1
        read !cfr %
        1 delete _
        setlocal undolevels=-123456 filetype=java buftype=nowrite
    endfunction
endif

" }}}

call s:init_plugins()
