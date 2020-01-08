runtime init.vim

function! s:xterm2rgb(color) abort
    let l:color = str2nr(a:color)
    if l:color < 16
        let l:rgb = [
            \ [0x00, 0x00, 0x00],
            \ [0xcd, 0x00, 0x00],
            \ [0x00, 0xcd, 0x00],
            \ [0xcd, 0xcd, 0x00],
            \ [0x00, 0x00, 0xee],
            \ [0xcd, 0x00, 0xcd],
            \ [0x00, 0xcd, 0xcd],
            \ [0xe5, 0xe5, 0xe5],
            \ [0x7f, 0x7f, 0x7f],
            \ [0xff, 0x00, 0x00],
            \ [0x00, 0xff, 0x00],
            \ [0xff, 0xff, 0x00],
            \ [0x5c, 0x5c, 0xff],
            \ [0xff, 0x00, 0xff],
            \ [0x00, 0xff, 0xff],
            \ [0xff, 0xff, 0xff]
        \ ][l:color]
    elseif l:color < 232
        let l:color -= 16
        let l:range = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff]
        let l:rgb = [
            \ l:range[l:color / 36],
            \ l:range[l:color / 6 % 6],
            \ l:range[l:color % 6]
        \ ]
    else
        let l:gray = (l:color - 232) * 10 + 8
        let l:rgb = [l:gray, l:gray, l:gray]
    endif
    return printf('#%02x%02x%02x', l:rgb[0], l:rgb[1], l:rgb[2])
endfunction

let lines = [
    \ 'Description = "vim '.colors_name.'"',
    \ 'Categories = {"'.&background.'", "vim"}',
    \ 'Keywords = {}',
    \ '',
    \ 'Canvas = {Colour = "#000000", Bold = false, Italic = false}',
\ ]
for [name, group] in [
    \ ['Default', 'Normal'],
    \ ['LineNum', 'LineNr'],
    \ ['BlockComment', 'Comment'],
    \ ['LineComment', 'Comment'],
    \ ['PreProcessor', 'PreProc'],
    \ ['StringPreProc', 'String'],
    \ ['String', 'String'],
    \ ['Escape', 'SpecialChar'],
    \ ['Interpolation', 'SpecialChar'],
    \ ['Number', 'Number'],
    \ ['Operator', 'Operator'],
    \ ['Keywords[1]', 'Keyword'],
    \ ['Keywords[2]', 'Type'],
    \ ['Keywords[3]', 'Identifier'],
    \ ['Keywords[4]', 'Function'],
\ ]
    let id = synIDtrans(hlID(group))
    let colour = 'Colour = "'.s:xterm2rgb(synIDattr(id, 'fg', 'cterm')).'"'
    let bold = 'Bold = '.(synIDattr(id, 'bold', 'cterm') ? 'true' : 'false')
    let italic = 'Italic = '.(synIDattr(id, 'italic', 'cterm') ? 'true' : 'false')
    call add(lines, name.' = {'.colour.', '.bold.', '.italic.'}')
endfor
call writefile(lines, 'themes/vim.theme')
