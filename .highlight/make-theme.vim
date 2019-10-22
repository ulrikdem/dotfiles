runtime init.vim
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
    let colour = 'Colour = "'.synIDattr(id, 'fg#', 'gui').'"'
    let bold = 'Bold = '.(synIDattr(id, 'bold', 'gui') ? 'true' : 'false')
    let italic = 'Italic = '.(synIDattr(id, 'italic', 'gui') ? 'true' : 'false')
    call add(lines, name.' = {'.colour.', '.bold.', '.italic.'}')
endfor
call writefile(lines, 'themes/vim.theme')
