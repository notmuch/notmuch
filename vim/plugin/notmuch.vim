" notmuch.vim plugin --- run notmuch within vim
"
" Copyright © Carl Worth
"
" This file is part of Notmuch.
"
" Notmuch is free software: you can redistribute it and/or modify it
" under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" Notmuch is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
" General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
"
" Authors: Bart Trojanowski <bart@jukie.net>

" --- defaults

if !exists('g:notmuch_cmd')
        let g:notmuch_cmd = 'notmuch'
endif

" --- implement search screen

function! s:NM_cmd_search(words)
        let data = s:NM_run(['search'] + a:words)
        "let data = substitute(data, '27/27', '25/27', '')
        "let data = substitute(data, '\[4/4\]', '[0/4]', '')
        let lines = split(data, "\n")
        let disp = copy(lines)
        call map(disp, 'substitute(v:val, "^thread:\\S* ", "", "")' )

        call s:NM_newBuffer('search', join(disp, "\n"))
        let b:nm_raw_data = lines

        nnoremap <buffer> <Enter> :call <SID>NM_search_display()<CR>
        setlocal cursorline
        setlocal nowrap
endfunction

function! s:NM_search_display()
        let line = line('.')
        if !exists('b:nm_raw_data')
                echo 'no b:nm_raw_data'
        else
                let info = b:nm_raw_data[line]
                let what = split(info, '\s\+')[0]
                call s:NM_cmd_show([what])
        endif
endfunction


" --- implement show screen

function! s:NM_cmd_show(words)
        let bufnr = bufnr('%')
        let data = s:NM_run(['show'] + a:words)

        call s:NM_newBuffer('show', data)
        setlocal bufhidden=delete
        let b:nm_raw_data = data

        exec printf("nnoremap <buffer> q :b %d<CR>", bufnr)
endfunction


" --- helper function

function! s:NM_newBuffer(ft, content)
        enew
        setlocal buftype=nofile readonly modifiable
        silent put=a:content
        keepjumps 0d
        setlocal nomodifiable
        execute printf('set filetype=notmuch-%s', a:ft)
        execute printf('set syntax=notmuch-%s', a:ft)
endfunction

function! s:NM_run(args)
        let cmd = g:notmuch_cmd . ' ' . join(a:args) . '< /dev/null'
        let out = system(cmd)
        if v:shell_error
                echohl Error
                echo substitute(out, '\n*$', '', '')
                echohl None
                return ''
        else
                return out
        endif
endfunction


" --- command handler

function! NotMuch(args)
        if !strlen(a:args)
                call s:NM_cmd_search(['tag:inbox'])
                return
        endif

        echo "blarg!"

        let words = split(a:args)
        " TODO: handle commands passed as arguments
endfunction
function! CompleteNotMuch(arg_lead, cmd_line, cursor_pos)
        return []
endfunction


" --- glue

command! -nargs=* -complete=customlist,CompleteNotMuch NotMuch call NotMuch(<q-args>)
cabbrev  notmuch <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'NotMuch' : 'notmuch')<CR>

" --- hacks, only for development :)

nnoremap ,nmr :source ~/.vim/plugin/notmuch.vim<CR>:call NotMuch('')<CR>
