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

if !exists('g:notmuch_search_reverse')
        let g:notmuch_search_reverse = 1
endif

" --- used to match output of notmuch

let s:notmuch_show_message_begin_regexp    = '^message{'
let s:notmuch_show_message_end_regexp      = '^message}'
let s:notmuch_show_header_begin_regexp     = '^header{'
let s:notmuch_show_header_end_regexp       = '^header}'
let s:notmuch_show_body_begin_regexp       = '^body{'
let s:notmuch_show_body_end_regexp         = '^body}'
let s:notmuch_show_attachment_begin_regexp = '^attachment{'
let s:notmuch_show_attachment_end_regexp   = '^attachment}'
let s:notmuch_show_part_begin_regexp       = '^part{'
let s:notmuch_show_part_end_regexp         = '^part}'
let s:notmuch_show_marker_regexp           = '^\\(message\\|header\\|body\\|attachment\\|part\\)[{}].*$'

let s:notmuch_show_id_regexp               = '\(id:[^ ]*\)'
let s:notmuch_show_depth_regexp            = ' depth:\([0-9]*\) '
let s:notmuch_show_filename_regexp         = 'filename:\(.*\)$'
let s:notmuch_show_tags_regexp             = '(\([^)]*\))$'

let s:notmuch_show_signature_regexp        = '^\(-- \?\|_\+\)$'
let s:notmuch_show_signature_lines_max     = 12

let s:notmuch_show_citation_regexp         = '^\s*>'

" --- implement search screen

function! s:NM_cmd_search(words)
        let cmd = ['search']
        if g:notmuch_search_reverse
                let cmd = cmd + ['--reverse']
        endif
        let data = s:NM_run(cmd + a:words)
        "let data = substitute(data, '27/27', '25/27', '')
        "let data = substitute(data, '\[4/4\]', '[0/4]', '')
        let lines = split(data, "\n")
        let disp = copy(lines)
        call map(disp, 'substitute(v:val, "^thread:\\S* ", "", "")' )

        call s:NM_newBuffer('search', join(disp, "\n"))
        let b:nm_raw_data = lines

        nnoremap <buffer> <Enter> :call <SID>NM_search_display()<CR>
        nnoremap <buffer> s       :call <SID>NM_cmd_search(split(input('NotMuch Search:')))<CR>
        setlocal cursorline
        setlocal nowrap
endfunction

function! s:NM_search_display()
        if !exists('b:nm_raw_data')
                echo 'no b:nm_raw_data'
        else
                let line = line('.')
                let info = b:nm_raw_data[line-1]
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

        call s:NM_cmd_show_mkfolds()
        setlocal foldtext=NM_cmd_show_foldtext()
        setlocal fillchars=
        setlocal foldcolumn=5

        exec printf("nnoremap <buffer> q :b %d<CR>", bufnr)
endfunction

function! s:NM_cmd_show_mkfolds()
        let msg_start = -1
        let hdr_start = -1
        let bdy_start = -1
        let prt_start = -1
        let modetype = ''
        let modeline = -1
        let lnum = 1
        let b:nm_fold_data = {}
        while lnum <= line('$')
                let line = getline(lnum)
                if match(line, s:notmuch_show_message_begin_regexp) != -1
                        let msg_start = lnum
                elseif match(line, s:notmuch_show_message_end_regexp) != -1
                        exec printf('%d,%dfold', msg_start, lnum)
                        exec printf('%dfoldopen', msg_start)
                        let b:nm_fold_data[msg_start] = ['msg', getline(msg_start)]

                elseif match(line, s:notmuch_show_header_begin_regexp) != -1
                        let hdr_start = lnum
                elseif match(line, s:notmuch_show_header_end_regexp) != -1
                        exec printf('%d,%dfold', hdr_start, lnum)
                        exec printf('%dfoldclose', hdr_start)
                        let b:nm_fold_data[hdr_start] = ['hdr', '* ' . getline(hdr_start+1) . ' [ Press "h" for full header. ]']

                elseif match(line, s:notmuch_show_body_begin_regexp) != -1
                        let bdy_start = lnum
                elseif match(line, s:notmuch_show_body_end_regexp) != -1
                        exec printf('%d,%dfold', bdy_start, lnum)
                        exec printf('%dfoldopen', bdy_start)
                        let b:nm_fold_data[bdy_start] = ['bdy', getline(bdy_start)]

                elseif match(line, s:notmuch_show_part_begin_regexp) != -1
                        let prt_start = lnum
                elseif match(line, s:notmuch_show_part_end_regexp) != -1
                        exec printf('%d,%dfold', prt_start, lnum)
                        exec printf('%dfoldopen', prt_start)
                        let b:nm_fold_data[msg_start] = ['msg', getline(prt_start)]

                elseif modetype == ''
                        if match(line, s:notmuch_show_signature_regexp) != -1
                                let modetype = 'sig'
                                let modeline = lnum
                        elseif match(line, s:notmuch_show_citation_regexp) != -1
                                let modetype = 'cit'
                                let modeline = lnum
                        endif
                elseif modetype == 'cit'
                        if match(line, s:notmuch_show_citation_regexp) == -1
                                exec printf('%d,%dfold', modeline, lnum)
                                let b:nm_fold_data[modeline] = [modetype, printf('[ %d-line citation.  Press "c" to show. ]', lnum - modeline)]
                                let modetype = ''
                        endif
                elseif modetype == 'sig'
                        if (lnum - modeline) > s:notmuch_show_signature_lines_max
                                let modetype = ''
                        elseif match(line, s:notmuch_show_part_end_regexp) != -1
                                let modeline2 = lnum - 1
                                exec printf('%d,%dfold', modeline, modeline2)
                                let b:nm_fold_data[modeline] = [modetype, printf('[ %d-line signature.  Press "s" to show. ]', modeline2 - modeline)]
                                let modetype = ''
                        endif
                endif

                let lnum = lnum + 1
        endwhile
endfunction

function! NM_cmd_show_foldtext()
        return b:nm_fold_data[v:foldstart][1]
endfunction


" --- helper functions

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
