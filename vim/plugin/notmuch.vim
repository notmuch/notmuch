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

let s:notmuch_show_message_parse_regexp    = '\(id:[^ ]*\) depth:\([0-9]*\) filename:\(.*\)$'
let s:notmuch_show_tags_regexp             = '(\([^)]*\))$'

let s:notmuch_show_signature_regexp        = '^\(-- \?\|_\+\)$'
let s:notmuch_show_signature_lines_max     = 12

let s:notmuch_show_citation_regexp         = '^\s*>'

let s:notmuch_show_headers                 = [ 'Subject', 'From' ]

let s:notmuch_show_fold_signatures         = 1
let s:notmuch_show_fold_citations          = 1

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
        let b:nm_raw_lines = lines

        nnoremap <buffer> <Enter> :call <SID>NM_search_display()<CR>
        nnoremap <buffer> s       :call <SID>NM_cmd_search(split(input('NotMuch Search:')))<CR>
        setlocal cursorline
        setlocal nowrap
endfunction

function! s:NM_search_display()
        if !exists('b:nm_raw_lines')
                echo 'no b:nm_raw_lines'
        else
                let line = line('.')
                let info = b:nm_raw_lines[line-1]
                let what = split(info, '\s\+')[0]
                call s:NM_cmd_show([what])
        endif
endfunction


" --- implement show screen

function! s:NM_cmd_show(words)
        let bufnr = bufnr('%')
        let data = s:NM_run(['show'] + a:words)
        let lines = split(data, "\n")

        let info = s:NM_cmd_show_parse(lines)

        call s:NM_newBuffer('show', join(info['disp'], "\n"))
        setlocal bufhidden=delete
        let b:nm_raw_info = info

        call s:NM_cmd_show_mkfolds()
        call s:NM_cmd_show_mksyntax()
        setlocal foldtext=NM_cmd_show_foldtext()
        setlocal fillchars=
        setlocal foldcolumn=6

        exec printf("nnoremap <buffer> q :b %d<CR>", bufnr)
endfunction

" s:NM_cmd_show_parse returns the following dictionary:
"    'disp':     lines to display
"    'msgs':     message info dicts { start, end, id, depth, filename, descr, header }
"    'folds':    fold info arrays [ type, start, end ]
"    'foldtext': fold text indexed by start line
function! s:NM_cmd_show_parse(inlines)
        let info = { 'disp': [],       
                   \ 'msgs': [],       
                   \ 'folds': [],      
                   \ 'foldtext': {} }  
        let msg = {}
        let hdr = {}

        let in_message = 0
        let in_header = 0
        let in_body = 0
        let in_part = ''

        let body_start = -1
        let part_start = -1

        let mode_type = ''
        let mode_start = -1

        let inlnum = 0
        for line in a:inlines
                let inlnum = inlnum + 1
                let foldinfo = []

                if strlen(in_part)
                        let part_end = 0

                        if match(line, s:notmuch_show_part_end_regexp) != -1
                                let part_end = len(info['disp'])
                        else
                                call add(info['disp'], line)
                        endif

                        if in_part == 'text/plain'
                                if !part_end && mode_type == ''
                                        if match(line, s:notmuch_show_signature_regexp) != -1
                                                let mode_type = 'sig'
                                                let mode_start = len(info['disp'])
                                        elseif match(line, s:notmuch_show_citation_regexp) != -1
                                                let mode_type = 'cit'
                                                let mode_start = len(info['disp'])
                                        endif
                                elseif mode_type == 'cit'
                                        if part_end || match(line, s:notmuch_show_citation_regexp) == -1
                                                let outlnum = len(info['disp']) -1
                                                let foldinfo = [ mode_type, mode_start, outlnum,
                                                               \ printf('[ %d-line citation.  Press "c" to show. ]', outlnum - mode_start) ]
                                                let mode_type = ''
                                        endif
                                elseif mode_type == 'sig'
                                        let outlnum = len(info['disp'])
                                        if (outlnum - mode_start) > s:notmuch_show_signature_lines_max
                                                echoe 'line ' . outlnum . ' stopped matching'
                                                let mode_type = ''
                                        elseif part_end
                                                let foldinfo = [ mode_type, mode_start, outlnum,
                                                               \ printf('[ %d-line signature.  Press "s" to show. ]', outlnum - mode_start) ]
                                                let mode_type = ''
                                        endif
                                endif
                        endif

                        if part_end
                                " FIXME: this is a hack for handling two folds being added for one line
                                "         we should handle addinga fold in a function
                                if len(foldinfo)
                                        call add(info['folds'], foldinfo[0:2])
                                        let info['foldtext'][foldinfo[1]] = foldinfo[3]
                                endif

                                let foldinfo = [ 'text', part_start, part_end,
                                               \ printf('[ %d-line %s.  Press "p" to show. ]', part_end - part_start, in_part) ]
                                let in_part = ''
                                call add(info['disp'], '')
                        endif

                elseif in_body
                        if !has_key(msg,'body_start')
                                let msg['body_start'] = len(info['disp']) + 1
                        endif
                        if match(line, s:notmuch_show_body_end_regexp) != -1
                                let body_end = len(info['disp'])
                                let foldinfo = [ 'body', body_start, body_end,
                                               \ printf('[ BODY %d - %d lines ]', len(info['msgs']), body_end - body_start) ]

                                let in_body = 0

                        elseif match(line, s:notmuch_show_part_begin_regexp) != -1
                                let m = matchlist(line, 'ID: \(\d\+\), Content-type: \(\S\+\)')
                                let in_part = 'unknown'
                                if len(m)
                                        let in_part = m[2]
                                endif
                                call add(info['disp'],
                                         \ printf('--- %s ---', in_part))
                                let part_start = len(info['disp']) + 1
                        endif

                elseif in_header
                        if in_header == 1
                                let msg['descr'] = line
                                call add(info['disp'], line)
                                let in_header = 2
                                let msg['hdr_start'] = len(info['disp']) + 1

                        else
                                if match(line, s:notmuch_show_header_end_regexp) != -1
                                        let msg['header'] = hdr
                                        let in_header = 0
                                        let hdr = {}
                                else
                                        let m = matchlist(line, '^\(\w\+\):\s*\(.*\)$')
                                        if len(m)
                                                let hdr[m[1]] = m[2]
                                                if match(s:notmuch_show_headers, m[1]) != -1
                                                        call add(info['disp'], line)
                                                endif
                                        endif
                                endif
                        endif

                elseif in_message
                        if match(line, s:notmuch_show_message_end_regexp) != -1
                                let msg['end'] = len(info['disp'])
                                call add(info['disp'], '')

                                let foldinfo = [ 'match', msg['start'], msg['end'],
                                               \ printf('[ MSG %d - %s ]', len(info['msgs']), msg['descr']) ]

                                call add(info['msgs'], msg)
                                let msg = {}
                                let in_message = 0
                                let in_header = 0
                                let in_body = 0
                                let in_part = ''

                        elseif match(line, s:notmuch_show_header_begin_regexp) != -1
                                let in_header = 1
                                continue

                        elseif match(line, s:notmuch_show_body_begin_regexp) != -1
                                let body_start = len(info['disp']) + 1
                                let in_body = 1
                                continue
                        endif

                else
                        if match(line, s:notmuch_show_message_begin_regexp) != -1
                                let msg['start'] = len(info['disp']) + 1

                                let m = matchlist(line, s:notmuch_show_message_parse_regexp)
                                if len(m)
                                        let msg['id'] = m[1]
                                        let msg['depth'] = m[2]
                                        let msg['filename'] = m[3]
                                endif

                                let in_message = 1
                        endif
                endif

                if len(foldinfo)
                        call add(info['folds'], foldinfo[0:2])
                        let info['foldtext'][foldinfo[1]] = foldinfo[3]
                endif
        endfor
        return info
endfunction

function! s:NM_cmd_show_mkfolds()
        let info = b:nm_raw_info

        for afold in info['folds']
                exec printf('%d,%dfold', afold[1], afold[2])
                if (afold[0] == 'sig' && s:notmuch_show_fold_signatures)
                 \ || (afold[0] == 'cit' && s:notmuch_show_fold_citations)
                        exec printf('%dfoldclose', afold[1])
                else
                        exec printf('%dfoldopen', afold[1])
                endif
        endfor
endfunction

function! NM_cmd_show_foldtext()
        let foldtext = b:nm_raw_info['foldtext']
        return foldtext[v:foldstart]
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
