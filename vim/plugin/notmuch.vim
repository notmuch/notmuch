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

" --- configuration defaults {{{1

let s:notmuch_defaults = {
        \ 'g:notmuch_cmd':                           'notmuch'                    ,
        \
        \ 'g:notmuch_search_newest_first':           1                            ,
        \ 'g:notmuch_search_from_column_width':      20                           ,
        \
        \ 'g:notmuch_show_fold_signatures':          1                            ,
        \ 'g:notmuch_show_fold_citations':           1                            ,
        \ 'g:notmuch_show_fold_bodies':              0                            ,
        \ 'g:notmuch_show_fold_headers':             1                            ,
        \
        \ 'g:notmuch_show_message_begin_regexp':     '^message{'                ,
        \ 'g:notmuch_show_message_end_regexp':       '^message}'                ,
        \ 'g:notmuch_show_header_begin_regexp':      '^header{'                 ,
        \ 'g:notmuch_show_header_end_regexp':        '^header}'                 ,
        \ 'g:notmuch_show_body_begin_regexp':        '^body{'                   ,
        \ 'g:notmuch_show_body_end_regexp':          '^body}'                   ,
        \ 'g:notmuch_show_attachment_begin_regexp':  '^attachment{'             ,
        \ 'g:notmuch_show_attachment_end_regexp':    '^attachment}'             ,
        \ 'g:notmuch_show_part_begin_regexp':        '^part{'                   ,
        \ 'g:notmuch_show_part_end_regexp':          '^part}'                   ,
        \ 'g:notmuch_show_marker_regexp':            '^\\(message\\|header\\|body\\|attachment\\|part\\)[{}].*$',
        \
        \ 'g:notmuch_show_message_parse_regexp':     '\(id:[^ ]*\) depth:\([0-9]*\) filename:\(.*\)$',
        \ 'g:notmuch_show_tags_regexp':              '(\([^)]*\))$'               ,
        \
        \ 'g:notmuch_show_signature_regexp':         '^\(-- \?\|_\+\)$'           ,
        \ 'g:notmuch_show_signature_lines_max':      12                           ,
        \
        \ 'g:notmuch_show_citation_regexp':          '^\s*>'                      ,
        \ }

" defaults for g:notmuch_initial_search_words
" override with: let g:notmuch_initial_search_words = [ ... ]
let s:notmuch_initial_search_words_defaults = [
        \ 'tag:inbox and tag:unread',
        \ ]

" defaults for g:notmuch_show_headers
" override with: let g:notmuch_show_headers = [ ... ]
let s:notmuch_show_headers_defaults = [
        \ 'Subject',
        \ 'To',
        \ 'Cc',
        \ 'Bcc',
        \ 'Date',
        \ ]

" --- keyboard mapping definitions {{{1

" --- --- bindings for search screen {{{2
let g:notmuch_search_maps = {
        \ '<Enter>':    ':call <SID>NM_search_show_thread()<CR>',
        \ 'a':          ':call <SID>NM_search_archive_thread()<CR>',
        \ 'f':          ':call <SID>NM_search_filter()<CR>',
        \ 'm':          ':call <SID>NM_new_mail()<CR>',
        \ 'o':          ':call <SID>NM_search_toggle_order()<CR>',
        \ 'r':          ':call <SID>NM_search_reply_to_thread()<CR>',
        \ 's':          ':call <SID>NM_search_prompt()<CR>',
        \ 'S':          ':call <SID>NM_search_edit()<CR>',
        \ 't':          ':call <SID>NM_search_filter_by_tag()<CR>',
        \ 'q':          ':call <SID>NM_kill_this_buffer()<CR>',
        \ '+':          ':call <SID>NM_search_add_tags([])<CR>',
        \ '-':          ':call <SID>NM_search_remove_tags([])<CR>',
        \ '=':          ':call <SID>NM_search_refresh_view()<CR>',
        \ }

" --- --- bindings for show screen {{{2
let g:notmuch_show_maps = {
        \ '<C-P>':      ':call <SID>NM_show_previous(1)<CR>',
        \ '<C-N>':      ':call <SID>NM_show_next(1)<CR>',
        \ 'q':          ':call <SID>NM_kill_this_buffer()<CR>',
        \
        \ 'b':          ':call <SID>NM_show_fold_toggle(''b'', ''bdy'', !g:notmuch_show_fold_bodies)<CR>',
        \ 'c':          ':call <SID>NM_show_fold_toggle(''c'', ''cit'', !g:notmuch_show_fold_citations)<CR>',
        \ 'h':          ':call <SID>NM_show_fold_toggle(''h'', ''hdr'', !g:notmuch_show_fold_headers)<CR>',
        \ 's':          ':call <SID>NM_show_fold_toggle(''s'', ''sig'', !g:notmuch_show_fold_signatures)<CR>',
        \
        \ 'a':          ':call <SID>NM_show_archive_thread()<CR>',
        \ 'A':          ':call <SID>NM_show_mark_read_then_archive_thread()<CR>',
        \ 'N':          ':call <SID>NM_show_mark_read_then_next_open_message()<CR>',
        \ 'v':          ':call <SID>NM_show_view_all_mime_parts()<CR>',
        \ '+':          ':call <SID>NM_show_add_tag()<CR>',
        \ '-':          ':call <SID>NM_show_remove_tag()<CR>',
        \ '<Space>':    ':call <SID>NM_show_advance_marking_read_and_archiving()<CR>',
        \ '\|':         ':call <SID>NM_show_pipe_message()<CR>',
        \
        \ 'r':          ':call <SID>NM_show_reply()<CR>',
        \ 'm':          ':call <SID>NM_new_mail()<CR>',
        \ }


" --- implement search screen {{{1

function! s:NM_cmd_search(words)
        let cmd = ['search']
        if g:notmuch_search_newest_first
                let cmd = cmd + ['--sort=newest-first']
        else
                let cmd = cmd + ['--sort=oldest-first']
        endif
        let data = s:NM_run(cmd + a:words)
        let lines = split(data, "\n")
        let disp = copy(lines)
        call map(disp, 's:NM_cmd_search_fmtline(v:val)')

        call <SID>NM_newBuffer('search', join(disp, "\n"))
        let b:nm_raw_lines = lines
        let b:nm_search_words = a:words

        call <SID>NM_cmd_search_mksyntax()
        call <SID>NM_set_map(g:notmuch_search_maps)
        setlocal cursorline
        setlocal nowrap
endfunction
function! s:NM_cmd_search_fmtline(line)
        let m = matchlist(a:line, '^\(thread:\S\+\)\s\([^]]\+\]\) \([^;]\+\); \(.*\) (\([^(]*\))$')
        if !len(m)
                return 'ERROR PARSING: ' . a:line
        endif
        let max = g:notmuch_search_from_column_width
        let from = m[3]
        if strlen(from) >= max
                let from = m[3][0:max-4] . '...'
        endif
        return printf('%s %-20s | %s (%s)', m[2], from, m[4], m[5])
endfunction
function! s:NM_cmd_search_mksyntax()
        syntax clear nmSearchFrom
        "syntax region nmSearchFrom start='\]\@<=' end='.'me=e+5,he=e+5,re=e+5 oneline contained
        "syntax match nmSearchFrom /\]\@<=.\{10\}/ oneline contained
        exec printf('syntax match nmSearchFrom /\(\] \)\@<=.\{%d\}/ oneline contained', g:notmuch_search_from_column_width)
        "exec printf('syntax region nmSearchFrom start=''\%%%dv'' end=''\%%%dv'' oneline contained', 20, 30)
endfunction

" --- --- search screen action functions {{{2

function! s:NM_search_show_thread()
        let id = <SID>NM_search_find_thread_id()
        if id != ''
                call <SID>NM_cmd_show([id])
        endif
endfunction

function! s:NM_search_prompt()
        " TODO: input() can support completion
        let text = input('NotMuch Search: ')
        if strlen(text)
                let tags = split(text)
        else
                let tags = s:notmuch_initial_search_words_defaults
        endif
        setlocal bufhidden=delete
        call <SID>NM_cmd_search(tags)
endfunction

function! s:NM_search_edit()
        " TODO: input() can support completion
        let text = input('NotMuch Search: ', join(b:nm_search_words, ' '))
        if strlen(text)
                call <SID>NM_cmd_search(split(text))
        endif
endfunction

function! s:NM_search_archive_thread()
        call <SID>NM_add_remove_tags_on_screen('-', ['inbox'])
        call <SID>NM_add_remove_tags('-', ['inbox'])
        norm j
endfunction

function! s:NM_search_filter()
        call <SID>NM_search_filter_helper('Filter: ', '')
endfunction

function! s:NM_search_filter_by_tag()
        call <SID>NM_search_filter_helper('Filter Tag(s): ', 'tag:')
endfunction

function! s:NM_search_filter_helper(prompt, prefix)
        " TODO: input() can support completion
        let text = input(a:prompt)
        if !strlen(text)
                return
        endif

        let tags = split(text)
        map(tags, 'and a:prefix . v:val')
        let tags = b:nm_search_words + tags
        echo tags

        let prev_bufnr = bufnr('%')
        setlocal bufhidden=hide
        call <SID>NM_cmd_search(tags)
        setlocal bufhidden=delete
        let b:nm_prev_bufnr = prev_bufnr
endfunction

function! s:NM_search_toggle_order()
        let g:notmuch_search_newest_first = !g:notmuch_search_newest_first
        " FIXME: maybe this would be better done w/o reading re-reading the lines
        "         reversing the b:nm_raw_lines and the buffer lines would be better
        call <SID>NM_search_refresh_view()
endfunction

function! s:NM_search_reply_to_thread()
        echo 'not implemented'
endfunction

function! s:NM_search_add_tags(tags)
        call <SID>NM_search_add_remove_tags('Add Tag(s): ', '+', a:tags)
endfunction

function! s:NM_search_remove_tags(tags)
        call <SID>NM_search_add_remove_tags('Remove Tag(s): ', '-', a:tags)
endfunction

function! s:NM_search_refresh_view()
        let lno = line('.')
        setlocal bufhidden=delete
        call <SID>NM_cmd_search(b:nm_search_words)
        " FIXME: should find the line of the thread we were on if possible
        exec printf('norm %dG', lno)
endfunction

" --- --- search screen helper functions {{{2

function! s:NM_search_find_thread_id()
        if !exists('b:nm_raw_lines')
                echoe 'no b:nm_raw_lines'
                return ''
        else
                let line = line('.')
                let info = b:nm_raw_lines[line-1]
                let what = split(info, '\s\+')[0]
                return what
        endif
endfunction

function! s:NM_search_add_remove_tags(prompt, prefix, intags)
        if type(a:intags) != type([]) || len(a:intags) == 0
                " TODO: input() can support completion
                let text = input(a:prompt)
                if !strlen(text)
                        return
                endif
                let tags = split(text, ' ')
        else
                let tags = a:intags
        endif
        call <SID>NM_add_remove_tags(a:prefix, tags)
        call <SID>NM_add_remove_tags_on_screen(a:prefix, tags)
endfunction

" --- implement show screen {{{1

function! s:NM_cmd_show(words)
        let prev_bufnr = bufnr('%')
        let data = s:NM_run(['show'] + a:words)
        let lines = split(data, "\n")

        let info = s:NM_cmd_show_parse(lines)

        setlocal bufhidden=hide
        call <SID>NM_newBuffer('show', join(info['disp'], "\n"))
        setlocal bufhidden=delete
        let b:nm_raw_info = info
        let b:nm_prev_bufnr = prev_bufnr

        call <SID>NM_cmd_show_mkfolds()
        call <SID>NM_cmd_show_mksyntax()
        call <SID>NM_set_map(g:notmuch_show_maps)
        setlocal foldtext=NM_cmd_show_foldtext()
        setlocal fillchars=
        setlocal foldcolumn=6

endfunction

function! s:NM_show_previous(can_change_thread)
        let info = b:nm_raw_info
        let lnum = line('.')
        for msg in reverse(copy(info['msgs']))
                if lnum <= msg['start']
                        continue
                endif

                exec printf('norm %dG', msg['start'])
                " TODO: try to fit the message on screen
                norm zz
                return
        endfor
        if !a:can_change_thread
                return
        endif
        call <SID>NM_kill_this_buffer()
        if line('.') != line('0')
                norm k
                call <SID>NM_search_show_thread()
                norm G
                call <SID>NM_show_previous(0)
        else
                echo 'No more messages.'
        endif
endfunction

function! s:NM_show_next(can_change_thread)
        let info = b:nm_raw_info
        let lnum = line('.')
        for msg in info['msgs']
                if lnum >= msg['start']
                        continue
                endif

                exec printf('norm %dG', msg['start'])
                " TODO: try to fit the message on screen
                norm zz
                return
        endfor
        if !a:can_change_thread
                return
        endif
        call <SID>NM_kill_this_buffer()
        if line('.') != line('$')
                norm j
                call <SID>NM_search_show_thread()
        else
                echo 'No more messages.'
        endif
endfunction

function! s:NM_show_archive_thread()
        echo 'not implemented'
endfunction

function! s:NM_show_mark_read_then_archive_thread()
        echo 'not implemented'
endfunction

function! s:NM_show_next_message()
        echo 'not implemented'
endfunction

function! s:NM_show_mark_read_then_next_open_message()
        echo 'not implemented'
endfunction

function! s:NM_show_previous_message()
        echo 'not implemented'
endfunction

function! s:NM_show_reply()
        echo 'not implemented'
endfunction

function! s:NM_show_view_all_mime_parts()
        echo 'not implemented'
endfunction

function! s:NM_show_view_raw_message()
        echo 'not implemented'
endfunction

function! s:NM_show_add_tag()
        echo 'not implemented'
endfunction

function! s:NM_show_remove_tag()
        echo 'not implemented'
endfunction

function! s:NM_show_advance_marking_read_and_archiving()
        echo 'not implemented'
endfunction

function! s:NM_show_pipe_message()
        echo 'not implemented'
endfunction

" --- --- search screen helper functions {{{2

function! s:NM_show_fold_toggle(key, type, fold)
        let info = b:nm_raw_info
        let act = 'open'
        if a:fold
                let act = 'close'
        endif
        for fld in info['folds']
                if fld[0] == a:type
                        exec printf('%dfold%s', fld[1], act)
                endif
        endfor
        exec printf('nnoremap <buffer> %s :call <SID>NM_show_fold_toggle(''%s'', ''%s'', %d)<CR>', a:key, a:key, a:type, !a:fold)
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

                        if match(line, g:notmuch_show_part_end_regexp) != -1
                                let part_end = len(info['disp'])
                        else
                                call add(info['disp'], line)
                        endif

                        if in_part == 'text/plain'
                                if !part_end && mode_type == ''
                                        if match(line, g:notmuch_show_signature_regexp) != -1
                                                let mode_type = 'sig'
                                                let mode_start = len(info['disp'])
                                        elseif match(line, g:notmuch_show_citation_regexp) != -1
                                                let mode_type = 'cit'
                                                let mode_start = len(info['disp'])
                                        endif
                                elseif mode_type == 'cit'
                                        if part_end || match(line, g:notmuch_show_citation_regexp) == -1
                                                let outlnum = len(info['disp'])
                                                if mode_start != outlnum
                                                        let foldinfo = [ mode_type, mode_start, outlnum-1,
                                                                       \ printf('[ %d-line citation.  Press "c" to show. ]', outlnum - mode_start) ]
                                                endif
                                                let mode_type = ''
                                        endif
                                elseif mode_type == 'sig'
                                        let outlnum = len(info['disp'])
                                        if (outlnum - mode_start) > g:notmuch_show_signature_lines_max
                                                let mode_type = ''
                                        elseif part_end
                                                if mode_start != outlnum
                                                        let foldinfo = [ mode_type, mode_start, outlnum-1,
                                                                       \ printf('[ %d-line signature.  Press "s" to show. ]', outlnum - mode_start) ]
                                                endif
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
                        if match(line, g:notmuch_show_body_end_regexp) != -1
                                let body_end = len(info['disp'])
                                let foldinfo = [ 'bdy', body_start, body_end,
                                               \ printf('[ BODY %d - %d lines ]', len(info['msgs']), body_end - body_start) ]

                                let in_body = 0

                        elseif match(line, g:notmuch_show_part_begin_regexp) != -1
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
                                if match(line, g:notmuch_show_header_end_regexp) != -1
                                        let hdr_start = msg['hdr_start']+1
                                        let hdr_end = len(info['disp'])
                                        let foldinfo = [ 'hdr', hdr_start, hdr_end,
                                               \ printf('[ %d-line headers.  Press "h" to show. ]', hdr_end + 1 - hdr_start) ]
                                        let msg['header'] = hdr
                                        let in_header = 0
                                        let hdr = {}
                                else
                                        let m = matchlist(line, '^\(\w\+\):\s*\(.*\)$')
                                        if len(m)
                                                let hdr[m[1]] = m[2]
                                                if match(g:notmuch_show_headers, m[1]) != -1
                                                        call add(info['disp'], line)
                                                endif
                                        endif
                                endif
                        endif

                elseif in_message
                        if match(line, g:notmuch_show_message_end_regexp) != -1
                                let msg['end'] = len(info['disp'])
                                call add(info['disp'], '')

                                let foldinfo = [ 'msg', msg['start'], msg['end'],
                                               \ printf('[ MSG %d - %s ]', len(info['msgs']), msg['descr']) ]

                                call add(info['msgs'], msg)
                                let msg = {}
                                let in_message = 0
                                let in_header = 0
                                let in_body = 0
                                let in_part = ''

                        elseif match(line, g:notmuch_show_header_begin_regexp) != -1
                                let in_header = 1
                                continue

                        elseif match(line, g:notmuch_show_body_begin_regexp) != -1
                                let body_start = len(info['disp']) + 1
                                let in_body = 1
                                continue
                        endif

                else
                        if match(line, g:notmuch_show_message_begin_regexp) != -1
                                let msg['start'] = len(info['disp']) + 1

                                let m = matchlist(line, g:notmuch_show_message_parse_regexp)
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
                if (afold[0] == 'sig' && g:notmuch_show_fold_signatures)
                 \ || (afold[0] == 'cit' && g:notmuch_show_fold_citations)
                 \ || (afold[0] == 'bdy' && g:notmuch_show_fold_bodies)
                 \ || (afold[0] == 'hdr' && g:notmuch_show_fold_headers)
                        exec printf('%dfoldclose', afold[1])
                else
                        exec printf('%dfoldopen', afold[1])
                endif
        endfor
endfunction

function! s:NM_cmd_show_mksyntax()
        let info = b:nm_raw_info
        let cnt = 0
        for msg in info['msgs']
                let cnt = cnt + 1
                let start = msg['start']
                let hdr_start = msg['hdr_start']
                let body_start = msg['body_start']
                let end = msg['end']
                exec printf('syntax region nmShowMsg%dDesc start=''\%%%dl'' end=''\%%%dl'' contains=@nmShowMsgDesc', cnt, start, start+1)
                exec printf('syntax region nmShowMsg%dHead start=''\%%%dl'' end=''\%%%dl'' contains=@nmShowMsgHead', cnt, hdr_start, body_start)
                exec printf('syntax region nmShowMsg%dBody start=''\%%%dl'' end=''\%%%dl'' contains=@nmShowMsgBody', cnt, body_start, end)
        endfor
endfunction

function! NM_cmd_show_foldtext()
        let foldtext = b:nm_raw_info['foldtext']
        return foldtext[v:foldstart]
endfunction


" --- notmuch helper functions {{{1

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

        let start = reltime()
        let out = system(cmd)
        let err = v:shell_error
        let delta = reltime(start)

        echo printf('[%s] {%s} %s', reltimestr(delta), string(err), string(cmd))

        if err
                echohl Error
                echo substitute(out, '\n*$', '', '')
                echohl None
                return ''
        else
                return out
        endif
endfunction

" --- external mail handling helpers {{{1

function! s:NM_new_mail()
        echo 'not implemented'
endfunction

" --- other helpers {{{1

function! s:NM_kill_this_buffer()
        if exists('b:nm_prev_bufnr')
                setlocal bufhidden=delete
                exec printf(":buffer %d", b:nm_prev_bufnr)
        else
                echo "Nothing to kill."
        endif
endfunction

function! s:NM_add_remove_tags(prefix, tags)
        let id = <SID>NM_search_find_thread_id()
        if id == ''
                echoe 'Eeek! I couldn''t find the thead id!'
        endif
        call map(a:tags, 'a:prefix . v:val')
        " TODO: handle errors
        call <SID>NM_run(['tag'] + a:tags + ['--', id])
endfunction

function! s:NM_add_remove_tags_on_screen(prefix, tags)
        let online = ''
        setlocal modifiable
        if a:prefix == '-'
                for tagname in a:tags
                        exec printf('silent %ss/(\([^)]*\)\<%s\>\([^)]*\))$/(\1\2)/', online, tagname)
                endfor
        else
                for tagname in a:tags
                        exec printf('silent %ss/(\([^)]*\)\([^)]*\))$/(\1 %s)/', online, tagname)
                endfor
        endif
        setlocal nomodifiable
endfunction

" --- process and set the defaults {{{1

function! NM_set_defaults(force)
        for [key, dflt] in items(s:notmuch_defaults)
                let cmd = ''
                if !a:force && exists(key) && type(dflt) == type(eval(key))
                        continue
                elseif type(dflt) == type(0)
                        let cmd = printf('let %s = %d', key, dflt)
                elseif type(dflt) == type('')
                        let cmd = printf('let %s = ''%s''', key, dflt)
                "elseif type(dflt) == type([])
                "        let cmd = printf('let %s = %s', key, string(dflt))
                else
                        echoe printf('E: Unknown type in NM_set_defaults(%d) using [%s,%s]',
                                                \ a:force, key, string(dflt))
                        continue
                endif
                exec cmd
        endfor
endfunction
call NM_set_defaults(0)

" for some reason NM_set_defaults() didn't work for arrays...
if !exists('g:notmuch_show_headers')
        let g:notmuch_show_headers = s:notmuch_show_headers_defaults
endif
if !exists('g:notmuch_initial_search_words')
        let g:notmuch_initial_search_words = s:notmuch_initial_search_words_defaults
endif


" --- assign keymaps {{{1

function! s:NM_set_map(maps)
        nmapclear
        for [key, code] in items(a:maps)
                exec printf('nnoremap <buffer> %s %s', key, code)
        endfor
        " --- this is a hack for development :)
        nnoremap ,nmr :source ~/.vim/plugin/notmuch.vim<CR>:call NotMuch('')<CR>
endfunction

" --- command handler {{{1

function! NotMuch(args)
        if !strlen(a:args)
                if exists('b:nm_search_words')
                        let words = b:nm_search_words
                else
                        let words = g:notmuch_initial_search_words
                endif
                call <SID>NM_cmd_search(words)
                return
        endif

        echo "blarg!"

        let words = split(a:args)
        " TODO: handle commands passed as arguments
endfunction
function! CompleteNotMuch(arg_lead, cmd_line, cursor_pos)
        return []
endfunction


" --- glue {{{1

command! -nargs=* -complete=customlist,CompleteNotMuch NotMuch call NotMuch(<q-args>)
cabbrev  notmuch <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'NotMuch' : 'notmuch')<CR>

" vim: set ft=vim ts=8 sw=8 et foldmethod=marker :
