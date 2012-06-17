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
" Contributors: Felipe Contreras <felipe.contreras@gmail.com>,
"   Peter Hartman <peterjohnhartman@gmail.com>
"
" --- configuration defaults {{{1

let s:notmuch_defaults = {
        \ 'g:notmuch_cmd':                           'notmuch'                    ,
        \ 'g:notmuch_sendmail':                      '/usr/sbin/sendmail'         ,
        \ 'g:notmuch_debug':                         0                            ,
        \
        \ 'g:notmuch_search_newest_first':           1                            ,
        \ 'g:notmuch_search_from_column_width':      20                           ,
        \
        \ 'g:notmuch_show_fold_signatures':          1                            ,
        \ 'g:notmuch_show_fold_citations':           1                            ,
        \ 'g:notmuch_show_fold_bodies':              0                            ,
        \ 'g:notmuch_show_fold_headers':             1                            ,
        \
        \ 'g:notmuch_show_message_begin_regexp':     'message{'                ,
        \ 'g:notmuch_show_message_end_regexp':       'message}'                ,
        \ 'g:notmuch_show_header_begin_regexp':      'header{'                 ,
        \ 'g:notmuch_show_header_end_regexp':        'header}'                 ,
        \ 'g:notmuch_show_body_begin_regexp':        'body{'                   ,
        \ 'g:notmuch_show_body_end_regexp':          'body}'                   ,
        \ 'g:notmuch_show_attachment_begin_regexp':  'attachment{'             ,
        \ 'g:notmuch_show_attachment_end_regexp':    'attachment}'             ,
        \ 'g:notmuch_show_part_begin_regexp':        'part{'                   ,
        \ 'g:notmuch_show_part_end_regexp':          'part}'                   ,
        \ 'g:notmuch_show_marker_regexp':            '\\(message\\|header\\|body\\|attachment\\|part\\)[{}].*$',
        \
        \ 'g:notmuch_show_message_parse_regexp':     '\(id:[^ ]*\) depth:\([0-9]*\) match:\([0-9]*\) excluded:\([0-9]*\) filename:\(.*\)$',
        \ 'g:notmuch_show_tags_regexp':              '(\([^)]*\))$'               ,
        \
        \ 'g:notmuch_show_signature_regexp':         '^\(-- \?\|_\+\)$'           ,
        \ 'g:notmuch_show_signature_lines_max':      12                           ,
        \
        \ 'g:notmuch_show_citation_regexp':          '^\s*>'                      ,
        \
        \ 'g:notmuch_compose_insert_mode_start':     1                            ,
        \ 'g:notmuch_compose_header_help':           1                            ,
        \ 'g:notmuch_compose_temp_file_dir':         '~/.notmuch/compose'         ,
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

" defaults for g:notmuch_folders
" override with: let g:notmuch_folders = [ ... ]
let s:notmuch_folders_defaults = [
        \ [ 'new',    'tag:inbox and tag:unread' ],
        \ [ 'inbox',  'tag:inbox'                ],
        \ [ 'unread', 'tag:unread'               ],
        \ ]

" defaults for g:notmuch_signature
" override with: let g:notmuch_signature = [ ... ]
let s:notmuch_signature_defaults = [
        \ '',
        \ '-- ',
        \ 'email sent from notmuch.vim plugin'
        \ ]

" defaults for g:notmuch_compose_headers
" override with: let g:notmuch_compose_headers = [ ... ]
let s:notmuch_compose_headers_defaults = [
        \ 'From',
        \ 'To',
        \ 'Cc',
        \ 'Bcc',
        \ 'Subject'
        \ ]

" --- keyboard mapping definitions {{{1

" --- --- bindings for folders mode {{{2

let g:notmuch_folders_maps = {
        \ 'm':          ':call <SID>NM_new_mail()<CR>',
        \ 's':          ':call <SID>NM_search_prompt()<CR>',
        \ 'q':          ':call <SID>NM_kill_this_buffer()<CR>',
        \ '=':          ':call <SID>NM_folders_refresh_view()<CR>',
        \ '<Enter>':    ':call <SID>NM_folders_show_search()<CR>',
        \ }

" --- --- bindings for search screen {{{2
let g:notmuch_search_maps = {
        \ '<Space>':    ':call <SID>NM_search_show_thread(0)<CR>',
        \ '<Enter>':    ':call <SID>NM_search_show_thread(1)<CR>',
        \ '<C-]>':      ':call <SID>NM_search_expand(''<cword>'')<CR>',
        \ 'I':          ':call <SID>NM_search_mark_read_thread()<CR>',
        \ 'a':          ':call <SID>NM_search_archive_thread()<CR>',
        \ 'A':          ':call <SID>NM_search_mark_read_then_archive_thread()<CR>',
        \ 'D':          ':call <SID>NM_search_delete_thread()<CR>',
        \ 'f':          ':call <SID>NM_search_filter()<CR>',
        \ 'm':          ':call <SID>NM_new_mail()<CR>',
        \ 'o':          ':call <SID>NM_search_toggle_order()<CR>',
        \ 'r':          ':call <SID>NM_search_reply_to_thread()<CR>',
        \ 's':          ':call <SID>NM_search_prompt()<CR>',
        \ ',s':         ':call <SID>NM_search_edit()<CR>',
        \ 't':          ':call <SID>NM_search_filter_by_tag()<CR>',
        \ 'q':          ':call <SID>NM_kill_this_buffer()<CR>',
        \ '+':          ':call <SID>NM_search_add_tags([])<CR>',
        \ '-':          ':call <SID>NM_search_remove_tags([])<CR>',
        \ '=':          ':call <SID>NM_search_refresh_view()<CR>',
        \ '?':          ':echo <SID>NM_search_thread_id() . ''  @ '' . join(<SID>NM_get_search_words())<CR>',
        \ }

" --- --- bindings for show screen {{{2
let g:notmuch_show_maps = {
        \ '<C-P>':      ':call <SID>NM_show_previous(1, 0)<CR>',
        \ '<C-N>':      ':call <SID>NM_show_next(1, 0)<CR>',
        \ '<C-]>':      ':call <SID>NM_search_expand(''<cword>'')<CR>',
        \ 'q':          ':call <SID>NM_kill_this_buffer()<CR>',
        \ 's':          ':call <SID>NM_search_prompt()<CR>',
        \
        \ 'b':          ':call <SID>NM_show_fold_toggle(''b'', ''bdy'', !g:notmuch_show_fold_bodies)<CR>',
        \ 'c':          ':call <SID>NM_show_fold_toggle(''c'', ''cit'', !g:notmuch_show_fold_citations)<CR>',
        \ 'h':          ':call <SID>NM_show_fold_toggle(''h'', ''hdr'', !g:notmuch_show_fold_headers)<CR>',
        \ 'i':          ':call <SID>NM_show_fold_toggle(''i'', ''sig'', !g:notmuch_show_fold_signatures)<CR>',
        \
        \ 'I':          ':call <SID>NM_show_mark_read_thread()<CR>',
        \ 'a':          ':call <SID>NM_show_archive_thread()<CR>',
        \ 'A':          ':call <SID>NM_show_mark_read_then_archive_thread()<CR>',
        \ 'D':          ':call <SID>NM_show_delete_thread()<CR>',
        \ 'd':          ':call <SID>NM_show_delete_message()<CR>',
        \ 'N':          ':call <SID>NM_show_mark_read_then_next_open_message()<CR>',
        \ 'v':          ':call <SID>NM_show_view_all_mime_parts()<CR>',
        \ '+':          ':call <SID>NM_show_add_tag()<CR>',
        \ '-':          ':call <SID>NM_show_remove_tag()<CR>',
        \ '<Space>':    ':call <SID>NM_show_advance_marking_read_and_archiving()<CR>',
        \ '\|':         ':call <SID>NM_show_pipe_message()<CR>',
        \
        \ '<S-Tab>':    ':call <SID>NM_show_previous_fold()<CR>',
        \ '<Tab>':      ':call <SID>NM_show_next_fold()<CR>',
        \ '<Enter>':    ':call <SID>NM_show_toggle_fold()<CR>',
        \
        \ 'r':          ':call <SID>NM_show_reply()<CR>',
        \ 'm':          ':call <SID>NM_new_mail()<CR>',
        \ '?':          ':echo <SID>NM_show_message_id() . ''  @ '' . join(<SID>NM_get_search_words())<CR>',
        \ }

" --- --- bindings for compose screen {{{2
let g:notmuch_compose_nmaps = {
        \ ',s':         ':call <SID>NM_compose_send()<CR>',
        \ ',a':         ':call <SID>NM_compose_attach()<CR>',
        \ ',q':         ':call <SID>NM_kill_this_buffer()<CR>',
        \ '<Tab>':      ':call <SID>NM_compose_next_entry_area()<CR>',
        \ }
let g:notmuch_compose_imaps = {
        \ '<Tab>':      '<C-r>=<SID>NM_compose_next_entry_area()<CR>',
        \ }

" --- implement folders screen {{{1

function! s:NM_cmd_folders(words)
        if len(a:words)
                throw 'Not expecting any arguments for folders command.'
        endif
        let cmd = ['count']
        let disp = []
        let searches = []
        for entry in g:notmuch_folders
                let [ name, search ] = entry
                let data = s:NM_run(cmd + [search])
                let cnt = matchlist(data, '\(\d\+\)')[1]
                call add(disp, printf('%9d %-20s (%s)', cnt, name, search))
                call add(searches, search)
        endfor

        call <SID>NM_newBuffer('', 'folders', join(disp, "\n"))
        let b:nm_searches = searches
        let b:nm_timestamp = reltime()

        call <SID>NM_cmd_folders_mksyntax()
        call <SID>NM_set_map('n', g:notmuch_folders_maps)
        setlocal cursorline
        setlocal nowrap
endfunction

function! s:NM_cmd_folders_mksyntax()
endfunction

" --- --- folders screen action functions {{{2

function! s:NM_folders_refresh_view()
        let lno = line('.')
        setlocal bufhidden=delete
        call s:NM_cmd_folders([])
        exec printf('norm %dG', lno)
endfunction

function! s:NM_folders_show_search()
        let line = line('.')
        let search = b:nm_searches[line-1]

        let prev_bufnr = bufnr('%')
        setlocal bufhidden=hide
        call <SID>NM_cmd_search([search])
        setlocal bufhidden=delete
        let b:nm_prev_bufnr = prev_bufnr
endfunction


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

        call <SID>NM_newBuffer('', 'search', join(disp, "\n"))
        let b:nm_raw_lines = lines
        let b:nm_search_words = a:words

        call <SID>NM_set_map('n', g:notmuch_search_maps)
        setlocal cursorline
        setlocal nowrap
endfunction
function! s:NM_cmd_search_fmtline(line)
        let m = matchlist(a:line, '^\(thread:\S\+\)\s\(.\{12\}\) \[\(\d\+\)/\d\+\] \([^;]\+\); \%(\[[^\[]\+\] \)*\(.*\) (\([^(]*\))$')
        if !len(m)
                return 'ERROR PARSING: ' . a:line
        endif
        let max = g:notmuch_search_from_column_width
        let flist = {}
        for at in split(m[4], '[|,] ')
                let p = split(at, '[@.]')
                let flist[p[0]] = 1
        endfor
        let from = join(keys(flist), ", ")
        return printf("%-12s %3s %-20.20s | %s (%s)", m[2], m[3], from, m[5], m[6])
endfunction

" --- --- search screen action functions {{{2

function! s:NM_search_show_thread(everything)
        let words = [ <SID>NM_search_thread_id() ]
        if !a:everything && exists('b:nm_search_words')
                call extend(words, ['AND', '('])
                call extend(words, b:nm_search_words)
                call add(words, ')')
        endif
        call <SID>NM_cmd_show(words)
        let b:nm_show_everything = a:everything
endfunction

function! s:NM_search_prompt()
        " TODO: input() can support completion
        let text = input('NotMuch Search: ')
        if strlen(text)
                let tags = split(text)
        else
                let tags = s:notmuch_initial_search_words_defaults
        endif
        let prev_bufnr = bufnr('%')
        if b:nm_type == 'search' && exists('b:nm_prev_bufnr')
                " TODO: we intend to replace the current buffer,
                "       ... maybe we could just clear it
                let prev_bufnr = b:nm_prev_bufnr
                setlocal bufhidden=delete
        else
                setlocal bufhidden=hide
        endif
        call <SID>NM_cmd_search(tags)
        setlocal bufhidden=delete
        let b:nm_prev_bufnr = prev_bufnr
endfunction

function! s:NM_search_edit()
        " TODO: input() can support completion
        let text = input('NotMuch Search: ', join(b:nm_search_words, ' '))
        if strlen(text)
                call <SID>NM_cmd_search(split(text))
        endif
endfunction

function! s:NM_search_mark_read_thread()
        call <SID>NM_tag([], ['-unread'])
        norm j
endfunction

function! s:NM_search_archive_thread()
        call <SID>NM_tag([], ['-inbox'])
        norm j
endfunction

function! s:NM_search_mark_read_then_archive_thread()
        call <SID>NM_tag([], ['-unread', '-inbox'])
        norm j
endfunction

function! s:NM_search_delete_thread()
        call <SID>NM_tag([], ['+delete','-inbox','-unread'])
        norm j
endfunction

function! s:NM_search_filter()
        call <SID>NM_search_filter_helper('Filter: ', '', '')
endfunction

function! s:NM_search_filter_by_tag()
        call <SID>NM_search_filter_helper('Filter Tag(s): ', 'tag:', 'and')
endfunction

function! s:NM_search_filter_helper(prompt, prefix, joiner)
        " TODO: input() can support completion
        let text = substitute(input(a:prompt), '\v(^\s*|\s*$|\n)', '', 'g')
        if !strlen(text)
                return
        endif

        let tags = b:nm_search_words + ['AND']
                 \ + <SID>NM_combine_tags(a:prefix, split(text), a:joiner, '()')

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
        let cmd = ['reply']
        call add(cmd, <SID>NM_search_thread_id())
        call add(cmd, 'AND')
        call extend(cmd, <SID>NM_get_search_words())

        let data = <SID>NM_run(cmd)
        let lines = split(data, "\n")
        call <SID>NM_newComposeBuffer(lines, 0)
endfunction

function! s:NM_search_add_tags(tags)
        call <SID>NM_search_add_remove_tags('Add Tag(s): ', '+', a:tags)
endfunction

function! s:NM_search_remove_tags(tags)
        call <SID>NM_search_add_remove_tags('Remove Tag(s): ', '-', a:tags)
endfunction

function! s:NM_search_refresh_view()
        let lno = line('.')
        let prev_bufnr = b:nm_prev_bufnr
        setlocal bufhidden=delete
        call <SID>NM_cmd_search(b:nm_search_words)
        let b:nm_prev_bufnr = prev_bufnr
        " FIXME: should find the line of the thread we were on if possible
        exec printf('norm %dG', lno)
endfunction

" --- --- search screen helper functions {{{2

function! s:NM_search_thread_id()
        if !exists('b:nm_raw_lines')
                throw 'Eeek! no b:nm_raw_lines'
        endif
        let mnum = line('.') - 1
        if len(b:nm_raw_lines) <= mnum
                return ''
        endif
        let info = b:nm_raw_lines[mnum]
        let what = split(info, '\s\+')[0]
        return what
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
        call map(tags, 'a:prefix . v:val')
        call <SID>NM_tag([], tags)
endfunction

" --- implement show screen {{{1

function! s:NM_cmd_show(words)
        let prev_bufnr = bufnr('%')
        let data = s:NM_run(['show', '--entire-thread'] + a:words)
        let lines = split(data, "\n")

        let info = s:NM_cmd_show_parse(lines)

        setlocal bufhidden=hide
        call <SID>NM_newBuffer('', 'show', join(info['disp'], "\n"))
        setlocal bufhidden=delete
        let b:nm_search_words = a:words
        let b:nm_raw_info = info
        let b:nm_prev_bufnr = prev_bufnr

        call <SID>NM_cmd_show_mkfolds()
        call <SID>NM_cmd_show_mksyntax()
        call <SID>NM_set_map('n', g:notmuch_show_maps)
        setlocal foldtext=NM_cmd_show_foldtext()
        setlocal fillchars=
        setlocal foldcolumn=6

endfunction

function! s:NM_show_previous(can_change_thread, find_matching)
        let everything = exists('b:nm_show_everything') ? b:nm_show_everything : 0
        let info = b:nm_raw_info
        let lnum = line('.')
        for msg in reverse(copy(info['msgs']))
                if a:find_matching && msg['match'] == '0'
                        continue
                endif
                if lnum <= msg['start']
                        continue
                endif

                exec printf('norm %dGzt', msg['start'])
                " TODO: try to fit the message on screen
                return
        endfor
        if !a:can_change_thread
                return
        endif
        call <SID>NM_kill_this_buffer()
        if line('.') > 1
                norm k
                call <SID>NM_search_show_thread(everything)
                norm G
                call <SID>NM_show_previous(0, a:find_matching)
        else
                echo 'No more messages.'
        endif
endfunction

function! s:NM_show_next(can_change_thread, find_matching)
        let info = b:nm_raw_info
        let lnum = line('.')
        for msg in info['msgs']
                if a:find_matching && msg['match'] == '0'
                        continue
                endif
                if lnum >= msg['start']
                        continue
                endif

                exec printf('norm %dGzt', msg['start'])
                " TODO: try to fit the message on screen
                return
        endfor
        if a:can_change_thread
                call <SID>NM_show_next_thread()
        endif
endfunction

function! s:NM_show_next_thread()
        let everything = exists('b:nm_show_everything') ? b:nm_show_everything : 0
        call <SID>NM_kill_this_buffer()
        if line('.') != line('$')
                norm j
                call <SID>NM_search_show_thread(everything)
        else
                echo 'No more messages.'
        endif
endfunction

function! s:NM_show_mark_read_thread()
        call <SID>NM_tag(b:nm_search_words, ['-unread'])
        call <SID>NM_show_next_thread()
endfunction

function! s:NM_show_archive_thread()
        call <SID>NM_tag(b:nm_search_words, ['-inbox'])
        call <SID>NM_show_next_thread()
endfunction

function! s:NM_show_mark_read_then_archive_thread()
        call <SID>NM_tag(b:nm_search_words, ['-unread', '-inbox'])
        call <SID>NM_show_next_thread()
endfunction

function! s:NM_show_delete_thread()
        call <SID>NM_tag(b:nm_search_words, ['+delete', '-inbox', '-unread'])
        call <SID>NM_show_next_thread()
endfunction

function! s:NM_show_delete_message()
        let msg = <SID>NM_show_get_message_for_line(line('.'))
        call <SID>NM_tag([msg['id']], ['+delete', '-inbox', '-unread'])
endfunction

function! s:NM_show_mark_read_then_next_open_message()
        echo 'not implemented'
endfunction

function! s:NM_show_previous_message()
        echo 'not implemented'
endfunction

function! s:NM_show_reply()
        let cmd = ['reply']
        call add(cmd, <SID>NM_show_message_id())
        call add(cmd, 'AND')
        call extend(cmd, <SID>NM_get_search_words())

        let data = <SID>NM_run(cmd)
        let lines = split(data, "\n")
        call <SID>NM_newComposeBuffer(lines, 0)
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

" if entire message is not visible scroll down 1/2 page or less to get to the bottom of message
" otherwise go to next message
" any message that is viewed entirely has inbox and unread tags removed
function! s:NM_show_advance_marking_read_and_archiving()
        let advance_tags = ['unread', 'inbox']

        let vis_top = line('w0')
        let vis_bot = line('w$')

        let msg_top = <SID>NM_show_get_message_for_line(vis_top)
        if !has_key(msg_top,'id')
                throw "No top visible message."
        endif

        " if the top message is the last message, just expunge the entire thread and move on
        if msg_top['end'] == line('$')
                let ids = []
                for msg in b:nm_raw_info['msgs']
                        if has_key(msg,'match') && msg['match'] != '0'
                                call add(ids, msg['id'])
                        endif
                endfor
                let filter = <SID>NM_combine_tags('tag:', advance_tags, 'OR', '()')
                         \ + ['AND']
                         \ + <SID>NM_combine_tags('', ids, 'OR', '()')
                call map(advance_tags, '"-" . v:val')
                call <SID>NM_tag(filter, advance_tags)
                call <SID>NM_show_next(1, 1)
                return
        endif

        let msg_bot = <SID>NM_show_get_message_for_line(vis_bot)
        if !has_key(msg_bot,'id')
                throw "No bottom visible message."
        endif

        " if entire message fits on the screen, read/archive it, move to the next one
        if msg_top['id'] != msg_bot['id'] || msg_top['end'] <= vis_bot
                exec printf('norm %dG', vis_top)
                call <SID>NM_show_next(0, 1)
                if has_key(msg_top,'match') && msg_top['match'] != '0'
                        redraw
                        " do this last to hide the latency
                        let filter = <SID>NM_combine_tags('tag:', advance_tags, 'OR', '()')
                                 \ + ['AND', msg_top['id']]
                        call map(advance_tags, '"-" . v:val')
                        call <SID>NM_tag(filter, advance_tags)
                endif
                return
        endif

        " entire message does not fit on the screen, scroll down to bottom, max 1/2 screen
        let jmp = winheight(winnr()) / 2
        let max = msg_bot['end'] - vis_bot
        if jmp > max
                let jmp = max
        endif
        exec printf('norm %dGzt', vis_top + jmp)
        return
endfunction

function! s:NM_show_pipe_message()
        echo 'not implemented'
endfunction

function! s:NM_show_previous_fold()
        echo 'not implemented'
endfunction

function! s:NM_show_next_fold()
        echo 'not implemented'
endfunction

function! s:NM_show_toggle_fold()
        echo 'not implemented'
endfunction


" --- --- show screen helper functions {{{2

function! s:NM_show_get_message_for_line(line)
        for msg in b:nm_raw_info['msgs']
                if a:line > msg['end']
                        continue
                endif
                return msg
        endfor
        return {}
endfunction

function! s:NM_show_message_id()
        if !exists('b:nm_raw_info')
                throw 'Eeek! no b:nm_raw_info'
        endif
        let msg = <SID>NM_show_get_message_for_line(line('.'))
        if has_key(msg,'id')
                return msg['id']
        endif
        return ''
endfunction

function! s:NM_show_fold_toggle(key, type, fold)
        let info = b:nm_raw_info
        let act = 'open'
        if a:fold
                let act = 'close'
        endif
        for fld in info['folds']
                if fld[0] != a:type
                        continue
                endif
                "let idx = fld[3]
                "let msg = info['msgs'][idx]
                "if has_key(msg,'match') && msg['match'] == '0'
                "        continue
                "endif
                let cls = foldclosed(fld[1])
                if cls != -1 && cls != fld[1]
                        continue
                endif
                exec printf('%dfold%s', fld[1], act)
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
                                                if !part_end
                                                        let outlnum = outlnum - 1
                                                endif
                                                let foldinfo = [ mode_type, mode_start, outlnum, len(info['msgs']),
                                                               \ printf('[ %d-line citation.  Press "c" to show. ]', 1 + outlnum - mode_start) ]
                                                let mode_type = ''
                                        endif
                                elseif mode_type == 'sig'
                                        let outlnum = len(info['disp'])
                                        if (outlnum - mode_start) > g:notmuch_show_signature_lines_max
                                                let mode_type = ''
                                        elseif part_end
                                                let foldinfo = [ mode_type, mode_start, outlnum, len(info['msgs']),
                                                               \ printf('[ %d-line signature.  Press "i" to show. ]', 1 + outlnum - mode_start) ]
                                                let mode_type = ''
                                        endif
                                endif
                        endif

                        if part_end
                                " FIXME: this is a hack for handling two folds being added for one line
                                "         we should handle adding a fold in a function
                                if len(foldinfo) && foldinfo[1] < foldinfo[2]
                                        call add(info['folds'], foldinfo[0:3])
                                        let info['foldtext'][foldinfo[1]] = foldinfo[4]
                                endif

                                let foldinfo = [ 'text', part_start, part_end, len(info['msgs']),
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
                                let foldinfo = [ 'bdy', body_start, body_end, len(info['msgs']),
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
                                " We don't yet handle nested parts, so pop
                                " multipart/* immediately so text/plain
                                " sub-parts are parsed properly
                                if match(in_part, '^multipart/') != -1
                                        let in_part = ''
                                else
                                        let part_start = len(info['disp']) + 1
                                endif
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
                                        let foldinfo = [ 'hdr', hdr_start, hdr_end, len(info['msgs']),
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

                                let foldinfo = [ 'msg', msg['start'], msg['end'], len(info['msgs']),
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
                                        let msg['match'] = m[3]
                                        let msg['excluded'] = m[4]
                                        let msg['filename'] = m[5]
                                endif

                                let in_message = 1
                        endif
                endif

                if len(foldinfo) && foldinfo[1] < foldinfo[2]
                        call add(info['folds'], foldinfo[0:3])
                        let info['foldtext'][foldinfo[1]] = foldinfo[4]
                endif
        endfor
        return info
endfunction

function! s:NM_cmd_show_mkfolds()
        let info = b:nm_raw_info

        for afold in info['folds']
                exec printf('%d,%dfold', afold[1], afold[2])
                let state = 'open'
                if (afold[0] == 'sig' && g:notmuch_show_fold_signatures)
                 \ || (afold[0] == 'cit' && g:notmuch_show_fold_citations)
                 \ || (afold[0] == 'bdy' && g:notmuch_show_fold_bodies)
                 \ || (afold[0] == 'hdr' && g:notmuch_show_fold_headers)
                        let state = 'close'
                elseif afold[0] == 'msg'
                        let idx = afold[3]
                        let msg = info['msgs'][idx]
                        if has_key(msg,'match') && msg['match'] == '0'
                                let state = 'close'
                        endif
                endif
                exec printf('%dfold%s', afold[1], state)
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


" --- implement compose screen {{{1

function! s:NM_cmd_compose(words, body_lines)
        let lines = []
        let start_on_line = 0

        let hdrs = { }
        for word in a:words
                let m = matchlist(word, '^\(\w[^:]*\):\s*\(.*\)\s*$')
                if !len(m)
                        throw 'Eeek! bad parameter ''' . string(word) . ''''
                endif
                let key = substitute(m[1], '\<\w', '\U&', 'g')
                if !has_key(hdrs, key)
                        let hdrs[key] = []
                endif
                if strlen(m[2])
                        call add(hdrs[key], m[2])
                endif
        endfor

        if !has_key(hdrs, 'From') || !len(hdrs['From'])
                let me = <SID>NM_compose_get_user_email()
                let hdrs['From'] = [ me ]
        endif

        for key in g:notmuch_compose_headers
                let text = has_key(hdrs, key) ? join(hdrs[key], ', ') : ''
                call add(lines, key . ': ' . text)
                if !start_on_line && !strlen(text)
                        let start_on_line = len(lines)
                endif
        endfor

        for [key,val] in items(hdrs)
                if match(g:notmuch_compose_headers, key) == -1
                        let line = key . ': ' . join(val, ', ')
                        call add(lines, line)
                endif
        endfor

        call add(lines, '')
        if !start_on_line
                let start_on_line = len(lines) + 1
        endif

        if len(a:body_lines)
                call extend(lines, a:body_lines)
        else
                call extend(lines, [ '', '' ])
        endif

        call <SID>NM_newComposeBuffer(lines, start_on_line)
endfunction

function! s:NM_compose_send()
        call <SID>NM_assert_buffer_type('compose')
        let fname = expand('%')
        let lnum = 1
        let line = getline(lnum)
        let lst_hdr = ''
        while match(line, '^$') == -1
                if !exists("hdr_starts") && match(line, '^Notmuch-Help:') == -1
                        let hdr_starts = lnum - 1
                endif
                let lnum = lnum + 1
                let line = getline(lnum)
        endwhile
        let body_starts = lnum - 1

        call append(body_starts, 'Date: ' . strftime('%a, %d %b %Y %H:%M:%S %z'))
        exec printf(':0,%dd', hdr_starts)
        write

        let line = getline(1)
        let m = matchlist(line, '^From:\s*\(.*\)\s*<\(.*\)>$')
        if (len(m) >= 2)
                let from = m[2]
        else
                let m = matchlist(line, '^From:\s*\(.*\)$')
                let from = m[1]
        endif

        let cmdtxt = g:notmuch_sendmail . ' -t -f ' . from . ' < ' . fname
        let out = system(cmdtxt)
        let err = v:shell_error
        if err
                undo
                write
                call <SID>NM_newBuffer('new', 'error',
                            \ "While running...\n" .
                            \ '  ' . cmdtxt . "\n" .
                            \ "\n" .
                            \ "Failed with...\n" .
                            \ substitute(out, '^', '  ', 'g'))
                echohl Error
                echo 'Eeek! unable to send mail'
                echohl None
                return
        endif

        if !exists('b:nm_prev_bufnr')
                bdelete
        else
                let prev_bufnr = b:nm_prev_bufnr
                bdelete
                if prev_bufnr == bufnr('%')
                        exec printf("buffer %d", prev_bufnr)
                endif
        endif
        call delete(fname)
        echo 'Mail sent successfully.'
endfunction

function! s:NM_compose_attach()
        echo 'not implemented'
endfunction

function! s:NM_compose_next_entry_area()
        let lnum = line('.')
        let hdr_end = <SID>NM_compose_find_line_match(1,'^$',1)
        if lnum < hdr_end
                let lnum = lnum + 1
                let line = getline(lnum)
                if match(line, '^\([^:]\+\):\s*$') == -1
                        call cursor(lnum, strlen(line) + 1)
                        return ''
                endif
                while match(getline(lnum+1), '^\s') != -1
                        let lnum = lnum + 1
                endwhile
                call cursor(lnum, strlen(getline(lnum)) + 1)
                return ''

        elseif lnum == hdr_end
                call cursor(lnum+1, strlen(getline(lnum+1)) + 1)
                return ''
        endif
        if mode() == 'i'
                if !getbufvar(bufnr('.'), '&et')
                        return "\t"
                endif
		let space = ''
		let shiftwidth = a:shiftwidth
		let shiftwidth = shiftwidth - ((virtcol('.')-1) % shiftwidth)
                " we assume no one has shiftwidth set to more than 40 :)
                return '                                        '[0:shiftwidth]
        endif
endfunction

" --- --- compose screen helper functions {{{2

function! s:NM_compose_get_user_email()
        " TODO: do this properly (still), i.e., allow for multiple email accounts
        let email = substitute(system('notmuch config get user.primary_email'), '\v(^\s*|\s*$|\n)', '', 'g')
	return email
endfunction

function! s:NM_compose_find_line_match(start, pattern, failure)
        let lnum = a:start
        let lend = line('$')
        while lnum < lend
                if match(getline(lnum), a:pattern) != -1
                        return lnum
                endif
                let lnum = lnum + 1
        endwhile
        return a:failure
endfunction


" --- notmuch helper functions {{{1

function! s:NM_newBuffer(how, type, content)
        if strlen(a:how)
                exec a:how
        else
                enew
        endif
        setlocal buftype=nofile readonly modifiable scrolloff=0 sidescrolloff=0
        silent put=a:content
        keepjumps 0d
        setlocal nomodifiable
        execute printf('set filetype=notmuch-%s', a:type)
        execute printf('set syntax=notmuch-%s', a:type)
        let b:nm_type = a:type
endfunction

function! s:NM_newFileBuffer(fdir, fname, type, lines)
        let fdir = expand(a:fdir)
        if !isdirectory(fdir)
                call mkdir(fdir, 'p')
        endif
        let file_name = <SID>NM_mktemp(fdir, a:fname)
        if writefile(a:lines, file_name)
                throw 'Eeek! couldn''t write to temporary file ' . file_name
        endif
        exec printf('edit %s', file_name)
        setlocal buftype= noreadonly modifiable scrolloff=0 sidescrolloff=0
        execute printf('set filetype=notmuch-%s', a:type)
        execute printf('set syntax=notmuch-%s', a:type)
        let b:nm_type = a:type
endfunction

function! s:NM_newComposeBuffer(lines, start_on_line)
        let lines = a:lines
        let start_on_line = a:start_on_line
        let real_hdr_start = 1
        if g:notmuch_compose_header_help
                let help_lines = [
                  \ 'Notmuch-Help: Type in your message here; to help you use these bindings:',
                  \ 'Notmuch-Help:   ,a    - attach a file',
                  \ 'Notmuch-Help:   ,s    - send the message (Notmuch-Help lines will be removed)',
                  \ 'Notmuch-Help:   ,q    - abort the message',
                  \ 'Notmuch-Help:   <Tab> - skip through header lines',
                  \ ]
                call extend(lines, help_lines, 0)
                let real_hdr_start = len(help_lines)
                if start_on_line > 0
                        let start_on_line = start_on_line + len(help_lines)
                endif
        endif
        call extend(lines, g:notmuch_signature)


        let prev_bufnr = bufnr('%')
        setlocal bufhidden=hide
        call <SID>NM_newFileBuffer(g:notmuch_compose_temp_file_dir, '%s.mail',
                                  \ 'compose', lines)
        setlocal bufhidden=hide
        let b:nm_prev_bufnr = prev_bufnr

        call <SID>NM_set_map('n', g:notmuch_compose_nmaps)
        call <SID>NM_set_map('i', g:notmuch_compose_imaps)

        if start_on_line > 0 && start_on_line <= len(lines)
                call cursor(start_on_line, strlen(getline(start_on_line)) + 1)
        else
                call cursor(real_hdr_start, strlen(getline(real_hdr_start)) + 1)
                call <SID>NM_compose_next_entry_area()
        endif

        if g:notmuch_compose_insert_mode_start
                startinsert!
        endif
        echo 'Type your message, use <TAB> to jump to next header and then body.'
endfunction

function! s:NM_assert_buffer_type(type)
        if !exists('b:nm_type') || b:nm_type != a:type
                throw printf('Eeek! expected type %s, but got %s.', a:type,
                            \ exists(b:nm_type) ? b:nm_type : 'something else')
        endif
endfunction

function! s:NM_mktemp(dir, name)
        let time_stamp = strftime('%Y%m%d-%H%M%S')
        let file_name = substitute(a:dir,'/*$','/','') . printf(a:name, time_stamp)
        " TODO: check if it exists, try again
        return file_name
endfunction

function! s:NM_shell_escape(word)
        " TODO: use shellescape()
        let word = substitute(a:word, '''', '\\''', 'g')
        return '''' . word . ''''
endfunction

" this function was taken from git.vim, then fixed up
" http://github.com/motemen/git-vim
function! s:NM_shell_split(cmd)
        let l:split_cmd = []
        let cmd = a:cmd
        let iStart = 0
        while 1
                let t = match(cmd, '\S', iStart)
                if t < iStart
                        break
                endif
                let iStart = t

                let iSpace = match(cmd, '\v(\s|$)', iStart)
                if iSpace < iStart
                        break
                endif

                let iQuote1 = match(cmd, '\(^["'']\|[^\\]\@<=["'']\)', iStart)
                if iQuote1 > iSpace || iQuote1 < iStart
                        let iEnd = iSpace - 1
                        let l:split_cmd += [ cmd[iStart : iEnd] ]
                else
                        let q = cmd[iQuote1]
                        let iQuote2 = match(cmd, '[^\\]\@<=[' . q . ']', iQuote1 + 1)
                        if iQuote2 < iQuote1
                                throw 'No matching ' . q . ' quote'
                        endif
                        let iEnd = iQuote2
                        let l:split_cmd += [ cmd[iStart+1 : iEnd-1 ] ]
                endif


                let iStart = iEnd + 1
        endwhile

        return l:split_cmd
endfunction


function! s:NM_run(args)
        let words = a:args
        call map(words, 's:NM_shell_escape(v:val)')
        let cmd = g:notmuch_cmd . ' ' . join(words) . '< /dev/null'

        if exists('g:notmuch_debug') && g:notmuch_debug
                let start = reltime()
                let out = system(cmd)
                let err = v:shell_error
                let delta = reltime(start)

                echo printf('[%s] {%s} %s', reltimestr(delta), string(err), string(cmd))
        else
                let out = system(cmd)
                let err = v:shell_error
        endif

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
        call <SID>NM_cmd_compose([], [])
endfunction

" --- tag manipulation helpers {{{1

" used to combine an array of words with prefixes and separators
" example:
"     NM_combine_tags('tag:', ['one', 'two', 'three'], 'OR', '()')
"  -> ['(', 'tag:one', 'OR', 'tag:two', 'OR', 'tag:three', ')']
function! s:NM_combine_tags(word_prefix, words, separator, brackets)
        let res = []
        for word in a:words
                if len(res) && strlen(a:separator)
                        call add(res, a:separator)
                endif
                call add(res, a:word_prefix . word)
        endfor
        if len(res) > 1 && strlen(a:brackets)
                if strlen(a:brackets) != 2
                        throw 'Eeek! brackets arg to NM_combine_tags must be 2 chars'
                endif
                call insert(res, a:brackets[0])
                call add(res, a:brackets[1])
        endif
        return res
endfunction

" --- other helpers {{{1

function! s:NM_get_search_words()
        if !exists('b:nm_search_words')
                throw 'Eeek! no b:nm_search_words'
        endif
        return b:nm_search_words
endfunction

function! s:NM_kill_this_buffer()
        if exists('b:nm_prev_bufnr')
                let prev_bufnr = b:nm_prev_bufnr
                bdelete!
                exec printf("buffer %d", prev_bufnr)
        else
                echo "This is the last buffer; use :q<CR> to quit."
        endif
endfunction

function! s:NM_search_expand(arg)
        let word = expand(a:arg)
        let prev_bufnr = bufnr('%')
        setlocal bufhidden=hide
        call <SID>NM_cmd_search([word])
        setlocal bufhidden=delete
        let b:nm_prev_bufnr = prev_bufnr
endfunction

function! s:NM_tag(filter, tags)
        let filter = len(a:filter) ? a:filter : [<SID>NM_search_thread_id()]
        if !len(filter)
                throw 'Eeek! I couldn''t find the thread id!'
        endif
        let args = ['tag']
        call extend(args, a:tags)
        call add(args, '--')
        call extend(args, filter)
        " TODO: handle errors
        call <SID>NM_run(args)
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
                " FIXME: not sure why this didn't work when dflt is an array
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
if !exists('g:notmuch_folders')
        let g:notmuch_folders = s:notmuch_folders_defaults
endif

if !exists('g:notmuch_signature')
        let g:notmuch_signature = s:notmuch_signature_defaults
endif
if !exists('g:notmuch_compose_headers')
        let g:notmuch_compose_headers = s:notmuch_compose_headers_defaults
endif

" --- assign keymaps {{{1

function! s:NM_set_map(type, maps)
        nmapclear
        for [key, code] in items(a:maps)
                exec printf('%snoremap <buffer> %s %s', a:type, key, code)
        endfor
        " --- this is a hack for development :)
        nnoremap ,nmr :runtime! plugin/notmuch.vim<CR>
endfunction

" --- command handler {{{1

function! NotMuch(args)
        let args = a:args
        if !strlen(args)
                let args = 'folders'
        endif

        let words = <SID>NM_shell_split(args)
        if words[0] == 'folders' || words[0] == 'f'
                let words = words[1:]
                call <SID>NM_cmd_folders(words)

        elseif words[0] == 'search' || words[0] == 's'
                if len(words) > 1
                        let words = words[1:]
                elseif exists('b:nm_search_words')
                        let words = b:nm_search_words
                else
                        let words = g:notmuch_initial_search_words
                endif
                call <SID>NM_cmd_search(words)

        elseif words[0] == 'show'
                echoe 'show is not yet implemented.'

        elseif words[0] == 'new' || words[0] == 'compose'
                let words = words[1:]
                call <SID>NM_cmd_compose(words, [])
        endif
endfunction
function! CompleteNotMuch(arg_lead, cmd_line, cursor_pos)
        return []
endfunction


" --- glue {{{1

command! -nargs=* -complete=customlist,CompleteNotMuch NotMuch call NotMuch(<q-args>)
cabbrev  notmuch <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'NotMuch' : 'notmuch')<CR>

" vim: set ft=vim ts=8 sw=8 et foldmethod=marker :
