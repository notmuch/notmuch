" notmuch show mode syntax file

syntax region nmShowMessage    start='message{'    end='message}'    contains=nmBlockStart,nmShowHeader,nmShowBody,nmShowAttachment,nmShowPart,nmBlockEnd
syntax region nmShowHeader     start='header{'     end='header}'     contained contains=nmBlockStart,nmBlockEnd
syntax region nmShowBody       start='body{'       end='body}'       contained contains=nmBlockStart,nmShowAttachment,nmShowPart,nmBlockEnd
syntax region nmShowAttachment start='attachment{' end='attachment}' contained contains=nmBlockStart,nmBlockEnd
syntax region nmShowPart       start='part{'       end='part}'       contained contains=nmBlockStart,nmBlockEnd

syntax region nmBlockStart     start='^[a-z]\+{'    end='$'            oneline
syntax region nmBlockEnd       start='^[a-z]\+}'    end='$'            oneline

highlight link nmShowMessage    Error
highlight link nmShowHeader     Type
highlight link nmShowBody       Statement
highlight link nmShowAttachment Statement
highlight link nmShowPart       String
highlight link nmBlockStart     Ignore
highlight link nmBlockEnd       Ignore

highlight Folded term=reverse ctermfg=LightGrey ctermbg=Black guifg=LightGray guibg=Black
