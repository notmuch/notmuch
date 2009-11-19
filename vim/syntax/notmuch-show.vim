" notmuch show mode syntax file

syntax region nmShowMessage    start="message{"    end="message}"    contains=nmShowHeader,nmShowBody,nmShowAttachment,nmShowPart
syntax region nmShowHeader     start="header{"     end="header}"     contained
syntax region nmShowBody       start="body{"       end="body}"       contained contains=nmShowAttachment,nmShowPart
syntax region nmShowAttachment start="attachment{" end="attachment}" contained
syntax region nmShowPart       start="part{"       end="part}"       contained

highlight link nmShowMessage    Error
highlight link nmShowHeader     Type
highlight link nmShowBody       Statement
highlight link nmShowAttachment Statement
highlight link nmShowPart       String
