" notmuch search mode syntax file

" TODO: I cannot figure out why nmSearchTags is not matching anything :(

syntax region nmSearchDate      start='^' end='\%13v'
syntax region nmSearchCountAndFrom start='\%14v\[' end=';' oneline contains=nmSearchCount,nmSearchFrom
syntax match  nmSearchFrom      ' .*;'                     contained
syntax region nmSearchCount     start='\%14v\[' end='\]'   contained contains=nmSearchCountZero,nmSearchCountSome,nmSearchCountAll
syntax match  nmSearchCountZero '0/\(\d\+\)'               contained
syntax match  nmSearchCountSome '\([1-9]\d*\)/\(\d\+\)'    contained
syntax match  nmSearchCountAll  '\(\d\+\)/\1'              contained
syntax match  nmSearchTags      /([^)]\+)$/

highlight link nmSearchDate      Statement
"highlight link nmSearchCount     Comment
highlight link nmSearchCountZero Function
highlight link nmSearchCountSome Special
highlight link nmSearchCountAll  Type
highlight link nmSearchFrom      Include
highlight link nmSearchTags      String

highlight CursorLine term=reverse cterm=reverse gui=reverse
