" notmuch folders mode syntax file

syntax region nmFoldersCount     start='^' end='\%10v'
syntax region nmFoldersName      start='\%11v' end='\%31v'
syntax match  nmFoldersSearch    /([^()]\+)$/

highlight link nmFoldersCount     Statement
highlight link nmFoldersName      Type
highlight link nmFoldersSearch    String

highlight CursorLine term=reverse cterm=reverse gui=reverse

