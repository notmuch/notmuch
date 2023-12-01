syn match diffRemoved	"^-.*"
syn match diffAdded	"^+.*"

syn match diffSeparator	"^---$"
syn match diffSubname	" @@..*"ms=s+3 contained
syn match diffLine	"^@.*" contains=diffSubname

syn match diffFile	"^diff .*"
syn match diffNewFile	"^+++ .*"
syn match diffOldFile	"^--- .*"

hi def link diffOldFile		diffFile
hi def link diffNewFile		diffFile

hi def link diffFile		Type
hi def link diffRemoved		Special
hi def link diffAdded		Identifier
hi def link diffLine		Statement
hi def link diffSubname		PreProc

syntax match gitDiffStatLine /^ .\{-}\zs[+-]\+$/ contains=gitDiffStatAdd,gitDiffStatDelete
syntax match gitDiffStatAdd    /+/ contained
syntax match gitDiffStatDelete /-/ contained

hi def link gitDiffStatAdd diffAdded
hi def link gitDiffStatDelete diffRemoved
