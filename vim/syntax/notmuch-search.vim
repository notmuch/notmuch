syntax region nmSearch		start=/^/ end=/$/		oneline contains=nmSearchDate
syntax match nmSearchDate	/^.\{-13}/			contained nextgroup=nmSearchNum
syntax match nmSearchNum	/.\{-4}/			contained nextgroup=nmSearchFrom
syntax match nmSearchFrom	/.\{-21}/			contained nextgroup=nmSearchSubject
syntax match nmSearchSubject	/.\{0,}\(([^()]\+)$\)\@=/	contained nextgroup=nmSearchTags
syntax match nmSearchTags	/.\+$/				contained

highlight link nmSearchDate	Statement
highlight link nmSearchNum	Type
highlight link nmSearchFrom	Include
highlight link nmSearchSubject	Normal
highlight link nmSearchTags	String
