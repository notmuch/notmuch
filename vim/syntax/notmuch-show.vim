" notmuch show mode syntax file

syntax cluster nmShowMsgDesc contains=nmShowMsgDescWho,nmShowMsgDescDate,nmShowMsgDescTags
syntax match   nmShowMsgDescWho /[^)]\+)/ contained
syntax match   nmShowMsgDescDate / ([^)]\+[0-9]) / contained
syntax match   nmShowMsgDescTags /([^)]\+)$/ contained

syntax cluster nmShowMsgHead contains=nmShowMsgHeadKey,nmShowMsgHeadVal
syntax match   nmShowMsgHeadKey /^[^:]\+: / contained
syntax match   nmShowMsgHeadVal /^\([^:]\+: \)\@<=.*/ contained

syntax cluster nmShowMsgBody contains=@nmShowMsgBodyMail,@nmShowMsgBodyGit
syntax include @nmShowMsgBodyMail syntax/mail.vim

" git-diff.vim marks up diffs in emails, see README for details
silent! syntax include @nmShowMsgBodyGit syntax/git-diff.vim

highlight nmShowMsgDescWho term=reverse cterm=reverse gui=reverse
highlight link nmShowMsgDescDate Type
highlight link nmShowMsgDescTags String

highlight link nmShowMsgHeadKey  Macro
"highlight link nmShowMsgHeadVal  NONE

highlight Folded term=reverse ctermfg=LightGrey ctermbg=Black guifg=LightGray guibg=Black
