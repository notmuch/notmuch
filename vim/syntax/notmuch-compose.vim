runtime! syntax/mail.vim

syntax region nmComposeHelp          contains=nmComposeHelpLine start='^Notmuch-Help:\%1l' end='^\(Notmuch-Help:\)\@!'
syntax match  nmComposeHelpLine      /Notmuch-Help:/ contained

highlight link nmComposeHelp        Include
highlight link nmComposeHelpLine    Error
