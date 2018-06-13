#compdef notmuch

# ZSH completion for `notmuch`
# Copyright © 2009 Ingmar Vanhassel <ingmar@exherbo.org>

_notmuch_commands()
{
  local -a notmuch_commands
  notmuch_commands=(
    'help:display documentation for a subcommand'
    'setup:interactively configure notmuch'

    'address:output addresses from matching messages'
    'compact:compact the notmuch database'
    'config:access notmuch configuration file'
    'count:count messages matching the given search terms'
    'dump:creates a plain-text dump of the tags of each message'
    'insert:add a message to the maildir and notmuch database'
    'new:incorporate new mail into the notmuch database'
    'reply:constructs a reply template for a set of messages'
    'restore:restores the tags from the given file (see notmuch dump)'
    'search:search for messages matching the given search terms'
    'show:show messages matching the given search terms'
    'tag:add/remove tags for all messages matching the search terms'
  )

  _describe -t command 'command' notmuch_commands
}

_notmuch_dump()
{
  _files
}

_notmuch_help_topics()
{
  local -a notmuch_help_topics
  notmuch_help_topics=(
    'search-terms:show common search-terms syntax'
  )
  _describe -t notmuch-help-topics 'topic' notmuch_help_topics
}

_notmuch_help()
{
  _alternative \
    _notmuch_commands \
    _notmuch_help_topics
}

_notmuch_restore()
{
  _files
}

_notmuch_search()
{
  _arguments -s : \
    '--max-threads=[display only the first x threads from the search results]:number of threads to show: ' \
    '--first=[omit the first x threads from the search results]:number of threads to omit: ' \
    '--sort=[sort results]:sorting:((newest-first\:"reverse chronological order" oldest-first\:"chronological order"))' \
    '--output=[select what to output]:output:((summary threads messages files tags))'
}

_notmuch_address()
{
  _arguments -s : \
    '--sort=[sort results]:sorting:((newest-first\:"reverse chronological order" oldest-first\:"chronological order"))' \
    '--output=[select what to output]:output:((sender recipients count))'
}

_notmuch()
{
  if (( CURRENT > 2 )) ; then
    local cmd=${words[2]}
    curcontext="${curcontext%:*:*}:notmuch-$cmd"
    (( CURRENT-- ))
    shift words
    _call_function ret _notmuch_$cmd
    return ret
  else
    _notmuch_commands
  fi
}

_notmuch "$@"

# vim: set sw=2 sts=2 ts=2 et ft=zsh :
