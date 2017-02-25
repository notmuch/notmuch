# bash completion for notmuch                              -*- shell-script -*-
#
# Copyright © 2013 Jani Nikula
#
# Based on the bash-completion package:
# https://github.com/scop/bash-completion
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see https://www.gnu.org/licenses/ .
#
# Author: Jani Nikula <jani@nikula.org>
#
#
# BUGS:
#
# Add space after an --option without parameter (e.g. reply --decrypt)
# on completion.
#

_notmuch_shared_options="--help --uuid= --version"

# $1: current input of the form prefix:partialinput, where prefix is
# to or from.
_notmuch_email()
{
    local output prefix cur

    prefix="${1%%:*}"
    cur="${1#*:}"

    # Cut the input to be completed at punctuation because
    # (apparently) Xapian does not support the trailing wildcard '*'
    # operator for input with punctuation. We let compgen handle the
    # extra filtering required.
    cur="${cur%%[^a-zA-Z0-9]*}"

    case "$prefix" in
	# Note: It would be more accurate and less surprising to have
	# output=recipients here for to: addresses, but as gathering
	# the recipient addresses requires disk access for each
	# matching message, this becomes prohibitively slow.
	to|from) output=sender;;
	*) return;;
    esac

    # Only emit plain, lower case, unique addresses.
    notmuch address --output=$output $prefix:"${cur}*" | \
	sed 's/[^<]*<\([^>]*\)>/\1/' | tr "[:upper:]" "[:lower:]" | sort -u
}

_notmuch_mimetype()
{
    # use mime types from mime-support package if available, and fall
    # back to a handful of common ones otherwise
    if [ -r "/etc/mime.types" ]; then
	sed -n '/^[[:alpha:]]/{s/[[:space:]].*//;p;}' /etc/mime.types
    else
	cat <<EOF
application/gzip
application/msword
application/pdf
application/zip
audio/mpeg
audio/ogg
image/gif
image/jpeg
image/png
message/rfc822
text/calendar
text/html
text/plain
text/vcard
text/x-diff
text/x-vcalendar
EOF
    fi
}

_notmuch_search_terms()
{
    local cur prev words cword split
    # handle search prefixes and tags with colons and equal signs
    _init_completion -n := || return

    case "${cur}" in
	tag:*)
	    COMPREPLY=( $(compgen -P "tag:" -W "`notmuch search --output=tags \*`" -- ${cur##tag:}) )
	    ;;
	to:*)
	    COMPREPLY=( $(compgen -P "to:" -W "`_notmuch_email ${cur}`" -- ${cur##to:}) )
	    ;;
	from:*)
	    COMPREPLY=( $(compgen -P "from:" -W "`_notmuch_email ${cur}`" -- ${cur##from:}) )
	    ;;
	path:*)
	    local path=`notmuch config get database.path`
	    compopt -o nospace
	    COMPREPLY=( $(compgen -d "$path/${cur##path:}" | sed "s|^$path/||" ) )
	    ;;
	folder:*)
	    local path=`notmuch config get database.path`
	    compopt -o nospace
	    COMPREPLY=( $(compgen -d "$path/${cur##folder:}" | \
		sed "s|^$path/||" | grep -v "\(^\|/\)\(cur\|new\|tmp\)$" ) )
	    ;;
	mimetype:*)
	    compopt -o nospace
	    COMPREPLY=( $(compgen -P "mimetype:" -W "`_notmuch_mimetype ${cur}`" -- ${cur##mimetype:}) )
	    ;;
	query:*)
	    compopt -o nospace
	    COMPREPLY=( $(compgen -P "query:" -W "`notmuch config list | sed -n '/^query\./s/^query\.\([^=]*\)=.*/\1/p'`" -- ${cur##query:}) )
	    ;;
	*)
	    local search_terms="from: to: subject: attachment: mimetype: tag: id: thread: folder: path: date: lastmod: query: property:"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "${search_terms}" -- ${cur}) )
	    ;;
    esac
    # handle search prefixes and tags with colons
    __ltrim_colon_completions "${cur}"
}

_notmuch_compact()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--backup)
	    _filedir -d
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--backup= --quiet ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
    esac
}

_notmuch_config()
{
    local cur prev words cword split
    _init_completion || return

    case "${prev}" in
	config)
	    COMPREPLY=( $(compgen -W "get set list" -- ${cur}) )
	    ;;
	get|set)
	    COMPREPLY=( $(compgen -W "`notmuch config list | sed 's/=.*\$//'`" -- ${cur}) )
	    ;;
	# these will also complete on config get, but we don't care
	database.path)
	    _filedir -d
	    ;;
	maildir.synchronize_flags)
	    COMPREPLY=( $(compgen -W "true false" -- ${cur}) )
	    ;;
    esac
}

_notmuch_count()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--output)
	    COMPREPLY=( $( compgen -W "messages threads files" -- "${cur}" ) )
	    return
	    ;;
	--exclude)
	    COMPREPLY=( $( compgen -W "true false" -- "${cur}" ) )
	    return
	    ;;
	--input)
	    _filedir
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--output= --exclude= --batch --input= --lastmod ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
	*)
	    _notmuch_search_terms
	    ;;
    esac
}

_notmuch_dump()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--format)
	    COMPREPLY=( $( compgen -W "sup batch-tag" -- "${cur}" ) )
	    return
	    ;;
	--output)
	    _filedir
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--gzip --format= --output= ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
	*)
	    _notmuch_search_terms
	    ;;
    esac
}

_notmuch_emacs_mua()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--to|--cc|--bcc)
	    COMPREPLY=( $(compgen -W "`_notmuch_email to:${cur}`" -- ${cur}) )
	    return
	    ;;
	--body)
	    _filedir
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
        -*)
	    local options="--subject= --to= --cc= --bcc= --body= --no-window-system --client --auto-daemon --create-frame --print --help --hello"

	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
	*)
	    COMPREPLY=( $(compgen -W "`_notmuch_email to:${cur}`" -- ${cur}) )
	    return
	    ;;
    esac
}

_notmuch_insert()
{
    local cur prev words cword split
    # handle tags with colons and equal signs
    _init_completion -s -n := || return

    $split &&
    case "${prev}" in
	--folder)
	    local path=`notmuch config get database.path`
	    compopt -o nospace
	    COMPREPLY=( $(compgen -d "$path/${cur}" | \
		sed "s|^$path/||" | grep -v "\(^\|/\)\(cur\|new\|tmp\)$" ) )
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	--*)
	    local options="--create-folder --folder= --keep --no-hooks ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    return
	    ;;
	+*)
	    COMPREPLY=( $(compgen -P "+" -W "`notmuch search --output=tags \*`" -- ${cur##+}) )
	    ;;
	-*)
	    COMPREPLY=( $(compgen -P "-" -W "`notmuch search --output=tags \*`" -- ${cur##-}) )
	    ;;
    esac
    # handle tags with colons
    __ltrim_colon_completions "${cur}"
}

_notmuch_new()
{
    local cur prev words cword split
    _init_completion || return

    case "${cur}" in
	-*)
	    local options="--no-hooks --quiet ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "${options}" -- ${cur}) )
	    ;;
    esac
}

_notmuch_reply()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--format)
	    COMPREPLY=( $( compgen -W "default json sexp headers-only" -- "${cur}" ) )
	    return
	    ;;
	--reply-to)
	    COMPREPLY=( $( compgen -W "all sender" -- "${cur}" ) )
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--format= --format-version= --reply-to= --decrypt ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
	*)
	    _notmuch_search_terms
	    ;;
    esac
}

_notmuch_restore()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--format)
	    COMPREPLY=( $( compgen -W "sup batch-tag auto" -- "${cur}" ) )
	    return
	    ;;
	--input)
	    _filedir
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--format= --accumulate --input= ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
    esac
}

_notmuch_search()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--format)
	    COMPREPLY=( $( compgen -W "json sexp text text0" -- "${cur}" ) )
	    return
	    ;;
	--output)
	    COMPREPLY=( $( compgen -W "summary threads messages files tags" -- "${cur}" ) )
	    return
	    ;;
	--sort)
	    COMPREPLY=( $( compgen -W "newest-first oldest-first" -- "${cur}" ) )
	    return
	    ;;
	--exclude)
	    COMPREPLY=( $( compgen -W "true false flag all" -- "${cur}" ) )
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--format= --output= --sort= --offset= --limit= --exclude= --duplicate= ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
	*)
	    _notmuch_search_terms
	    ;;
    esac
}

_notmuch_address()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--format)
	    COMPREPLY=( $( compgen -W "json sexp text text0" -- "${cur}" ) )
	    return
	    ;;
	--output)
	    COMPREPLY=( $( compgen -W "sender recipients count" -- "${cur}" ) )
	    return
	    ;;
	--sort)
	    COMPREPLY=( $( compgen -W "newest-first oldest-first" -- "${cur}" ) )
	    return
	    ;;
	--exclude)
	    COMPREPLY=( $( compgen -W "true false flag all" -- "${cur}" ) )
	    return
	    ;;
	--deduplicate)
	    COMPREPLY=( $( compgen -W "no mailbox address" -- "${cur}" ) )
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--format= --output= --sort= --exclude= --deduplicate= ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
	*)
	    _notmuch_search_terms
	    ;;
    esac
}

_notmuch_show()
{
    local cur prev words cword split
    _init_completion -s || return

    $split &&
    case "${prev}" in
	--entire-thread)
	    COMPREPLY=( $( compgen -W "true false" -- "${cur}" ) )
	    return
	    ;;
	--format)
	    COMPREPLY=( $( compgen -W "text json sexp mbox raw" -- "${cur}" ) )
	    return
	    ;;
	--exclude|--body)
	    COMPREPLY=( $( compgen -W "true false" -- "${cur}" ) )
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	-*)
	    local options="--entire-thread= --format= --exclude= --body= --format-version= --part= --verify --decrypt --include-html ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    ;;
	*)
	    _notmuch_search_terms
	    ;;
    esac
}

_notmuch_tag()
{
    local cur prev words cword split
    # handle tags with colons and equal signs
    _init_completion -s -n := || return

    $split &&
    case "${prev}" in
	--input)
	    _filedir
	    return
	    ;;
    esac

    ! $split &&
    case "${cur}" in
	--*)
	    local options="--batch --input= --remove-all ${_notmuch_shared_options}"
	    compopt -o nospace
	    COMPREPLY=( $(compgen -W "$options" -- ${cur}) )
	    return
	    ;;
	+*)
	    COMPREPLY=( $(compgen -P "+" -W "`notmuch search --output=tags \*`" -- ${cur##+}) )
	    ;;
	-*)
	    COMPREPLY=( $(compgen -P "-" -W "`notmuch search --output=tags \*`" -- ${cur##-}) )
	    ;;
	*)
	    _notmuch_search_terms
	    return
	    ;;
    esac
    # handle tags with colons
    __ltrim_colon_completions "${cur}"
}

_notmuch()
{
    local _notmuch_commands="compact config count dump help insert new reply restore search address setup show tag emacs-mua"
    local arg cur prev words cword split

    # require bash-completion with _init_completion
    type -t _init_completion >/dev/null 2>&1 || return

    _init_completion || return

    COMPREPLY=()

    # subcommand
    _get_first_arg

    # complete --help option like the subcommand
    if [ -z "${arg}" -a "${prev}" = "--help" ]; then
	arg="help"
    fi

    if [ -z "${arg}" ]; then
	# top level completion
	case "${cur}" in
	    -*)
		# XXX: handle ${_notmuch_shared_options} and --config=
		local options="--help --version"
		COMPREPLY=( $(compgen -W "${options}" -- ${cur}) )
		;;
	    *)
		COMPREPLY=( $(compgen -W "${_notmuch_commands}" -- ${cur}) )
		;;
	esac
    elif [ "${arg}" = "help" ]; then
	# handle help command specially due to _notmuch_commands usage
	local help_topics="$_notmuch_commands hooks search-terms"
	COMPREPLY=( $(compgen -W "${help_topics}" -- ${cur}) )
    else
	# complete using _notmuch_subcommand if one exist
	local completion_func="_notmuch_${arg//-/_}"
	declare -f $completion_func >/dev/null && $completion_func
    fi
} &&
complete -F _notmuch notmuch
