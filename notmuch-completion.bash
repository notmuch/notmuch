# Bash completion for notmuch
#
# Copyright © 2009 Carl Worth
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see http://www.gnu.org/licenses/ .
#
# Author: Carl Worth <cworth@cworth.org>
#
# Based on "notmuch help" as follows:
#
# Usage: notmuch <command> [args...]
#
# Where <command> and [args...] are as follows:
#
#	setup
#
#	new
#
#	search [options] <search-term> [...]
#
#	show <search-terms>
#
#	reply <search-terms>
#
#	tag +<tag>|-<tag> [...] [--] <search-terms> [...]
#
#	dump [<filename>]
#
#	restore <filename>
#
#	help [<command>]

_notmuch()
{
    current="$2"

    commands="setup new search show reply tag dump restore help"

    help_options="setup new search show reply tag dump restore search-terms"

    COMPREPLY=()
    prev=${COMP_WORDS[COMP_CWORD-1]}

    if [ "$COMP_CWORD" = "1" ]; then
	COMPREPLY=( $(compgen -W "${commands}" -- ${current}) )
    fi

    if [ $prev = "help" ] && [ "$COMP_CWORD" = "2" ]; then
	COMPREPLY=( $(compgen -W "${help_options}" -- ${current}) )
    fi
}
complete -o default -o bashdefault -F _notmuch notmuch
