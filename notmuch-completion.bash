# Bash completion for notmutch
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
#	search <search-term> [...]
#
#	show <thread-id>
#
#	dump [<filename>]
#
#	restore <filename>

_notmuch()
{
    current="$2"

    commands="help setup new search show dump restore"

    COMPREPLY=()

    if [ "$COMP_CWORD" = "1" ]; then
	COMPREPLY=( $(compgen -W "${commands}" -- ${current}) )
    fi
}
complete -o default -o bashdefault -F _notmuch notmuch
