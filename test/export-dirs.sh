# Source this script to set and export NOTMUCH_SRCDIR and
# NOTMUCH_BUILDDIR.
#
# For this to work, always have current directory somewhere within the
# build directory hierarchy, and run the script sourcing this script
# using a path (relative or absolute) to the source directory.

if [[ -z "${NOTMUCH_SRCDIR}" ]]; then
	export NOTMUCH_SRCDIR="$(cd "$(dirname "$0")"/.. && pwd)"
fi

find_builddir () {
	local dir="$1"

	while [[ -n "$dir" ]] && [[ "$dir" != "/" ]]; do
		if [[ -x "$dir/notmuch" ]] && [[ ! -d "$dir/notmuch" ]]; then
			echo "$dir"
			break
		fi
		dir="$(dirname "$dir")"
	done
}

if [[ -z "${NOTMUCH_BUILDDIR}" ]]; then
	export NOTMUCH_BUILDDIR="$(find_builddir "$(pwd)")"

	if [[ -z "${NOTMUCH_BUILDDIR}" ]]; then
		echo "Run tests in a subdir of built notmuch tree." >&2
		exit 1
	fi
fi
