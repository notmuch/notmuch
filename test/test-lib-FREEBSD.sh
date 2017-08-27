# If present, use GNU Coreutils instead of a native BSD utils
if command -v gdate >/dev/null
   then
       date () { gdate "$@"; }
       base64 () { gbase64 "$@"; }
       wc () { gwc "$@"; }
       sed () { gsed "$@"; }
       sha256sum () { gsha256sum "$@"; }
   fi
