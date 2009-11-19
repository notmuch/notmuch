set NOTMUCH_CMD=`notmuch help | awk '/\t/' | cut -f2 |grep -v '^$'`
complete notmuch 'p/1/$NOTMUCH_CMD/'
