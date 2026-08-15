#!/bin/sh

# 100MB for /tmp and ~/.cache tmpfs mounts
TMPSIZE=$((100*1024*1024))

bwrap                                                                       \
    --ro-bind / /                                                           \
    --size "$TMPSIZE" --tmpfs /tmp --size "$TMPSIZE" --tmpfs $HOME/.cache   \
    --dev /dev --unshare-all --new-session -- "$@"
