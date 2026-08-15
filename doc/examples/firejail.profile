# Firejail profile for notmuch attachment filter

quiet

# Persistent global definitions
include globals.local

include disable-common.inc

apparmor
caps.drop all
machine-id
net none
no3d
nodvd
nogroups
noinput
nonewprivs
noroot
nosound
notv
nou2f
novideo
protocol unix
seccomp
seccomp.block-secondary
tracelog
x11 none

disable-mnt
private-cache
private-dev
private-etc gconf
private-tmp

dbus-user none
dbus-system none

restrict-namespaces

read-only ${HOME}
