# We want the all target to be the implicit target (if no target is
# given explicitly on the command line) so mention it first.
all:

# List all subdirectories here. Each contains its own Makefile.local
subdirs = compat completion emacs lib

# We make all targets depend on the Makefiles themselves.
global_deps = Makefile Makefile.local \
	$(subdirs:%=%/Makefile) $(subdirs:%=%/Makefile.local)

# Finally, include all of the Makefile.local fragments where all the
# real work is done.
include Makefile.local $(subdirs:%=%/Makefile.local)
