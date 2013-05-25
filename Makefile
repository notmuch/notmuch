# We want the all target to be the implicit target (if no target is
# given explicitly on the command line) so mention it first.
all:

# List all subdirectories here. Each contains its own Makefile.local.
# Use of '=', without '+=', seems to be required for out-of-tree
# builds to work.
subdirs = compat completion emacs lib man parse-time-string performance-test util test

# We make all targets depend on the Makefiles themselves.
global_deps = Makefile Makefile.config Makefile.local \
	$(subdirs:%=%/Makefile) $(subdirs:%=%/Makefile.local)

# Sub-directory Makefile.local fragments can append to these variables
# to have directory-specific cflags as necessary.

extra_cflags :=
extra_cxxflags :=

# Get settings from the output of configure by running it to generate
# Makefile.config if it doesn't exist yet.

# If Makefile.config doesn't exist, then srcdir won't be
# set. Conditionally set it (assuming a plain srcdir build) so that
# the rule to generate Makefile.config can actually work.
srcdir ?= .

include Makefile.config
Makefile.config: $(srcdir)/configure
ifeq ($(configure_options),)
	@echo ""
	@echo "Note: Calling ./configure with no command-line arguments. This is often fine,"
	@echo "      but if you want to specify any arguments (such as an alternate prefix"
	@echo "      into which to install), call ./configure explicitly and then make again."
	@echo "      See \"./configure --help\" for more details."
	@echo ""
endif
	$(srcdir)/configure $(configure_options)

# Finally, include all of the Makefile.local fragments where all the
# real work is done.

include $(subdirs:%=%/Makefile.local) Makefile.local
