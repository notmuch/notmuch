# We want the all target to be the implicit target (if no target is
# given explicitly on the command line) so mention it first.
all:

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

# We make all targets depend on the Makefiles themselves.
global_deps = Makefile Makefile.config Makefile.local \
	$(subdirs:%=%/Makefile) $(subdirs:%=%/Makefile.local)

INCLUDE_MORE := yes
ifneq ($(filter clean distclean dataclean, $(word 1, $(MAKECMDGOALS))),)
CLEAN_GOAL := $(word 1, $(MAKECMDGOALS))

# If there are more goals following CLEAN_GOAL, run $(MAKE)s in parts.
ifneq ($(word 2, $(MAKECMDGOALS)),)
INCLUDE_MORE := no
FOLLOWING_GOALS := $(wordlist 2, 99, $(MAKECMDGOALS))

.PHONY: $(FOLLOWING_GOALS) make_in_parts
$(FOLLOWING_GOALS):
	@true
$(CLEAN_GOAL): make_in_parts
make_in_parts:
	$(MAKE) $(CLEAN_GOAL)
	$(MAKE) $(FOLLOWING_GOALS) configure_options="$(configure_options)"
endif

else
CLEAN_GOAL :=
endif

# Potentially speedup make clean, distclean and dataclean ; avoid
# re-creating Makefile.config if it exists but configure is newer.
ifneq ($(CLEAN_GOAL),)
Makefile.config: | $(srcdir)/configure
else
Makefile.config: $(srcdir)/configure
endif
ifeq ($(configure_options),)
	@echo ""
	@echo "Note: Calling ./configure with no command-line arguments. This is often fine,"
	@echo "      but if you want to specify any arguments (such as an alternate prefix"
	@echo "      into which to install), call ./configure explicitly and then make again."
	@echo "      See \"./configure --help\" for more details."
	@echo ""
endif
	$(srcdir)/configure $(configure_options)

ifeq ($(INCLUDE_MORE),yes)
# runtime variable definitions available in all subdirs
include $(srcdir)/Makefile.global
# Finally, include all of the Makefile.local fragments where all the
# real work is done.

include $(subdirs:%=%/Makefile.local) Makefile.local
endif
