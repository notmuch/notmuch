# Default FLAGS, (can be overridden by user such as "make CFLAGS=-O2")
WARN_FLAGS=-Wall -Wextra -Wmissing-declarations -Wwrite-strings -Wswitch-enum
CFLAGS=-O2

# Additional programs that are used during the compilation process.
EMACS ?= emacs
# Lowercase to avoid clash with GZIP environment variable for passing
# arguments to gzip.
gzip = gzip

# Additional flags that we will append to whatever the user set.
# These aren't intended for the user to manipulate.
extra_cflags := $(shell pkg-config --cflags glib-2.0 gmime-2.4 talloc)
extra_cxxflags := $(shell xapian-config --cxxflags)

emacs_lispdir := $(shell pkg-config emacs --variable sitepkglispdir)
# Hard-code if this system doesn't have an emacs.pc file
ifeq ($(emacs_lispdir),)
	emacs_lispdir = $(prefix)/share/emacs/site-lisp
endif

bash_completion_dir = /etc/bash_completion.d

all_deps = Makefile Makefile.local Makefile.config \
		   lib/Makefile lib/Makefile.local

# Now smash together user's values with our extra values
override CFLAGS += $(WARN_FLAGS) $(extra_cflags)
override CXXFLAGS += $(WARN_FLAGS) $(extra_cflags) $(extra_cxxflags)

override LDFLAGS += \
	$(shell pkg-config --libs glib-2.0 gmime-2.4 talloc) \
	$(shell xapian-config --libs)

all: notmuch notmuch.1.gz

# Before including any other Makefile fragments, get settings from the
# output of configure
Makefile.config: configure
	./configure

include Makefile.config

include lib/Makefile.local
include compat/Makefile.local
include Makefile.local

# The user has not set any verbosity, default to quiet mode and inform the
# user how to enable verbose compiles.
ifeq ($(V),)
quiet_DOC := "Use \"$(MAKE) V=1\" to see the verbose compile lines.\n"
quiet = @echo $(quiet_DOC)$(eval quiet_DOC:=)"  $1	$@"; $($1)
endif
# The user has explicitly enabled quiet compilation.
ifeq ($(V),0)
quiet = @echo "  $1	$@"; $($1)
endif
# Otherwise, print the full command line.
quiet ?= $($1)

%.o: %.cc $(all_deps)
	$(call quiet,CXX) -c $(CXXFLAGS) $< -o $@

%.o: %.c $(all_deps)
	$(call quiet,CC) -c $(CFLAGS) $< -o $@

%.elc: %.el
	$(call quiet,EMACS) -batch -f batch-byte-compile $<

.deps/%.d: %.c $(all_deps)
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CC) -M $(CPPFLAGS) $(CFLAGS) $< > $@.$$$$; \
	sed 's,'$$(basename $*)'\.o[ :]*,$*.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.deps/%.d: %.cc $(all_deps)
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CXX) -M $(CPPFLAGS) $(CXXFLAGS) $< > $@.$$$$; \
	sed 's,'$$(basename $*)'\.o[ :]*,$*.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

DEPS := $(SRCS:%.c=.deps/%.d)
DEPS := $(DEPS:%.cc=.deps/%.d)
-include $(DEPS)

.PHONY : clean
clean:
	rm -f $(CLEAN); rm -rf .deps
