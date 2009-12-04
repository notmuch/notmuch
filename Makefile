WARN_CXXFLAGS=-Wall -Wextra -Wwrite-strings -Wswitch-enum
WARN_CFLAGS=$(WARN_CXXFLAGS) -Wmissing-declarations

# Additional programs that are used during the compilation process.
EMACS ?= emacs
# Lowercase to avoid clash with GZIP environment variable for passing
# arguments to gzip.
gzip = gzip

bash_completion_dir = /etc/bash_completion.d

all_deps = Makefile Makefile.local Makefile.config \
		   lib/Makefile lib/Makefile.local

extra_cflags :=
extra_cxxflags :=

# Now smash together user's values with our extra values
FINAL_CFLAGS = $(CFLAGS) $(WARN_CFLAGS) $(CONFIGURE_CFLAGS) $(extra_cflags)
FINAL_CXXFLAGS = $(CXXFLAGS) $(WARN_CXXFLAGS) $(CONFIGURE_CXXFLAGS) $(extra_cflags) $(extra_cxxflags)
FINAL_LDFLAGS = $(LDFLAGS) $(CONFIGURE_LDFLAGS)

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
quiet = @printf $(quiet_DOC)$(eval quiet_DOC:=)"  $1 $2	$@\n"; $($1)
endif
# The user has explicitly enabled quiet compilation.
ifeq ($(V),0)
quiet = @printf "  $1	$@\n"; $($1)
endif
# Otherwise, print the full command line.
quiet ?= $($1)

%.o: %.cc $(all_deps)
	$(call quiet,CXX,$(CXXFLAGS)) -c $(FINAL_CXXFLAGS) $< -o $@

%.o: %.c $(all_deps)
	$(call quiet,CC,$(CFLAGS)) -c $(FINAL_CFLAGS) $< -o $@

%.elc: %.el
	$(call quiet,EMACS) -batch -f batch-byte-compile $<

.deps/%.d: %.c $(all_deps)
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CC) -M $(CPPFLAGS) $(FINAL_CFLAGS) $< > $@.$$$$; \
	sed 's,'$$(basename $*)'\.o[ :]*,$*.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.deps/%.d: %.cc $(all_deps)
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CXX) -M $(CPPFLAGS) $(FINAL_CXXFLAGS) $< > $@.$$$$; \
	sed 's,'$$(basename $*)'\.o[ :]*,$*.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

DEPS := $(SRCS:%.c=.deps/%.d)
DEPS := $(DEPS:%.cc=.deps/%.d)
-include $(DEPS)

.PHONY : clean
clean:
	rm -f $(CLEAN); rm -rf .deps
