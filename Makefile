# Default FLAGS, (can be overriden by user such as "make CFLAGS=-O2")
WARN_FLAGS=-Wall -Wextra -Wmissing-declarations -Wwrite-strings -Wswitch-enum
CFLAGS=-O2

# Additional flags that we will append to whatever the user set.
# These aren't intended for the user to manipulate.
extra_cflags = `pkg-config --cflags glib-2.0 gmime-2.4 talloc`
extra_cxxflags = `xapian-config --cxxflags`

# Now smash together user's values with our extra values
override CFLAGS += $(WARN_FLAGS) $(extra_cflags)
override CXXFLAGS += $(WARN_FLAGS) $(extra_cflags) $(extra_cxxflags)

override LDFLAGS += `pkg-config --libs glib-2.0 gmime-2.4 talloc` \
			`xapian-config --libs`

# Include our local Makfile.local first so that its first target is default
include Makefile.local
include lib/Makefile.local

# And get user settings from the output of configure
include Makefile.config

%.o: %.cc
	$(CXX) -c $(CFLAGS) $(CXXFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

.deps/%.d: %.c
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CC) -M $(CPPFLAGS) $(CFLAGS) $< > $@.$$$$; \
	sed 's,'$$(basename $*)'\.o[ :]*,$*.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.deps/%.d: %.cc
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CXX) -M $(CPPFLAGS) $(CXXFLAGS) $< > $@.$$$$; \
	sed 's,'$$(basename $*)'\.o[ :]*,$*.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

DEPS := $(SRCS:%.c=.deps/%.d)
DEPS := $(DEPS:%.cc=.deps/%.d)
-include $(DEPS)

clean:
	rm -f $(CLEAN); rm -rf .deps
