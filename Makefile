WARN_FLAGS=-Wall -Wextra -Wmissing-declarations -Wwrite-strings -Wswitch-enum

NOTMUCH_DEPENDS_FLAGS=-I./lib `pkg-config --cflags glib-2.0 gmime-2.4 talloc`
NOTMUCH_CXX_DEPENDS_FLAGS=$(NOTMUCH_DEPENDS_FLAGS) `xapian-config --cxxflags`

NOTMUCH_CFLAGS=$(WARN_FLAGS) -O0 -g $(NOTMUCH_DEPENDS_FLAGS)
NOTMUCH_CXXFLAGS=$(WARN_FLAGS) -O0 -g $(NOTMUCH_CXX_DEPENDS_FLAGS)

NOTMUCH_LDFLAGS=`pkg-config --libs glib-2.0 gmime-2.4 talloc` \
		`xapian-config --libs`

# Include our local Makfile.local first so that its first target is default
include Makefile.local
include lib/Makefile.local

%.o: %.cc
	$(CXX) -c $(CFLAGS) $(CXXFLAGS) $(NOTMUCH_CXXFLAGS) $< -o $@

%.o: %.c
	$(CC) -c $(CFLAGS) $(NOTMUCH_CFLAGS) $< -o $@

.deps/%.d: %.c
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CC) -M $(CPPFLAGS) $(NOTMUCH_DEPENDS_FLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

.deps/%.d: %.cc
	@set -e; rm -f $@; mkdir -p $$(dirname $@) ; \
	$(CXX) -M $(CPPFLAGS) $(NOTMUCH_CXX_DEPENDS_FLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

DEPS := $(SRCS:%.c=.deps/%.d)
DEPS := $(DEPS:%.cc=.deps/%.d)
-include $(DEPS)

clean:
	rm -f $(CLEAN); rm -rf .deps
