prefix = $(HOME)/.vim

INSTALL = install -v -D -m644
D = $(DESTDIR)

all:
	@echo "Nothing to build"

install:
	$(INSTALL) $(CURDIR)/notmuch.vim $(D)$(prefix)/plugin/notmuch.vim
	$(INSTALL) $(CURDIR)/notmuch.txt $(D)$(prefix)/doc/notmuch.txt
	@$(foreach file,$(wildcard syntax/*), \
		$(INSTALL) $(CURDIR)/$(file) $(D)$(prefix)/$(file);)

.PHONY: all install
