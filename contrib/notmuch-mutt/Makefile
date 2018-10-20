NAME = notmuch-mutt

-include ../../Makefile.config
PERL_ABSOLUTE ?= $(shell command -v perl 2>/dev/null)
prefix ?= /usr/local
sysconfdir ?= ${prefix}/etc
mandir ?= ${prefix}/share/man

all: $(NAME) $(NAME).1

$(NAME).1: $(NAME)
	pod2man $< > $@

README.html: README
	markdown $< > $@

install: all
	mkdir -p $(DESTDIR)$(prefix)/bin
	sed "1s|^#!.*|#! $(PERL_ABSOLUTE)|" < $(NAME) > $(DESTDIR)$(prefix)/bin/$(NAME)
	chmod 755 $(DESTDIR)$(prefix)/bin/$(NAME)
	install -D -m 644 $(NAME).1 $(DESTDIR)$(mandir)/man1/$(NAME).1
	install -D -m 644 $(NAME).rc $(DESTDIR)$(sysconfdir)/Muttrc.d/$(NAME).rc

clean:
	rm -f notmuch-mutt.1 README.html
