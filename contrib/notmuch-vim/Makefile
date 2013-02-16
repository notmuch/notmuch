.PHONY: all help install link symlink

files = plugin/notmuch.vim \
	$(wildcard syntax/notmuch-*.vim)
prefix = $(HOME)/.vim
destdir = $(prefix)/plugin

INSTALL = install -D -m644

all: help

help:
	@echo "I don't actually build anything, but I will help you install"
	@echo "notmuch support for vim."
	@echo
	@echo "    make install     - copy plugin scripts and syntax files to ~/.vim"
	@echo "    make symlink     - create symlinks in ~/.vim (useful for development)"

install:
	@for x in $(files); do $(INSTALL) $(CURDIR)/$$x $(prefix)/$$x; done

link symlink: INSTALL = ln -fs
link symlink: install
