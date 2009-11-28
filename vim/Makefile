.PHONY: all help install link symlink

FILES     = plugin/notmuch.vim \
	    $(wildcard syntax/notmuch-*.vim)

PREFIX    = $(shell ls -d ~/.vim/)

OUT_FILES = $(FILES:%=${PREFIX}/%)

all: help

help:
	@echo "I don't actually build anything, but I will help you install"
	@echo "notmuch support for vim."
	@echo
	@echo "    make install     - copy plugin scripts and syntax files to ~/.vim"
	@echo "    make symlink     - create symlinks in ~/.vim (useful for development)"

install: ${OUT_FILES}
link symlink:
	${MAKE} SYMLINK=1 install

${OUT_FILES}: ${PREFIX}/%: %
	$(if ${SYMLINK},ln -fs,cp) `pwd`/$< $@
