# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
DOCBUILDDIR   := _build

SRCDIR ?= .
ALLSPHINXOPTS := -d $(DOCBUILDDIR)/doctrees $(SPHINXOPTS) $(SRCDIR)

MAN_RST_FILES := $(shell find $(SRCDIR)/man* -name '*.rst')
MAN_ROFF_FILES := $(patsubst $(SRCDIR)/man%.rst,$(DOCBUILDDIR)/man/man%,$(MAN_RST_FILES))
MAN_GZIP_FILES := $(addsuffix .gz,$(MAN_ROFF_FILES))

.PHONY: build-man
build-man: $(MAN_GZIP_FILES)

%.gz: %
	rm -f $@ && gzip --stdout $^ > $@

$(MAN_ROFF_FILES): $(DOCBUILDDIR)/.roff.stamp

# By using $(DOCBUILDDIR)/.roff.stamp instead of $(MAN_ROFF_FILES), we
# convey to make that a single invocation of this recipe builds all
# of the roff files.  This prevents parallel make from starting an
# instance of this recipe for each roff file.
$(DOCBUILDDIR)/.roff.stamp $(MAN_ROFF_FILES): $(MAN_RST_FILES)
	mkdir -p $(DOCBUILDDIR)
	touch $(DOCBUILDDIR)/.roff.stamp
	$(SPHINXBUILD) -b man $(ALLSPHINXOPTS) $(DOCBUILDDIR)/man
	for section in 1 5; do \
	    mkdir -p $(DOCBUILDDIR)/man/man$${section}; \
	    mv $(DOCBUILDDIR)/man/*.$${section} $(DOCBUILDDIR)/man/man$${section}; \
	done

clean:
	rm -rf $(DOCBUILDDIR) $(SRCDIR)/conf.pyc
