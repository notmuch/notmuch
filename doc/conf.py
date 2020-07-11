
# -*- coding: utf-8 -*-

import sys
import os

extensions = [ 'sphinx.ext.autodoc' ]

# The suffix of source filenames.
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = u'notmuch'
copyright = u'2009-2020, Carl Worth and many others'

location = os.path.dirname(__file__)

for pathdir in ['.', '..']:
    version_file = os.path.join(location,pathdir,'version')
    if os.path.exists(version_file):
        with open(version_file,'r') as infile:
            version=infile.read().replace('\n','')

# for autodoc
sys.path.insert(0, os.path.join(location, '..', 'bindings', 'python-cffi', 'notmuch2'))

# read generated config
for pathdir in ['.', '..']:
    conf_file = os.path.join(location,pathdir,'sphinx.config')
    if os.path.exists(conf_file):
        with open(conf_file,'r') as infile:
            exec(''.join(infile.readlines()))

# The full version, including alpha/beta/rc tags.
release = version

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build']

if tags.has('WITH_EMACS'):
    # Hacky reimplementation of include to workaround limitations of
    # sphinx-doc
    lines = ['.. include:: /../emacs/rstdoc.rsti\n\n'] # in the source tree
    for file in ('notmuch.rsti', 'notmuch-lib.rsti', 'notmuch-show.rsti', 'notmuch-tag.rsti'):
        lines.extend(open(rsti_dir+'/'+file))
    rst_epilog = ''.join(lines)
    del lines
else:
    # If we don't have emacs (or the user configured --without-emacs),
    # don't build the notmuch-emacs docs, as they need emacs to generate
    # the docstring include files
    exclude_patterns.append('notmuch-emacs.rst')

if not tags.has('WITH_PYTHON'):
    exclude_patterns.append('python-bindings.rst')

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = 'default'


# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = []

# Output file base name for HTML help builder.
htmlhelp_basename = 'notmuchdoc'

# Disable SmartyPants, as it mangles command lines.
# Despite the name, this actually affects manual pages as well.
html_use_smartypants = False

# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).

notmuch_authors = u'Carl Worth and many others'

man_pages = [
    ('man1/notmuch', 'notmuch',
     u'thread-based email index, search, and tagging',
     [notmuch_authors], 1),

    ('man1/notmuch-address', 'notmuch-address',
     u'output addresses from matching messages',
     [notmuch_authors], 1),

    ('man1/notmuch-compact', 'notmuch-compact',
     u'compact the notmuch database',
     [notmuch_authors], 1),

    ('man1/notmuch-config', 'notmuch-config',
     u'access notmuch configuration file',
     [notmuch_authors], 1),

    ('man1/notmuch-count', 'notmuch-count',
     u'count messages matching the given search terms',
     [notmuch_authors], 1),

    ('man1/notmuch-dump', 'notmuch-dump',
     u'creates a plain-text dump of the tags of each message',
     [notmuch_authors], 1),

    ('man1/notmuch-emacs-mua', 'notmuch-emacs-mua',
     u'send mail with notmuch and emacs',
     [notmuch_authors], 1),

    ('man5/notmuch-hooks', 'notmuch-hooks',
     u'hooks for notmuch',
     [notmuch_authors], 5),

    ('man1/notmuch-insert', 'notmuch-insert',
     u'add a message to the maildir and notmuch database',
     [notmuch_authors], 1),

    ('man1/notmuch-new', 'notmuch-new',
     u'incorporate new mail into the notmuch database',
     [notmuch_authors], 1),

    ('man7/notmuch-properties', 'notmuch-properties',
     u'notmuch message property conventions and documentation',
     [notmuch_authors], 7),

    ('man1/notmuch-reindex', 'notmuch-reindex',
     u're-index matching messages',
     [notmuch_authors], 1),

    ('man1/notmuch-reply', 'notmuch-reply',
     u'constructs a reply template for a set of messages',
     [notmuch_authors], 1),

    ('man1/notmuch-restore', 'notmuch-restore',
     u'restores the tags from the given file (see notmuch dump)',
     [notmuch_authors], 1),

    ('man1/notmuch-search', 'notmuch-search',
     u'search for messages matching the given search terms',
     [notmuch_authors], 1),

    ('man7/notmuch-search-terms', 'notmuch-search-terms',
     u'syntax for notmuch queries',
     [notmuch_authors], 7),

    ('man1/notmuch-show', 'notmuch-show',
     u'show messages matching the given search terms',
     [notmuch_authors], 1),

    ('man1/notmuch-tag', 'notmuch-tag',
     u'add/remove tags for all messages matching the search terms',
     [notmuch_authors], 1),
]

# If true, show URL addresses after external links.
#man_show_urls = False

# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
# If true, do not generate a @detailmenu in the "Top" node's menu.
texinfo_no_detailmenu = True

texinfo_documents = [
    ('notmuch-emacs', 'notmuch-emacs', u'notmuch-emacs documentation',
     notmuch_authors, 'notmuch-emacs',
     'emacs based front-end for notmuch', 'Miscellaneous'),
]

# generate texinfo list from man page list
texinfo_documents += [
    (
        x[0],				# source start file
        x[1],				# target name
        x[1] + u' documentation',	# title
        x[3][0],			# author
        x[1],				# dir menu entry
        x[2],				# description
        'Miscellaneous'			# category
    ) for x in man_pages]
