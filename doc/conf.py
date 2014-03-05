
# -*- coding: utf-8 -*-

import sys
import os

# The suffix of source filenames.
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = u'notmuch'
copyright = u'2014, Carl Worth and many others'

location = os.path.dirname(__file__)

for pathdir in ['.', '..']:
    version_file = os.path.join(location,pathdir,'version')
    if os.path.exists(version_file):
        with open(version_file,'r') as infile:
            version=infile.read().replace('\n','')

# The full version, including alpha/beta/rc tags.
release = version

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build', 'notmuch-emacs.rst']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = 'default'


# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# Output file base name for HTML help builder.
htmlhelp_basename = 'notmuchdoc'

# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).

man_pages = [

('man1/notmuch','notmuch',
        u'thread-based email index, search, and tagging',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-compact','notmuch-compact',
        u'compact the notmuch database',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-config','notmuch-config',
        u'access notmuch configuration file',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-count','notmuch-count',
        u'count messages matching the given search terms',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-dump','notmuch-dump',
        u'creates a plain-text dump of the tags of each message',
        [u'Carl Worth and many others'], 1),

('man5/notmuch-hooks','notmuch-hooks',
        u'hooks for notmuch',
        [u'Carl Worth and many others'], 5),

('man1/notmuch-insert','notmuch-insert',
        u'add a message to the maildir and notmuch database',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-new','notmuch-new',
        u'incorporate new mail into the notmuch database',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-reply','notmuch-reply',
        u'constructs a reply template for a set of messages',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-restore','notmuch-restore',
        u'restores the tags from the given file (see notmuch dump)',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-search','notmuch-search',
        u'search for messages matching the given search terms',
        [u'Carl Worth and many others'], 1),

('man7/notmuch-search-terms','notmuch-search-terms',
        u'syntax for notmuch queries',
        [u'Carl Worth and many others'], 7),

('man1/notmuch-show','notmuch-show',
        u'show messages matching the given search terms',
        [u'Carl Worth and many others'], 1),

('man1/notmuch-tag','notmuch-tag',
        u'add/remove tags for all messages matching the search terms',
        [u'Carl Worth and many others'], 1),


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
 ('notmuch-emacs', 'notmuch-emacs', u'notmuch Documentation',
   u'Carl Worth and many others', 'notmuch-emacs',
   'emacs based front-end for notmuch', 'Miscellaneous'),
('man1/notmuch','notmuch',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch',
      'thread-based email index, search, and tagging','Miscellaneous'),
('man1/notmuch-compact','notmuch-compact',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-compact',
      'compact the notmuch database','Miscellaneous'),
('man1/notmuch-config','notmuch-config',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-config',
      'access notmuch configuration file','Miscellaneous'),
('man1/notmuch-count','notmuch-count',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-count',
      'count messages matching the given search terms','Miscellaneous'),
('man1/notmuch-dump','notmuch-dump',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-dump',
      'creates a plain-text dump of the tags of each message','Miscellaneous'),
('man5/notmuch-hooks','notmuch-hooks',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-hooks',
      'hooks for notmuch','Miscellaneous'),
('man1/notmuch-insert','notmuch-insert',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-insert',
      'add a message to the maildir and notmuch database','Miscellaneous'),
('man1/notmuch-new','notmuch-new',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-new',
      'incorporate new mail into the notmuch database','Miscellaneous'),
('man1/notmuch-reply','notmuch-reply',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-reply',
      'constructs a reply template for a set of messages','Miscellaneous'),
('man1/notmuch-restore','notmuch-restore',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-restore',
      'restores the tags from the given file (see notmuch dump)','Miscellaneous'),
('man1/notmuch-search','notmuch-search',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-search',
      'search for messages matching the given search terms','Miscellaneous'),
('man7/notmuch-search-terms','notmuch-search-terms',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-search-terms',
      'syntax for notmuch queries','Miscellaneous'),
('man1/notmuch-show','notmuch-show',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-show',
      'show messages matching the given search terms','Miscellaneous'),
('man1/notmuch-tag','notmuch-tag',u'notmuch Documentation',
      u'Carl Worth and many others', 'notmuch-tag',
      'add/remove tags for all messages matching the search terms','Miscellaneous'),
]
