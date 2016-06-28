# -*- coding: utf-8 -*-

import os.path

# The suffix of source filenames.
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = 'notmuch'
authors = 'Carl Worth and many others'
copyright = '2009-2015, {0}'.format(authors)

location = os.path.dirname(__file__)

dirname = location
while True:
    version_file = os.path.join(dirname, 'version')
    if os.path.exists(version_file):
        with open(version_file,'r') as f:
            version = f.read().strip()
            break
    if dirname == '/':
        raise ValueError(
            'no version file found in this directory or its ancestors')
    dirname = os.path.dirname(dirname)

# The full version, including alpha/beta/rc tags.
release = version

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build']

# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).

man_pages = [
    ('man1/notmuch-report.1', 'notmuch-report',
     'generate reports from notmuch queries', [authors], 1),
    ('man5/notmuch-report.json.5', 'notmuch-report.json',
     'configure notmuch-report', [authors], 5),
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
    ('man1/notmuch-report.1', 'notmuch-report',
     'generate reports from notmuch queries', authors, 'notmuch-report',
     'generate reports from notmuch queries', 'Miscellaneous'),
    ('man5/notmuch-report.json.5', 'notmuch-report.json',
     'configure notmuch-report', authors, 'notmuch-report.json',
     'configure notmuch-report', 'Miscellaneous'),
]
