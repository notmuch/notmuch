=============
notmuch-emacs
=============

About this Manual
=================

This manual covers only the Emacs interface to Notmuch. For information
on the command line interface, see section “Description” in the Notmuch
Manual Pages. To save typing, we will sometimes use *notmuch* in this
manual to refer to the Emacs interface to Notmuch. When this distinction
is important, we’ll refer to the Emacs interface as
*notmuch-emacs*.

Notmuch-emacs is highly customizable via the the Emacs customization
framework (or just by setting the appropriate variables). We try to
point out relevant variables in this manual, but in order to avoid
duplication of information, you can usually find the most detailed
description in the variables' docstring.

notmuch-hello
=============

.. index::
   single: notmuch-hello
   single: notmuch

``notmuch-hello`` is the main entry point for Notmuch. You can start it
with ``M-x notmuch`` or ``M-x notmuch-hello``. The startup screen looks
something like the following. There are some hints at the bottom of the
screen. There are three main parts to the notmuch-hello screen,
discussed below. The **bold** text indicates buttons you can click with
a mouse or by positioning the cursor and pressing ``<return>``

|   Welcome to **notmuch** You have 52 messages.
|
| Saved searches: **[edit]**
|
|	  52 **inbox**           52 **unread**
|
| Search: ____________________________________
|
| All tags: **[show]**
|
|	 Hit \`?' for context-sensitive help in any Notmuch screen.
|		      Customize Notmuch or this page.

You can change the overall appearance of the notmuch-hello screen by
customizing the variable :index:`notmuch-hello-sections`.



notmuch-hello key bindings
--------------------------

``<tab>``
    Move to the next widget (button or text entry field)

``<backspace>``
    Move to the previous widget.

``<return>``
    Activate the current widget.

``=``
    Refresh the buffer; mainly update the counts of messages for various
    saved searches.

``G``
    Import mail, See :ref:`importing`

``m``
    Compose a message

``s``
    Search the notmuch database using :ref:`notmuch-search`

``v``
    Print notmuch version

``q``
    Quit

.. _saved-searches:

Saved Searches
--------------

Since notmuch is entirely search-based, it's often useful to organize
mail around common searches.  To facilitate this, the first section of
notmuch-hello presents a customizable set of saved searches.  Saved
searches can also be accessed from anywhere in notmuch by pressing
``j`` to access :ref:`notmuch-jump`.

The saved searches default to various common searches such as
``tag:inbox`` to access the inbox and ``tag:unread`` to access all
unread mail, but there are several options for customization:

:index:`notmuch-saved-searches`
    The list of saved searches, including names, queries, and
    additional per-query options.

:index:`notmuch-saved-searches-sort-function`
    This variable controls how saved searches should be sorted. A value
    of ``nil`` displays the saved searches in the order they are stored
    in ‘notmuch-saved-searches’.

:index:`notmuch-column-control`
    Controls the number of columns for displaying saved-searches/tags

Search Box
----------

The search box lets the user enter a Notmuch query. See section
“Description” in Notmuch Query Syntax, for more info on Notmuch query
syntax. A history of recent searches is also displayed by default. The
latter is controlled by the variable :index:`notmuch-hello-recent-searches-max`.

Known Tags
----------

One special kind of saved search provided by default is for each
individual tag defined in the database. This can be controlled via the
following variables.

:index:`notmuch-hello-tag-list-make-query`
    Control how to construct a search (“virtual folder”) from a given
    tag.

:index:`notmuch-hello-hide-tags`
    Which tags not to display at all.

:index:`notmuch-column-control`
    Controls the number of columns for displaying saved-searches/tags

.. _notmuch-search:

notmuch-search
==============

``notmuch-search-mode`` is used to display the results from executing
a query via ``notmuch-search``. The syntax for these queries is the
the same as :ref:`saved-searches`. For details of this syntax see
info:notmuch-search-terms

By default the output approximates that of the command line See section
“Description” in notmuch search command.

The main purpose of the ``notmuch-search-mode`` buffer is to act as a
menu of results that the user can explore further by pressing
``<return>`` on the appropriate line.

``n,C-n,<down>``
    Move to next line

``p,C-p,<up>``
    Move to previous line

``<return>``
    Open thread on current line in :ref:`notmuch-show` mode

``?``
    Display full set of key bindings

The presentation of results can be controlled by the following
variables.

:index:`notmuch-search-result-format`
    Control how each thread of messages is presented in the
    ``notmuch-show-mode`` buffer

:index:`notmuch-search-oldest-first`
    Display the oldest threads at the top of the buffer

.. _notmuch-show:

notmuch-show
============

notmuch-tree
============

Global key bindings
===================

Several features are accessible from anywhere in notmuch through the
following key bindings:

``j``
    Jump to saved searches using :ref:`notmuch-jump`.

.. _notmuch-jump:

notmuch-jump
------------

Saved searches configured through :ref:`saved-searches` can
include a "shortcut key" that's accessible through notmuch-jump.
Pressing ``j`` anywhere in notmuch followed by the configured shortcut
key of a saved search will immediately jump to that saved search.  For
example, in the default configuration ``j i`` jumps immediately to the
inbox search.  When you press ``j``, notmuch-jump shows the saved
searches and their shortcut keys in the mini-buffer.

Configuration
=============

.. _importing:

Importing Mail
--------------

:index:`notmuch-poll`

:index:`notmuch-poll-script`

Init File
---------

When Notmuch is loaded, it will read the ``notmuch-init-file``
(``~/.emacs.d/notmuch-config`` by default) file. This is normal Emacs Lisp
file and can be used to avoid cluttering your ``~/.emacs`` with Notmuch
stuff. If the file with ``.elc``, ``.elc.gz``, ``.el`` or ``.el.gz``
suffix exist it will be read instead (just one of these, chosen in this
order). Most often users create ``~/.emacs.d/notmuch-config.el`` and just
work with it. If Emacs was invoked with the ``-q`` or ``--no-init-file``
options, ``notmuch-init-file`` is not read.
