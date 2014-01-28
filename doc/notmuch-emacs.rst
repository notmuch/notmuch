=============
notmuch-emacs
=============

About this Manual
=================

This manual covers only the emacs interface to notmuch. For information
on the command line interface, see See section “Description” in Notmuch
Manual Pager. To save typing, we will sometimes use *notmuch* in this
manual to refer to the Emacs interface to notmuch. If the distinction
should every be important, we’ll refer to the Emacs inteface as
*notmuch-emacs*.

Notmuch-emacs is highly customizable via the the Emacs customization
framework (or just by setting the appropriate variables). We try to
point out relevant variables in this manual, but in order to avoid
duplication of information, but you can usually find the most detailed
description in the varables docstring.

notmuch-hello
=============

.. index::
   single: notmuch-hello
   single: notmuch

``notmuch-hello`` is the main entry point for notmuch. You can start it
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
|	 Type a search query and hit RET to view matching threads.
|		Edit saved searches with the ``edit`` button.
| Hit RET or click on a saved search or tag name to view matching threads.
|     ``=`` to refresh this screen. ``s`` to search messages. ``q`` to quit.
|		    **Customize** this page.

You can change the overall appearence of the notmuch-hello screen by
customizing the variable :index:`notmuch-hello-sections`.



notmuch-hello key bindings
--------------------------

``<tab>``
    Move to the next widget (button or text entry field)

``<backtab>``
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

Notmuch replaces the static assignment of messages with the more dynamic
notion of searching. Notmuch-hello presents the user with a customizable
set of saved searchs. The initial defaults are ``tag:inbox`` and
``tag:unread``, but you can customize the following variables

:index:`notmuch-saved-searches`
    A list of cons pairs, the first being the name to display, the
    second being a query string for notmuch. See section “Description”
    in Notmuch Query Syntax.

:index:`notmuch-saved-searches-sort-function`
    This variable controls how saved searches should be sorted. A value
    of ``nil`` displays the saved searches in the order they are stored
    in ‘notmuch-saved-searches’.

:index:`notmuch-column-control`
    Controls the number of columns for displaying saved-searches/tags

Search Box
----------

The search box lets the user enter an notmuch query. See section
“Description” in Notmuch Query Syntax, for more info on notmuch query
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

Configuration
=============

.. _importing:

Importing Mail
--------------

:index:`notmuch-poll`

:index:`notmuch-poll-script`
