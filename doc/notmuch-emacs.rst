.. _notmuch-emacs:

==========================
Emacs Frontend for Notmuch
==========================

About this Manual
=================

This manual covers only the Emacs interface to Notmuch. For information
on the command line interface, see section “Description” in the Notmuch
Manual Pages. To save typing, we will sometimes use *notmuch* in this
manual to refer to the Emacs interface to Notmuch. When this distinction
is important, we’ll refer to the Emacs interface as
*notmuch-emacs*.

Notmuch-emacs is highly customizable via the Emacs customization
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
customizing the variables

.. el:defcustom:: notmuch-hello-sections

       |docstring::notmuch-hello-sections|

.. el:defcustom:: notmuch-hello-thousands-separator

       |docstring::notmuch-hello-thousands-separator|

.. el:defcustom:: notmuch-show-logo

       |docstring::notmuch-show-logo|

.. el:defcustom:: notmuch-column-control

    Controls the number of columns for saved searches/tags in notmuch view.

    This variable has three potential types of values:

    .. describe:: t

       Automatically calculate the number of columns possible based
       on the tags to be shown and the window width.

    .. describe:: integer <n>

       A lower bound on the number of characters that will
       be used to display each column.

    .. describe:: float <f>

       A fraction of the window width that is the lower bound on the
       number of characters that should be used for each column.

    So:

    - if you would like two columns of tags, set this to 0.5.

    - if you would like a single column of tags, set this to 1.0.

    - if you would like tags to be 30 characters wide, set this to 30.

    - if you don't want to worry about all of this nonsense, leave
      this set to `t`.

.. el:defcustom:: notmuch-show-empty-saved-searches

   |docstring::notmuch-show-empty-saved-searches|

notmuch-hello key bindings
--------------------------

.. el:define-key:: <tab>

    Move to the next widget (button or text entry field)

.. el:define-key:: <backtab>

    Move to the previous widget.

.. el:define-key:: <return>

    Activate the current widget.

.. el:define-key:: g
                   =

    Refresh the buffer; mainly update the counts of messages for various
    saved searches.

.. el:define-key:: G

    Import mail, See :ref:`importing`

.. el:define-key:: m

    Compose a message

.. el:define-key:: s

    Search the notmuch database using :ref:`notmuch-search`

.. el:define-key:: v

    Print notmuch version

.. el:define-key:: q

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

.. el:defcustom:: notmuch-saved-searches

    The list of saved searches, including names, queries, and
    additional per-query options.

.. el:defcustom:: notmuch-saved-search-sort-function

    This variable controls how saved searches should be sorted. A value
    of ``nil`` displays the saved searches in the order they are stored
    in ‘notmuch-saved-searches’.

Search Box
----------

The search box lets the user enter a Notmuch query. See section
“Description” in Notmuch Query Syntax, for more info on Notmuch query
syntax. A history of recent searches is also displayed by default. The
latter is controlled by the variable `notmuch-hello-recent-searches-max`.

.. el:defcustom:: notmuch-hello-recent-searches-max

              |docstring::notmuch-hello-recent-searches-max|

Known Tags
----------

One special kind of saved search provided by default is for each
individual tag defined in the database. This can be controlled via the
following variables.

.. el:defcustom:: notmuch-hello-tag-list-make-query

    Control how to construct a search (“virtual folder”) from a given
    tag.

.. el:defcustom:: notmuch-hello-hide-tags

    Which tags not to display at all.

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

.. el:define-key:: n
   C-n
   <down>

    Move to next line

.. el:define-key::
   p
   C-p
   <up>

    Move to previous line

.. el:define-key:: <return>

    Open thread on current line in :ref:`notmuch-show` mode

.. el:define-key:: g
   =

    Refresh the buffer

.. el:define-key:: ?

    Display full set of key bindings

The presentation of results can be controlled by the following
variables.

.. el:defcustom:: notmuch-search-result-format

   |docstring::notmuch-search-result-format|

   If the car of an element in notmuch-search-result-format is a
   function, insert the result of calling the function into the buffer.

   This allows a user to generate custom fields in the output of a
   search result. For example, with the following settings, the first
   few characters on each line of the search result are used to show
   information about some significant tags associated with the thread.

   .. code:: lisp

      (defun -notmuch-result-flags (format-string result)
        (let ((tags-to-letters '(("flagged" . "!")
                                 ("unread" . "u")
                                 ("mine" . "m")
                                 ("sent" . "s")
                                 ("replied" . "r")))
              (tags (plist-get result :tags)))
          (format format-string
                  (mapconcat (lambda (t2l)
                               (if (member (car t2l) tags)
                                   (cdr t2l)
                                 " "))
                             tags-to-letters ""))))

      (setq notmuch-search-result-format '((-notmuch-result-flags . "%s ")
                                           ("date" . "%12s ")
                                           ("count" . "%9s ")
                                           ("authors" . "%-30s ")
                                           ("subject" . "%s ")
                                           ("tags" . "(%s)")))

   See also :el:defcustom:`notmuch-tree-result-format` and
   :el:defcustom:`notmuch-unthreaded-result-format`.

.. el:defcustom:: notmuch-search-oldest-first

    Display the oldest threads at the top of the buffer

It is also possible to customize how the name of buffers containing
search results is formatted using the following variables:

.. el:defcustom:: notmuch-search-buffer-name-format

       |docstring::notmuch-search-buffer-name-format|

.. el:defcustom:: notmuch-saved-search-buffer-name-format

       |docstring::notmuch-saved-search-buffer-name-format|


.. _notmuch-show:

notmuch-show
============

``notmuch-show-mode`` is used to display a single thread of email from
your email archives.

By default, various components of email messages, (citations,
signatures, already-read messages), are hidden. You can make
these parts visible by clicking with the mouse button or by
pressing RET after positioning the cursor on a hidden part.

.. el:define-key:: <space>

    Scroll the current message (if necessary),
    advance to the next message, or advance to the next thread (if
    already on the last message of a thread).

.. el:define-key:: c

    :ref:`show-copy`

.. el:define-key:: N

    Move to next message

.. el:define-key:: P

    Move to previous message (or start of current message)

.. el:define-key:: n

    Move to next matching message

.. el:define-key:: p

    Move to previous matching message

.. el:define-key:: +
                   -

    Add or remove arbitrary tags from the current message.

.. el:define-key:: !

    |docstring::notmuch-show-toggle-elide-non-matching|

.. el:define-key:: ?

    Display full set of key bindings

Display of messages can be controlled by the following variables; see also :ref:`show-large`.

.. el:defcustom:: notmuch-message-headers

       |docstring::notmuch-message-headers|

.. el:defcustom:: notmuch-message-headers-visible

       |docstring::notmuch-message-headers-visible|

.. el:defcustom:: notmuch-show-header-line

       |docstring::notmuch-show-header-line|

.. el:defcustom:: notmuch-multipart/alternative-discouraged

   Which mime types to hide by default for multipart messages.

   Can either be a list of mime types (as strings) or a function
   mapping a plist representing the current message to such a list.
   The following example function would discourage `text/html` and
   `multipart/related` generally, but discourage `text/plain` should
   the message be sent from `whatever@example.com`.

   .. code:: lisp

      (defun my--determine-discouraged (msg)
        (let* ((headers (plist-get msg :headers))
               (from (or (plist-get headers :From) "")))
          (cond
           ((string-match "whatever@example.com" from)
            (list "text/plain"))
           (t
            (list "text/html" "multipart/related")))))

.. _show-large:

Dealing with large messages and threads
---------------------------------------

If you are finding :ref:`notmuch-show` is annoyingly slow displaying
large messages, you can customize
:el:defcustom:`notmuch-show-max-text-part-size`.  If you want to speed up the
display of large threads (with or without large messages), there are
several options.  First, you can display the same query in one of the
other modes. :ref:`notmuch-unthreaded` is the most robust for
extremely large queries, but :ref:`notmuch-tree` is also be faster
than :ref:`notmuch-show` in general, since it only renders a single
message a time. If you prefer to stay with the rendered thread
("conversation") view of :ref:`notmuch-show`, you can customize the
variables :el:defcustom:`notmuch-show-depth-limit`,
:el:defcustom:`notmuch-show-height-limit` and
:el:defcustom:`notmuch-show-max-text-part-size` to limit the amount of
rendering done initially. Note that these limits are implicitly
*OR*-ed together, and combinations might have surprising effects.

.. el:defcustom:: notmuch-show-depth-limit

       |docstring::notmuch-show-depth-limit|

.. el:defcustom:: notmuch-show-height-limit

       |docstring::notmuch-show-height-limit|

.. el:defcustom:: notmuch-show-max-text-part-size

       |docstring::notmuch-show-max-text-part-size|

.. _show-copy:

Copy to kill-ring
-----------------

You can use the usually Emacs ways of copying text to the kill-ring,
but notmuch also provides some shortcuts. These keys are available in
:ref:`notmuch-show`, and :ref:`notmuch-tree`. A subset are available
in :ref:`notmuch-search`.

.. el:define-key:: c F
   M-x notmuch-show-stash-filename

   |docstring::notmuch-show-stash-filename|

.. el:define-key:: c G
   M-x notmuch-show-stash-git-send-email

   |docstring::notmuch-show-stash-git-send-email|

.. el:define-key:: c I
   M-x notmuch-show-stash-message-id-stripped

   |docstring::notmuch-show-stash-message-id-stripped|

.. el:define-key:: c L
   M-x notmuch-show-stash-mlarchive-link-and-go

   |docstring::notmuch-show-stash-mlarchive-link-and-go|

.. el:define-key:: c T
   M-x notmuch-show-stash-tags

   |docstring::notmuch-show-stash-tags|

.. el:define-key:: c c
   M-x notmuch-show-stash-cc

   |docstring::notmuch-show-stash-cc|

.. el:define-key:: c d
   M-x notmuch-show-stash-date

   |docstring::notmuch-show-stash-date|

.. el:define-key:: c f
   M-x notmuch-show-stash-from

   |docstring::notmuch-show-stash-from|

.. el:define-key:: c i
   M-x notmuch-show-stash-message-id

   |docstring::notmuch-show-stash-message-id|

.. el:define-key:: c l
   M-x notmuch-show-stash-mlarchive-link

   |docstring::notmuch-show-stash-mlarchive-link|

.. el:define-key:: c s
   M-x notmuch-show-stash-subject

   |docstring::notmuch-show-stash-subject|

.. el:define-key:: c t
   M-x notmuch-show-stash-to

   |docstring::notmuch-show-stash-to|

.. el:define-key:: c ?
   M-x notmuch-subkeymap-help

   Show all available copying commands

.. _emacs-show-duplicates:

Dealing with duplicates
-----------------------

If there are multiple files with the same :mailheader:`Message-ID`
(see :any:`duplicate-files`), then :any:`notmuch-show` displays the
number of duplicates and identifies the current duplicate. In the
following example duplicate 3 of 5 is displayed.

.. code-block::
   :emphasize-lines: 1

    M. Mustermann <max@example.com> (Sat, 30 Jul 2022 10:33:10 -0300) (inbox signed)      3/5
    Subject: Re: Multiple files per message in emacs
    To: notmuch@notmuchmail.org

.. el:define-key:: %
   M-x notmuch-show-choose-duplicate

   |docstring::notmuch-show-choose-duplicate|

.. _notmuch-tree:

notmuch-tree
============

``notmuch-tree-mode`` displays the results of a "notmuch tree" of your
email archives. Each line in the buffer represents a single
message giving the relative date, the author, subject, and any
tags.

.. el:define-key:: c

    :ref:`show-copy`

.. el:define-key:: <return>

   Displays that message.

.. el:define-key:: N

    Move to next message

.. el:define-key:: P

    Move to previous message

.. el:define-key:: n

    Move to next matching message

.. el:define-key:: p

    Move to previous matching message

.. el:define-key:: o
   M-x notmuch-tree-toggle-order

   |docstring::notmuch-tree-toggle-order|

.. el:define-key:: l
   M-x notmuch-tree-filter

   Filter or LIMIT the current search results based on an additional query string

.. el:define-key:: t
   M-x notmuch-tree-filter-by-tag

   Filter the current search results based on an additional tag


.. el:define-key:: g
   =

    Refresh the buffer

.. el:define-key:: ?

    Display full set of key bindings

As is the case with :ref:`notmuch-search`, the presentation of results
can be controlled by the variable ``notmuch-search-oldest-first``.

.. el:defcustom:: notmuch-tree-result-format

   |docstring::notmuch-tree-result-format|

   The following example shows how to optionally display recipients instead
   of authors for sent mail (assuming the user is named Mustermann).

   .. code:: lisp

      (defun -notmuch-authors-or-to (format-string result)
        (let* ((headers (plist-get result :headers))
               (to (plist-get headers :To))
               (author (plist-get headers :From))
               (face (if (plist-get result :match)
                         'notmuch-tree-match-author-face
                       'notmuch-tree-no-match-author-face)))
          (propertize
           (format format-string
                   (if (string-match "Mustermann" author)
                       (concat "To:" (notmuch-tree-clean-address to))
                     author))
           'face face)))

      (setq notmuch-tree-result-format
            '(("date" . "%12s  ")
              (-notmuch-authors-or-to . "%-20.20s")
              ((("tree" . "%s")
                ("subject" . "%s"))
               . " %-54s ")
              ("tags" . "(%s)")))

   See also :el:defcustom:`notmuch-search-result-format` and
   :el:defcustom:`notmuch-unthreaded-result-format`.

.. _notmuch-tree-outline:

notmuch-tree-outline
--------------------

When this mode is set, each thread and subthread in the results
list is treated as a foldable section, with its first message as
its header.

The mode just makes available in the tree buffer all the
keybindings in info:emacs#Outline_Mode, and binds the following
additional keys:

.. el:define-key:: <tab>

   Cycle visibility state of the current message's tree.

.. el:define-key:: <M-tab>

   Cycle visibility state of all trees in the buffer.

The behaviour of this minor mode is affected by the following
customizable variables:

.. el:defcustom:: notmuch-tree-outline-enabled

   |docstring::notmuch-tree-outline-enabled|

.. el:defcustom:: notmuch-tree-outline-visibility

   |docstring::notmuch-tree-outline-visibility|

.. el:defcustom:: notmuch-tree-outline-auto-close

   |docstring::notmuch-tree-outline-auto-close|

.. el:defcustom:: notmuch-tree-outline-open-on-next

   |docstring::notmuch-tree-outline-open-on-next|

.. _notmuch-unthreaded:

notmuch-unthreaded
------------------

``notmuch-unthreaded-mode`` is similar to :any:`notmuch-tree` in that
each line corresponds to a single message, but no thread information
is presented.

Keybindings are the same as :any:`notmuch-tree`.

.. el:defcustom:: notmuch-unthreaded-result-format

   |docstring::notmuch-unthreaded-result-format|

   See also :el:defcustom:`notmuch-search-result-format` and
   :el:defcustom:`notmuch-tree-result-format`.

Global key bindings
===================

Several features are accessible from most places in notmuch through the
following key bindings:

.. el:define-key:: j

    Jump to saved searches using :ref:`notmuch-jump`.

.. el:define-key:: k

    Tagging operations using :ref:`notmuch-tag-jump`

.. el:define-key:: C-_
   C-/
   C-x u

   Undo previous tagging operation using :any:`notmuch-tag-undo`

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

.. _notmuch-tag-jump:

notmuch-tag-jump
----------------

Tagging operations configured through ``notmuch-tagging-keys`` can
be accessed via :kbd:`k` in :ref:`notmuch-show`,
:ref:`notmuch-search` and :ref:`notmuch-tree`.  With a
prefix (:kbd:`C-u k`), notmuch displays a menu of the reverses of the
operations specified in ``notmuch-tagging-keys``; i.e. each
``+tag`` is replaced by ``-tag`` and vice versa.

.. el:defcustom:: notmuch-tagging-keys

  |docstring::notmuch-tagging-keys|


notmuch-tag-undo
----------------

Each notmuch buffer supporting tagging operations (i.e. buffers in
:any:`notmuch-show`, :any:`notmuch-search`, :any:`notmuch-tree`, and
:any:`notmuch-unthreaded` mode) keeps a local stack of tagging
operations. These can be undone via :any:`notmuch-tag-undo`. By default
this is bound to the usual Emacs keys for undo.

.. el:define-key::  C-_
   C-/
   C-x u
   M-x notmuch-tag-undo

   |docstring::notmuch-tag-undo|

Buffer navigation
=================

.. el:define-key:: M-x notmuch-cycle-notmuch-buffers

   |docstring::notmuch-cycle-notmuch-buffers|

Configuration
=============

.. _importing:

Importing Mail
--------------

.. el:define-key:: M-x notmuch-poll

   |docstring::notmuch-poll|

.. el:defcustom:: notmuch-poll-script

   |docstring::notmuch-poll-script|

Sending Mail
------------

.. el:defcustom:: mail-user-agent

       Emacs consults the variable :code:`mail-user-agent` to choose a mail
       sending package for commands like :code:`report-emacs-bug` and
       :code:`compose-mail`.  To use ``notmuch`` for this, customize this
       variable to the symbol :code:`notmuch-user-agent`.

.. el:defcustom:: message-dont-reply-to-names

       When composing mail replies, Emacs's message mode uses the
       variable :code:`message-dont-reply-to-names` to exclude
       recipients matching a given collection of regular expressions
       or satisfying an arbitrary predicate.  Notmuch's MUA inherits
       this standard mechanism and will honour your customization of
       this variable.

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
