The notmuch 'binary'
====================

The cnotmuch module provides *notmuch*, a python reimplementation of the standard notmuch binary for two purposes: first, to allow running the standard notmuch testsuite over the cnotmuch bindings (for correctness and performance testing) and second, to give some examples as to how to use cnotmuch. 'Notmuch' provides a command line interface to your mail database.

A standard install via `easy_install cnotmuch` will not install the notmuch binary, however it is available in the `cnotmuch source code repository <http://bitbucket.org/spaetz/cnotmuch/src/>`_.


It is invoked with the following pattern: `notmuch <command> [args...]`.

Where <command> and [args...] are as follows:

  **setup**	Interactively setup notmuch for first use.
                This has not yet been implemented, and will probably not be
		implemented unless someone puts in the effort.

  **new**	[--verbose]
		Find and import new messages to the notmuch database.

		This has not been implemented yet. We cheat by calling
		the regular "notmuch" binary (which must be in your path
		somewhere).

  **search** [options...] <search-terms> [...]  Search for messages matching the given search terms.

		This has been implemented but for the `--format` and
		`--sort` options.

  **show**	<search-terms> [...]
		Show all messages matching the search terms.

		This has been partially implemented, we show a stub for each
		found message, but do not output the full message body yet.

  **count**	<search-terms> [...]
		Count messages matching the search terms.

		This has been fully implemented.

  **reply**	[options...] <search-terms> [...]
		Construct a reply template for a set of messages.

		This has not been implemented yet.

  **tag**	+<tag>|-<tag> [...] [--] <search-terms> [...]
		Add/remove tags for all messages matching the search terms.

		This has been fully implemented.

  **dump**	[<filename>]
		Create a plain-text dump of the tags for each message.

		This has been fully implemented.
  **restore**	<filename>
		Restore the tags from the given dump file (see 'dump').

		This has been fully implemented.

  **search-tags**	[<search-terms> [...] ]
		List all tags found in the database or matching messages.

		This has been fully implemented.

  **help**	[<command>]
		This message, or more detailed help for the named command.

		The 'help' page has been implemented, help for single
		commands are missing though. Patches are welcome.
