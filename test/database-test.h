#ifndef _DATABASE_TEST_H
#define _DATABASE_TEST_H
/* Add a new stub message to the given notmuch database.
 *
 * At least the following return values are possible:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully added to database.
 *
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID: Message has the same message
 *	ID as another message already in the database.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so no message can be added.
 */

notmuch_status_t
notmuch_database_add_stub_message (notmuch_database_t *database,
				   const char *message_id,
				   const char **tag_list);

#endif
