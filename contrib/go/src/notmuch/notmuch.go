// Wrapper around the notmuch library

package notmuch

/*
#cgo LDFLAGS: -lnotmuch

#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "notmuch.h"
*/
import "C"
import "unsafe"

// Status codes used for the return values of most functions
type Status C.notmuch_status_t

const (
	STATUS_SUCCESS Status = iota
	STATUS_OUT_OF_MEMORY
	STATUS_READ_ONLY_DATABASE
	STATUS_XAPIAN_EXCEPTION
	STATUS_FILE_ERROR
	STATUS_FILE_NOT_EMAIL
	STATUS_DUPLICATE_MESSAGE_ID
	STATUS_NULL_POINTER
	STATUS_TAG_TOO_LONG
	STATUS_UNBALANCED_FREEZE_THAW
	STATUS_UNBALANCED_ATOMIC

	STATUS_LAST_STATUS
)

func (self Status) String() string {
	var p *C.char

	// p is read-only
	p = C.notmuch_status_to_string(C.notmuch_status_t(self))
	if p != nil {
		s := C.GoString(p)
		return s
	}
	return ""
}

/* Various opaque data types. For each notmuch_<foo>_t see the various
 * notmuch_<foo> functions below. */

type Database struct {
	db *C.notmuch_database_t
}

type Query struct {
	query *C.notmuch_query_t
}

type Threads struct {
	threads *C.notmuch_threads_t
}

type Thread struct {
	thread *C.notmuch_thread_t
}

type Messages struct {
	messages *C.notmuch_messages_t
}

type Message struct {
	message *C.notmuch_message_t
}

type Tags struct {
	tags *C.notmuch_tags_t
}

type Directory struct {
	dir *C.notmuch_directory_t
}

type Filenames struct {
	fnames *C.notmuch_filenames_t
}

type DatabaseMode C.notmuch_database_mode_t

const (
	DATABASE_MODE_READ_ONLY DatabaseMode = iota
	DATABASE_MODE_READ_WRITE
)

// Create a new, empty notmuch database located at 'path'
func NewDatabase(path string) (*Database, Status) {

	var c_path *C.char = C.CString(path)
	defer C.free(unsafe.Pointer(c_path))

	if c_path == nil {
		return nil, STATUS_OUT_OF_MEMORY
	}

	self := &Database{db: nil}
	st := Status(C.notmuch_database_create(c_path, &self.db))
	if st != STATUS_SUCCESS {
		return nil, st
	}
	return self, st
}

/* Open an existing notmuch database located at 'path'.
 *
 * The database should have been created at some time in the past,
 * (not necessarily by this process), by calling
 * notmuch_database_create with 'path'. By default the database should be
 * opened for reading only. In order to write to the database you need to
 * pass the NOTMUCH_DATABASE_MODE_READ_WRITE mode.
 *
 * An existing notmuch database can be identified by the presence of a
 * directory named ".notmuch" below 'path'.
 *
 * The caller should call notmuch_database_destroy when finished with
 * this database.
 *
 * In case of any failure, this function returns NULL, (after printing
 * an error message on stderr).
 */
func OpenDatabase(path string, mode DatabaseMode) (*Database, Status) {

	var c_path *C.char = C.CString(path)
	defer C.free(unsafe.Pointer(c_path))

	if c_path == nil {
		return nil, STATUS_OUT_OF_MEMORY
	}

	self := &Database{db: nil}
	st := Status(C.notmuch_database_open(c_path, C.notmuch_database_mode_t(mode), &self.db))
	if st != STATUS_SUCCESS {
		return nil, st
	}
	return self, st
}

/* Close the given notmuch database, freeing all associated
 * resources. See notmuch_database_open. */
func (self *Database) Close() Status {
	return Status(C.notmuch_database_destroy(self.db))
}

/* Return the database path of the given database.
 */
func (self *Database) GetPath() string {

	/* The return value is a string owned by notmuch so should not be
	 * modified nor freed by the caller. */
	var p *C.char = C.notmuch_database_get_path(self.db)
	if p != nil {
		s := C.GoString(p)
		return s
	}
	return ""
}

/* Return the database format version of the given database. */
func (self *Database) GetVersion() uint {
	return uint(C.notmuch_database_get_version(self.db))
}

/* Does this database need to be upgraded before writing to it?
 *
 * If this function returns TRUE then no functions that modify the
 * database (notmuch_database_add_message, notmuch_message_add_tag,
 * notmuch_directory_set_mtime, etc.) will work unless the function
 * notmuch_database_upgrade is called successfully first. */
func (self *Database) NeedsUpgrade() bool {
	do_upgrade := C.notmuch_database_needs_upgrade(self.db)
	if do_upgrade == 0 {
		return false
	}
	return true
}

// TODO: notmuch_database_upgrade

/* Retrieve a directory object from the database for 'path'.
 *
 * Here, 'path' should be a path relative to the path of 'database'
 * (see notmuch_database_get_path), or else should be an absolute path
 * with initial components that match the path of 'database'.
 *
 * Can return NULL if a Xapian exception occurs.
 */
func (self *Database) GetDirectory(path string) (*Directory, Status) {
	var c_path *C.char = C.CString(path)
	defer C.free(unsafe.Pointer(c_path))

	if c_path == nil {
		return nil, STATUS_OUT_OF_MEMORY
	}

	var c_dir *C.notmuch_directory_t
	st := Status(C.notmuch_database_get_directory(self.db, c_path, &c_dir))
	if st != STATUS_SUCCESS || c_dir == nil {
		return nil, st
	}
	return &Directory{dir: c_dir}, st
}

/* Add a new message to the given notmuch database.
 *
 * Here,'filename' should be a path relative to the path of
 * 'database' (see notmuch_database_get_path), or else should be an
 * absolute filename with initial components that match the path of
 * 'database'.
 *
 * The file should be a single mail message (not a multi-message mbox)
 * that is expected to remain at its current location, (since the
 * notmuch database will reference the filename, and will not copy the
 * entire contents of the file.
 *
 * If 'message' is not NULL, then, on successful return '*message'
 * will be initialized to a message object that can be used for things
 * such as adding tags to the just-added message. The user should call
 * notmuch_message_destroy when done with the message. On any failure
 * '*message' will be set to NULL.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully added to database.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred,
 *	message not added.
 *
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID: Message has the same message
 *	ID as another message already in the database. The new
 *	filename was successfully added to the message in the database
 *	(if not already present).
 *
 * NOTMUCH_STATUS_FILE_ERROR: an error occurred trying to open the
 *	file, (such as permission denied, or file not found,
 *	etc.). Nothing added to the database.
 *
 * NOTMUCH_STATUS_FILE_NOT_EMAIL: the contents of filename don't look
 *	like an email message. Nothing added to the database.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so no message can be added.
 */
func (self *Database) AddMessage(fname string) (*Message, Status) {
	var c_fname *C.char = C.CString(fname)
	defer C.free(unsafe.Pointer(c_fname))

	if c_fname == nil {
		return nil, STATUS_OUT_OF_MEMORY
	}

	var c_msg *C.notmuch_message_t = new(C.notmuch_message_t)
	st := Status(C.notmuch_database_add_message(self.db, c_fname, &c_msg))

	return &Message{message: c_msg}, st
}

/* Remove a message from the given notmuch database.
 *
 * Note that only this particular filename association is removed from
 * the database. If the same message (as determined by the message ID)
 * is still available via other filenames, then the message will
 * persist in the database for those filenames. When the last filename
 * is removed for a particular message, the database content for that
 * message will be entirely removed.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: The last filename was removed and the
 *	message was removed from the database.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred,
 *	message not removed.
 *
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID: This filename was removed but
 *	the message persists in the database with at least one other
 *	filename.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so no message can be removed.
 */
func (self *Database) RemoveMessage(fname string) Status {

	var c_fname *C.char = C.CString(fname)
	defer C.free(unsafe.Pointer(c_fname))

	if c_fname == nil {
		return STATUS_OUT_OF_MEMORY
	}

	st := C.notmuch_database_remove_message(self.db, c_fname)
	return Status(st)
}

/* Find a message with the given message_id.
 *
 * If the database contains a message with the given message_id, then
 * a new notmuch_message_t object is returned. The caller should call
 * notmuch_message_destroy when done with the message.
 *
 * This function returns NULL in the following situations:
 *
 *	* No message is found with the given message_id
 *	* An out-of-memory situation occurs
 *	* A Xapian exception occurs
 */
func (self *Database) FindMessage(message_id string) (*Message, Status) {

	var c_msg_id *C.char = C.CString(message_id)
	defer C.free(unsafe.Pointer(c_msg_id))

	if c_msg_id == nil {
		return nil, STATUS_OUT_OF_MEMORY
	}

	msg := &Message{message: nil}
	st := Status(C.notmuch_database_find_message(self.db, c_msg_id, &msg.message))
	if st != STATUS_SUCCESS {
		return nil, st
	}
	return msg, st
}

/* Return a list of all tags found in the database.
 *
 * This function creates a list of all tags found in the database. The
 * resulting list contains all tags from all messages found in the database.
 *
 * On error this function returns NULL.
 */
func (self *Database) GetAllTags() *Tags {
	tags := C.notmuch_database_get_all_tags(self.db)
	if tags == nil {
		return nil
	}
	return &Tags{tags: tags}
}

/* Create a new query for 'database'.
 *
 * Here, 'database' should be an open database, (see
 * notmuch_database_open and notmuch_database_create).
 *
 * For the query string, we'll document the syntax here more
 * completely in the future, but it's likely to be a specialized
 * version of the general Xapian query syntax:
 *
 * https://xapian.org/docs/queryparser.html
 *
 * As a special case, passing either a length-zero string, (that is ""),
 * or a string consisting of a single asterisk (that is "*"), will
 * result in a query that returns all messages in the database.
 *
 * See notmuch_query_set_sort for controlling the order of results.
 * See notmuch_query_search_messages and notmuch_query_search_threads
 * to actually execute the query.
 *
 * User should call notmuch_query_destroy when finished with this
 * query.
 *
 * Will return NULL if insufficient memory is available.
 */
func (self *Database) CreateQuery(query string) *Query {

	var c_query *C.char = C.CString(query)
	defer C.free(unsafe.Pointer(c_query))

	if c_query == nil {
		return nil
	}

	q := C.notmuch_query_create(self.db, c_query)
	if q == nil {
		return nil
	}
	return &Query{query: q}
}

/* Sort values for notmuch_query_set_sort */
type Sort C.notmuch_sort_t

const (
	SORT_OLDEST_FIRST Sort = 0
	SORT_NEWEST_FIRST
	SORT_MESSAGE_ID
	SORT_UNSORTED
)

/* Return the query_string of this query. See notmuch_query_create. */
func (self *Query) String() string {
	// FIXME: do we own 'q' or not ?
	q := C.notmuch_query_get_query_string(self.query)
	//defer C.free(unsafe.Pointer(q))

	if q != nil {
		s := C.GoString(q)
		return s
	}

	return ""
}

/* Specify the sorting desired for this query. */
func (self *Query) SetSort(sort Sort) {
	C.notmuch_query_set_sort(self.query, C.notmuch_sort_t(sort))
}

/* Return the sort specified for this query. See notmuch_query_set_sort. */
func (self *Query) GetSort() Sort {
	return Sort(C.notmuch_query_get_sort(self.query))
}

/* Execute a query for threads, returning a notmuch_threads_t object
 * which can be used to iterate over the results. The returned threads
 * object is owned by the query and as such, will only be valid until
 * notmuch_query_destroy.
 *
 * Typical usage might be:
 *
 *     notmuch_query_t *query;
 *     notmuch_threads_t *threads;
 *     notmuch_thread_t *thread;
 *
 *     query = notmuch_query_create (database, query_string);
 *
 *     for (threads = notmuch_query_search_threads (query);
 *          notmuch_threads_valid (threads);
 *          notmuch_threads_move_to_next (threads))
 *     {
 *         thread = notmuch_threads_get (threads);
 *         ....
 *         notmuch_thread_destroy (thread);
 *     }
 *
 *     notmuch_query_destroy (query);
 *
 * Note: If you are finished with a thread before its containing
 * query, you can call notmuch_thread_destroy to clean up some memory
 * sooner (as in the above example). Otherwise, if your thread objects
 * are long-lived, then you don't need to call notmuch_thread_destroy
 * and all the memory will still be reclaimed when the query is
 * destroyed.
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_threads_t object. (For consistency, we do provide a
 * notmuch_threads_destroy function, but there's no good reason
 * to call it if the query is about to be destroyed).
 *
 * If a Xapian exception occurs this function will return NULL.
 */
func (self *Query) SearchThreads() *Threads {
	threads := C.notmuch_query_search_threads(self.query)
	if threads == nil {
		return nil
	}
	return &Threads{threads: threads}
}

/* Execute a query for messages, returning a notmuch_messages_t object
 * which can be used to iterate over the results. The returned
 * messages object is owned by the query and as such, will only be
 * valid until notmuch_query_destroy.
 *
 * Typical usage might be:
 *
 *     notmuch_query_t *query;
 *     notmuch_messages_t *messages;
 *     notmuch_message_t *message;
 *
 *     query = notmuch_query_create (database, query_string);
 *
 *     for (messages = notmuch_query_search_messages (query);
 *          notmuch_messages_valid (messages);
 *          notmuch_messages_move_to_next (messages))
 *     {
 *         message = notmuch_messages_get (messages);
 *         ....
 *         notmuch_message_destroy (message);
 *     }
 *
 *     notmuch_query_destroy (query);
 *
 * Note: If you are finished with a message before its containing
 * query, you can call notmuch_message_destroy to clean up some memory
 * sooner (as in the above example). Otherwise, if your message
 * objects are long-lived, then you don't need to call
 * notmuch_message_destroy and all the memory will still be reclaimed
 * when the query is destroyed.
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_messages_t object. (For consistency, we do provide a
 * notmuch_messages_destroy function, but there's no good
 * reason to call it if the query is about to be destroyed).
 *
 * If a Xapian exception occurs this function will return NULL.
 */
func (self *Query) SearchMessages() *Messages {
	msgs := C.notmuch_query_search_messages(self.query)
	if msgs == nil {
		return nil
	}
	return &Messages{messages: msgs}
}

/* Destroy a notmuch_query_t along with any associated resources.
 *
 * This will in turn destroy any notmuch_threads_t and
 * notmuch_messages_t objects generated by this query, (and in
 * turn any notmuch_thread_t and notmuch_message_t objects generated
 * from those results, etc.), if such objects haven't already been
 * destroyed.
 */
func (self *Query) Destroy() {
	if self.query != nil {
		C.notmuch_query_destroy(self.query)
	}
}

/* Return an estimate of the number of messages matching a search
 *
 * This function performs a search and returns Xapian's best
 * guess as to number of matching messages.
 *
 * If a Xapian exception occurs, this function may return 0 (after
 * printing a message).
 */
func (self *Query) CountMessages() uint {
	return uint(C.notmuch_query_count_messages(self.query))
}

/* Is the given 'threads' iterator pointing at a valid thread.
 *
 * When this function returns TRUE, notmuch_threads_get will return a
 * valid object. Whereas when this function returns FALSE,
 * notmuch_threads_get will return NULL.
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_threads_t object.
 */
func (self *Threads) Valid() bool {
	if self.threads == nil {
		return false
	}
	valid := C.notmuch_threads_valid(self.threads)
	if valid == 0 {
		return false
	}
	return true
}

/* Get the current thread from 'threads' as a notmuch_thread_t.
 *
 * Note: The returned thread belongs to 'threads' and has a lifetime
 * identical to it (and the query to which it belongs).
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_threads_t object.
 *
 * If an out-of-memory situation occurs, this function will return
 * NULL.
 */
func (self *Threads) Get() *Thread {
	if self.threads == nil {
		return nil
	}
	thread := C.notmuch_threads_get(self.threads)
	if thread == nil {
		return nil
	}
	return &Thread{thread}
}

/* Move the 'threads' iterator to the next thread.
 *
 * If 'threads' is already pointing at the last thread then the
 * iterator will be moved to a point just beyond that last thread,
 * (where notmuch_threads_valid will return FALSE and
 * notmuch_threads_get will return NULL).
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_threads_t object.
 */
func (self *Threads) MoveToNext() {
	if self.threads == nil {
		return
	}
	C.notmuch_threads_move_to_next(self.threads)
}

/* Destroy a notmuch_threads_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_threads_t object will be reclaimed when the
 * containg query object is destroyed.
 */
func (self *Threads) Destroy() {
	if self.threads != nil {
		C.notmuch_threads_destroy(self.threads)
	}
}

/**
 * Get the thread ID of 'thread'.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
func (self *Thread) GetThreadId() string {
	if self.thread == nil {
		return ""
	}
	id := C.notmuch_thread_get_thread_id(self.thread)
	if id == nil {
		return ""
	}
	return C.GoString(id)
}

/**
 * Get the total number of messages in 'thread'.
 *
 * This count consists of all messages in the database belonging to
 * this thread. Contrast with notmuch_thread_get_matched_messages() .
 */
func (self *Thread) GetTotalMessages() int {
	if self.thread == nil {
		return 0
	}
	return int(C.notmuch_thread_get_total_messages(self.thread))
}

/**
 * Get a notmuch_messages_t iterator for the top-level messages in
 * 'thread' in oldest-first order.
 *
 * This iterator will not necessarily iterate over all of the messages
 * in the thread. It will only iterate over the messages in the thread
 * which are not replies to other messages in the thread.
 *
 * The returned list will be destroyed when the thread is destroyed.
 */
func (self *Thread) GetToplevelMessages() (*Messages, Status) {
	if self.thread == nil {
		return nil, STATUS_NULL_POINTER
	}

	msgs := C.notmuch_thread_get_toplevel_messages(self.thread)
	if msgs == nil {
		return nil, STATUS_NULL_POINTER
	}
	return &Messages{msgs}, STATUS_SUCCESS
}

/**
 * Get a notmuch_thread_t iterator for all messages in 'thread' in
 * oldest-first order.
 *
 * The returned list will be destroyed when the thread is destroyed.
 */
func (self *Thread) GetMessages() (*Messages, Status) {
	if self.thread == nil {
		return nil, STATUS_NULL_POINTER
	}

	msgs := C.notmuch_thread_get_messages(self.thread)
	if msgs == nil {
		return nil, STATUS_NULL_POINTER
	}
	return &Messages{msgs}, STATUS_SUCCESS
}

/**
 * Get the number of messages in 'thread' that matched the search.
 *
 * This count includes only the messages in this thread that were
 * matched by the search from which the thread was created and were
 * not excluded by any exclude tags passed in with the query (see
 * notmuch_query_add_tag_exclude). Contrast with
 * notmuch_thread_get_total_messages() .
 */
func (self *Thread) GetMatchedMessages() int {
	if self.thread == nil {
		return 0
	}
	return int(C.notmuch_thread_get_matched_messages(self.thread))
}

/**
 * Get the authors of 'thread' as a UTF-8 string.
 *
 * The returned string is a comma-separated list of the names of the
 * authors of mail messages in the query results that belong to this
 * thread.
 *
 * The string contains authors of messages matching the query first, then
 * non-matched authors (with the two groups separated by '|'). Within
 * each group, authors are ordered by date.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
func (self *Thread) GetAuthors() string {
	if self.thread == nil {
		return ""
	}
	str := C.notmuch_thread_get_authors(self.thread)
	if str == nil {
		return ""
	}
	return C.GoString(str)
}

/**
 * Get the subject of 'thread' as a UTF-8 string.
 *
 * The subject is taken from the first message (according to the query
 * order---see notmuch_query_set_sort) in the query results that
 * belongs to this thread.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
func (self *Thread) GetSubject() string {
	if self.thread == nil {
		return ""
	}
	str := C.notmuch_thread_get_subject(self.thread)
	if str == nil {
		return ""
	}
	return C.GoString(str)
}

/**
 * Get the date of the oldest message in 'thread' as a time_t value.
 */
func (self *Thread) GetOldestDate() int64 {
	if self.thread == nil {
		return 0
	}
	date := C.notmuch_thread_get_oldest_date(self.thread)

	return int64(date)
}

/**
 * Get the date of the newest message in 'thread' as a time_t value.
 */
func (self *Thread) GetNewestDate() int64 {
	if self.thread == nil {
		return 0
	}
	date := C.notmuch_thread_get_newest_date(self.thread)

	return int64(date)
}

/**
 * Get the tags for 'thread', returning a notmuch_tags_t object which
 * can be used to iterate over all tags.
 *
 * Note: In the Notmuch database, tags are stored on individual
 * messages, not on threads. So the tags returned here will be all
 * tags of the messages which matched the search and which belong to
 * this thread.
 *
 * The tags object is owned by the thread and as such, will only be
 * valid for as long as the thread is valid, (for example, until
 * notmuch_thread_destroy or until the query from which it derived is
 * destroyed).
 *
 * Typical usage might be:
 *
 *     notmuch_thread_t *thread;
 *     notmuch_tags_t *tags;
 *     const char *tag;
 *
 *     thread = notmuch_threads_get (threads);
 *
 *     for (tags = notmuch_thread_get_tags (thread);
 *          notmuch_tags_valid (tags);
 *          notmuch_tags_move_to_next (tags))
 *     {
 *         tag = notmuch_tags_get (tags);
 *         ....
 *     }
 *
 *     notmuch_thread_destroy (thread);
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_tags_t object. (For consistency, we do provide a
 * notmuch_tags_destroy function, but there's no good reason to call
 * it if the message is about to be destroyed).
 */
func (self *Thread) GetTags() *Tags {
	if self.thread == nil {
		return nil
	}

	tags := C.notmuch_thread_get_tags(self.thread)
	if tags == nil {
		return nil
	}

	return &Tags{tags}
}

/**
 * Destroy a notmuch_thread_t object.
 */
func (self *Thread) Destroy() {
	if self.thread != nil {
		C.notmuch_thread_destroy(self.thread)
	}
}

/* Is the given 'messages' iterator pointing at a valid message.
 *
 * When this function returns TRUE, notmuch_messages_get will return a
 * valid object. Whereas when this function returns FALSE,
 * notmuch_messages_get will return NULL.
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_messages_t object.
 */
func (self *Messages) Valid() bool {
	if self.messages == nil {
		return false
	}
	valid := C.notmuch_messages_valid(self.messages)
	if valid == 0 {
		return false
	}
	return true
}

/* Get the current message from 'messages' as a notmuch_message_t.
 *
 * Note: The returned message belongs to 'messages' and has a lifetime
 * identical to it (and the query to which it belongs).
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_messages_t object.
 *
 * If an out-of-memory situation occurs, this function will return
 * NULL.
 */
func (self *Messages) Get() *Message {
	if self.messages == nil {
		return nil
	}
	msg := C.notmuch_messages_get(self.messages)
	if msg == nil {
		return nil
	}
	return &Message{message: msg}
}

/* Move the 'messages' iterator to the next message.
 *
 * If 'messages' is already pointing at the last message then the
 * iterator will be moved to a point just beyond that last message,
 * (where notmuch_messages_valid will return FALSE and
 * notmuch_messages_get will return NULL).
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_messages_t object.
 */
func (self *Messages) MoveToNext() {
	if self.messages == nil {
		return
	}
	C.notmuch_messages_move_to_next(self.messages)
}

/* Destroy a notmuch_messages_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_messages_t object will be reclaimed when the containing
 * query object is destroyed.
 */
func (self *Messages) Destroy() {
	if self.messages != nil {
		C.notmuch_messages_destroy(self.messages)
	}
}

/* Return a list of tags from all messages.
 *
 * The resulting list is guaranteed not to contain duplicated tags.
 *
 * WARNING: You can no longer iterate over messages after calling this
 * function, because the iterator will point at the end of the list.
 * We do not have a function to reset the iterator yet and the only
 * way how you can iterate over the list again is to recreate the
 * message list.
 *
 * The function returns NULL on error.
 */
func (self *Messages) CollectTags() *Tags {
	if self.messages == nil {
		return nil
	}
	tags := C.notmuch_messages_collect_tags(self.messages)
	if tags == nil {
		return nil
	}
	return &Tags{tags: tags}
}

/* Get the message ID of 'message'.
 *
 * The returned string belongs to 'message' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * message is valid, (which is until the query from which it derived
 * is destroyed).
 *
 * This function will not return NULL since Notmuch ensures that every
 * message has a unique message ID, (Notmuch will generate an ID for a
 * message if the original file does not contain one).
 */
func (self *Message) GetMessageId() string {

	if self.message == nil {
		return ""
	}
	id := C.notmuch_message_get_message_id(self.message)
	// we dont own id
	// defer C.free(unsafe.Pointer(id))
	if id == nil {
		return ""
	}
	return C.GoString(id)
}

/* Get the thread ID of 'message'.
 *
 * The returned string belongs to 'message' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * message is valid, (for example, until the user calls
 * notmuch_message_destroy on 'message' or until a query from which it
 * derived is destroyed).
 *
 * This function will not return NULL since Notmuch ensures that every
 * message belongs to a single thread.
 */
func (self *Message) GetThreadId() string {

	if self.message == nil {
		return ""
	}
	id := C.notmuch_message_get_thread_id(self.message)
	// we dont own id
	// defer C.free(unsafe.Pointer(id))

	if id == nil {
		return ""
	}

	return C.GoString(id)
}

/* Get a notmuch_messages_t iterator for all of the replies to
 * 'message'.
 *
 * Note: This call only makes sense if 'message' was ultimately
 * obtained from a notmuch_thread_t object, (such as by coming
 * directly from the result of calling notmuch_thread_get_
 * toplevel_messages or by any number of subsequent
 * calls to notmuch_message_get_replies).
 *
 * If 'message' was obtained through some non-thread means, (such as
 * by a call to notmuch_query_search_messages), then this function
 * will return NULL.
 *
 * If there are no replies to 'message', this function will return
 * NULL. (Note that notmuch_messages_valid will accept that NULL
 * value as legitimate, and simply return FALSE for it.)
 */
func (self *Message) GetReplies() *Messages {
	if self.message == nil {
		return nil
	}
	msgs := C.notmuch_message_get_replies(self.message)
	if msgs == nil {
		return nil
	}
	return &Messages{messages: msgs}
}

/* Get a filename for the email corresponding to 'message'.
 *
 * The returned filename is an absolute filename, (the initial
 * component will match notmuch_database_get_path() ).
 *
 * The returned string belongs to the message so should not be
 * modified or freed by the caller (nor should it be referenced after
 * the message is destroyed).
 *
 * Note: If this message corresponds to multiple files in the mail
 * store, (that is, multiple files contain identical message IDs),
 * this function will arbitrarily return a single one of those
 * filenames.
 */
func (self *Message) GetFileName() string {
	if self.message == nil {
		return ""
	}
	fname := C.notmuch_message_get_filename(self.message)
	// we dont own fname
	// defer C.free(unsafe.Pointer(fname))

	if fname == nil {
		return ""
	}

	return C.GoString(fname)
}

type Flag C.notmuch_message_flag_t

const (
	MESSAGE_FLAG_MATCH Flag = 0
)

/* Get a value of a flag for the email corresponding to 'message'. */
func (self *Message) GetFlag(flag Flag) bool {
	if self.message == nil {
		return false
	}
	v := C.notmuch_message_get_flag(self.message, C.notmuch_message_flag_t(flag))
	if v == 0 {
		return false
	}
	return true
}

/* Set a value of a flag for the email corresponding to 'message'. */
func (self *Message) SetFlag(flag Flag, value bool) {
	if self.message == nil {
		return
	}
	var v C.notmuch_bool_t = 0
	if value {
		v = 1
	}
	C.notmuch_message_set_flag(self.message, C.notmuch_message_flag_t(flag), v)
}

/* Get the timestamp (seconds since the epoch) of 'message'.
 *
 * Return status:
 *
 * NOTMUCH_STATUS_SUCCESS: Timestamp successfully retrieved
 *
 * NOTMUCH_STATUS_NULL_POINTER: The 'message' argument is NULL
 *
 */
func (self *Message) GetDate() (int64, Status) {
	if self.message == nil {
		return -1, STATUS_NULL_POINTER
	}
	timestamp := C.notmuch_message_get_date(self.message)
	return int64(timestamp), STATUS_SUCCESS
}

/* Get the value of the specified header from 'message'.
 *
 * The value will be read from the actual message file, not from the
 * notmuch database. The header name is case insensitive.
 *
 * The returned string belongs to the message so should not be
 * modified or freed by the caller (nor should it be referenced after
 * the message is destroyed).
 *
 * Returns an empty string ("") if the message does not contain a
 * header line matching 'header'. Returns NULL if any error occurs.
 */
func (self *Message) GetHeader(header string) string {
	if self.message == nil {
		return ""
	}

	var c_header *C.char = C.CString(header)
	defer C.free(unsafe.Pointer(c_header))

	/* we dont own value */
	value := C.notmuch_message_get_header(self.message, c_header)
	if value == nil {
		return ""
	}

	return C.GoString(value)
}

/* Get the tags for 'message', returning a notmuch_tags_t object which
 * can be used to iterate over all tags.
 *
 * The tags object is owned by the message and as such, will only be
 * valid for as long as the message is valid, (which is until the
 * query from which it derived is destroyed).
 *
 * Typical usage might be:
 *
 *     notmuch_message_t *message;
 *     notmuch_tags_t *tags;
 *     const char *tag;
 *
 *     message = notmuch_database_find_message (database, message_id);
 *
 *     for (tags = notmuch_message_get_tags (message);
 *          notmuch_tags_valid (tags);
 *          notmuch_result_move_to_next (tags))
 *     {
 *         tag = notmuch_tags_get (tags);
 *         ....
 *     }
 *
 *     notmuch_message_destroy (message);
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_tags_t object. (For consistency, we do provide a
 * notmuch_tags_destroy function, but there's no good reason to call
 * it if the message is about to be destroyed).
 */
func (self *Message) GetTags() *Tags {
	if self.message == nil {
		return nil
	}
	tags := C.notmuch_message_get_tags(self.message)
	if tags == nil {
		return nil
	}
	return &Tags{tags: tags}
}

/* The longest possible tag value. */
const TAG_MAX = 200

/* Add a tag to the given message.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Tag successfully added to message
 *
 * NOTMUCH_STATUS_NULL_POINTER: The 'tag' argument is NULL
 *
 * NOTMUCH_STATUS_TAG_TOO_LONG: The length of 'tag' is too long
 *	(exceeds NOTMUCH_TAG_MAX)
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
func (self *Message) AddTag(tag string) Status {
	if self.message == nil {
		return STATUS_NULL_POINTER
	}
	c_tag := C.CString(tag)
	defer C.free(unsafe.Pointer(c_tag))

	return Status(C.notmuch_message_add_tag(self.message, c_tag))
}

/* Remove a tag from the given message.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Tag successfully removed from message
 *
 * NOTMUCH_STATUS_NULL_POINTER: The 'tag' argument is NULL
 *
 * NOTMUCH_STATUS_TAG_TOO_LONG: The length of 'tag' is too long
 *	(exceeds NOTMUCH_TAG_MAX)
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
func (self *Message) RemoveTag(tag string) Status {
	if self.message == nil {
		return STATUS_NULL_POINTER
	}
	c_tag := C.CString(tag)
	defer C.free(unsafe.Pointer(c_tag))

	return Status(C.notmuch_message_remove_tag(self.message, c_tag))
}

/* Remove all tags from the given message.
 *
 * See notmuch_message_freeze for an example showing how to safely
 * replace tag values.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
func (self *Message) RemoveAllTags() Status {
	if self.message == nil {
		return STATUS_NULL_POINTER
	}
	return Status(C.notmuch_message_remove_all_tags(self.message))
}

/* Freeze the current state of 'message' within the database.
 *
 * This means that changes to the message state, (via
 * notmuch_message_add_tag, notmuch_message_remove_tag, and
 * notmuch_message_remove_all_tags), will not be committed to the
 * database until the message is thawed with notmuch_message_thaw.
 *
 * Multiple calls to freeze/thaw are valid and these calls will
 * "stack". That is there must be as many calls to thaw as to freeze
 * before a message is actually thawed.
 *
 * The ability to do freeze/thaw allows for safe transactions to
 * change tag values. For example, explicitly setting a message to
 * have a given set of tags might look like this:
 *
 *    notmuch_message_freeze (message);
 *
 *    notmuch_message_remove_all_tags (message);
 *
 *    for (i = 0; i < NUM_TAGS; i++)
 *        notmuch_message_add_tag (message, tags[i]);
 *
 *    notmuch_message_thaw (message);
 *
 * With freeze/thaw used like this, the message in the database is
 * guaranteed to have either the full set of original tag values, or
 * the full set of new tag values, but nothing in between.
 *
 * Imagine the example above without freeze/thaw and the operation
 * somehow getting interrupted. This could result in the message being
 * left with no tags if the interruption happened after
 * notmuch_message_remove_all_tags but before notmuch_message_add_tag.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully frozen.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
func (self *Message) Freeze() Status {
	if self.message == nil {
		return STATUS_NULL_POINTER
	}
	return Status(C.notmuch_message_freeze(self.message))
}

/* Thaw the current 'message', synchronizing any changes that may have
 * occurred while 'message' was frozen into the notmuch database.
 *
 * See notmuch_message_freeze for an example of how to use this
 * function to safely provide tag changes.
 *
 * Multiple calls to freeze/thaw are valid and these calls with
 * "stack". That is there must be as many calls to thaw as to freeze
 * before a message is actually thawed.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully thawed, (or at least
 *	its frozen count has successfully been reduced by 1).
 *
 * NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW: An attempt was made to thaw
 *	an unfrozen message. That is, there have been an unbalanced
 *	number of calls to notmuch_message_freeze and
 *	notmuch_message_thaw.
 */
func (self *Message) Thaw() Status {
	if self.message == nil {
		return STATUS_NULL_POINTER
	}

	return Status(C.notmuch_message_thaw(self.message))
}

/* Destroy a notmuch_message_t object.
 *
 * It can be useful to call this function in the case of a single
 * query object with many messages in the result, (such as iterating
 * over the entire database). Otherwise, it's fine to never call this
 * function and there will still be no memory leaks. (The memory from
 * the messages get reclaimed when the containing query is destroyed.)
 */
func (self *Message) Destroy() {
	if self.message == nil {
		return
	}
	C.notmuch_message_destroy(self.message)
}

/* Is the given 'tags' iterator pointing at a valid tag.
 *
 * When this function returns TRUE, notmuch_tags_get will return a
 * valid string. Whereas when this function returns FALSE,
 * notmuch_tags_get will return NULL.
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
func (self *Tags) Valid() bool {
	if self.tags == nil {
		return false
	}
	v := C.notmuch_tags_valid(self.tags)
	if v == 0 {
		return false
	}
	return true
}

/* Get the current tag from 'tags' as a string.
 *
 * Note: The returned string belongs to 'tags' and has a lifetime
 * identical to it (and the query to which it ultimately belongs).
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
func (self *Tags) Get() string {
	if self.tags == nil {
		return ""
	}
	s := C.notmuch_tags_get(self.tags)
	// we dont own 's'

	return C.GoString(s)
}
func (self *Tags) String() string {
	return self.Get()
}

/* Move the 'tags' iterator to the next tag.
 *
 * If 'tags' is already pointing at the last tag then the iterator
 * will be moved to a point just beyond that last tag, (where
 * notmuch_tags_valid will return FALSE and notmuch_tags_get will
 * return NULL).
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
func (self *Tags) MoveToNext() {
	if self.tags == nil {
		return
	}
	C.notmuch_tags_move_to_next(self.tags)
}

/* Destroy a notmuch_tags_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_tags_t object will be reclaimed when the containing
 * message or query objects are destroyed.
 */
func (self *Tags) Destroy() {
	if self.tags == nil {
		return
	}
	C.notmuch_tags_destroy(self.tags)
}

// TODO: wrap notmuch_directory_<fct>

/* Destroy a notmuch_directory_t object. */
func (self *Directory) Destroy() {
	if self.dir == nil {
		return
	}
	C.notmuch_directory_destroy(self.dir)
}

// TODO: wrap notmuch_filenames_<fct>

/* Destroy a notmuch_filenames_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_filenames_t object will be reclaimed when the
 * containing directory object is destroyed.
 *
 * It is acceptable to pass NULL for 'filenames', in which case this
 * function will do nothing.
 */
func (self *Filenames) Destroy() {
	if self.fnames == nil {
		return
	}
	C.notmuch_filenames_destroy(self.fnames)
}

/* EOF */
