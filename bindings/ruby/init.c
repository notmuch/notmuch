/* The Ruby interface to the notmuch mail library
 *
 * Copyright © 2010, 2011, 2012 Ali Polatel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Ali Polatel <alip@exherbo.org>
 */

#include "defs.h"

VALUE notmuch_rb_cDatabase;
VALUE notmuch_rb_cDirectory;
VALUE notmuch_rb_cFileNames;
VALUE notmuch_rb_cQuery;
VALUE notmuch_rb_cThreads;
VALUE notmuch_rb_cThread;
VALUE notmuch_rb_cMessages;
VALUE notmuch_rb_cMessage;
VALUE notmuch_rb_cTags;

VALUE notmuch_rb_eBaseError;
VALUE notmuch_rb_eDatabaseError;
VALUE notmuch_rb_eMemoryError;
VALUE notmuch_rb_eReadOnlyError;
VALUE notmuch_rb_eXapianError;
VALUE notmuch_rb_eFileError;
VALUE notmuch_rb_eFileNotEmailError;
VALUE notmuch_rb_eNullPointerError;
VALUE notmuch_rb_eTagTooLongError;
VALUE notmuch_rb_eUnbalancedFreezeThawError;
VALUE notmuch_rb_eUnbalancedAtomicError;

ID ID_call;
ID ID_db_create;
ID ID_db_mode;

/*
 * Document-module: Notmuch
 *
 * == Summary
 *
 * Ruby extension to the <tt>notmuch</tt> mail library.
 *
 * == Classes
 *
 * Following are the classes that are most likely to be of interest to
 * the user:
 *
 * - Notmuch::Database
 * - Notmuch::FileNames
 * - Notmuch::Query
 * - Notmuch::Threads
 * - Notmuch::Messages
 * - Notmuch::Thread
 * - Notmuch::Message
 * - Notmuch::Tags
 */

void
Init_notmuch (void)
{
    VALUE mod;

    ID_call = rb_intern ("call");
    ID_db_create = rb_intern ("create");
    ID_db_mode = rb_intern ("mode");

    mod = rb_define_module ("Notmuch");

    /*
     * Document-const: Notmuch::MODE_READ_ONLY
     *
     * Open the database in read only mode
     */
    rb_define_const (mod, "MODE_READ_ONLY", INT2FIX (NOTMUCH_DATABASE_MODE_READ_ONLY));
    /*
     * Document-const: Notmuch::MODE_READ_WRITE
     *
     * Open the database in read write mode
     */
    rb_define_const (mod, "MODE_READ_WRITE", INT2FIX (NOTMUCH_DATABASE_MODE_READ_WRITE));
    /*
     * Document-const: Notmuch::SORT_OLDEST_FIRST
     *
     * Sort query results by oldest first
     */
    rb_define_const (mod, "SORT_OLDEST_FIRST", INT2FIX (NOTMUCH_SORT_OLDEST_FIRST));
    /*
     * Document-const: Notmuch::SORT_NEWEST_FIRST
     *
     * Sort query results by newest first
     */
    rb_define_const (mod, "SORT_NEWEST_FIRST", INT2FIX (NOTMUCH_SORT_NEWEST_FIRST));
    /*
     * Document-const: Notmuch::SORT_MESSAGE_ID
     *
     * Sort query results by message id
     */
    rb_define_const (mod, "SORT_MESSAGE_ID", INT2FIX (NOTMUCH_SORT_MESSAGE_ID));
    /*
     * Document-const: Notmuch::SORT_UNSORTED
     *
     * Do not sort query results
     */
    rb_define_const (mod, "SORT_UNSORTED", INT2FIX (NOTMUCH_SORT_UNSORTED));
    /*
     * Document-const: Notmuch::MESSAGE_FLAG_MATCH
     *
     * Message flag "match"
     */
    rb_define_const (mod, "MESSAGE_FLAG_MATCH", INT2FIX (NOTMUCH_MESSAGE_FLAG_MATCH));
    /*
     * Document-const: Notmuch::MESSAGE_FLAG_EXCLUDED
     *
     * Message flag "excluded"
     */
    rb_define_const (mod, "MESSAGE_FLAG_EXCLUDED", INT2FIX (NOTMUCH_MESSAGE_FLAG_EXCLUDED));
    /*
     * Document-const: Notmuch::TAG_MAX
     *
     * Maximum allowed length of a tag
     */
    rb_define_const (mod, "TAG_MAX", INT2FIX (NOTMUCH_TAG_MAX));

    /*
     * Document-class: Notmuch::BaseError
     *
     * Base class for all notmuch exceptions
     */
    notmuch_rb_eBaseError = rb_define_class_under (mod, "BaseError", rb_eStandardError);
    /*
     * Document-class: Notmuch::DatabaseError
     *
     * Raised when the database can't be created or opened.
     */
    notmuch_rb_eDatabaseError = rb_define_class_under (mod, "DatabaseError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::MemoryError
     *
     * Raised when notmuch is out of memory
     */
    notmuch_rb_eMemoryError = rb_define_class_under (mod, "MemoryError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::ReadOnlyError
     *
     * Raised when an attempt was made to write to a database opened in read-only
     * mode.
     */
    notmuch_rb_eReadOnlyError = rb_define_class_under (mod, "ReadOnlyError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::XapianError
     *
     * Raised when a Xapian exception occurs
     */
    notmuch_rb_eXapianError = rb_define_class_under (mod, "XapianError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::FileError
     *
     * Raised when an error occurs trying to read or write to a file.
     */
    notmuch_rb_eFileError = rb_define_class_under (mod, "FileError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::FileNotEmailError
     *
     * Raised when a file is presented that doesn't appear to be an email message.
     */
    notmuch_rb_eFileNotEmailError = rb_define_class_under (mod, "FileNotEmailError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::NullPointerError
     *
     * Raised when the user erroneously passes a +NULL+ pointer to a notmuch
     * function.
     */
    notmuch_rb_eNullPointerError = rb_define_class_under (mod, "NullPointerError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::TagTooLongError
     *
     * Raised when a tag value is too long (exceeds Notmuch::TAG_MAX)
     */
    notmuch_rb_eTagTooLongError = rb_define_class_under (mod, "TagTooLongError", notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::UnbalancedFreezeThawError
     *
     * Raised when the notmuch_message_thaw function has been called more times
     * than notmuch_message_freeze.
     */
    notmuch_rb_eUnbalancedFreezeThawError = rb_define_class_under (mod, "UnbalancedFreezeThawError",
								   notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::UnbalancedAtomicError
     *
     * Raised when notmuch_database_end_atomic has been called more times than
     * notmuch_database_begin_atomic
     */
    notmuch_rb_eUnbalancedAtomicError = rb_define_class_under (mod, "UnbalancedAtomicError",
							       notmuch_rb_eBaseError);
    /*
     * Document-class: Notmuch::Database
     *
     * Notmuch database interaction
     */
    notmuch_rb_cDatabase = rb_define_class_under (mod, "Database", rb_cData);
    rb_define_alloc_func (notmuch_rb_cDatabase, notmuch_rb_database_alloc);
    rb_define_singleton_method (notmuch_rb_cDatabase, "open", notmuch_rb_database_open, -1); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "initialize", notmuch_rb_database_initialize, -1); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "close", notmuch_rb_database_close, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "path", notmuch_rb_database_path, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "version", notmuch_rb_database_version, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "needs_upgrade?", notmuch_rb_database_needs_upgrade, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "upgrade!", notmuch_rb_database_upgrade, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "begin_atomic", notmuch_rb_database_begin_atomic, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "end_atomic", notmuch_rb_database_end_atomic, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "get_directory", notmuch_rb_database_get_directory, 1); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "add_message", notmuch_rb_database_add_message, 1); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "remove_message", notmuch_rb_database_remove_message, 1); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "find_message",
		      notmuch_rb_database_find_message, 1); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "find_message_by_filename",
		      notmuch_rb_database_find_message_by_filename, 1); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "all_tags", notmuch_rb_database_get_all_tags, 0); /* in database.c */
    rb_define_method (notmuch_rb_cDatabase, "query", notmuch_rb_database_query_create, 1); /* in database.c */

    /*
     * Document-class: Notmuch::Directory
     *
     * Notmuch directory
     */
    notmuch_rb_cDirectory = rb_define_class_under (mod, "Directory", rb_cData);
    rb_undef_method (notmuch_rb_cDirectory, "initialize");
    rb_define_method (notmuch_rb_cDirectory, "destroy!", notmuch_rb_directory_destroy, 0); /* in directory.c */
    rb_define_method (notmuch_rb_cDirectory, "mtime", notmuch_rb_directory_get_mtime, 0); /* in directory.c */
    rb_define_method (notmuch_rb_cDirectory, "mtime=", notmuch_rb_directory_set_mtime, 1); /* in directory.c */
    rb_define_method (notmuch_rb_cDirectory, "child_files", notmuch_rb_directory_get_child_files, 0); /* in directory.c */
    rb_define_method (notmuch_rb_cDirectory, "child_directories", notmuch_rb_directory_get_child_directories, 0); /* in directory.c */

    /*
     * Document-class: Notmuch::FileNames
     *
     * Notmuch file names
     */
    notmuch_rb_cFileNames = rb_define_class_under (mod, "FileNames", rb_cData);
    rb_undef_method (notmuch_rb_cFileNames, "initialize");
    rb_define_method (notmuch_rb_cFileNames, "destroy!", notmuch_rb_filenames_destroy, 0); /* in filenames.c */
    rb_define_method (notmuch_rb_cFileNames, "each", notmuch_rb_filenames_each, 0); /* in filenames.c */
    rb_include_module (notmuch_rb_cFileNames, rb_mEnumerable);

    /*
     * Document-class: Notmuch::Query
     *
     * Notmuch query
     */
    notmuch_rb_cQuery = rb_define_class_under (mod, "Query", rb_cData);
    rb_undef_method (notmuch_rb_cQuery, "initialize");
    rb_define_method (notmuch_rb_cQuery, "destroy!", notmuch_rb_query_destroy, 0); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "sort", notmuch_rb_query_get_sort, 0); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "sort=", notmuch_rb_query_set_sort, 1); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "to_s", notmuch_rb_query_get_string, 0); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "add_tag_exclude", notmuch_rb_query_add_tag_exclude, 1); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "omit_excluded=", notmuch_rb_query_set_omit_excluded, 1); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "search_threads", notmuch_rb_query_search_threads, 0); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "search_messages", notmuch_rb_query_search_messages, 0); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "count_messages", notmuch_rb_query_count_messages, 0); /* in query.c */
    rb_define_method (notmuch_rb_cQuery, "count_threads", notmuch_rb_query_count_threads, 0); /* in query.c */

    /*
     * Document-class: Notmuch::Threads
     *
     * Notmuch threads
     */
    notmuch_rb_cThreads = rb_define_class_under (mod, "Threads", rb_cData);
    rb_undef_method (notmuch_rb_cThreads, "initialize");
    rb_define_method (notmuch_rb_cThreads, "destroy!", notmuch_rb_threads_destroy, 0); /* in threads.c */
    rb_define_method (notmuch_rb_cThreads, "each", notmuch_rb_threads_each, 0); /* in threads.c */
    rb_include_module (notmuch_rb_cThreads, rb_mEnumerable);

    /*
     * Document-class: Notmuch::Messages
     *
     * Notmuch messages
     */
    notmuch_rb_cMessages = rb_define_class_under (mod, "Messages", rb_cData);
    rb_undef_method (notmuch_rb_cMessages, "initialize");
    rb_define_method (notmuch_rb_cMessages, "destroy!", notmuch_rb_messages_destroy, 0); /* in messages.c */
    rb_define_method (notmuch_rb_cMessages, "each", notmuch_rb_messages_each, 0); /* in messages.c */
    rb_define_method (notmuch_rb_cMessages, "tags", notmuch_rb_messages_collect_tags, 0); /* in messages.c */
    rb_include_module (notmuch_rb_cMessages, rb_mEnumerable);

    /*
     * Document-class: Notmuch::Thread
     *
     * Notmuch thread
     */
    notmuch_rb_cThread = rb_define_class_under (mod, "Thread", rb_cData);
    rb_undef_method (notmuch_rb_cThread, "initialize");
    rb_define_method (notmuch_rb_cThread, "destroy!", notmuch_rb_thread_destroy, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "thread_id", notmuch_rb_thread_get_thread_id, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "total_messages", notmuch_rb_thread_get_total_messages, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "toplevel_messages", notmuch_rb_thread_get_toplevel_messages, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "messages", notmuch_rb_thread_get_messages, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "matched_messages", notmuch_rb_thread_get_matched_messages, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "authors", notmuch_rb_thread_get_authors, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "subject", notmuch_rb_thread_get_subject, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "oldest_date", notmuch_rb_thread_get_oldest_date, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "newest_date", notmuch_rb_thread_get_newest_date, 0); /* in thread.c */
    rb_define_method (notmuch_rb_cThread, "tags", notmuch_rb_thread_get_tags, 0); /* in thread.c */

    /*
     * Document-class: Notmuch::Message
     *
     * Notmuch message
     */
    notmuch_rb_cMessage = rb_define_class_under (mod, "Message", rb_cData);
    rb_undef_method (notmuch_rb_cMessage, "initialize");
    rb_define_method (notmuch_rb_cMessage, "destroy!", notmuch_rb_message_destroy, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "message_id", notmuch_rb_message_get_message_id, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "thread_id", notmuch_rb_message_get_thread_id, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "replies", notmuch_rb_message_get_replies, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "filename", notmuch_rb_message_get_filename, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "filenames", notmuch_rb_message_get_filenames, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "get_flag", notmuch_rb_message_get_flag, 1); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "set_flag", notmuch_rb_message_set_flag, 2); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "date", notmuch_rb_message_get_date, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "header", notmuch_rb_message_get_header, 1); /* in message.c */
    rb_define_alias (notmuch_rb_cMessage, "[]", "header");
    rb_define_method (notmuch_rb_cMessage, "tags", notmuch_rb_message_get_tags, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "add_tag", notmuch_rb_message_add_tag, 1); /* in message.c */
    rb_define_alias (notmuch_rb_cMessage, "<<", "add_tag");
    rb_define_method (notmuch_rb_cMessage, "remove_tag", notmuch_rb_message_remove_tag, 1); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "remove_all_tags", notmuch_rb_message_remove_all_tags, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "maildir_flags_to_tags", notmuch_rb_message_maildir_flags_to_tags, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "tags_to_maildir_flags", notmuch_rb_message_tags_to_maildir_flags, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "freeze", notmuch_rb_message_freeze, 0); /* in message.c */
    rb_define_method (notmuch_rb_cMessage, "thaw", notmuch_rb_message_thaw, 0); /* in message.c */

    /*
     * Document-class: Notmuch::Tags
     *
     * Notmuch tags
     */
    notmuch_rb_cTags = rb_define_class_under (mod, "Tags", rb_cData);
    rb_undef_method (notmuch_rb_cTags, "initialize");
    rb_define_method (notmuch_rb_cTags, "destroy!", notmuch_rb_tags_destroy, 0); /* in tags.c */
    rb_define_method (notmuch_rb_cTags, "each", notmuch_rb_tags_each, 0); /* in tags.c */
    rb_include_module (notmuch_rb_cTags, rb_mEnumerable);
}
