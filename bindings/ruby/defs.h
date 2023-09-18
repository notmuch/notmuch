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

#ifndef DEFS_H
#define DEFS_H

#include <notmuch.h>
#include <ruby.h>
#include <talloc.h>

extern VALUE notmuch_rb_cDatabase;
extern VALUE notmuch_rb_cDirectory;
extern VALUE notmuch_rb_cFileNames;
extern VALUE notmuch_rb_cQuery;
extern VALUE notmuch_rb_cThreads;
extern VALUE notmuch_rb_cThread;
extern VALUE notmuch_rb_cMessages;
extern VALUE notmuch_rb_cMessage;

extern VALUE notmuch_rb_eBaseError;
extern VALUE notmuch_rb_eDatabaseError;
extern VALUE notmuch_rb_eMemoryError;
extern VALUE notmuch_rb_eReadOnlyError;
extern VALUE notmuch_rb_eXapianError;
extern VALUE notmuch_rb_eFileError;
extern VALUE notmuch_rb_eFileNotEmailError;
extern VALUE notmuch_rb_eNullPointerError;
extern VALUE notmuch_rb_eTagTooLongError;
extern VALUE notmuch_rb_eUnbalancedFreezeThawError;
extern VALUE notmuch_rb_eUnbalancedAtomicError;

extern ID ID_call;

/* RSTRING_PTR() is new in ruby-1.9 */
#if !defined(RSTRING_PTR)
# define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined (RSTRING_PTR) */

extern const rb_data_type_t notmuch_rb_object_type;
extern const rb_data_type_t notmuch_rb_database_type;
extern const rb_data_type_t notmuch_rb_directory_type;
extern const rb_data_type_t notmuch_rb_query_type;
extern const rb_data_type_t notmuch_rb_threads_type;
extern const rb_data_type_t notmuch_rb_thread_type;
extern const rb_data_type_t notmuch_rb_messages_type;
extern const rb_data_type_t notmuch_rb_message_type;
extern const rb_data_type_t notmuch_rb_tags_type;

#define Data_Get_Notmuch_Rb_Object(obj, type, ptr)		    		    \
    do {									    \
	(ptr) = rb_check_typeddata ((obj), (type));				    \
	if (RB_UNLIKELY (!(ptr))) {						    \
	    VALUE cname = rb_class_name (CLASS_OF ((obj)));			    \
	    rb_raise (rb_eRuntimeError, "%"PRIsVALUE" object destroyed", cname);    \
	}									    \
    } while (0)

#define Data_Get_Notmuch_Object(obj, type, ptr)			\
    do {							\
	notmuch_rb_object_t *rb_wrapper;			\
	Data_Get_Notmuch_Rb_Object ((obj), (type), rb_wrapper);	\
	(ptr) = rb_wrapper->nm_object;				\
    } while (0)

#define Data_Wrap_Notmuch_Object(klass, type, ptr) \
    TypedData_Wrap_Struct ((klass), (type), notmuch_rb_object_create ((ptr), "notmuch_rb_object: " __location__))

#define Data_Get_Notmuch_Database(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_database_type, (ptr))

#define Data_Get_Notmuch_Directory(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_directory_type, (ptr))

#define Data_Get_Notmuch_Query(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_query_type, (ptr))

#define Data_Get_Notmuch_Threads(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_threads_type, (ptr))

#define Data_Get_Notmuch_Messages(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_messages_type, (ptr))

#define Data_Get_Notmuch_Thread(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_thread_type, (ptr))

#define Data_Get_Notmuch_Message(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_message_type, (ptr))

#define Data_Get_Notmuch_Tags(obj, ptr) \
    Data_Get_Notmuch_Object ((obj), &notmuch_rb_tags_type, (ptr))

typedef struct {
    void *nm_object;
} notmuch_rb_object_t;

static inline void *
notmuch_rb_object_create (void *nm_object, const char *name)
{
    notmuch_rb_object_t *rb_wrapper = talloc_named_const (NULL, sizeof (*rb_wrapper), name);

    if (RB_UNLIKELY (!rb_wrapper))
	return NULL;

    rb_wrapper->nm_object = nm_object;
    talloc_steal (rb_wrapper, nm_object);
    return rb_wrapper;
}

static inline void
notmuch_rb_object_free (void *rb_wrapper)
{
    talloc_free (rb_wrapper);
}

static inline void
notmuch_rb_object_destroy (VALUE rb_object, const rb_data_type_t *type)
{
    notmuch_rb_object_t *rb_wrapper;

    Data_Get_Notmuch_Rb_Object (rb_object, type, rb_wrapper);

    /* Call the corresponding notmuch_*_destroy function */
    ((void (*)(void *)) type->data) (rb_wrapper->nm_object);
    notmuch_rb_object_free (rb_wrapper);
    DATA_PTR (rb_object) = NULL;
}

/* status.c */
void
notmuch_rb_status_raise (notmuch_status_t status);

/* database.c */
VALUE
notmuch_rb_database_alloc (VALUE klass);

VALUE
notmuch_rb_database_destroy (VALUE self);

VALUE
notmuch_rb_database_initialize (int argc, VALUE *argv, VALUE klass);

VALUE
notmuch_rb_database_open (int argc, VALUE *argv, VALUE klass);

VALUE
notmuch_rb_database_close (VALUE self);

VALUE
notmuch_rb_database_path (VALUE self);

VALUE
notmuch_rb_database_version (VALUE self);

VALUE
notmuch_rb_database_needs_upgrade (VALUE self);

VALUE
notmuch_rb_database_upgrade (VALUE self);

VALUE
notmuch_rb_database_begin_atomic (VALUE self);

VALUE
notmuch_rb_database_end_atomic (VALUE self);

VALUE
notmuch_rb_database_get_directory (VALUE self, VALUE pathv);

VALUE
notmuch_rb_database_add_message (VALUE self, VALUE pathv);

VALUE
notmuch_rb_database_remove_message (VALUE self, VALUE pathv);

VALUE
notmuch_rb_database_find_message (VALUE self, VALUE idv);

VALUE
notmuch_rb_database_find_message_by_filename (VALUE self, VALUE pathv);

VALUE
notmuch_rb_database_get_all_tags (VALUE self);

VALUE
notmuch_rb_database_query_create (int argc, VALUE *argv, VALUE self);

/* directory.c */
VALUE
notmuch_rb_directory_destroy (VALUE self);

VALUE
notmuch_rb_directory_get_mtime (VALUE self);

VALUE
notmuch_rb_directory_set_mtime (VALUE self, VALUE mtimev);

VALUE
notmuch_rb_directory_get_child_files (VALUE self);

VALUE
notmuch_rb_directory_get_child_directories (VALUE self);

/* filenames.c */
VALUE
notmuch_rb_filenames_get (notmuch_filenames_t *fnames);

/* query.c */
VALUE
notmuch_rb_query_destroy (VALUE self);

VALUE
notmuch_rb_query_get_sort (VALUE self);

VALUE
notmuch_rb_query_set_sort (VALUE self, VALUE sortv);

VALUE
notmuch_rb_query_get_string (VALUE self);

VALUE
notmuch_rb_query_add_tag_exclude (VALUE self, VALUE tagv);

VALUE
notmuch_rb_query_set_omit_excluded (VALUE self, VALUE omitv);

VALUE
notmuch_rb_query_search_threads (VALUE self);

VALUE
notmuch_rb_query_search_messages (VALUE self);

VALUE
notmuch_rb_query_count_messages (VALUE self);

VALUE
notmuch_rb_query_count_threads (VALUE self);

/* threads.c */
VALUE
notmuch_rb_threads_destroy (VALUE self);

VALUE
notmuch_rb_threads_each (VALUE self);

/* messages.c */
VALUE
notmuch_rb_messages_destroy (VALUE self);

VALUE
notmuch_rb_messages_each (VALUE self);

VALUE
notmuch_rb_messages_collect_tags (VALUE self);

/* thread.c */
VALUE
notmuch_rb_thread_destroy (VALUE self);

VALUE
notmuch_rb_thread_get_thread_id (VALUE self);

VALUE
notmuch_rb_thread_get_total_messages (VALUE self);

VALUE
notmuch_rb_thread_get_toplevel_messages (VALUE self);

VALUE
notmuch_rb_thread_get_messages (VALUE self);

VALUE
notmuch_rb_thread_get_matched_messages (VALUE self);

VALUE
notmuch_rb_thread_get_authors (VALUE self);

VALUE
notmuch_rb_thread_get_subject (VALUE self);

VALUE
notmuch_rb_thread_get_oldest_date (VALUE self);

VALUE
notmuch_rb_thread_get_newest_date (VALUE self);

VALUE
notmuch_rb_thread_get_tags (VALUE self);

/* message.c */
VALUE
notmuch_rb_message_destroy (VALUE self);

VALUE
notmuch_rb_message_get_message_id (VALUE self);

VALUE
notmuch_rb_message_get_thread_id (VALUE self);

VALUE
notmuch_rb_message_get_replies (VALUE self);

VALUE
notmuch_rb_message_get_filename (VALUE self);

VALUE
notmuch_rb_message_get_filenames (VALUE self);

VALUE
notmuch_rb_message_get_flag (VALUE self, VALUE flagv);

VALUE
notmuch_rb_message_set_flag (VALUE self, VALUE flagv, VALUE valuev);

VALUE
notmuch_rb_message_get_date (VALUE self);

VALUE
notmuch_rb_message_get_header (VALUE self, VALUE headerv);

VALUE
notmuch_rb_message_get_tags (VALUE self);

VALUE
notmuch_rb_message_add_tag (VALUE self, VALUE tagv);

VALUE
notmuch_rb_message_remove_tag (VALUE self, VALUE tagv);

VALUE
notmuch_rb_message_remove_all_tags (VALUE self);

VALUE
notmuch_rb_message_maildir_flags_to_tags (VALUE self);

VALUE
notmuch_rb_message_tags_to_maildir_flags (VALUE self);

VALUE
notmuch_rb_message_freeze (VALUE self);

VALUE
notmuch_rb_message_thaw (VALUE self);

/* tags.c */
VALUE
notmuch_rb_tags_get (notmuch_tags_t *tags);

/* init.c */
void
Init_notmuch (void);

#endif
