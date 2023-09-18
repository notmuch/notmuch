/* The Ruby interface to the notmuch mail library
 *
 * Copyright © 2010, 2011 Ali Polatel
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

VALUE
notmuch_rb_database_alloc (VALUE klass)
{
    return Data_Wrap_Notmuch_Object (klass, &notmuch_rb_database_type, NULL);
}

/*
 * call-seq: DB.destroy => nil
 *
 * Destroys the database, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_database_destroy (VALUE self)
{
    notmuch_rb_object_destroy (self, &notmuch_rb_database_type);

    return Qnil;
}

/*
 * call-seq: Notmuch::Database.new(path [, {:create => false, :mode => Notmuch::MODE_READ_ONLY}]) => DB
 *
 * Create or open a notmuch database using the given path.
 *
 * If :create is +true+, create the database instead of opening.
 *
 * The argument :mode specifies the open mode of the database.
 */
VALUE
notmuch_rb_database_initialize (int argc, VALUE *argv, VALUE self)
{
    const char *path;
    int create, mode;
    VALUE pathv, hashv;
    VALUE modev;
    notmuch_database_t *database;
    notmuch_status_t ret;

    path = NULL;
    create = 0;
    mode = NOTMUCH_DATABASE_MODE_READ_ONLY;

    /* Check arguments */
    rb_scan_args (argc, argv, "02", &pathv, &hashv);

    if (!NIL_P (pathv)) {
	SafeStringValue (pathv);
	path = RSTRING_PTR (pathv);
    }

    if (!NIL_P (hashv)) {
	VALUE rmode, rcreate;
	VALUE kwargs[2];
	static ID keyword_ids[2];

	if (!keyword_ids[0]) {
	    keyword_ids[0] = rb_intern_const ("mode");
	    keyword_ids[1] = rb_intern_const ("create");
	}

	rb_get_kwargs (hashv, keyword_ids, 0, 2, kwargs);

	rmode = kwargs[0];
	rcreate = kwargs[1];

	if (rmode != Qundef) {
	    if (!FIXNUM_P (rmode))
		rb_raise (rb_eTypeError, ":mode isn't a Fixnum");
	    else {
		mode = FIX2INT (rmode);
		switch (mode) {
		case NOTMUCH_DATABASE_MODE_READ_ONLY:
		case NOTMUCH_DATABASE_MODE_READ_WRITE:
		    break;
		default:
		    rb_raise ( rb_eTypeError, "Invalid mode");
		}
	    }
	}
	if (rcreate != Qundef)
	    create = RTEST (rcreate);
    }

    rb_check_typeddata (self, &notmuch_rb_database_type);
    if (create)
	ret = notmuch_database_create (path, &database);
    else
	ret = notmuch_database_open_with_config (path, mode, NULL, NULL, &database, NULL);
    notmuch_rb_status_raise (ret);

    DATA_PTR (self) = notmuch_rb_object_create (database, "notmuch_rb_database");

    return self;
}

/*
 * call-seq: Notmuch::Database.open(path [, ahash]) {|db| ...}
 *
 * Identical to new, except that when it is called with a block, it yields with
 * the new instance and closes it, and returns the result which is returned from
 * the block.
 */
VALUE
notmuch_rb_database_open (int argc, VALUE *argv, VALUE klass)
{
    VALUE obj;

    obj = rb_class_new_instance (argc, argv, klass);
    if (!rb_block_given_p ())
	return obj;

    return rb_ensure (rb_yield, obj, notmuch_rb_database_close, obj);
}

/*
 * call-seq: DB.close => nil
 *
 * Close the notmuch database.
 */
VALUE
notmuch_rb_database_close (VALUE self)
{
    notmuch_database_t *db;
    notmuch_status_t ret;

    Data_Get_Notmuch_Database (self, db);

    ret = notmuch_database_close (db);
    notmuch_rb_status_raise (ret);

    return Qnil;
}

/*
 * call-seq: DB.path => String
 *
 * Return the path of the database
 */
VALUE
notmuch_rb_database_path (VALUE self)
{
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    return rb_str_new2 (notmuch_database_get_path (db));
}

/*
 * call-seq: DB.version => Fixnum
 *
 * Return the version of the database
 */
VALUE
notmuch_rb_database_version (VALUE self)
{
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    return INT2FIX (notmuch_database_get_version (db));
}

/*
 * call-seq: DB.needs_upgrade? => true or false
 *
 * Return the +true+ if the database needs upgrading, +false+ otherwise
 */
VALUE
notmuch_rb_database_needs_upgrade (VALUE self)
{
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    return notmuch_database_needs_upgrade (db) ? Qtrue : Qfalse;
}

static void
notmuch_rb_upgrade_notify (void *closure, double progress)
{
    VALUE *block = (VALUE *) closure;
    rb_funcall (*block, ID_call, 1, rb_float_new (progress));
}

/*
 * call-seq: DB.upgrade! [{|progress| block }] => nil
 *
 * Upgrade the database.
 *
 * If a block is given the block is called with a progress indicator as a
 * floating point value in the range of [0.0..1.0].
 */
VALUE
notmuch_rb_database_upgrade (VALUE self)
{
    notmuch_status_t ret;
    void (*pnotify) (void *closure, double progress);
    notmuch_database_t *db;
    VALUE block;

    Data_Get_Notmuch_Database (self, db);

    if (rb_block_given_p ()) {
	pnotify = notmuch_rb_upgrade_notify;
	block = rb_block_proc ();
    }
    else
	pnotify = NULL;

    ret = notmuch_database_upgrade (db, pnotify, pnotify ? &block : NULL);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: DB.begin_atomic => nil
 *
 * Begin an atomic database operation.
 */
VALUE
notmuch_rb_database_begin_atomic (VALUE self)
{
    notmuch_status_t ret;
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    ret = notmuch_database_begin_atomic (db);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: DB.end_atomic => nil
 *
 * Indicate the end of an atomic database operation.
 */
VALUE
notmuch_rb_database_end_atomic (VALUE self)
{
    notmuch_status_t ret;
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    ret = notmuch_database_end_atomic (db);
    notmuch_rb_status_raise (ret);

    return Qtrue;
}

/*
 * call-seq: DB.get_directory(path) => DIR
 *
 * Retrieve a directory object from the database for 'path'
 */
VALUE
notmuch_rb_database_get_directory (VALUE self, VALUE pathv)
{
    const char *path;
    notmuch_status_t ret;
    notmuch_directory_t *dir;
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    SafeStringValue (pathv);
    path = RSTRING_PTR (pathv);

    ret = notmuch_database_get_directory (db, path, &dir);
    notmuch_rb_status_raise (ret);
    if (dir)
	return Data_Wrap_Notmuch_Object (notmuch_rb_cDirectory, &notmuch_rb_directory_type, dir);
    return Qnil;
}

/*
 * call-seq: DB.add_message(path) => MESSAGE, isdup
 *
 * Add a message to the database and return it.
 *
 * +isdup+ is a boolean that specifies whether the added message was a
 * duplicate.
 */
VALUE
notmuch_rb_database_add_message (VALUE self, VALUE pathv)
{
    const char *path;
    notmuch_status_t ret;
    notmuch_message_t *message;
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    SafeStringValue (pathv);
    path = RSTRING_PTR (pathv);

    ret = notmuch_database_index_file (db, path, NULL, &message);
    notmuch_rb_status_raise (ret);
    return rb_assoc_new (Data_Wrap_Notmuch_Object (notmuch_rb_cMessage, &notmuch_rb_message_type, message),
        (ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) ? Qtrue : Qfalse);
}

/*
 * call-seq: DB.remove_message (path) => isdup
 *
 * Remove a message from the database.
 *
 * +isdup+ is a boolean that specifies whether the removed message was a
 * duplicate.
 */
VALUE
notmuch_rb_database_remove_message (VALUE self, VALUE pathv)
{
    const char *path;
    notmuch_status_t ret;
    notmuch_database_t *db;

    Data_Get_Notmuch_Database (self, db);

    SafeStringValue (pathv);
    path = RSTRING_PTR (pathv);

    ret = notmuch_database_remove_message (db, path);
    notmuch_rb_status_raise (ret);
    return (ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) ? Qtrue : Qfalse;
}

/*
 * call-seq: DB.find_message(id) => MESSAGE or nil
 *
 * Find a message by message id.
 */
VALUE
notmuch_rb_database_find_message (VALUE self, VALUE idv)
{
    const char *id;
    notmuch_status_t ret;
    notmuch_database_t *db;
    notmuch_message_t *message;

    Data_Get_Notmuch_Database (self, db);

    SafeStringValue (idv);
    id = RSTRING_PTR (idv);

    ret = notmuch_database_find_message (db, id, &message);
    notmuch_rb_status_raise (ret);

    if (message)
	return Data_Wrap_Notmuch_Object (notmuch_rb_cMessage, &notmuch_rb_message_type, message);
    return Qnil;
}

/*
 * call-seq: DB.find_message_by_filename(path) => MESSAGE or nil
 *
 * Find a message by filename.
 */
VALUE
notmuch_rb_database_find_message_by_filename (VALUE self, VALUE pathv)
{
    const char *path;
    notmuch_status_t ret;
    notmuch_database_t *db;
    notmuch_message_t *message;

    Data_Get_Notmuch_Database (self, db);

    SafeStringValue (pathv);
    path = RSTRING_PTR (pathv);

    ret = notmuch_database_find_message_by_filename (db, path, &message);
    notmuch_rb_status_raise (ret);

    if (message)
	return Data_Wrap_Notmuch_Object (notmuch_rb_cMessage, &notmuch_rb_message_type, message);
    return Qnil;
}

/*
 * call-seq: DB.get_all_tags() => TAGS
 *
 * Returns a list of all tags found in the database.
 */
VALUE
notmuch_rb_database_get_all_tags (VALUE self)
{
    notmuch_database_t *db;
    notmuch_tags_t *tags;

    Data_Get_Notmuch_Database (self, db);

    tags = notmuch_database_get_all_tags (db);
    if (!tags) {
	const char *msg = notmuch_database_status_string (db);
	if (!msg)
	    msg = "Unknown notmuch error";

	rb_raise (notmuch_rb_eBaseError, "%s", msg);
    }
    return notmuch_rb_tags_get (tags);
}

/*
 * call-seq:
 *   DB.query(query) => QUERY
 *   DB.query(query, sort:, excluded_tags:, omit_excluded:) => QUERY
 *
 * Retrieve a query object for the query string 'query'. When using keyword
 * arguments they are passwed to the query object.
 */
VALUE
notmuch_rb_database_query_create (int argc, VALUE *argv, VALUE self)
{
    VALUE qstrv;
    VALUE opts;
    const char *qstr;
    notmuch_query_t *query;
    notmuch_database_t *db;

    rb_scan_args (argc, argv, "1:", &qstrv, &opts);

    Data_Get_Notmuch_Database (self, db);

    SafeStringValue (qstrv);
    qstr = RSTRING_PTR (qstrv);

    query = notmuch_query_create (db, qstr);
    if (!query)
        rb_raise (notmuch_rb_eMemoryError, "Out of memory");

    if (!NIL_P (opts)) {
	VALUE sort, exclude_tags, omit_excluded;
	VALUE kwargs[3];
	static ID keyword_ids[3];

	if (!keyword_ids[0]) {
	    keyword_ids[0] = rb_intern_const ("sort");
	    keyword_ids[1] = rb_intern_const ("exclude_tags");
	    keyword_ids[2] = rb_intern_const ("omit_excluded");
	}

	rb_get_kwargs (opts, keyword_ids, 0, 3, kwargs);

	sort = kwargs[0];
	exclude_tags = kwargs[1];
	omit_excluded = kwargs[2];

	if (sort != Qundef)
	    notmuch_query_set_sort (query, FIX2UINT (sort));

	if (exclude_tags != Qundef) {
	    for (int i = 0; i < RARRAY_LEN (exclude_tags); i++) {
		VALUE e = RARRAY_AREF (exclude_tags, i);
		notmuch_query_add_tag_exclude (query, RSTRING_PTR (e));
	    }
	}

	if (omit_excluded != Qundef) {
	    notmuch_exclude_t omit;
	    omit = FIXNUM_P (omit_excluded) ? FIX2UINT (omit_excluded) : RTEST(omit_excluded);
	    notmuch_query_set_omit_excluded (query, omit);
	}
    }

    return Data_Wrap_Notmuch_Object (notmuch_rb_cQuery, &notmuch_rb_query_type, query);
}
