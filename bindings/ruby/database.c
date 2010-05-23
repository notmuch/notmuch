/* The Ruby interface to the notmuch mail library
 *
 * Copyright © 2010 Ali Polatel
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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Ali Polatel <alip@exherbo.org>
 */

#include "defs.h"

/*
 * call-seq: Notmuch::Database.new(path, [{:create => false, :mode => notmuch::MODE_READ_ONLY}]) => DB
 *
 * Create or open a notmuch database using the given path.
 * If :create is +true+, create the database instead of opening.
 * The argument :mode specifies the open mode of the database.
 */
VALUE
notmuch_rb_database_new(int argc, VALUE *argv, VALUE klass)
{
    const char *path;
    int create, mode;
    notmuch_rb_database_t *db;
    VALUE modev, dbv;

#if !defined(RSTRING_PTR)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_PTR) */

    /* Check arguments */
    if (argc < 1 || argc > 2)
        rb_raise(rb_eTypeError, "Wrong number of arguments");

    SafeStringValue(argv[0]);
    path = RSTRING_PTR(argv[0]);

    if (argc == 2) {
        Check_Type(argv[1], T_HASH);
        create = RTEST(rb_hash_aref(argv[1], ID2SYM(ID_db_create)));
        modev = rb_hash_aref(argv[1], ID2SYM(ID_db_mode));
        if (NIL_P(modev))
            mode = NOTMUCH_DATABASE_MODE_READ_ONLY;
        else if (!FIXNUM_P(modev))
            rb_raise(rb_eTypeError, ":mode isn't a Fixnum");
        else {
            mode = FIX2INT(modev);
            switch (mode) {
            case NOTMUCH_DATABASE_MODE_READ_ONLY:
            case NOTMUCH_DATABASE_MODE_READ_WRITE:
                break;
            default:
                rb_raise(rb_eTypeError, "Invalid mode");
            }
        }
    }
    else {
        create = 0;
        mode = NOTMUCH_DATABASE_MODE_READ_ONLY;
    }

    dbv = Data_Make_Struct(klass, notmuch_rb_database_t, NULL, notmuch_rb_database_free, db);
    db->nm_db = create ? notmuch_database_create(path) : notmuch_database_open(path, mode);
    if (!db->nm_db)
        rb_raise(notmuch_rb_eDatabaseError, "failed to open database");

    return dbv;
}

/*
 * call-seq: DB.close => nil
 *
 * Close the notmuch database.
 */
VALUE
notmuch_rb_database_close(VALUE self)
{
    notmuch_rb_database_t *db;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (db->nm_db) {
        notmuch_database_close(db->nm_db);
        db->nm_db = NULL;
    }

    return Qnil;
}

/*
 * call-seq: DB.path => String
 *
 * Return the path of the database
 */
VALUE
notmuch_rb_database_path(VALUE self)
{
    notmuch_rb_database_t *db;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

    return rb_str_new2(notmuch_database_get_path(db->nm_db));
}

/*
 * call-seq: DB.version => Fixnum
 *
 * Return the version of the database
 */
VALUE
notmuch_rb_database_version(VALUE self)
{
    notmuch_rb_database_t *db;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

    return INT2FIX(notmuch_database_get_version(db->nm_db));
}

/*
 * call-seq: DB.needs_upgrade? => true or false
 *
 * Return the +true+ if the database needs upgrading, +false+ otherwise
 */
VALUE
notmuch_rb_database_needs_upgrade(VALUE self)
{
    notmuch_rb_database_t *db;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

    return notmuch_database_needs_upgrade(db->nm_db) ? Qtrue : Qfalse;
}

static void
notmuch_rb_upgrade_notify(void *closure, double progress)
{
    VALUE *block = (VALUE *)closure;
    rb_funcall(*block, ID_call, 1, rb_float_new(progress));
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
notmuch_rb_database_upgrade(VALUE self)
{
    notmuch_status_t ret;
    void (*pnotify) (void *closure, double progress);
    notmuch_rb_database_t *db;
    VALUE block;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

    if (rb_block_given_p()) {
        pnotify = notmuch_rb_upgrade_notify;
        block = rb_block_proc();
    }
    else
        pnotify = NULL;

    ret = notmuch_database_upgrade(db->nm_db, pnotify, pnotify ? &block : NULL);
    notmuch_rb_status_raise(ret);
    return Qtrue;
}

/*
 * call-seq: DB.get_directory(path) => DIR
 *
 * Retrieve a directory object from the database for 'path'
 */
VALUE
notmuch_rb_database_get_directory(VALUE self, VALUE pathv)
{
    const char *path;
    notmuch_rb_directory_t *dir;
    notmuch_rb_database_t *db;
    VALUE dirv;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

#if !defined(RSTRING_PTR)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_PTR) */

    SafeStringValue(pathv);
    path = RSTRING_PTR(pathv);

    dirv = Data_Make_Struct(notmuch_rb_cDirectory, notmuch_rb_directory_t,
            notmuch_rb_directory_mark, notmuch_rb_directory_free, dir);
    dir->nm_dir = notmuch_database_get_directory(db->nm_db, path);
    dir->db = self;
    if (!dir->nm_dir)
        rb_raise(notmuch_rb_eXapianError, "Xapian exception");

    return dirv;
}

/*
 * call-seq: DB.add_message(path) => MESSAGE, isdup
 *
 * Add a message to the database and return it
 * +isdup+ is a boolean that specifies whether the added message was a
 * duplicate.
 */
VALUE
notmuch_rb_database_add_message(VALUE self, VALUE pathv)
{
    const char *path;
    notmuch_status_t ret;
    notmuch_rb_message_t *message;
    notmuch_rb_database_t *db;
    VALUE messagev;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

#if !defined(RSTRING_PTR)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_PTR) */

    SafeStringValue(pathv);
    path = RSTRING_PTR(pathv);

    messagev = Data_Make_Struct(notmuch_rb_cMessage, notmuch_rb_message_t,
            notmuch_rb_message_mark, notmuch_rb_message_free, message);
    ret = notmuch_database_add_message(db->nm_db, path, &message->nm_message);
    message->parent = self;
    notmuch_rb_status_raise(ret);
    return rb_assoc_new(messagev, (ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) ? Qtrue : Qfalse);
}

/*
 * call-seq: DB.remove_message(path) => isdup
 *
 * Remove a message from the database.
 * +isdup+ is a boolean that specifies whether the removed message was a
 * duplicate.
 */
VALUE
notmuch_rb_database_remove_message(VALUE self, VALUE pathv)
{
    const char *path;
    notmuch_status_t ret;
    notmuch_rb_database_t *db;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

#if !defined(RSTRING_PTR)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_PTR) */

    SafeStringValue(pathv);
    path = RSTRING_PTR(pathv);

    ret = notmuch_database_remove_message(db->nm_db, path);
    notmuch_rb_status_raise(ret);
    return (ret == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) ? Qtrue : Qfalse;
}

/*
 * call-seq: DB.query(query) => QUERY
 *
 * Retrieve a query object for the query string 'query'
 */
VALUE
notmuch_rb_database_query_create(VALUE self, VALUE qstrv)
{
    const char *qstr;
    notmuch_rb_query_t *query;
    notmuch_rb_database_t *db;
    VALUE queryv;

    Data_Get_Struct(self, notmuch_rb_database_t, db);
    if (!db->nm_db)
        rb_raise(rb_eRuntimeError, "Database closed");

#if !defined(RSTRING_PTR)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_PTR) */

    SafeStringValue(qstrv);
    qstr = RSTRING_PTR(qstrv);

    queryv = Data_Make_Struct(notmuch_rb_cQuery, notmuch_rb_query_t,
            notmuch_rb_query_mark, notmuch_rb_query_free, query);
    query->nm_query = notmuch_query_create(db->nm_db, qstr);
    query->db = self;
    if (!query->nm_query)
        rb_raise(notmuch_rb_eMemoryError, "out of memory");

    return queryv;
}
