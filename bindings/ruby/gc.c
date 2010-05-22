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

#include <stdlib.h>

static notmuch_rb_database_t *
_notmuch_rb_messages_db(notmuch_rb_messages_t *messages);

static notmuch_rb_database_t *
_notmuch_rb_message_db(notmuch_rb_message_t *message)
{
    notmuch_rb_messages_t *messages;
    notmuch_rb_database_t *db;

    if (rb_obj_is_instance_of(message->parent, notmuch_rb_cDatabase)) {
        Data_Get_Struct(message->parent, notmuch_rb_database_t, db);
    }
    else if (rb_obj_is_instance_of(message->parent, notmuch_rb_cMessages)) {
        Data_Get_Struct(message->parent, notmuch_rb_messages_t, messages);
        db = _notmuch_rb_messages_db(messages);
    }
    else
        rb_bug("message's parent is neither database nor messages");

    return db;
}

static notmuch_rb_database_t *
_notmuch_rb_messages_db(notmuch_rb_messages_t *messages)
{
    notmuch_rb_query_t *query;
    notmuch_rb_thread_t *thread;
    notmuch_rb_message_t *message;
    notmuch_rb_threads_t *threads;
    notmuch_rb_database_t *db;

    if (rb_obj_is_instance_of(messages->parent, notmuch_rb_cQuery)) {
        Data_Get_Struct(messages->parent, notmuch_rb_query_t, query);
        Data_Get_Struct(query->db, notmuch_rb_database_t, db);
    }
    else if (rb_obj_is_instance_of(messages->parent, notmuch_rb_cThread)) {
        Data_Get_Struct(messages->parent, notmuch_rb_thread_t, thread);
        Data_Get_Struct(thread->threads, notmuch_rb_threads_t, threads);
        Data_Get_Struct(threads->query, notmuch_rb_query_t, query);
        Data_Get_Struct(query->db, notmuch_rb_database_t, db);
    }
    else if (rb_obj_is_instance_of(messages->parent, notmuch_rb_cMessage)) {
        Data_Get_Struct(messages->parent, notmuch_rb_message_t, message);
        db = _notmuch_rb_message_db(message);
    }
    else
        rb_bug("messages' parent is neither query nor thread nor message");

    return db;
}

static notmuch_rb_database_t *
_notmuch_rb_thread_db(notmuch_rb_thread_t *thread)
{
    notmuch_rb_threads_t *threads;
    notmuch_rb_query_t *query;
    notmuch_rb_database_t *db;

    Data_Get_Struct(thread->threads, notmuch_rb_threads_t, threads);
    Data_Get_Struct(threads->query, notmuch_rb_query_t, query);
    Data_Get_Struct(query->db, notmuch_rb_database_t, db);

    return db;
}

void
notmuch_rb_database_free(notmuch_rb_database_t *db)
{
    if (db->nm_db)
        notmuch_database_close(db->nm_db);

    free(db);
}

void
notmuch_rb_directory_mark(notmuch_rb_directory_t *dir)
{
    rb_gc_mark(dir->db);
}

void
notmuch_rb_directory_free(notmuch_rb_directory_t *dir)
{
    notmuch_rb_database_t *db;

    Data_Get_Struct(dir->db, notmuch_rb_database_t, db);

    if (db->nm_db && dir->nm_dir)
        notmuch_directory_destroy(dir->nm_dir);

    free(dir);
}

void
notmuch_rb_filenames_mark(notmuch_rb_filenames_t *flist)
{
    rb_gc_mark(flist->dir);
}

void
notmuch_rb_filenames_free(notmuch_rb_filenames_t *flist)
{
    notmuch_rb_directory_t *dir;
    notmuch_rb_database_t *db;

    Data_Get_Struct(flist->dir, notmuch_rb_directory_t, dir);
    Data_Get_Struct(dir->db, notmuch_rb_database_t, db);

    if (db->nm_db && flist->nm_flist)
        notmuch_filenames_destroy(flist->nm_flist);

    free(flist);
}

void
notmuch_rb_query_mark(notmuch_rb_query_t *query)
{
    rb_gc_mark(query->db);
}

void
notmuch_rb_query_free(notmuch_rb_query_t *query)
{
    notmuch_rb_database_t *db;

    Data_Get_Struct(query->db, notmuch_rb_database_t, db);

    if (db->nm_db && query->nm_query)
        notmuch_query_destroy(query->nm_query);

    free(query);
}

void
notmuch_rb_threads_mark(notmuch_rb_threads_t *threads)
{
    rb_gc_mark(threads->query);
}

void
notmuch_rb_threads_free(notmuch_rb_threads_t *threads)
{
    notmuch_rb_query_t *query;
    notmuch_rb_database_t *db;

    Data_Get_Struct(threads->query, notmuch_rb_query_t, query);
    Data_Get_Struct(query->db, notmuch_rb_database_t, db);

    if (db->nm_db && threads->nm_threads)
        notmuch_threads_destroy(threads->nm_threads);

    free(threads);
}

void
notmuch_rb_messages_mark(notmuch_rb_messages_t *messages)
{
    rb_gc_mark(messages->parent);
}

void
notmuch_rb_messages_free(notmuch_rb_messages_t *messages)
{
    notmuch_rb_database_t *db;

    db = _notmuch_rb_messages_db(messages);

    if (db->nm_db && messages->nm_messages)
        notmuch_messages_destroy(messages->nm_messages);

    free(messages);
}

void
notmuch_rb_thread_mark(notmuch_rb_thread_t *thread)
{
    rb_gc_mark(thread->threads);
}

void
notmuch_rb_thread_free(notmuch_rb_thread_t *thread)
{
    notmuch_rb_database_t *db;

    db = _notmuch_rb_thread_db(thread);

    if (db->nm_db && thread->nm_thread)
        notmuch_thread_destroy(thread->nm_thread);

    free(thread);
}

void
notmuch_rb_message_mark(notmuch_rb_message_t *message)
{
    rb_gc_mark(message->parent);
}

void
notmuch_rb_message_free(notmuch_rb_message_t *message)
{
    notmuch_rb_database_t *db;

    db = _notmuch_rb_message_db(message);
    if (db->nm_db && message->nm_message)
        notmuch_message_destroy(message->nm_message);

    free(message);
}

void
notmuch_rb_tags_mark(notmuch_rb_tags_t *tags)
{
    rb_gc_mark(tags->parent);
}

void
notmuch_rb_tags_free(notmuch_rb_tags_t *tags)
{
    notmuch_rb_message_t *message;
    notmuch_rb_messages_t *messages;
    notmuch_rb_thread_t *thread;
    notmuch_rb_database_t *db;

    if (rb_obj_is_instance_of(tags->parent, notmuch_rb_cThread)) {
        Data_Get_Struct(tags->parent, notmuch_rb_thread_t, thread);
        db = _notmuch_rb_thread_db(thread);
    }
    else if (rb_obj_is_instance_of(tags->parent, notmuch_rb_cMessage)) {
        Data_Get_Struct(tags->parent, notmuch_rb_message_t, message);
        db = _notmuch_rb_message_db(message);
    }
    else if (rb_obj_is_instance_of(tags->parent, notmuch_rb_cMessages)) {
        Data_Get_Struct(tags->parent, notmuch_rb_messages_t, messages);
        db = _notmuch_rb_messages_db(messages);
    }
    else
        rb_bug("tags' parent is neither thread nor message nor messages");

    if (db->nm_db && tags->nm_tags)
        notmuch_tags_destroy(tags->nm_tags);

    free(tags);
}
