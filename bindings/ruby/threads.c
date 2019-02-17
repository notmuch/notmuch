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

/*
 * call-seq: THREADS.destroy! => nil
 *
 * Destroys the threads, freeing all resources allocated for it.
 */
VALUE
notmuch_rb_threads_destroy (VALUE self)
{
    notmuch_threads_t *threads;

    Data_Get_Struct (self, notmuch_threads_t, threads);

    notmuch_threads_destroy (threads);
    DATA_PTR (self) = NULL;

    return Qnil;
}

/* call-seq: THREADS.each {|item| block } => THREADS
 *
 * Calls +block+ once for each thread in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_threads_each (VALUE self)
{
    notmuch_thread_t *thread;
    notmuch_threads_t *threads;

    Data_Get_Notmuch_Threads (self, threads);

    for (; notmuch_threads_valid (threads); notmuch_threads_move_to_next (threads)) {
	thread = notmuch_threads_get (threads);
	rb_yield (Data_Wrap_Struct (notmuch_rb_cThread, NULL, NULL, thread));
    }

    return self;
}
