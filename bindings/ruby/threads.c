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

/* call-seq: THREADS.each {|item| block } => THREADS
 *
 * Calls +block+ once for each thread in +self+, passing that element as a
 * parameter.
 */
VALUE
notmuch_rb_threads_each(VALUE self)
{
    notmuch_rb_thread_t *thread;
    notmuch_rb_threads_t *threads;
    VALUE threadv;

    Data_Get_Struct(self, notmuch_rb_threads_t, threads);
    if (!threads->nm_threads)
        return self;

    for (; notmuch_threads_valid(threads->nm_threads);
            notmuch_threads_move_to_next(threads->nm_threads))
    {
        threadv = Data_Make_Struct(notmuch_rb_cThread, notmuch_rb_thread_t,
                notmuch_rb_thread_mark, notmuch_rb_thread_free, thread);
        thread->nm_thread = notmuch_threads_get(threads->nm_threads);
        thread->threads = self;
        rb_yield(threadv);
    }

    return self;
}
