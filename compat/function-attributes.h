/* function-attributes.h - Provides compiler abstractions for
 *                         function attributes
 *
 * Copyright (c) 2012 Justus Winter <4winter@informatik.uni-hamburg.de>
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
 */

#ifndef FUNCTION_ATTRIBUTES_H
#define FUNCTION_ATTRIBUTES_H

/* clang provides this macro to test for support for function
 * attributes. If it isn't defined, this provides a compatibility
 * macro for other compilers.
 */
#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

/* Provide a NORETURN_ATTRIBUTE macro similar to PRINTF_ATTRIBUTE from
 * talloc.
 *
 * This attribute is understood by gcc since version 2.5. clang
 * provides support for testing for function attributes.
 */
#ifndef NORETURN_ATTRIBUTE
#if (__GNUC__ >= 3 ||				\
     (__GNUC__ == 2 && __GNUC_MINOR__ >= 5) ||	\
     __has_attribute (noreturn))
#define NORETURN_ATTRIBUTE __attribute__ ((noreturn))
#else
#define NORETURN_ATTRIBUTE
#endif
#endif

#endif
