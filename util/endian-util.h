/* this file mimics the macros present in recent GCC and CLANG */

#ifndef _ENDIAN_UTIL_H
#define _ENDIAN_UTIL_H

/* This are prefixed with UTIL to avoid collisions
 *
 * You can use something like the following to define UTIL_BYTE_ORDER
 * in a configure script.
 */
#if 0
#include <stdio.h>
#include <stdint.h>
uint32_t test = 0x34333231;
int main() { printf("%.4s\n", (const char*)&test); return 0; }
#endif

#define UTIL_ORDER_BIG_ENDIAN	  4321
#define UTIL_ORDER_LITTLE_ENDIAN  1234


#if !defined(UTIL_BYTE_ORDER) || ((UTIL_BYTE_ORDER != UTIL_ORDER_BIG_ENDIAN) && \
				  (UTIL_BYTE_ORDER != UTIL_ORDER_LITTLE_ENDIAN))
#undef UTIL_BYTE_ORDER
#ifdef __BYTE_ORDER__
#  if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#    define UTIL_BYTE_ORDER UTIL_ORDER_LITTLE_ENDIAN
#  elif (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#    define UTIL_BYTE_ORDER UTIL_ORDER_BIG_ENDIAN
#  else
#    error "Unsupported __BYTE_ORDER__"
#  endif
#else
#  error "UTIL_BYTE_ORDER not correctly defined and __BYTE_ORDER__ not defined."
#endif
#endif

#endif
