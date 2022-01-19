#ifndef SPIO_INTERNALS_H_INCLUDED
#define SPIO_INTERNALS_H_INCLUDED 1

#ifdef SPIO_TYPES_H_INCLUDED
#error "Must not include spio_internals.h after spio_types.h"
#endif  /* SPIO_TYPES_H_INCLUDED */

#ifndef SPIO_INCLUDE_OS_TYPES
#define SPIO_INCLUDE_OS_TYPES 1 /* see spio_types.h */
#endif  /* SPIO_INCLUDE_OS_TYPES */

#include "spio_types.h"
#ifndef SPIO_TYPES_H_INCLUDED
#error "spio_types.h did not set SPIO_TYPES_H_INCLUDED"
#endif  /* SPIO_TYPES_H_INCLUDED */

#include "spio_debug.h"

#define BARF SPIO_BARF
#define CHECK SPIO_CHECK
#define SPIO_BARF_LABEL barf
#define NULL_CHECK SPIO_NULL_CHECK
#define POSIX_CHECK SPIO_POSIX_CHECK
#define POSIX_CHECK1 SPIO_POSIX_CHECK1
#define WIN32_CHECK SPIO_WIN32_CHECK
#define WIN32_CHECK1 SPIO_WIN32_CHECK1
#define BARF_LAST_ERROR SPIO_BARF_LAST_ERROR
#define HANDLE_CHECK SPIO_HANDLE_CHECK
#if 0
#define CHECK_LAST_ERROR SPIO_CHECK_LAST_ERROR
#endif  /* 0 */

#define SPIO_ALLOC_PTR(PTR) NULL_CHECK((PTR) = spio_alloc(sizeof *(PTR)))

#endif  /* SPIO_INTERNALS_H_INCLUDED */
