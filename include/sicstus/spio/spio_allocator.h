#ifndef SPIO_ALLOCATOR_H_INCLUDED
#define SPIO_ALLOCATOR_H_INCLUDED 1

#include <stddef.h>
#include "spio_types.h"
#include "spio_errors.h"
#include "spio_debug.h"


/* The allocator hook does malloc,realloc and free like traditional realloc  */
typedef void * spio_t_allocator_hook (void *cookie, void *ptr, size_t size);

/*
  FIXME: eventually we should have spio_t_allocator.ic that
  instantiates a local allocator when included. this way spio.c and
  spio_buf.c could have their own allocators independently of each
  other
*/

spio_t_error_code spio_init_allocator(spio_t_allocator_hook *fun, void *cookie, spio_t_bits options);
spio_t_error_code spio_deinit_allocator(void);

#if SPIO_DEBUG
#define spio_allocator_alloc(SIZE) spio_allocator_alloc_((SIZE) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_allocator_realloc(PTR, SIZE) spio_allocator_realloc_((PTR), (SIZE) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_allocator_free(PTR) spio_allocator_free_((PTR) SPIO_ALLOCATOR_DEBUG_ARGS)
#else  /* !SPIO_DEBUG */
#define spio_allocator_alloc_ spio_allocator_alloc
#define spio_allocator_realloc_ spio_allocator_realloc
#define spio_allocator_free_ spio_allocator_free
#endif  /* !SPIO_DEBUG */



void *spio_allocator_alloc_(size_t size SPIO_ALLOCATOR_DEBUG_ARGS_DECL);
void *spio_allocator_realloc_(void *p, size_t size SPIO_ALLOCATOR_DEBUG_ARGS_DECL);
void spio_allocator_free_(void *p SPIO_ALLOCATOR_DEBUG_ARGS_DECL);

#endif /* SPIO_ALLOCATOR_H_INCLUDED */

