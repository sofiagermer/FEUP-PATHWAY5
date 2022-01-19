#ifndef SPIO_BUF_H_INCLUDED
#define SPIO_BUF_H_INCLUDED 1

#include "spio_types.h"
#include "spio_allocator.h"
#include "spio_utils.h"
#include "spio_errors.h"

/* 
   SPIO buffers.

   a SPIO buffer (spio_t_buf) has a growable byte vector BUF, a fill
   pointer (EXTENT) and a (current) maximum size (WRITABLE_BYTES).
   
   By default, when adding data to a spio_t_buf it should be added at
   offset EXTENT, up to offset WRITABLE_BYTES (exclusive), and EXTENT
   incremented with the number of bytes written.
   
   By default, when reading data from a spio_t_buf it should be read
   from offset zero up to EXTENT (exclusive).

   The BUF field is normally dynamically allocated so it can be
   reallocated, by the buffer functions, to make room for more (or
   less) data.

   However, if SPIO_BUF_F_STATIC_BUF is set then BUF was allocated by
   some other means and the following additional cases applies:

   -- writable_size == 0, this means that the BUF cannot be written
   to. The BUF can be allocated and copied though to make writing
   possible (e.g., by spio_buf_reserve). (WRITABLE_SIZE > 0 whenever
   the buffer data is dynamically allocated).

   -- SPIO_BUF_F_FIXED_BUF, this means that the BUF can be written to
   but that it cannot change size nor be replaced. In essence, it
   makes WRITABLE_SIZE read-only. This is used so that you can wrap a
   fixed size byte vector in a spio_t_buf. Any attempt to resize such
   a spio_t_buf will barf with SPIO_E_???.


   The spio_t_buf struct may be statically or dynamically
   allocated. This only affect whether spio_buf_free should free the
   struct or not, it does not affect whether the byte vector BUF
   should be freed or not. Flagged by SPIO_BUF_F_STATIC_STRUCT.


 */


typedef struct spio_t_buf_ spio_t_buf;
struct spio_t_buf_ {
  spio_t_uint8 *buf;
  size_t extent;         /* buf[0] .. buf[extent-1] is available */
  size_t writable_size;  /* 0<= extent <= writable_size if !SPIO_BUF_F_STATIC_BUF */
  spio_t_bits flags;          /*  */
  spio_t_bits pad;            /*  */
};


#define SPIO_BUF_INITIALIZER_(BUF,EXTENT,WRITABLE_SIZE,FLAGS) \
  {                  \
    (BUF),           \
    (EXTENT),        \
    (WRITABLE_SIZE), \
    (FLAGS),         \
    /*pad=*/ 0       \
  }

#define SPIO_BUF_F_FREED         SPIO_BIT(0) /* The buf has been freed. For debug. */
#define SPIO_BUF_F_STATIC_BUF    SPIO_BIT(1) /* the buf.buf is static (if writable_size==0 then it is also read-only) */
#define SPIO_BUF_F_FIXED_BUF     SPIO_BIT(3) /* the buf.buf is not reallocatable (combine with SPIO_BUF_F_STATIC_BUF) */
#define SPIO_BUF_F_STATIC_STRUCT SPIO_BIT(2) /* the struct spio_t_buf_ is static */


#define SPIO_BUF_IS_EMPTY(BUF) ((BUF)->extent == 0)

#define SPIO_DECLARE_INDIRECT_RO_BUF(NAME, SIZE, DATA)              \
 spio_t_buf NAME ## _SPIO_STATIC_RO =                               \
   SPIO_BUF_INITIALIZER_(                                           \
      /* buf= */ (DATA),                                            \
      /* extent= */ (SIZE),                                         \
      /* writable_size= */ 0,                                       \
      /* flags= */ (SPIO_BUF_F_STATIC_BUF|SPIO_BUF_F_STATIC_STRUCT) \
      );                                                            \
 spio_t_buf *NAME = &NAME ## _SPIO_STATIC_RO

#define SPIO_DECLARE_INDIRECT_RW_BUF(NAME, SIZE, DATA)              \
 spio_t_buf NAME ## _SPIO_STATIC_RW =                               \
   SPIO_BUF_INITIALIZER_(                                           \
      /* buf= */ (DATA),          \
      /* extent= */ (SIZE),                                         \
      /* writable_size= */ (SIZE),                                  \
      /* flags= */ (SPIO_BUF_F_STATIC_BUF|SPIO_BUF_F_STATIC_STRUCT) \
      );                                                            \
 spio_t_buf *NAME = &NAME ## _SPIO_STATIC_RW

#define SPIO_DECLARE_EMPTY_STATIC_BUF(NAME) SPIO_DECLARE_INDIRECT_RO_BUF(NAME, 0, NULL)

/* initially empty (extent == 0) with SIZE bytes available for writing */
#define SPIO_DECLARE_INDIRECT_FIXED_RW_BUF(NAME, SIZE, DATA)        \
 spio_t_buf NAME ## _SPIO_STATIC_FIXED_RW =                         \
   SPIO_BUF_INITIALIZER_(                                           \
      /* buf= */ (DATA),                                            \
      /* extent= */ 0,                                              \
      /* writable_size= */ (SIZE),                                  \
      /* flags= */ (SPIO_BUF_F_FIXED_BUF|SPIO_BUF_F_STATIC_BUF|SPIO_BUF_F_STATIC_STRUCT) \
      );                                                            \
 spio_t_buf *NAME = &NAME ## _SPIO_STATIC_FIXED_RW

#if SPIO_DEBUG
#define spio_buf_alloc(DATA,SIZE,OPTIONS) spio_buf_alloc_((DATA),(SIZE),(OPTIONS) SPIO_ALLOCATOR_DEBUG_ARGS)

/* #define spio_buf_realloc(PTR, SIZE) spio_buf_realloc_((PTR), (SIZE) SPIO_ALLOCATOR_DEBUG_ARGS) */
#define spio_buf_reserve(PTR, EXTRA) spio_buf_reserve_((PTR), (EXTRA) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_buf_reset(PTR, SIZE) spio_buf_reset_((PTR), (SIZE) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_buf_free(PTR) spio_buf_free_((PTR) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_buf_trim(PTR) spio_buf_trim_((PTR) SPIO_ALLOCATOR_DEBUG_ARGS)
#define spio_buf_get_buf(PTR, PDATA, PSIZE) spio_buf_get_buf_((PTR), (PDATA), (PSIZE) SPIO_ALLOCATOR_DEBUG_ARGS)
#else  /* !SPIO_DEBUG */
#define spio_buf_alloc_ spio_buf_alloc

/* #define spio_buf_realloc_ spio_buf_realloc */
#define spio_buf_reserve_ spio_buf_reserve
#define spio_buf_reset_ spio_buf_reset
#define spio_buf_free_ spio_buf_free
#define spio_buf_trim_ spio_buf_trim
#define spio_buf_get_buf_ spio_buf_get_buf
#endif  /* !SPIO_DEBUG */


extern spio_t_error_code spio_init_buf(spio_t_bits options); /* init spio_buf.c module */

/* a size of zero means allocate some default size (SPIO_DEFAULT_BUFSIZE) */
#define SPIO_BUF_ALLOC_OPTION_FREE_DATA SPIO_BIT(0) /* free the data (with spio_free) eventually (buffer owns data) */
#define SPIO_BUF_ALLOC_OPTION_READ_ONLY SPIO_NEXT_BIT(SPIO_BUF_ALLOC_OPTION_FREE_DATA) /* data can only be read, not written */
#define SPIO_BUF_ALLOC_OPTION_COPY_DATA SPIO_NEXT_BIT(SPIO_BUF_ALLOC_OPTION_READ_ONLY) /* allocate our own buffer, with a copy of data */
#define SPIO_BUF_ALLOC_OPTION_FIXED_SIZE SPIO_NEXT_BIT(SPIO_BUF_ALLOC_OPTION_COPY_DATA) /* not allowed to change size */

extern spio_t_buf *spio_buf_alloc_(void *data, size_t size, spio_t_bits options SPIO_ALLOCATOR_DEBUG_ARGS_DECL);

#if 0
#error "unclear semantics if SPIO_BUF_F_FIXED_BUF"
   /* will never move the struct */
   extern spio_t_error_code spio_buf_realloc_(spio_t_buf *, size_t size SPIO_ALLOCATOR_DEBUG_ARGS_DECL);
#endif

/* will never move the struct */
extern spio_t_error_code spio_buf_reserve_(spio_t_buf *, size_t extra SPIO_ALLOCATOR_DEBUG_ARGS_DECL);

/* will never move the struct */
extern spio_t_error_code spio_buf_reset_(spio_t_buf *, size_t size SPIO_ALLOCATOR_DEBUG_ARGS_DECL);

/* allowed to shrink buffer to extent if deemed beneficial */
/* will never move the struct */
extern spio_t_error_code spio_buf_trim_(spio_t_buf * SPIO_ALLOCATOR_DEBUG_ARGS_DECL);

extern void spio_buf_free_(spio_t_buf * SPIO_ALLOCATOR_DEBUG_ARGS_DECL);



#define SPIO_BUF_CONCAT_OPTION_ALLOW_SWITCH SPIO_BIT(0)

extern spio_t_error_code spio_buf_concat(spio_t_buf *buf, spio_t_buf *suffix, spio_t_bits options);
extern spio_t_error_code spio_buf_concat_bytes(spio_t_buf *buf, void const *bytes, size_t nbytes);
extern spio_t_error_code spio_buf_concat_string(spio_t_buf *buf, char const *string);

extern spio_t_error_code spio_buf_push(spio_t_buf *buf, spio_t_uint8 byte);
extern spio_t_error_code spio_buf_pop(spio_t_buf *buf, spio_t_uint8 *pbyte);

/* positive amount removes from start of buffer, negative from extent */
extern spio_t_error_code spio_buf_contract(spio_t_buf *buf, spio_t_ssize amount);

/* initializes already allocated storage at buf to an empty static buf  */
extern void spio_buf_init_buf(spio_t_buf *buf);

/* returning the previous data (spio_alloced) of buf, resetting it to empty if possible, otherwise copying the data*/
spio_t_error_code spio_buf_get_buf_(spio_t_buf *buf, spio_t_uint8 **pdata, size_t *psize SPIO_ALLOCATOR_DEBUG_ARGS_DECL);


#endif  /* SPIO_BUF_H_INCLUDED */
