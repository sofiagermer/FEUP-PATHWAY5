#ifndef SPIO_ZLIB_H_INCLUDED
#define SPIO_ZLIB_H_INCLUDED

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_buf.h"

typedef struct spio_t_zlib_deflate_state_ spio_t_zlib_deflate_state;
typedef struct spio_t_zlib_inflate_state_ spio_t_zlib_inflate_state;

extern spio_t_error_code spio_zlib_deflate_init(spio_t_zlib_deflate_state **state, spio_t_bits options);

extern spio_t_error_code spio_zlib_inflate_init(spio_t_zlib_inflate_state **state, spio_t_bits options);

/* Call without SPIO_ZLIB_DEFLATE_OPTION_Z_FINISH any number of
   times. Finally call with SPIO_ZLIB_DEFLATE_OPTION_Z_FINISH with no
   input data (*pfrom_buf_offset == from_buf->extent) until
   SPIO_S_DONE is returned. Finally call spio_zlib_deflate_end to free
   the deflate state */
#define SPIO_ZLIB_DEFLATE_OPTION_Z_FINISH SPIO_BIT(0)
#define SPIO_ZLIB_DEFLATE_OPTION_EOF SPIO_ZLIB_DEFLATE_OPTION_Z_FINISH
extern spio_t_error_code spio_zlib_deflate(spio_t_zlib_deflate_state *state, spio_t_buf *from_buf, size_t *pfrom_buf_offset, spio_t_buf *to_buf, spio_t_bits options);

/* Keep calling with input and output data until SPIO_S_DONE is
   returned at which point all compressed input has been consumed and
   all output has been produced. */
/* There is no more input data forthcoming. If there is no input data
   then report SPIO_E_ENCODING_INVALID if SPIO_S_DONE has not already
   happened. */
#define SPIO_ZLIB_INFLATE_OPTION_EOF SPIO_BIT(0)
extern spio_t_error_code spio_zlib_inflate(spio_t_zlib_inflate_state *state, spio_t_buf *from_buf, size_t *pfrom_buf_offset, spio_t_buf *to_buf, spio_t_bits options);

/* Free the initialized (or NULL) deflate state */
extern void spio_zlib_deflate_end(spio_t_zlib_deflate_state *state, spio_t_bits options);

/* Free the initialized (or NULL) inflate state */
void spio_zlib_inflate_end(spio_t_zlib_inflate_state *state, spio_t_bits options);

#endif  /* SPIO_ZLIB_H_INCLUDED */
