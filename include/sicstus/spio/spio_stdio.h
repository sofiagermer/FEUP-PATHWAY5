#ifndef SPIO_STDIO_H_INCLUDED
#define SPIO_STDIO_H_INCLUDED 1

#include "spio.h"
#include "spio_errors.h"

typedef struct spio_t_file_ spio_t_file;
typedef spio_t_file SPIOFILE;

/* return value is spio_t_error_code if < 0, otherwise spio_t_wchar */
spio_t_wcharint spiofile_get_char(spio_t_file *f);
/* return value is spio_t_error_code if < 0, otherwise spio_t_uint8 */
spio_t_wcharint spiofile_get_byte(spio_t_file *f);

spio_t_error_code spiofile_put_char(spio_t_file *f, spio_t_wchar);
spio_t_error_code spiofile_put_byte(spio_t_file *f, spio_t_uint8);

spio_t_error_code spiofile_flush_output(spio_t_file *f, spio_t_bits options);

#define SPIOFILE_SEEK_OPTION_SET SPIO_BIT(0)
spio_t_error_code spiofile_seek(spio_t_file *f, spio_t_offset offset, spio_t_bits options);
spio_t_error_code spiofile_tell(spio_t_file *f, spio_t_offset *poffset, spio_t_bits options);

spio_t_error_code spiofile_clrerr(spio_t_file *f, spio_t_bits options);
spio_t_error_code spiofile_seen_eof(spio_t_file *f);

spio_t_error_code spiofile_open_file(spio_t_file **pf, char const *pathname, char const *charset, spio_t_bits options);

#define SPIOFILE_OPEN_OPTION_DUMMY_ SPIO_BIT(0)
#define SPIOFILE_OPEN_OPTION_READ   SPIO_NEXT_BIT(SPIOFILE_OPEN_OPTION_DUMMY_)
#define SPIOFILE_OPEN_OPTION_WRITE  SPIO_NEXT_BIT(SPIOFILE_OPEN_OPTION_READ)
#define SPIOFILE_OPEN_OPTION_APPEND SPIO_NEXT_BIT(SPIOFILE_OPEN_OPTION_WRITE)
#define SPIOFILE_OPEN_OPTION_TEXT   SPIO_NEXT_BIT(SPIOFILE_OPEN_OPTION_APPEND)
#define SPIOFILE_OPEN_OPTION_BINARY SPIO_NEXT_BIT(SPIOFILE_OPEN_OPTION_TEXT)
/* Consider a SPIOFILE_OPEN_OPTION_REPOSITION, see spio.txt */
#define SPIOFILE_OPEN_OPTION_SEEK SPIO_NEXT_BIT(SPIOFILE_OPEN_OPTION_BINARY)

spio_t_error_code spiofile_open(spio_t_file **pf, SPIO *s, spio_t_bits options);

#define SPIOFILE_CLOSE_OPTION_ABORT SPIO_BIT(0)
spio_t_error_code spiofile_close(spio_t_file *f, spio_t_bits options);

#endif  /* SPIO_STDIO_H_INCLUDED */
