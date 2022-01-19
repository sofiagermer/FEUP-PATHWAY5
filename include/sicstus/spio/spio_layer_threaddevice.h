#ifndef SPIO_LAYER_THREADDEVICE_H_INCLUDED
#define SPIO_LAYER_THREADDEVICE_H_INCLUDED 1

#include "spio_types.h"
/* #include "spio_threaddevice.h" */

#define SPIO_LAYER_THREADDEVICE_OPEN_OPTION_SMALL_BUF SPIO_PRIVATE_OPTION_1 /* dbg */

#define SPIO_LAYER_THREADDEVICE_OPEN_OPTION_PRIVATE_OPTION_1 SPIO_PRIVATE_OPTION_3
#define SPIO_LAYER_THREADDEVICE_OPEN_OPTION_PRIVATE_OPTION_2 SPIO_PRIVATE_OPTION_4
#define SPIO_LAYER_THREADDEVICE_OPEN_OPTION_PRIVATE_OPTION_3 SPIO_PRIVATE_OPTION_5
#define SPIO_LAYER_THREADDEVICE_OPEN_OPTION_PRIVATE_OPTION_4 SPIO_PRIVATE_OPTION_6
#define SPIO_LAYER_THREADDEVICE_OPEN_OPTION_PRIVATE_OPTION_5 SPIO_PRIVATE_OPTION_7

/* FIXME: clean up the flag/option names and meanings */

#define SPIO_LAYER_THREADDEVICE_FLAG_FD_OPEN_READ  SPIO_LAYER_FLAG_PRIVATE_1
#define SPIO_LAYER_THREADDEVICE_FLAG_FD_OPEN_WRITE SPIO_LAYER_FLAG_PRIVATE_2
#define SPIO_LAYER_THREADDEVICE_FLAG_USE_FSYNC SPIO_LAYER_FLAG_PRIVATE_3 /* use fsync in flush_output (for files) */
#define SPIO_LAYER_THREADDEVICE_FLAG_SOCKET SPIO_LAYER_FLAG_PRIVATE_4 /* a socket (needs shutdown etc) */
#define SPIO_LAYER_THREADDEVICE_FLAG_OBEY_FILE_OFFSET SPIO_LAYER_FLAG_PRIVATE_5
/* for fd opened with O_APPEND (unavailable on Windows) */
#define SPIO_LAYER_THREADDEVICE_FLAG_FD_O_APPEND SPIO_LAYER_FLAG_PRIVATE_6
/* if set then read and write positions cannot be moved independently (static property) */
#define SPIO_LAYER_THREADDEVICE_FLAG_SINGLE_POS SPIO_LAYER_FLAG_PRIVATE_7


extern void spio_layer_threaddevice_destroy(spio_t_layer *layer);

extern spio_t_error_code spio_init_layer_threaddevice(spio_t_bits options);

/* only for spio_layer_threaddevice_{unix,win32} */
extern spio_t_error_code spio_layer_threaddevice_ioctl_default(SPIO *s, spio_t_ioctl_operation operation, void *ioctl_param, spio_t_bits options);

extern spio_t_error_code spio_layer_threaddevice_open_file_handle_unix(SPIO *s, spio_t_os_file_handle hFile, spio_t_bits open_options);

#endif  /* SPIO_LAYER_THREADDEVICE_H_INCLUDED */
