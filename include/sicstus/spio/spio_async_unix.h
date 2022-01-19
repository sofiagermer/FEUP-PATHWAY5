#ifndef SPIO_ASYNC_UNIX_H_INCLUDED
#define SPIO_ASYNC_UNIX_H_INCLUDED 1

#include "spio_types.h"

#if SPIO_ASYNC_INTERNAL

extern void spio_async_free_unix(spio_t_async_info *async_info);
extern spio_t_error_code spio_async_poll_for_completion_unix(spio_t_async_info *async_info);

#endif  /* SPIO_ASYNC_INTERNAL */

/* OS-specific (file-)operations */

/* All spio_async_unix_... takes these */
#define SPIO_ASYNC_UNIX_OPTION_PREFER_AIO SPIO_BIT(0)
#define SPIO_ASYNC_UNIX_OPTION_OBEY_FILE_OFFSET SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_PREFER_AIO)

/* device type */
#define SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_FILE SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_OBEY_FILE_OFFSET)
#define SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_PIPE SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_FILE)
#define SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_TTY SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_PIPE)
#define SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_SOCKET SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_TTY)
#define SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_UNKNOWN SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_SOCKET)

#define SPIO_ASYNC_UNIX_OPTION_SEEKABLE_OFFSET SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_UNKNOWN)
/* File descriptor is opened with O_APPEND, this affects at least write and seek */
#define SPIO_ASYNC_UNIX_OPTION_FD_O_APPEND SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_SEEKABLE_OFFSET)

/* FIXME: use this */
#define SPIO_ASYNC_UNIX_OPTION_SMALL_BUF SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_FD_O_APPEND) /* DBG */

/* These options should be passed on between all spio_async_unix_... */
#define SPIO_ASYNC_UNIX_OPTION_COMMON_MASK ( 0                                            \
                                             | SPIO_ASYNC_UNIX_OPTION_PREFER_AIO          \
                                             | SPIO_ASYNC_UNIX_OPTION_OBEY_FILE_OFFSET    \
                                             | SPIO_ASYNC_UNIX_OPTION_SMALL_BUF           \
                                             | SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_FILE    \
                                             | SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_PIPE    \
                                             | SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_TTY     \
                                             | SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_SOCKET  \
                                             | SPIO_ASYNC_UNIX_OPTION_DEVICE_TYPE_UNKNOWN \
                                             | SPIO_ASYNC_UNIX_OPTION_SEEKABLE_OFFSET     \
                                             | SPIO_ASYNC_UNIX_OPTION_FD_O_APPEND         \
                                            )


/* private options for spio_async_unix_... to use */
#define SPIO_ASYNC_UNIX_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_SMALL_BUF)
#define SPIO_ASYNC_UNIX_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_PRIVATE_1)
#define SPIO_ASYNC_UNIX_OPTION_PRIVATE_3 SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_PRIVATE_2)
#define SPIO_ASYNC_UNIX_OPTION_PRIVATE_4 SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_PRIVATE_3)
#define SPIO_ASYNC_UNIX_OPTION_PRIVATE_5 SPIO_NEXT_BIT(SPIO_ASYNC_UNIX_OPTION_PRIVATE_4)



extern spio_t_error_code spio_async_unix_read(spio_t_async_info **pasync_info, spio_t_offset file_offset, spio_t_os_file_handle hFile, spio_t_event *event, spio_t_bits options);

extern spio_t_error_code spio_async_unix_write(spio_t_async_info **pasync_info,  spio_t_buf *buf, spio_t_offset file_offset, spio_t_os_file_handle hFile, spio_t_event *event, spio_t_bits options);
extern spio_t_error_code spio_async_unix_fsync(spio_t_async_info **pasync_info,  spio_t_os_file_handle hFile, spio_t_event *event, spio_t_bits options);

extern spio_t_error_code spio_async_suspend_unix(spio_t_async_info * const list[], int nent, spio_t_timespec* timeout);
extern spio_t_error_code spio_async_cancel_unix(spio_t_async_info *async_info);

extern spio_t_error_code spio_init_async_unix(spio_t_bits options);

#endif  /* SPIO_ASYNC_UNIX_H_INCLUDED */
